import * as THREE from 'three';

// Painted floor and wall footprints for faked lights (#782). One small
// irradiance atlas (floor, back, left and right walls, stage top and stage
// front) is accumulated on the GPU in a single draw from a light table,
// independent of screen resolution, then sampled by the room surface materials
// as extra diffuse light. Full lights keep lighting the room through their real
// SpotLight; during a cross-fade each side carries its share of the weight.
// Footprints are unshadowed: trusses do not occlude faked lights.

export const FOOTPRINT_TILES = Object.freeze(['floor', 'back', 'left', 'right', 'stage-top', 'stage-front']);
const TABLE_ROWS = 4; // texels per light in the table texture

/** Stage box from room.js: half width, top height, back z and front z. */
export function stageBounds([width, , depth]) {
  const stageDepth = Math.min(2.3, depth * .27), stageZ = -depth / 2 + stageDepth / 2 + .16;
  return [width * .68 / 2, .42, stageZ - stageDepth / 2, stageZ + stageDepth / 2];
}

/** Texels per tile: about 5 cm or finer up to a 25 m room, at most 512. */
export function footprintTileSize(room) {
  return Math.max(...room) > 12.8 ? 512 : 256;
}

const vertexShader = `out vec2 vUv;
void main(){vUv=uv;gl_Position=vec4(position.xy,0.,1.);}`;

const fragmentShader = `precision highp float;
in vec2 vUv;
uniform sampler2D lightTable;
uniform int lightCount;
uniform vec3 room;
uniform vec4 stage;
out vec4 irradiance;
// 3 x 2 tiles: floor, back, left / right, stage top, stage front.
void surface(vec2 uv,out vec3 position,out vec3 normal){
  vec2 tile=floor(uv*vec2(3.,2.)),local=fract(uv*vec2(3.,2.));
  float w=room.x,h=room.y,d=room.z;
  int index=int(tile.x+3.*tile.y);
  if(index==0){position=vec3((local.x-.5)*w,0.,(local.y-.5)*d);normal=vec3(0.,1.,0.);}
  else if(index==1){position=vec3((local.x-.5)*w,local.y*h,-.5*d);normal=vec3(0.,0.,1.);}
  else if(index==2){position=vec3(-.5*w,local.y*h,(local.x-.5)*d);normal=vec3(1.,0.,0.);}
  else if(index==3){position=vec3(.5*w,local.y*h,(local.x-.5)*d);normal=vec3(-1.,0.,0.);}
  else if(index==4){position=vec3((local.x*2.-1.)*stage.x,stage.y,mix(stage.z,stage.w,local.y));normal=vec3(0.,1.,0.);}
  else{position=vec3((local.x*2.-1.)*stage.x,local.y*stage.y,stage.w);normal=vec3(0.,0.,1.);}
}
void main(){
  vec3 position,normal;surface(vUv,position,normal);
  vec3 sum=vec3(0.);
  for(int i=0;i<128;i++){
    if(i>=lightCount)break;
    vec4 a=texelFetch(lightTable,ivec2(0,i),0); // virtual apex, apex distance
    vec4 b=texelFetch(lightTable,ivec2(1,i),0); // direction, outer cosine
    vec4 c=texelFetch(lightTable,ivec2(2,i),0); // colour × intensity, inner cosine
    vec4 e=texelFetch(lightTable,ivec2(3,i),0); // range
    vec3 delta=position-a.xyz;float distance=length(delta);vec3 travel=delta/max(distance,.001);
    float cosine=dot(travel,b.xyz);
    if(cosine<=b.w||dot(delta,b.xyz)<a.w)continue;
    float lambert=dot(normal,-travel);
    if(lambert<=0.)continue;
    float spot=smoothstep(b.w,max(b.w+.00001,c.w),cosine);
    float range=e.x;
    float falloff=range>0.?pow(clamp(1.-pow(distance/range,4.),0.,1.),2.):1.;
    sum+=c.rgb*spot*lambert*falloff/max(.25,distance*distance);
  }
  irradiance=vec4(sum,1.);
}`;

export class FootprintAtlas {
  constructor(renderer, {capacity = 128} = {}) {
    this.renderer = renderer; this.capacity = capacity; this.count = 0; this.dirty = true; this.redraws = 0;
    this.tile = 256;
    this.target = new THREE.WebGLRenderTarget(3 * this.tile, 2 * this.tile, {type: THREE.HalfFloatType, depthBuffer: false,
      minFilter: THREE.LinearFilter, magFilter: THREE.LinearFilter, generateMipmaps: false});
    this.table = new THREE.DataTexture(new Float32Array(TABLE_ROWS * capacity * 4), TABLE_ROWS, capacity,
      THREE.RGBAFormat, THREE.FloatType);
    this.table.minFilter = this.table.magFilter = THREE.NearestFilter; this.table.needsUpdate = true;
    this.uniforms = {lightTable: {value: this.table}, lightCount: {value: 0}, room: {value: new THREE.Vector3(12, 5, 10)},
      stage: {value: new THREE.Vector4(...stageBounds([12, 5, 10]))}};
    this.material = new THREE.ShaderMaterial({vertexShader, fragmentShader, uniforms: this.uniforms,
      glslVersion: THREE.GLSL3, depthTest: false, depthWrite: false, blending: THREE.NoBlending, toneMapped: false});
    this.quad = new THREE.Mesh(new THREE.PlaneGeometry(2, 2), this.material); this.quad.frustumCulled = false;
    this.scene = new THREE.Scene(); this.scene.add(this.quad); this.camera = new THREE.Camera();
    // Shared with every patched room material.
    this.materialUniforms = {tolglowFootprints: {value: this.target.texture}, tolglowRoom: {value: new THREE.Vector3(12, 5, 10)},
      tolglowStage: {value: this.uniforms.stage.value}, tolglowFootprintStrength: {value: 0}};
  }

  setRoom(room) {
    this.uniforms.room.value.fromArray(room); this.materialUniforms.tolglowRoom.value.fromArray(room);
    this.uniforms.stage.value.set(...stageBounds(room));
    const tile = footprintTileSize(room);
    if (tile !== this.tile) { this.tile = tile; this.target.setSize(3 * tile, 2 * tile); }
    this.dirty = true;
  }

  /** lights: [{virtualPosition, apexDistance, direction, outerCos, innerCos, color (rgb × intensity), range}] */
  update(lights) {
    const n = Math.min(this.capacity, lights.length), data = this.table.image.data;
    let changed = n !== this.count;
    for (let i = 0; i < n; i++) {
      const light = lights[i], row = i * TABLE_ROWS * 4;
      const values = [...light.virtualPosition, light.apexDistance, ...light.direction, light.outerCos,
        ...light.color, light.innerCos, light.range, 0, 0, 0];
      // The table is float32: compare in that precision, or nothing ever matches.
      for (let k = 0; k < values.length; k++) {
        const value = Math.fround(values[k]);
        if (data[row + k] !== value) { data[row + k] = value; changed = true; }
      }
    }
    this.count = n;
    if (changed) { this.table.needsUpdate = true; this.dirty = true; }
    this.materialUniforms.tolglowFootprintStrength.value = n && !this.disabled ? 1 : 0;
  }

  /** Re-accumulate only when the light table or room changed, or the context was restored. */
  render() {
    if (!this.dirty) return false;
    this.dirty = false; this.redraws++;
    const renderer = this.renderer, previous = renderer.getRenderTarget();
    this.uniforms.lightCount.value = this.count;
    try { renderer.setRenderTarget(this.target); renderer.render(this.scene, this.camera); }
    finally { renderer.setRenderTarget(previous); }
    return true;
  }

  dispose() {
    this.target.dispose(); this.table.dispose(); this.material.dispose(); this.quad.geometry.dispose(); this.scene.clear();
  }
}

// Room surface materials sample the atlas tile facing their normal: the stage
// top and front where the fragment lies on them, otherwise the floor or wall
// plane, weighted by proximity to that plane, added as Lambert diffuse.
export function footprintMaterial(material, uniforms) {
  const previous = material.onBeforeCompile, previousKey = material.customProgramCacheKey;
  material.onBeforeCompile = (shader, renderer) => {
    previous?.call(material, shader, renderer);
    Object.assign(shader.uniforms, uniforms);
    shader.vertexShader = 'varying vec3 tolglowWorld; varying vec3 tolglowWorldNormal;\n' + shader.vertexShader.replace('#include <project_vertex>',
      `#include <project_vertex>
       tolglowWorld = (modelMatrix * vec4(transformed, 1.0)).xyz;
       tolglowWorldNormal = normalize(mat3(modelMatrix) * objectNormal);`);
    shader.fragmentShader = `varying vec3 tolglowWorld; varying vec3 tolglowWorldNormal;
uniform sampler2D tolglowFootprints; uniform vec3 tolglowRoom; uniform vec4 tolglowStage; uniform float tolglowFootprintStrength;
vec3 tolglowTile(float index, vec2 local){
  vec2 tile=vec2(mod(index,3.),floor(index/3.));
  vec2 texel=1./vec2(textureSize(tolglowFootprints,0));
  vec2 uv=(tile+clamp(local,0.,1.))/vec2(3.,2.);
  return texture(tolglowFootprints,clamp(uv,tile/vec2(3.,2.)+texel,(tile+1.)/vec2(3.,2.)-texel)).rgb;
}
vec3 tolglowFootprint(vec3 p, vec3 n){
  if(tolglowFootprintStrength<=0.) return vec3(0.);
  vec4 s=tolglowStage;
  bool onStage=abs(p.x)<=s.x+.02&&p.z>=s.z-.02&&p.z<=s.w+.02;
  if(n.y>.7&&onStage&&abs(p.y-s.y)<.12) return tolglowTile(4.,vec2(p.x/(2.*s.x)+.5,(p.z-s.z)/(s.w-s.z)))*tolglowFootprintStrength;
  if(n.z>.7&&abs(p.x)<=s.x+.02&&abs(p.z-s.w)<.08&&p.y<=s.y+.02) return tolglowTile(5.,vec2(p.x/(2.*s.x)+.5,p.y/s.y))*tolglowFootprintStrength;
  vec3 a=abs(n); vec2 local; float gap, index;
  if(a.y>=a.x&&a.y>=a.z){ if(n.y<0.) return vec3(0.);
    index=0.; local=vec2(p.x/tolglowRoom.x+.5,p.z/tolglowRoom.z+.5); gap=p.y; }
  else if(a.z>=a.x){ if(n.z<0.) return vec3(0.);
    index=1.; local=vec2(p.x/tolglowRoom.x+.5,p.y/tolglowRoom.y); gap=p.z+.5*tolglowRoom.z; }
  else if(n.x>0.){ index=2.; local=vec2(p.z/tolglowRoom.z+.5,p.y/tolglowRoom.y); gap=p.x+.5*tolglowRoom.x; }
  else { index=3.; local=vec2(p.z/tolglowRoom.z+.5,p.y/tolglowRoom.y); gap=.5*tolglowRoom.x-p.x; }
  float near=1.-smoothstep(.2,.5,abs(gap));
  if(near<=0.) return vec3(0.);
  return tolglowTile(index,local)*near*tolglowFootprintStrength;
}
` + shader.fragmentShader.replace('#include <lights_fragment_end>', `#include <lights_fragment_end>
  reflectedLight.directDiffuse += tolglowFootprint(tolglowWorld, normalize(tolglowWorldNormal)) * BRDF_Lambert(material.diffuseColor);`);
  };
  material.customProgramCacheKey = () => (previousKey ? previousKey.call(material) : '') + '+tolglow-footprints-v2';
}
