import * as THREE from 'three';

// Painted floor and wall footprints for faked lights (#782). One small
// irradiance atlas (floor, back, left and right walls) is accumulated on the
// GPU in a single draw from a light table, independent of screen resolution,
// then sampled by the room surface materials as extra diffuse light. Full
// lights keep lighting the room through their real SpotLight, so they are not
// added here. Footprints are unshadowed: trusses do not occlude faked lights.

export const FOOTPRINT_TILES = Object.freeze(['floor', 'back', 'left', 'right']);
const TABLE_ROWS = 4; // texels per light in the table texture

const vertexShader = `out vec2 vUv;
void main(){vUv=uv;gl_Position=vec4(position.xy,0.,1.);}`;

const fragmentShader = `precision highp float;
in vec2 vUv;
uniform sampler2D lightTable;
uniform int lightCount;
uniform vec3 room;
out vec4 irradiance;
// Atlas tiles: (0,0) floor, (1,0) back wall, (0,1) left wall, (1,1) right wall.
void surface(vec2 uv,out vec3 position,out vec3 normal){
  vec2 tile=floor(uv*2.),local=fract(uv*2.);
  float w=room.x,h=room.y,d=room.z;
  if(tile.y<.5&&tile.x<.5){position=vec3((local.x-.5)*w,0.,(local.y-.5)*d);normal=vec3(0.,1.,0.);}
  else if(tile.y<.5){position=vec3((local.x-.5)*w,local.y*h,-.5*d);normal=vec3(0.,0.,1.);}
  else if(tile.x<.5){position=vec3(-.5*w,local.y*h,(local.x-.5)*d);normal=vec3(1.,0.,0.);}
  else{position=vec3(.5*w,local.y*h,(local.x-.5)*d);normal=vec3(-1.,0.,0.);}
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
  constructor(renderer, {size = 512, capacity = 128} = {}) {
    this.renderer = renderer; this.capacity = capacity; this.count = 0; this.dirty = true;
    this.target = new THREE.WebGLRenderTarget(size, size, {type: THREE.HalfFloatType, depthBuffer: false,
      minFilter: THREE.LinearFilter, magFilter: THREE.LinearFilter, generateMipmaps: false});
    this.table = new THREE.DataTexture(new Float32Array(TABLE_ROWS * capacity * 4), TABLE_ROWS, capacity,
      THREE.RGBAFormat, THREE.FloatType);
    this.table.minFilter = this.table.magFilter = THREE.NearestFilter; this.table.needsUpdate = true;
    this.uniforms = {lightTable: {value: this.table}, lightCount: {value: 0}, room: {value: new THREE.Vector3(12, 5, 10)}};
    this.material = new THREE.ShaderMaterial({vertexShader, fragmentShader, uniforms: this.uniforms,
      glslVersion: THREE.GLSL3, depthTest: false, depthWrite: false, blending: THREE.NoBlending, toneMapped: false});
    this.quad = new THREE.Mesh(new THREE.PlaneGeometry(2, 2), this.material); this.quad.frustumCulled = false;
    this.scene = new THREE.Scene(); this.scene.add(this.quad); this.camera = new THREE.Camera();
    // Shared with every patched room material.
    this.materialUniforms = {tolglowFootprints: {value: this.target.texture}, tolglowRoom: {value: new THREE.Vector3(12, 5, 10)},
      tolglowFootprintStrength: {value: 0}};
  }

  setRoom(room) {
    this.uniforms.room.value.fromArray(room); this.materialUniforms.tolglowRoom.value.fromArray(room); this.dirty = true;
  }

  /** lights: [{virtualPosition, apexDistance, direction, outerCos, innerCos, color (rgb × intensity), range}] */
  update(lights) {
    const n = Math.min(this.capacity, lights.length), data = this.table.image.data;
    let changed = n !== this.count;
    for (let i = 0; i < n; i++) {
      const light = lights[i], row = i * TABLE_ROWS * 4;
      const values = [...light.virtualPosition, light.apexDistance, ...light.direction, light.outerCos,
        ...light.color, light.innerCos, light.range, 0, 0, 0];
      for (let k = 0; k < values.length; k++) if (data[row + k] !== values[k]) { data[row + k] = values[k]; changed = true; }
    }
    this.count = n;
    if (changed) { this.table.needsUpdate = true; this.dirty = true; }
    this.materialUniforms.tolglowFootprintStrength.value = n && !this.disabled ? 1 : 0;
  }

  /** Re-accumulate only when the light table or room changed. */
  render() {
    if (!this.dirty) return false;
    this.dirty = false;
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

// Room surface materials sample the atlas tile facing their normal, weighted by
// proximity to that room plane, and add the irradiance as Lambert diffuse.
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
uniform sampler2D tolglowFootprints; uniform vec3 tolglowRoom; uniform float tolglowFootprintStrength;
vec3 tolglowFootprint(vec3 p, vec3 n){
  if(tolglowFootprintStrength<=0.) return vec3(0.);
  vec3 a=abs(n); vec2 uv; float gap;
  if(a.y>=a.x&&a.y>=a.z){ if(n.y<0.) return vec3(0.);
    uv=vec2(p.x/tolglowRoom.x+.5,p.z/tolglowRoom.z+.5)*.5; gap=p.y; }
  else if(a.z>=a.x){ if(n.z<0.) return vec3(0.);
    uv=vec2(.5+(p.x/tolglowRoom.x+.5)*.5,clamp(p.y/tolglowRoom.y,0.,1.)*.5); gap=p.z+.5*tolglowRoom.z; }
  else if(n.x>0.){ uv=vec2((p.z/tolglowRoom.z+.5)*.5,.5+clamp(p.y/tolglowRoom.y,0.,1.)*.5); gap=p.x+.5*tolglowRoom.x; }
  else { uv=vec2(.5+(p.z/tolglowRoom.z+.5)*.5,.5+clamp(p.y/tolglowRoom.y,0.,1.)*.5); gap=.5*tolglowRoom.x-p.x; }
  float near=1.-smoothstep(.35,.9,abs(gap));
  if(near<=0.) return vec3(0.);
  vec2 tile=floor(uv*2.); vec2 inner=clamp(uv,tile*.5+.001,tile*.5+.499);
  return texture(tolglowFootprints,inner).rgb*near*tolglowFootprintStrength;
}
` + shader.fragmentShader.replace('#include <lights_fragment_end>', `#include <lights_fragment_end>
  reflectedLight.directDiffuse += tolglowFootprint(tolglowWorld, normalize(tolglowWorldNormal)) * BRDF_Lambert(material.diffuseColor);`);
  };
  material.customProgramCacheKey = () => (previousKey ? previousKey.call(material) : '') + '+tolglow-footprints-v1';
}
