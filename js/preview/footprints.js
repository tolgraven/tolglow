import * as THREE from 'three';
import {stageDimensions} from './room.js';

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
export function stageBounds(room) {
  const {width, height, z, depth} = stageDimensions(room);
  return [width / 2, height, z - depth / 2, z + depth / 2];
}

// Tile planes: origin, u and v spans (metres) and the lit-side normal.
function tilePlanes([w, h, d], [hw, top, back, front]) {
  return [
    {origin: [-w / 2, 0, -d / 2], u: [w, 0, 0], v: [0, 0, d], normal: [0, 1, 0]},
    {origin: [-w / 2, 0, -d / 2], u: [w, 0, 0], v: [0, h, 0], normal: [0, 0, 1]},
    {origin: [-w / 2, 0, -d / 2], u: [0, 0, d], v: [0, h, 0], normal: [1, 0, 0]},
    {origin: [w / 2, 0, -d / 2], u: [0, 0, d], v: [0, h, 0], normal: [-1, 0, 0]},
    {origin: [-hw, top, back], u: [2 * hw, 0, 0], v: [0, 0, front - back], normal: [0, 1, 0]},
    {origin: [-hw, 0, front], u: [2 * hw, 0, 0], v: [0, top, 0], normal: [0, 0, 1]},
  ];
}

/**
 * Per tile, the indices of lights whose cone can reach it. A light is kept if
 * the bounding box of its axis and eight cone-edge rays' hits on the tile
 * plane overlaps the tile, or if any point of a 7 × 7 grid over the tile lies
 * inside its (slightly widened) cone within range. The first catches narrow
 * footprints between grid points, the second grazing and unbounded ones.
 */
export function footprintTileLights(lights, room, stage) {
  const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
  const planes = tilePlanes(room, stage), lists = planes.map(() => []);
  lights.forEach((light, index) => {
    const axis = light.direction, apex = light.virtualPosition;
    const helper = Math.abs(axis[1]) < .95 ? [0, 1, 0] : [1, 0, 0];
    const cross = (a, b) => [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]];
    const p = cross(helper, axis), pl = Math.hypot(...p), e1 = p.map(v => v / pl), e2 = cross(axis, e1);
    const sine = Math.sqrt(Math.max(0, 1 - light.outerCos ** 2)), cosine = light.outerCos;
    const rays = [axis];
    for (let k = 0; k < 8; k++) {
      const a = k * Math.PI / 4, c = Math.cos(a) * sine, s = Math.sin(a) * sine;
      rays.push([0, 1, 2].map(i => axis[i] * cosine + e1[i] * c + e2[i] * s));
    }
    planes.forEach((plane, tile) => {
      const toApex = [0, 1, 2].map(i => apex[i] - plane.origin[i]);
      if (dot(toApex, plane.normal) <= 0) return; // the light is behind this surface
      const uu = dot(plane.u, plane.u), vv = dot(plane.v, plane.v);
      let lo = [Infinity, Infinity], hi = [-Infinity, -Infinity], hits = 0;
      for (const ray of rays) {
        const facing = dot(ray, plane.normal);
        if (facing >= -1e-6) continue;
        const t = -dot(toApex, plane.normal) / facing;
        if (light.range > 0 && t > light.range) continue;
        const hit = [0, 1, 2].map(i => toApex[i] + ray[i] * t);
        const uvx = dot(hit, plane.u) / uu, uvy = dot(hit, plane.v) / vv;
        lo = [Math.min(lo[0], uvx), Math.min(lo[1], uvy)]; hi = [Math.max(hi[0], uvx), Math.max(hi[1], uvy)]; hits++;
      }
      const pad = .05 + .1 * Math.max(hi[0] - lo[0], hi[1] - lo[1], 0);
      if (hits && lo[0] - pad <= 1 && hi[0] + pad >= 0 && lo[1] - pad <= 1 && hi[1] + pad >= 0) { lists[tile].push(index); return; }
      const widened = Math.cos(Math.min(Math.PI / 2, Math.acos(Math.min(1, cosine)) + .05));
      for (let gy = 0; gy <= 6; gy++) for (let gx = 0; gx <= 6; gx++) {
        const point = [0, 1, 2].map(i => -toApex[i] + plane.u[i] * gx / 6 + plane.v[i] * gy / 6);
        const distance = Math.hypot(...point);
        if (distance < 1e-6 || (light.range > 0 && distance > light.range)) continue;
        if (dot(point, axis) / distance >= widened) { lists[tile].push(index); return; }
      }
    });
  });
  return lists;
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
uniform int tile, tileCount;
uniform int tileLights[128];
uniform vec3 room;
uniform vec4 stage;
out vec4 irradiance;
// 3 x 2 tiles: floor, back, left / right, stage top, stage front. Each draw
// covers one tile's viewport and only the lights whose cones can reach it.
void surface(vec2 local,out vec3 position,out vec3 normal){
  float w=room.x,h=room.y,d=room.z;
  int index=tile;
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
  for(int n=0;n<128;n++){
    if(n>=tileCount)break;
    int i=tileLights[n];
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
    this.lights = []; this.room = [12, 5, 10]; this.tileLit = new Array(6).fill(true); this.lastRender = -Infinity; this.tileDraws = 0;
    this.tile = 256;
    this.target = new THREE.WebGLRenderTarget(3 * this.tile, 2 * this.tile, {type: THREE.HalfFloatType, depthBuffer: false,
      minFilter: THREE.LinearFilter, magFilter: THREE.LinearFilter, generateMipmaps: false});
    this.table = new THREE.DataTexture(new Float32Array(TABLE_ROWS * capacity * 4), TABLE_ROWS, capacity,
      THREE.RGBAFormat, THREE.FloatType);
    this.table.minFilter = this.table.magFilter = THREE.NearestFilter; this.table.needsUpdate = true;
    this.uniforms = {lightTable: {value: this.table}, tile: {value: 0}, tileCount: {value: 0},
      tileLights: {value: new Int32Array(capacity)}, room: {value: new THREE.Vector3(12, 5, 10)},
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
    this.room = [...room]; this.tileLit.fill(true);
    this.uniforms.room.value.fromArray(room); this.materialUniforms.tolglowRoom.value.fromArray(room);
    this.uniforms.stage.value.set(...stageBounds(room));
    const tile = footprintTileSize(room);
    if (tile !== this.tile) { this.tile = tile; this.target.setSize(3 * tile, 2 * tile); }
    this.dirty = this.force = true;
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
    this.count = n; this.lights = lights.slice(0, n);
    if (changed) { this.table.needsUpdate = true; this.dirty = true; }
    this.materialUniforms.tolglowFootprintStrength.value = n && !this.disabled ? 1 : 0;
  }

  /**
   * Re-accumulate only when the light table or room changed, or the context
   * was restored, and at most `maxHz` times a second while lights keep moving
   * (a footprint lags by at most one refresh). Each tile draws only the lights
   * whose cones can reach it; a tile that stays unlit is not redrawn.
   */
  render(now = globalThis.performance?.now?.() ?? 0, maxHz = 30) {
    if (this.disabled || !this.dirty || (!this.force && now - this.lastRender < 1000 / maxHz)) return false;
    this.dirty = this.force = false; this.redraws++; this.lastRender = now;
    const renderer = this.renderer, previous = renderer.getRenderTarget(), u = this.uniforms;
    const lists = footprintTileLights(this.lights, this.room, stageBounds(this.room));
    try {
      for (let tile = 0; tile < 6; tile++) {
        const list = lists[tile];
        if (!list.length && !this.tileLit[tile]) continue;
        this.tileLit[tile] = list.length > 0;
        u.tile.value = tile; u.tileCount.value = list.length; u.tileLights.value.set(list);
        // Scissor as well as viewport: the automatic clear must not erase other tiles.
        this.target.viewport.set((tile % 3) * this.tile, Math.floor(tile / 3) * this.tile, this.tile, this.tile);
        this.target.scissor.copy(this.target.viewport); this.target.scissorTest = true;
        renderer.setRenderTarget(this.target); renderer.render(this.scene, this.camera); this.tileDraws++;
      }
    } finally {
      this.target.viewport.set(0, 0, 3 * this.tile, 2 * this.tile); this.target.scissor.copy(this.target.viewport);
      this.target.scissorTest = false; renderer.setRenderTarget(previous);
    }
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
