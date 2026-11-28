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

// Tile planes, packed: origin, u and v spans (metres), the lit-side normal,
// |u|², |v|², the tile centre and its circumradius.
const PLANE = 20, TILE_COUNT = 6;
const O = 0, U = 3, V = 6, N = 9, UU = 12, VV = 13, C = 14, R = 17;

/** Packed tile planes for a room and stage box (see `cullFootprintTiles`). */
export function footprintTilePlanes([w, h, d], [hw, top, back, front]) {
  const planes = [
    [[-w / 2, 0, -d / 2], [w, 0, 0], [0, 0, d], [0, 1, 0]],
    [[-w / 2, 0, -d / 2], [w, 0, 0], [0, h, 0], [0, 0, 1]],
    [[-w / 2, 0, -d / 2], [0, 0, d], [0, h, 0], [1, 0, 0]],
    [[w / 2, 0, -d / 2], [0, 0, d], [0, h, 0], [-1, 0, 0]],
    [[-hw, top, back], [2 * hw, 0, 0], [0, 0, front - back], [0, 1, 0]],
    [[-hw, 0, front], [2 * hw, 0, 0], [0, top, 0], [0, 0, 1]],
  ];
  const packed = new Float64Array(TILE_COUNT * PLANE);
  planes.forEach(([origin, u, v, normal], tile) => {
    const p = tile * PLANE;
    packed.set(origin, p + O); packed.set(u, p + U); packed.set(v, p + V); packed.set(normal, p + N);
    packed[p + UU] = u[0] * u[0] + u[1] * u[1] + u[2] * u[2];
    packed[p + VV] = v[0] * v[0] + v[1] * v[1] + v[2] * v[2];
    for (let i = 0; i < 3; i++) packed[p + C + i] = origin[i] + u[i] / 2 + v[i] / 2;
    packed[p + R] = Math.sqrt(packed[p + UU] + packed[p + VV]) / 2;
  });
  return packed;
}

// Math.hypot boxes its result in V8 (about 300 young-generation collections
// per 6000 big-room culls); room-scale vectors cannot overflow a plain sum.
const length = (x, y, z) => Math.sqrt(x * x + y * y + z * z);

// Per-light scratch: nine rays (the axis, then eight cone-edge rays).
const rays = new Float64Array(27);
// Angles (radians) by which a tile must clear the widened cone, and metres by
// which it must lie beyond range, before the grid is skipped. Far above the
// rounding of the grid's own comparisons, so the skip never changes a result.
const ANGLE_MARGIN = 1e-7, RANGE_MARGIN = 1e-7;

/**
 * Per tile, the indices of lights whose cone can reach it. A light is kept if
 * the bounding box of its axis and eight cone-edge rays' hits on the tile
 * plane overlaps the tile, or if any point of a 7 × 7 grid over the tile lies
 * inside its (slightly widened) cone within range. The first catches narrow
 * footprints between grid points, the second grazing and unbounded ones.
 *
 * Writes tile `t`'s ascending light indices to `indices[t * stride …]` and
 * their number to `counts[t]`, and allocates nothing. Two shortcuts keep the
 * same lists: a tile the axis itself hits (the floor under most heads) is
 * accepted without the edge rays, and the grid is skipped when the tile's
 * bounding sphere lies wholly outside the widened cone or beyond range.
 */
export function cullFootprintTiles(lights, count, planes, indices, counts, stride = indices.length / TILE_COUNT) {
  counts.fill(0);
  for (let index = 0; index < count; index++) {
    const light = lights[index], axis = light.direction, apex = light.virtualPosition;
    const a0 = axis[0], a1 = axis[1], a2 = axis[2], range = light.range, cosine = light.outerCos;
    let edges = false, widenedAngle = -1, widened = 0;
    for (let tile = 0; tile < TILE_COUNT; tile++) {
      const p = tile * PLANE;
      const t0 = apex[0] - planes[p + O], t1 = apex[1] - planes[p + O + 1], t2 = apex[2] - planes[p + O + 2];
      const n0 = planes[p + N], n1 = planes[p + N + 1], n2 = planes[p + N + 2];
      const height = t0 * n0 + t1 * n1 + t2 * n2;
      if (height <= 0) continue; // the light is behind this surface
      const u0 = planes[p + U], u1 = planes[p + U + 1], u2 = planes[p + U + 2], uu = planes[p + UU];
      const v0 = planes[p + V], v1 = planes[p + V + 1], v2 = planes[p + V + 2], vv = planes[p + VV];
      // The axis hit inside the tile is in the box below, so the tile is reached.
      const axisFacing = a0 * n0 + a1 * n1 + a2 * n2;
      if (axisFacing < -1e-6) {
        const t = -height / axisFacing;
        if (!(range > 0 && t > range)) {
          const h0 = t0 + a0 * t, h1 = t1 + a1 * t, h2 = t2 + a2 * t;
          const uvx = (h0 * u0 + h1 * u1 + h2 * u2) / uu, uvy = (h0 * v0 + h1 * v1 + h2 * v2) / vv;
          if (uvx >= 0 && uvx <= 1 && uvy >= 0 && uvy <= 1) { indices[tile * stride + counts[tile]++] = index; continue; }
        }
      }
      if (!edges) {
        edges = true;
        const h0 = Math.abs(a1) < .95 ? 0 : 1, h1 = Math.abs(a1) < .95 ? 1 : 0; // helper (h0, h1, 0)
        const p0 = h1 * a2 - 0 * a1, p1 = 0 * a0 - h0 * a2, p2 = h0 * a1 - h1 * a0, pl = length(p0, p1, p2);
        const e10 = p0 / pl, e11 = p1 / pl, e12 = p2 / pl;
        const e20 = a1 * e12 - a2 * e11, e21 = a2 * e10 - a0 * e12, e22 = a0 * e11 - a1 * e10;
        const sine = Math.sqrt(Math.max(0, 1 - cosine ** 2));
        rays[0] = a0; rays[1] = a1; rays[2] = a2;
        for (let k = 0; k < 8; k++) {
          const angle = k * Math.PI / 4, c = Math.cos(angle) * sine, s = Math.sin(angle) * sine, r = 3 + 3 * k;
          rays[r] = a0 * cosine + e10 * c + e20 * s;
          rays[r + 1] = a1 * cosine + e11 * c + e21 * s;
          rays[r + 2] = a2 * cosine + e12 * c + e22 * s;
        }
      }
      let lox = Infinity, loy = Infinity, hix = -Infinity, hiy = -Infinity, hits = 0;
      for (let r = 0; r < 27; r += 3) {
        const r0 = rays[r], r1 = rays[r + 1], r2 = rays[r + 2];
        const facing = r0 * n0 + r1 * n1 + r2 * n2;
        if (facing >= -1e-6) continue;
        const t = -height / facing;
        if (range > 0 && t > range) continue;
        const h0 = t0 + r0 * t, h1 = t1 + r1 * t, h2 = t2 + r2 * t;
        const uvx = (h0 * u0 + h1 * u1 + h2 * u2) / uu, uvy = (h0 * v0 + h1 * v1 + h2 * v2) / vv;
        lox = Math.min(lox, uvx); loy = Math.min(loy, uvy); hix = Math.max(hix, uvx); hiy = Math.max(hiy, uvy); hits++;
      }
      const pad = .05 + .1 * Math.max(hix - lox, hiy - loy, 0);
      if (hits && lox - pad <= 1 && hix + pad >= 0 && loy - pad <= 1 && hiy + pad >= 0) {
        indices[tile * stride + counts[tile]++] = index; continue;
      }
      if (widenedAngle < 0) {
        widenedAngle = Math.min(Math.PI / 2, Math.acos(Math.min(1, cosine)) + .05); widened = Math.cos(widenedAngle);
      }
      // Every grid point lies in the tile's bounding sphere.
      const w0 = planes[p + C] - apex[0], w1 = planes[p + C + 1] - apex[1], w2 = planes[p + C + 2] - apex[2];
      const centre = length(w0, w1, w2), radius = planes[p + R];
      if (centre > radius) {
        if (range > 0 && centre - radius > range + RANGE_MARGIN) continue;
        const off = Math.acos(Math.max(-1, Math.min(1, (w0 * a0 + w1 * a1 + w2 * a2) / centre)));
        if (off - Math.asin(radius / centre) > widenedAngle + ANGLE_MARGIN) continue;
      }
      grid: for (let gy = 0; gy <= 6; gy++) for (let gx = 0; gx <= 6; gx++) {
        const q0 = -t0 + u0 * gx / 6 + v0 * gy / 6, q1 = -t1 + u1 * gx / 6 + v1 * gy / 6, q2 = -t2 + u2 * gx / 6 + v2 * gy / 6;
        const distance = length(q0, q1, q2);
        if (distance < 1e-6 || (range > 0 && distance > range)) continue;
        if ((q0 * a0 + q1 * a1 + q2 * a2) / distance >= widened) { indices[tile * stride + counts[tile]++] = index; break grid; }
      }
    }
  }
  return counts;
}

/** Per-tile light index lists, as arrays (allocates; the atlas uses `cullFootprintTiles`). */
export function footprintTileLights(lights, room, stage) {
  const stride = Math.max(1, lights.length), indices = new Int32Array(TILE_COUNT * stride), counts = new Int32Array(TILE_COUNT);
  cullFootprintTiles(lights, lights.length, footprintTilePlanes(room, stage), indices, counts, stride);
  return Array.from(counts, (count, tile) => Array.from(indices.subarray(tile * stride, tile * stride + count)));
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

function store(data, index, value) {
  const single = Math.fround(value);
  if (data[index] === single) return false;
  data[index] = single; return true;
}

export class FootprintAtlas {
  constructor(renderer, {capacity = 128} = {}) {
    this.renderer = renderer; this.capacity = capacity; this.count = 0; this.dirty = true; this.redraws = 0;
    this.lights = []; this.room = [12, 5, 10]; this.tileLit = new Array(6).fill(true); this.lastRender = -Infinity; this.tileDraws = 0;
    // Culling reuses these on every redraw.
    this.planes = footprintTilePlanes(this.room, stageBounds(this.room));
    this.tileIndices = new Int32Array(TILE_COUNT * capacity); this.tileCounts = new Int32Array(TILE_COUNT);
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
    const stage = stageBounds(room);
    this.uniforms.stage.value.set(...stage); this.planes = footprintTilePlanes(this.room, stage);
    const tile = footprintTileSize(room);
    if (tile !== this.tile) { this.tile = tile; this.target.setSize(3 * tile, 2 * tile); }
    this.dirty = this.force = true;
  }

  /** lights: [{virtualPosition, apexDistance, direction, outerCos, innerCos, color (rgb × intensity), range}] */
  update(lights) {
    const n = Math.min(this.capacity, lights.length), data = this.table.image.data;
    let changed = n !== this.count;
    for (let i = 0; i < n; i++) {
      const light = lights[i], row = i * TABLE_ROWS * 4, {virtualPosition: a, direction: b, color: c} = light;
      // The table is float32: compare in that precision, or nothing ever matches.
      changed = store(data, row, a[0]) | store(data, row + 1, a[1]) | store(data, row + 2, a[2]) | store(data, row + 3, light.apexDistance)
        | store(data, row + 4, b[0]) | store(data, row + 5, b[1]) | store(data, row + 6, b[2]) | store(data, row + 7, light.outerCos)
        | store(data, row + 8, c[0]) | store(data, row + 9, c[1]) | store(data, row + 10, c[2]) | store(data, row + 11, light.innerCos)
        | store(data, row + 12, light.range) | store(data, row + 13, 0) | store(data, row + 14, 0) | store(data, row + 15, 0) | changed;
    }
    this.count = n; this.lights = lights;
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
    const counts = cullFootprintTiles(this.lights, this.count, this.planes, this.tileIndices, this.tileCounts, this.capacity);
    try {
      for (let tile = 0; tile < 6; tile++) {
        const count = counts[tile];
        if (!count && !this.tileLit[tile]) continue;
        this.tileLit[tile] = count > 0;
        u.tile.value = tile; u.tileCount.value = count;
        for (let i = 0, from = tile * this.capacity; i < count; i++) u.tileLights.value[i] = this.tileIndices[from + i];
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
