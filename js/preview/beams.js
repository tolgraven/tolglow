import * as THREE from 'three';
import {fogTransmissionGLSL} from './fog-optics.generated.js';

// Faked beams for lights outside the full-light budget and for the low tier
// (#782). One shared open cone is instanced per light and drawn additively
// into the fog's reduced raster, before the depth-aware composite and bloom.
// Brightness follows the raymarch's single-scattering estimate: intensity /
// distance², isotropic phase, the cone chord weighted by the spot profile, and
// the same extinction on the incident path from the lens and the view path
// from the eye. Opaque depth comes from the scene target the fog already reads;
// the scene is never rendered a second time. Zero fog draws nothing.

const vertexShader = `precision highp float;
in vec3 position;
in vec2 uv;
in vec3 beamPosition, beamDirection;
in vec4 beamColor; // linear rgb × intensity × faked weight, penumbra
in vec4 beamShape; // aperture radius, tan(half-angle), length, apex distance
uniform mat4 viewMatrix, projectionMatrix;
out vec3 vWorld, vNormal, vColor, vLens;
out vec3 vBeam; // distance from the virtual apex, radius there, along 0..1
out vec2 vAround;
out float vPenumbra;
void main(){
  float along=clamp(.5-position.y,0.,1.);
  vec3 axis=normalize(beamDirection);
  vec3 helper=abs(axis.y)<.95?vec3(0.,1.,0.):vec3(1.,0.,0.);
  vec3 u=normalize(cross(helper,axis)),v=cross(axis,u);
  vec2 ring=normalize(position.xz);
  vec3 radial=u*ring.x+v*ring.y;
  float s=along*beamShape.z;
  float radius=beamShape.x+s*beamShape.y;
  vec3 world=beamPosition+axis*s+radial*radius;
  vWorld=world; vNormal=normalize(radial-axis*beamShape.y); vColor=beamColor.rgb; vPenumbra=beamColor.w;
  vLens=beamPosition;
  vBeam=vec3(beamShape.w+s,radius,along);
  // The cylinder's own u has a duplicated seam column, so it never wraps
  // across a triangle.
  vAround=vec2(uv.x,s);
  gl_Position=projectionMatrix*viewMatrix*vec4(world,1.);
}`;

const fragmentShader = `precision highp float;
precision highp sampler3D;
${fogTransmissionGLSL}
in vec3 vWorld, vNormal, vColor, vBeam, vLens;
in vec2 vAround;
in float vPenumbra;
uniform vec3 cameraPosition, room;
uniform sampler2D sceneDepth, noiseTexture;
uniform sampler3D densityTexture;
uniform mat4 viewMatrix;
uniform vec2 resolution;
uniform float nearPlane, farPlane, amount, extinctionPerM, time;
out vec4 beamOutput;
float viewDepth(float z){return (nearPlane*farPlane)/(farPlane-(farPlane-nearPlane)*z);}
float hash(vec3 p){p=fract(p*.3183099+vec3(.11,.23,.37));p*=17.;return fract(p.x*p.y*p.z*(p.x+p.y+p.z));}
float valueNoise(vec3 p){vec3 i=floor(p),f=fract(p);f=f*f*(3.-2.*f);
 return mix(mix(mix(hash(i),hash(i+vec3(1,0,0)),f.x),mix(hash(i+vec3(0,1,0)),hash(i+vec3(1,1,0)),f.x),f.y),
  mix(mix(hash(i+vec3(0,0,1)),hash(i+vec3(1,0,1)),f.x),mix(hash(i+vec3(0,1,1)),hash(i+vec3(1,1,1)),f.x),f.y),f.z);}
float fbm(vec3 p){float sum=0.,weight=.5;for(int i=0;i<4;i++){sum+=weight*valueNoise(p);p=p*2.03+vec3(1.7,9.2,3.1);weight*=.5;}return sum/.9375;}
float densityAt(vec3 p){
  vec3 coordinate=vec3(p.x/room.x+.5,p.y/room.y,p.z/room.z+.5);
  return amount*texture(densityTexture,clamp(coordinate,vec3(0.),vec3(1.))).r;
}
const float bulkPhase=1./12.5663706;
void main(){
  if(amount<=0.){discard;}
  vec3 toEye=cameraPosition-vWorld;float eyeDistance=length(toEye);
  float facing=abs(dot(normalize(vNormal),toEye/eyeDistance));
  // Opaque geometry: soft intersection against the resolved scene depth.
  float fragmentDepth=-(viewMatrix*vec4(vWorld,1.)).z;
  float opaque=viewDepth(texture(sceneDepth,gl_FragCoord.xy/resolution).r);
  float soft=clamp((opaque-fragmentDepth)/.18,0.,1.);
  if(soft<=0.)discard;
  float near=smoothstep(.12,1.1,fragmentDepth);
  float density=densityAt(vWorld);
  if(density<=0.)discard;
  // Same extinction model as the raymarch: the incident path from the lens
  // (one midpoint tap) and the view path from the eye, clipped to the room
  // (three taps).
  float incidentLength=length(vWorld-vLens);
  float incident=fogTransmission(extinctionPerM,densityAt(mix(vLens,vWorld,.5)),incidentLength);
  vec3 ray=-toEye/eyeDistance;
  vec3 safe=vec3(abs(ray.x)<1e-7?1e-7:ray.x,abs(ray.y)<1e-7?1e-7:ray.y,abs(ray.z)<1e-7?1e-7:ray.z);
  vec3 a=(vec3(-room.x*.5,0.,-room.z*.5)-cameraPosition)/safe,b=(vec3(room.x*.5,room.y,room.z*.5)-cameraPosition)/safe;
  vec3 lo=min(a,b);
  float start=clamp(max(max(lo.x,lo.y),lo.z),0.,eyeDistance),span=eyeDistance-start;
  vec3 entry=cameraPosition+ray*start;
  float viewDensity=(densityAt(mix(entry,vWorld,1./6.))+densityAt(mix(entry,vWorld,.5))+densityAt(mix(entry,vWorld,5./6.)))/3.;
  float view=fogTransmission(extinctionPerM,viewDensity,span);
  // Drifting haze: two scrolling samples of a tiling grey noise plus a
  // world-space fbm, so neighbouring beams do not share one pattern.
  float n1=texture(noiseTexture,vec2(vAround.x*3.,vAround.y*.35-time*.06)).r;
  float n2=texture(noiseTexture,vec2(vAround.x*5.+.37,vAround.y*.6+time*.045)).r;
  float haze=fbm(vWorld*1.3+vec3(-time*.13,-time*.04,time*.02));
  float structure=.35+1.3*(.45*n1+.25*n2+.3*haze);
  // Chord through the cone at this silhouette point (r·|n·v| per face, two
  // faces), weighted by the spot profile at the chord's closest radius.
  float closest=sqrt(max(0.,1.-facing*facing));
  float profile=smoothstep(1.,1.-max(.02,vPenumbra),closest);
  float chord=vBeam.y*facing;
  float scatter=1.-fogTransmission(extinctionPerM,density,chord);
  float distance2=max(.25,vBeam.x*vBeam.x);
  float fade=1.-smoothstep(.72,1.,vBeam.z);
  vec3 radiance=vColor*bulkPhase*.92*scatter*profile/distance2*incident*view*structure*soft*near*fade;
  beamOutput=vec4(radiance,0.);
}`;

function noiseTexture(size = 256) {
  // Deterministic tiling value noise (bilinear lattice of a seeded hash).
  const data = new Uint8Array(size * size), cells = 16, lattice = new Float32Array(cells * cells);
  let seed = 0x9e3779b9;
  for (let i = 0; i < lattice.length; i++) { seed = (seed * 1664525 + 1013904223) >>> 0; lattice[i] = seed / 2 ** 32; }
  const at = (x, y) => lattice[(y % cells) * cells + (x % cells)];
  for (let y = 0; y < size; y++) for (let x = 0; x < size; x++) {
    const fx = x * cells / size, fy = y * cells / size, ix = Math.floor(fx), iy = Math.floor(fy);
    const tx = (fx - ix) ** 2 * (3 - 2 * (fx - ix)), ty = (fy - iy) ** 2 * (3 - 2 * (fy - iy));
    const top = at(ix, iy) * (1 - tx) + at(ix + 1, iy) * tx, bottom = at(ix, iy + 1) * (1 - tx) + at(ix + 1, iy + 1) * tx;
    const fine = ((x * 7919 + y * 104729) % 251) / 251;
    data[y * size + x] = Math.round(255 * (.85 * (top * (1 - ty) + bottom * ty) + .15 * fine));
  }
  const texture = new THREE.DataTexture(data, size, size, THREE.RedFormat, THREE.UnsignedByteType);
  texture.wrapS = texture.wrapT = THREE.RepeatWrapping;
  texture.minFilter = THREE.LinearMipmapLinearFilter; texture.magFilter = THREE.LinearFilter;
  texture.generateMipmaps = true; texture.needsUpdate = true;
  return texture;
}

/** Distance along a ray from inside the room box to its first wall/floor/ceiling. */
export function roomExit(origin, direction, room) {
  const half = [room[0] / 2, room[1], room[2] / 2], lower = [-half[0], 0, -half[2]];
  let exit = Infinity;
  for (let axis = 0; axis < 3; axis++) {
    const d = direction[axis];
    if (Math.abs(d) < 1e-9) continue;
    const bound = d > 0 ? half[axis] : lower[axis];
    const t = (bound - origin[axis]) / d;
    if (t >= 0) exit = Math.min(exit, t);
  }
  return Number.isFinite(exit) ? exit : Math.max(...room) * 2;
}

export class FakedBeams {
  constructor(capacity = 128) {
    this.capacity = capacity; this.count = 0;
    const cone = new THREE.CylinderGeometry(1, 1, 1, 28, 10, true);
    const geometry = new THREE.InstancedBufferGeometry();
    geometry.index = cone.index; geometry.setAttribute('position', cone.getAttribute('position'));
    geometry.setAttribute('uv', cone.getAttribute('uv'));
    cone.dispose();
    const attribute = (name, size) => {
      const value = new THREE.InstancedBufferAttribute(new Float32Array(capacity * size), size);
      value.setUsage(THREE.DynamicDrawUsage); geometry.setAttribute(name, value); return value;
    };
    this.attributes = {position: attribute('beamPosition', 3), direction: attribute('beamDirection', 3),
      color: attribute('beamColor', 4), shape: attribute('beamShape', 4)};
    geometry.instanceCount = 0;
    this.noise = noiseTexture();
    this.uniforms = {sceneDepth: {value: null}, noiseTexture: {value: this.noise}, densityTexture: {value: null},
      room: {value: new THREE.Vector3(12, 5, 10)}, resolution: {value: new THREE.Vector2(1, 1)},
      nearPlane: {value: .08}, farPlane: {value: 100}, amount: {value: 0}, extinctionPerM: {value: 1.25},
      time: {value: 0}};
    this.material = new THREE.RawShaderMaterial({vertexShader, fragmentShader, uniforms: this.uniforms,
      glslVersion: THREE.GLSL3, transparent: true, depthTest: false, depthWrite: false, side: THREE.DoubleSide,
      blending: THREE.CustomBlending, blendEquation: THREE.AddEquation, blendSrc: THREE.OneFactor, blendDst: THREE.OneFactor,
      blendSrcAlpha: THREE.ZeroFactor, blendDstAlpha: THREE.OneFactor, toneMapped: false});
    this.mesh = new THREE.Mesh(geometry, this.material); this.mesh.frustumCulled = false;
    this.scene = new THREE.Scene(); this.scene.add(this.mesh);
  }

  /** beams: [{position, direction, color (linear rgb × intensity), penumbra, radius, tangent, length, apexDistance}] */
  update(beams) {
    const n = Math.min(this.capacity, beams.length), a = this.attributes;
    for (let i = 0; i < n; i++) {
      const beam = beams[i];
      a.position.setXYZ(i, beam.position[0], beam.position[1], beam.position[2]);
      a.direction.setXYZ(i, beam.direction[0], beam.direction[1], beam.direction[2]);
      a.color.setXYZW(i, beam.color[0], beam.color[1], beam.color[2], beam.penumbra ?? .5);
      a.shape.setXYZW(i, beam.radius, beam.tangent, beam.length, beam.apexDistance);
    }
    for (const value of Object.values(a)) { value.clearUpdateRanges(); value.addUpdateRange(0, n * value.itemSize); value.needsUpdate = true; }
    this.mesh.geometry.instanceCount = this.count = n;
  }

  render(renderer, camera, target, sceneTarget, {room, amount, extinctionPerM, time, densityTexture}) {
    if (!this.count || !(amount > 0)) return false;
    const u = this.uniforms;
    u.sceneDepth.value = sceneTarget.depthTexture; u.densityTexture.value = densityTexture;
    u.room.value.fromArray(room); u.resolution.value.set(target.width, target.height);
    u.nearPlane.value = camera.near; u.farPlane.value = camera.far; u.amount.value = amount;
    u.extinctionPerM.value = extinctionPerM; u.time.value = time;
    const previous = renderer.getRenderTarget(), autoClear = renderer.autoClear;
    try {
      renderer.autoClear = false; renderer.setRenderTarget(target); renderer.render(this.scene, camera);
    } finally { renderer.setRenderTarget(previous); renderer.autoClear = autoClear; }
    return true;
  }

  stats() { return {count: this.count, capacity: this.capacity}; }

  dispose() {
    this.mesh.geometry.dispose(); this.material.dispose(); this.noise.dispose(); this.scene.clear(); this.count = 0;
  }
}
