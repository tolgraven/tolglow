import * as THREE from 'three';
import {Pass} from 'three/addons/postprocessing/Pass.js';
import {RoundedBoxGeometry} from 'three/addons/geometries/RoundedBoxGeometry.js';
import {MarchingCubes} from 'three/addons/objects/MarchingCubes.js';

const vertex = `
varying vec3 worldPoint;
varying vec3 viewPoint;
varying vec3 viewNormal;
varying vec3 localPoint;
void main(){
  worldPoint=(modelMatrix*vec4(position,1.0)).xyz;
  viewPoint=(modelViewMatrix*vec4(position,1.0)).xyz;
  localPoint=position;
  viewNormal=normalize(normalMatrix*normal);
  gl_Position=projectionMatrix*vec4(viewPoint,1.0);
}`;

const fragment = `
uniform sampler2D sceneColor, opaqueDepth, atmosphere;
uniform samplerCube roomEnvironment;
uniform vec2 resolution;
uniform mat4 lens;
uniform vec3 tint, sourcePos[4], sourceColor[4], sourceDir[4];
uniform vec4 coneCos;
uniform float sizeM, clockS, velocity, nearZ, farZ, workLight, fogAmount, strength;
varying vec3 worldPoint,viewPoint,viewNormal,localPoint;
float eyeDepth(float z){return (nearZ*farZ)/(farZ-(farZ-nearZ)*z);}
vec2 uvOf(vec3 p){vec4 q=lens*vec4(p,1.0);return q.xy/max(q.w,0.001)*0.5+0.5;}
vec2 bounded(vec2 uv){return clamp(uv,0.5/resolution,1.0-0.5/resolution);}
void main(){
  vec2 uv=gl_FragCoord.xy/resolution;
  float opaque=eyeDepth(texture2D(opaqueDepth,uv).r), depth=-viewPoint.z;
  if(depth>opaque+0.004) discard;
  vec3 V=normalize(-viewPoint), N=normalize(viewNormal);
  // Subpixel waves are filtered by screen-space derivatives to keep highlights stable in motion.
  vec3 p=localPoint*2.4;
  float phase=clockS*(0.42+min(velocity,4.0)*0.14);
  float aa=1.0/(1.0+4.0*length(fwidth(p)));
  vec3 ripples=vec3(sin(p.y*2.1+phase)*cos(p.z*1.7-phase),cos(p.x*1.9-phase)*sin(p.z+phase),sin(p.x+p.y-phase));
  N=normalize(N+aa*(0.012+0.008*min(velocity,3.0))*(ripples-N*dot(ripples,N)));
  if(dot(N,V)<0.0) N=-N;
  float facing=max(0.0,dot(N,V));
  float fresnel=0.037+0.963*pow(1.0-facing,5.0);
  float thickness=min(sizeM*mix(0.24,1.72,facing),max(0.02,opaque-depth));
  vec3 T=refract(-V,N,1.0/1.46);
  vec2 shift=uvOf(viewPoint+T*thickness)-uv;
  // Suppress samples that would drag an opaque foreground object through the surface.
  float sampleDepth=eyeDepth(texture2D(opaqueDepth,bounded(uv+shift)).r);
  shift*=smoothstep(-0.015,max(0.025,sizeM*0.1),sampleDepth-depth);
  shift*=smoothstep(0.0,0.035,min(min(uv.x,uv.y),min(1.0-uv.x,1.0-uv.y)));
  vec3 transmitted=vec3(texture2D(sceneColor,bounded(uv+shift*1.023)).r,
                         texture2D(sceneColor,bounded(uv+shift)).g,
                         texture2D(sceneColor,bounded(uv+shift*0.977)).b);
  vec3 absorption=exp(-thickness*(0.018+0.13*(1.0-clamp(tint,0.0,1.0))));
  vec3 R=reflect(-V,N);
  vec3 reflected=texture2D(sceneColor,bounded(uvOf(viewPoint+R*sizeM*1.4))).rgb;
  vec3 worldN=normalize((vec4(N,0.0)*viewMatrix).xyz);
  vec3 worldV=normalize(cameraPosition-worldPoint);
  vec3 specular=vec3(0.0);
  float lobe=clamp(105.0/(1.0+18.0*length(fwidth(N))),22.0,105.0);
  for(int i=0;i<4;i++){
    vec3 lightDelta=sourcePos[i]-worldPoint;
    vec3 L=lightDelta/max(0.01,length(lightDelta));
    float alignment=dot(-L,sourceDir[i]);
    float beam=smoothstep(coneCos[i]-0.06,coneCos[i]+0.025,alignment);
    float h=max(0.0,dot(worldN,normalize(L+worldV)));
    specular+=sourceColor[i]*beam*(pow(h,lobe)*3.5+pow(h,12.0)*0.12)/(1.0+dot(lightDelta,lightDelta)*0.065);
  }
  // A broad ceiling and floor reflection from the room work light gives the lens
  // a readable silhouette even before performers launch a directional fixture.
  vec3 wr=reflect(-worldV,worldN);
  float ceiling=smoothstep(0.4,0.92,wr.y);
  float strip=exp(-abs(wr.y-0.27-0.12*wr.x)*18.0);
  vec3 room=workLight*(mix(vec3(0.11,0.085,0.064),vec3(0.7,0.83,1.0),ceiling)+strip*vec3(2.4,2.25,1.95));
  // The same high dynamic range room lights and panels illuminate the fixture
  // materials. Polished glass resolves their shapes in its front and back surfaces.
  vec3 environment=textureCube(roomEnvironment,wr).rgb;
  vec3 refractedWorld=normalize((vec4(T,0.0)*viewMatrix).xyz);
  vec3 backReflection=textureCube(roomEnvironment,reflect(refractedWorld,-worldN)).rgb;
  float internal=0.045+0.095*pow(1.0-facing,1.65);
  vec3 reflectedLight=reflected+room+environment*(workLight*2.2);
  vec3 color=mix(transmitted*absorption,reflectedLight,fresnel)+specular
             +backReflection*workLight*internal*absorption;
  // Thin blue/amber edges arise from slightly different ray directions in glass.
  vec3 film=0.58+0.42*cos(6.28*(facing*1.3+0.08*localPoint.y+0.035*sin(phase+localPoint.x))+
                              vec3(0.0,2.1,4.2));
  float lightLevel=workLight+dot(specular+reflected*0.3,vec3(0.2126,0.7152,0.0722));
  vec3 edgeTint=mix(vec3(0.84,0.93,1.0),clamp(tint,0.0,1.0),0.65);
  color+=min(2.0,lightLevel)*pow(1.0-facing,2.2)*(edgeTint*1.35+film*0.48);
  float edgeAA=smoothstep(0.0,max(0.003,fwidth(facing)*0.58),facing);
  if(fogAmount>0.0){
    vec4 fog=texture2D(atmosphere,uv);
    float fraction=clamp(depth/max(opaque,0.01),0.0,1.0);
    float front=pow(clamp(fog.a,0.001,1.0),fraction);
    color=mix(transmitted,color,front)+fog.rgb*(1.0-front)*fresnel*0.3;
  }
  vec3 original=texture2D(sceneColor,uv).rgb;
  gl_FragColor=vec4(mix(original,color,clamp(strength,0.0,1.0)*edgeAA),1.0);
}`;

function fieldGeometry({resolution, field, padding}) {
  const material = new THREE.MeshBasicMaterial();
  const surface = new MarchingCubes(resolution, material, false, false, 18000);
  surface.field.set(field);
  surface.isolation = 0;
  surface.update();

  const geometry = new THREE.BufferGeometry();
  const coordinates = Math.min(surface.count, surface.geometry.attributes.position.count) * 3;
  for (const name of ['position', 'normal']) {
    const data = surface.geometry.attributes[name].array.slice(0, coordinates);
    geometry.setAttribute(name, new THREE.BufferAttribute(data, 3));
  }
  surface.geometry.dispose();
  material.dispose();
  geometry.scale(padding, padding, padding);
  geometry.computeBoundingSphere();
  return geometry;
}

export function glassGeometry(descriptor) {
  switch (descriptor.kind) {
    case 'sphere': return new THREE.SphereGeometry(1, 48, 32);
    case 'box': return new THREE.BoxGeometry(2, 2, 2);
    case 'cylinder': return new THREE.CylinderGeometry(1, 1, 2, 48);
    case 'field': return fieldGeometry(descriptor);
    case 'rounded-box': {
      const [x, y, z] = descriptor.size;
      const radius = Math.max(0, descriptor.rounding) * Math.min(x, y, z) / 2;
      const geometry = radius > 0 ? new RoundedBoxGeometry(x, y, z, 5, radius) : new THREE.BoxGeometry(x, y, z);
      return geometry.scale(2 / x, 2 / y, 2 / z);
    }
    default: throw new Error('Unknown optical surface');
  }
}

const uniform = value => ({value});
const vectors = () => Array.from({length: 4}, () => new THREE.Vector3());

function newSurface(geometry, shared, target, now) {
  const material = new THREE.ShaderMaterial({
    vertexShader: vertex, fragmentShader: fragment, side: THREE.DoubleSide,
    uniforms: {...shared, tint: uniform(new THREE.Color()), sizeM: uniform(1), strength: uniform(1), velocity: uniform(0)}
  });
  const mesh = new THREE.Mesh(geometry, material);
  mesh.position.copy(target);
  return {mesh, start: target.clone(), end: target.clone(), at: now, period: 25, speed: 0};
}

function updateSurface(state, item, target, now) {
  const {mesh} = state;
  if (!state.end.equals(target)) {
    const elapsed = now - state.at;
    state.speed = Math.min(6, target.distanceTo(state.end) / (Math.max(1, elapsed) / 1000));
    state.start.copy(elapsed > 220 ? target : mesh.position);
    state.end.copy(target);
    state.at = now;
    state.period = THREE.MathUtils.clamp(elapsed, 8, 80);
  }
  const squash = THREE.MathUtils.clamp(item.deformation || 0, 0, 0.5);
  const lateral = 1 / Math.sqrt(1 - squash);
  const scale = mesh.scale.fromArray(item['half-size']);
  scale.set(scale.x * lateral, scale.y * (1 - squash), scale.z * lateral);
  const {sizeM, tint, strength} = mesh.material.uniforms;
  sizeM.value = Math.max(0.02, Math.min(scale.x, scale.y, scale.z));
  tint.value.fromArray(item.color || [0.65, 0.85, 1]);
  strength.value = item.strength ?? 1;
}

function updateLights(uniforms, lights) {
  const brightest = lights.filter(light => light.intensity > 0)
    .sort((a, b) => b.intensity - a.intensity).slice(0, 4);
  for (let i = 0; i < 4; i++) {
    const light = brightest[i], color = uniforms.sourceColor.value[i];
    if (!light) {
      color.set(0, 0, 0);
      continue;
    }
    const position = uniforms.sourcePos.value[i].setFromMatrixPosition(light.matrixWorld);
    uniforms.sourceDir.value[i].setFromMatrixPosition(light.target.matrixWorld).sub(position).normalize();
    color.set(light.color.r, light.color.g, light.color.b).multiplyScalar(light.intensity / 50);
    uniforms.coneCos.value.setComponent(i, Math.cos(light.angle));
  }
}

/** Composite bounded live surfaces into the fogged HDR image before bloom.
 * The source image and opaque depth are independent of the destination. */
export class GlassPass extends Pass {
  constructor(engine) {
    super();
    this.engine = engine;
    this.enabled = false;
    this.needsSwap = false;
    this.scene = new THREE.Scene();
    this.geometry = new Map();
    this.instances = new Map();
    this.target = new THREE.Vector3();
    this.uniforms = {
      sceneColor: uniform(null), roomEnvironment: uniform(null), opaqueDepth: uniform(null), atmosphere: uniform(null),
      resolution: uniform(new THREE.Vector2(1, 1)), lens: uniform(new THREE.Matrix4()), clockS: uniform(0),
      workLight: uniform(0), fogAmount: uniform(0), nearZ: uniform(0.1), farZ: uniform(100),
      sourcePos: uniform(vectors()), sourceColor: uniform(vectors()), sourceDir: uniform(vectors()),
      coneCos: uniform(new THREE.Vector4())
    };
  }

  setSize(width, height) {
    this.uniforms.resolution.value.set(width, height);
  }

  update(shapes, objects, now = performance.now()) {
    const used = new Set(), seen = new Set();
    for (const item of (objects || []).slice(0, 12)) {
      const descriptor = shapes?.[item.mesh];
      if (!descriptor) continue;
      if (!this.geometry.has(item.mesh)) this.geometry.set(item.mesh, glassGeometry(descriptor));
      const geometry = this.geometry.get(item.mesh), target = this.target.fromArray(item.position);
      let state = this.instances.get(item.id);
      if (!state) {
        state = newSurface(geometry, this.uniforms, target, now);
        this.instances.set(item.id, state);
        this.scene.add(state.mesh);
      }
      state.mesh.geometry = geometry;
      updateSurface(state, item, target, now);
      used.add(item.mesh);
      seen.add(item.id);
    }
    for (const [id, {mesh}] of this.instances) {
      if (seen.has(id)) continue;
      this.scene.remove(mesh);
      mesh.material.dispose();
      this.instances.delete(id);
    }
    for (const [id, geometry] of this.geometry) {
      if (used.has(id)) continue;
      geometry.dispose();
      this.geometry.delete(id);
    }
    this.enabled = seen.size > 0;
  }

  render(renderer, _writeBuffer, readBuffer) {
    const {camera, clock, compositeTarget, sceneTarget, fog, fogAmount, workLight, spots, environmentTarget} = this.engine;
    const u = this.uniforms, now = performance.now();
    for (const state of this.instances.values()) {
      const elapsed = Math.max(0, now - state.at);
      state.mesh.position.lerpVectors(state.start, state.end, Math.min(1, elapsed / state.period));
      state.mesh.material.uniforms.velocity.value = state.speed * Math.exp(-Math.max(0, elapsed - state.period) / 220);
    }
    u.sceneColor.value = compositeTarget.texture;
    u.opaqueDepth.value = sceneTarget.depthTexture;
    u.atmosphere.value = fog.target.texture;
    u.roomEnvironment.value = environmentTarget.texture;
    u.clockS.value = clock;
    u.fogAmount.value = fogAmount;
    u.workLight.value = workLight;
    u.lens.value.copy(camera.projectionMatrix);
    u.nearZ.value = camera.near;
    u.farZ.value = camera.far;
    updateLights(u, spots || []);
    const clear = renderer.autoClear, previous = renderer.getRenderTarget();
    try {
      renderer.setRenderTarget(readBuffer);
      renderer.autoClear = false;
      renderer.clearDepth();
      renderer.render(this.scene, camera);
    } finally {
      renderer.autoClear = clear;
      renderer.setRenderTarget(previous);
    }
  }

  dispose() { this.update({}, []); }
}
