import * as THREE from 'three';
import {fogTransmissionGLSL} from './fog-optics.generated.js';
import {DEFAULT_EXTINCTION_PER_M, ShowAtmosphere} from './show-atmosphere.js';
import { FogField, FOG_QUALITY } from './fog-field.js';

const vertex = `out vec2 vUv;
void main(){vUv=uv;gl_Position=vec4(position.xy,0.0,1.0);}`;

const fogIntegrator = `precision highp float;
precision highp sampler3D;
#include <packing>
${fogTransmissionGLSL}
in vec2 vUv;
uniform sampler2D sceneDepth;
uniform sampler3D densityTexture;
uniform mat4 inverseProjection, cameraWorld;
uniform vec3 eye, room;
uniform vec3 detailMin, detailMax;
uniform float amount, time, mixedDensity, extinctionPerM;
uniform int raySteps, lightCount;
uniform vec3 lightPosition[16], lightDirection[16], lightColor[16];
uniform vec3 lightShape[16]; // half-angle cosine, intensity, maximum distance
uniform vec4 lightProfile[16]; // inner cosine, optical tangent, halo tangent increment, halo cosine
uniform float lightStart[16]; // virtual apex to optical lens plane
uniform vec3 ambientSky,ambientGround;
uniform int lightShadow[16];
uniform highp sampler2DShadow shadow0, shadow1, shadow2, shadow3;
uniform mat4 shadowMatrix[4];
uniform vec2 shadowTexel[4];
uniform float shadowBias[4];

float hash(vec3 p){p=fract(p*0.3183099+vec3(.11,.23,.37));p*=17.;return fract(p.x*p.y*p.z*(p.x+p.y+p.z));}
float noise(vec3 p){vec3 i=floor(p),f=fract(p);f=f*f*(3.-2.*f);
return mix(mix(mix(hash(i),hash(i+vec3(1,0,0)),f.x),mix(hash(i+vec3(0,1,0)),hash(i+vec3(1,1,0)),f.x),f.y),
 mix(mix(hash(i+vec3(0,0,1)),hash(i+vec3(1,0,1)),f.x),mix(hash(i+vec3(0,1,1)),hash(i+vec3(1,1,1)),f.x),f.y),f.z);}
vec2 boxHit(vec3 ro,vec3 rd,vec3 lower,vec3 upper){
 vec3 safe=vec3(abs(rd.x)<1e-7?(rd.x<0.?-1e-7:1e-7):rd.x,abs(rd.y)<1e-7?(rd.y<0.?-1e-7:1e-7):rd.y,abs(rd.z)<1e-7?(rd.z<0.?-1e-7:1e-7):rd.z);
 vec3 a=(lower-ro)/safe,b=(upper-ro)/safe,lo=min(a,b),hi=max(a,b);
 return vec2(max(max(lo.x,lo.y),lo.z),min(min(hi.x,hi.y),hi.z));
}
vec2 roomHit(vec3 ro,vec3 rd){
 return boxHit(ro,rd,vec3(-room.x*.5,0.,-room.z*.5),vec3(room.x*.5,room.y,room.z*.5));
}
float shadowCompare(int slot,vec2 uv,float depth){
 if(slot==0)return texture(shadow0,vec3(uv,depth));
 if(slot==1)return texture(shadow1,vec3(uv,depth));
 if(slot==2)return texture(shadow2,vec3(uv,depth));
 return texture(shadow3,vec3(uv,depth));
}
float visibility(int slot,vec3 position){
 if(slot<0)return 1.;
 vec4 coordinate=shadowMatrix[slot]*vec4(position,1.);coordinate.xyz/=coordinate.w;
 if(coordinate.w<=0.||coordinate.z<0.||coordinate.z>1.||any(lessThan(coordinate.xy,vec2(0.)))||any(greaterThan(coordinate.xy,vec2(1.))))return 1.;
 float depth=coordinate.z+shadowBias[slot];vec2 texel=shadowTexel[slot]*.65;
 return .25*(shadowCompare(slot,coordinate.xy+vec2(-texel.x,-texel.y),depth)+
 shadowCompare(slot,coordinate.xy+vec2(texel.x,-texel.y),depth)+
 shadowCompare(slot,coordinate.xy+vec2(-texel.x,texel.y),depth)+
 shadowCompare(slot,coordinate.xy+texel,depth));
}
// A uniform bulk medium scatters the same share of incident light per steradian.
const float bulkPhase = 1./12.5663706;
vec2 coneHit(int i,float angleCos,vec3 ro,vec3 rd,float lo,float hi){
 vec3 q=ro-lightPosition[i];float axial=dot(q,lightDirection[i]),slope=dot(rd,lightDirection[i]);
 float plane=lightStart[i];
 if(abs(slope)<1e-7){if(axial<plane)return vec2(1.,0.);}
 else if(slope>0.)lo=max(lo,(plane-axial)/slope);else hi=min(hi,(plane-axial)/slope);
 float range=lightShape[i].z;
 if(range>0.){float b=dot(q,rd),discriminant=b*b-dot(q,q)+range*range;
  if(discriminant<0.)return vec2(1.,0.);float root=sqrt(discriminant);lo=max(lo,-b-root);hi=min(hi,-b+root);}
 float cos2=angleCos*angleCos;
 float a=slope*slope-cos2,b=2.*(axial*slope-cos2*dot(q,rd)),c=axial*axial-cos2*dot(q,q);
 if(abs(a)<1e-7){if(abs(b)<1e-7){if(c<0.)return vec2(1.,0.);}else if(b>0.)lo=max(lo,-c/b);else hi=min(hi,-c/b);}
 else{float discriminant=b*b-4.*a*c;
  if(discriminant<0.){if(a<0.)return vec2(1.,0.);}
  else{float root=sqrt(discriminant),first=(-b-root)/(2.*a),last=(-b+root)/(2.*a);
   if(first>last){float swap=first;first=last;last=swap;}
   if(a<0.){lo=max(lo,first);hi=min(hi,last);}else if(slope>0.)lo=max(lo,last);else hi=min(hi,first);}}
 return vec2(lo,hi);
}
vec3 incident(int i,vec3 position,float density,bool halo){
  vec3 delta=position-lightPosition[i];float distance=length(delta);vec3 travel=delta/max(distance,.001);
  float cosine=dot(travel,lightDirection[i]);float outer=lightShape[i].x;
  float inner=lightProfile[i].x;
  float spot=smoothstep(outer,max(outer+.00001,inner),cosine);
  vec3 shadowPoint=position;
  float axial=dot(delta,lightDirection[i]);
  float path=max(0.,axial-lightStart[i]);
  if(halo){
   // Forward scattering can illuminate the room just outside the optical
   // field. The additional width starts at zero at the actual lens.
   vec3 lateral=delta-axial*lightDirection[i];
   float coreRadius=axial*lightProfile[i].y;
   float width=path*lightProfile[i].z;
   float radial=length(lateral), outside=radial-coreRadius;
   if(width<=0.||outside>=width)return vec3(0.);
   // The core already contributes its optical scattering. Start this extra
   // contribution continuously outside that field, without lighting it twice.
   spot=smoothstep(0.,width*.15,outside)*.26*(1.-fogTransmission(extinctionPerM,density,path))*(1.-smoothstep(0.,width,outside));
   // Sample the shadow on the illuminated edge that feeds this halo.
   shadowPoint=lightPosition[i]+axial*lightDirection[i]+lateral*min(1.,.98*coreRadius/max(radial,.001));
  }
  float range=lightShape[i].z;
  if(spot<.00001||(range>0.&&distance>=range))return vec3(0.);
  float falloff=range>0.?pow(clamp(1.-pow(distance/range,4.),0.,1.),2.):1.;
  float attenuation=falloff/max(.25,distance*distance);
  // Coarse incident-path extinction as well as the independently integrated
  // viewer-path extinction. Density is measured in the same grid for both.
  vec3 optical=lightPosition[i]+lightStart[i]*lightDirection[i];
  vec3 midpoint=mix(optical,shadowPoint,.5);
  vec3 uvw=vec3(midpoint.x/room.x+.5,midpoint.y/room.y,midpoint.z/room.z+.5);
  float incidentDensity=amount*texture(densityTexture,clamp(uvw,vec3(0.),vec3(1.))).r;
  float transmission=fogTransmission(extinctionPerM,incidentDensity,length(shadowPoint-optical));
  return lightColor[i]*lightShape[i].y*spot*attenuation*transmission*bulkPhase*visibility(lightShadow[i],shadowPoint);
}
float densityAt(vec3 position){
 vec3 coordinate=vec3(position.x/room.x+.5,position.y/room.y,position.z/room.z+.5);
 float density=texture(densityTexture,clamp(coordinate,vec3(0.),vec3(1.))).r;
 // Resolved density owns the plume. Coherent advected detail erodes dense jet
 // material into billows; it cannot create fog where the field is empty.
 float localDensity=max(0.,density-mixedDensity);
 float detailWeight=smoothstep(.10,.7,localDensity),structure=1.;
 if(detailWeight>.001){
  vec3 drift=vec3(-time*.13,-time*.04,time*.02);
  vec3 p=position*1.6+drift;
  float warp=noise(p*.55+vec3(3.1,7.9,1.2));p+=vec3(warp,-warp,warp*.6)*1.4;
  float billow=.57*noise(p)+.29*noise(p*2.03+vec3(7.1,1.9,3.7))+.14*noise(p*4.07);
  float folds=smoothstep(.36,.64,billow);
  structure=mix(1.,.08+2.6*folds*folds,detailWeight);
 }
 return (density+(structure-1.)*localDensity)*amount;
}
void integrateFog(out vec4 volumeOutput){
 if(amount<=0.){volumeOutput=vec4(0.,0.,0.,1.);return;}
 vec4 farView=inverseProjection*vec4(vUv*2.-1.,1.,1.);farView/=farView.w;
 vec3 farWorld=(cameraWorld*farView).xyz;vec3 ray=normalize(farWorld-eye);
 float depth=texture(sceneDepth,vUv).r;
 vec4 surfaceView=inverseProjection*vec4(vUv*2.-1.,depth*2.-1.,1.);surfaceView/=surfaceView.w;
 vec3 surfaceWorld=(cameraWorld*surfaceView).xyz;
 vec2 interval=roomHit(eye,ray);float start=max(0.,interval.x),finish=min(interval.y,length(surfaceWorld-eye));
 if(finish<=start){volumeOutput=vec4(0.,0.,0.,1.);return;}
 vec4 lightIntervals[16];int rayLights[16];int rayLightCount=0;
 for(int i=0;i<16;i++){
  if(i>=lightCount)break;
  if(dot(lightColor[i],lightColor[i])<=.000001 || lightShape[i].y<=0.)continue;
  vec4 spans=vec4(coneHit(i,lightShape[i].x,eye,ray,start,finish),
                  coneHit(i,lightProfile[i].w,eye,ray,start,finish));
  // Most screen rays cross very few beams. Cull once, before marching, while
  // retaining each exact clipped interval (including sub-step narrow shafts).
  if(spans.y<=spans.x && spans.w<=spans.z)continue;
  lightIntervals[rayLightCount]=spans;rayLights[rayLightCount]=i;rayLightCount++;
 }
 float baseStep=(finish-start)/float(raySteps);
 vec2 detailSpan=vec2(1.,0.);
 if(all(lessThan(detailMin,detailMax)))detailSpan=boxHit(eye,ray,detailMin,detailMax);
 // Desktop has fewer spare evaluations than dense base intervals can occupy.
 // Distribute those subdivisions along the whole plume instead of refining
 // only its near end. Integer differences keep the total at most 72.
 int detailFirst=int(clamp(floor((detailSpan.x-start)/baseStep),0.,float(raySteps)));
 int detailLast=int(clamp(ceil((detailSpan.y-start)/baseStep),0.,float(raySteps)));
 int detailCount=max(0,detailLast-detailFirst),extra=min(72-raySteps,detailCount);
 float jitter=fract(52.9829189*fract(dot(gl_FragCoord.xy,vec2(.06711056,.00583715))));
 vec3 radiance=vec3(0.);float transmission=1.;
 int baseIndex=0,subdivision=0,subdivisions=1;
 float stepSize=baseStep;
 for(int n=0;n<72;n++){
  if(baseIndex>=raySteps||transmission<.002)break;
  float baseBegin=start+float(baseIndex)*baseStep;
  if(subdivision==0){
   // Upload bounds every cell that can enable procedural plume detail. Spend
   // the existing 72-sample ceiling there; uniform haze keeps its base budget.
   bool detailed=detailSpan.y>detailSpan.x && baseBegin<detailSpan.y && baseBegin+baseStep>detailSpan.x;
   subdivisions=detailed?max(1,72/raySteps):1;
   if(detailed && raySteps>36 && raySteps<72 && detailCount>0){
    int local=baseIndex-detailFirst;
    // Floating overlap can include a touching neighbor outside floor/ceil's
    // integer range. Only that range owns extra samples; otherwise a local=-1
    // interval can spend a 25th extra sample and truncate the far end of a ray.
    if(local>=0 && local<detailCount)
     subdivisions=1+(local+1)*extra/detailCount-local*extra/detailCount;
   }
   stepSize=baseStep/float(subdivisions);
  }
  float begin=baseBegin+float(subdivision)*stepSize;
  float distance=begin+(.2+.6*jitter)*stepSize;vec3 position=eye+ray*distance;
  float density=densityAt(position);
  float segment=fogTransmission(extinctionPerM,density,stepSize);
  // A bounded hemisphere approximation makes illuminated plumes visible outside
  // direct spots. Caller owns worklight/bounce estimates; this is not GI.
  // Sky irradiance also reaches material near the floor. A height-only mix
  // incorrectly made a warm white plume look like black smoke at its source.
  vec3 ambient=mix(ambientGround,ambientSky,.65+.15*clamp(position.y/room.y,0.,1.));
  if(density*extinctionPerM>.12 && dot(ambient,ambient)>0.){
   // Three coarse canopy samples approximate environment occlusion inside a
   // dense plume. Multiple scattering retains a bright floor, unlike opaque
   // smoke. This is a local lighting estimate, not a second fluid simulation.
   vec3 coordinate=vec3(position.x/room.x+.5,position.y/room.y,position.z/room.z+.5);
   float canopy=0.;
   canopy+=texture(densityTexture,clamp(coordinate+vec3(.15,.3,.05)/room,vec3(0.),vec3(1.))).r*.3;
   canopy+=texture(densityTexture,clamp(coordinate+vec3(.32,.7,.12)/room,vec3(0.),vec3(1.))).r*.4;
   canopy+=texture(densityTexture,clamp(coordinate+vec3(.55,1.3,.2)/room,vec3(0.),vec3(1.))).r*.6;
   ambient*=.78+.22*fogTransmission(extinctionPerM,canopy*amount,.56);
  }
  radiance+=transmission*(1.-segment)*.92*ambient;
  for(int light=0;light<16;light++){
   if(light>=rayLightCount)break;
   int i=rayLights[light];
   for(int layer=0;layer<2;layer++){
    bool halo=layer==1;
    vec2 span=halo?lightIntervals[light].zw:lightIntervals[light].xy;
    float lo=max(begin,span.x),hi=min(begin+stepSize,span.y);
    if(hi<=lo)continue;
    // Jitter inside the clipped light interval too, so lit density does not
    // align into marching slabs. The full interval still covers narrow beams.
    vec3 samplePosition=eye+ray*(lo+(.2+.6*jitter)*(hi-lo));
    float localDensity=densityAt(samplePosition);
    if(localDensity<=0.)continue;
    // Jitter resolves fine density; smooth optical distance/profile belongs at
    // the clipped interval's midpoint, independent of that random sample.
    vec3 opticalPosition=eye+ray*((lo+hi)*.5);
    float foreground=fogTransmission(extinctionPerM,density,lo-begin);
    float chord=hi-lo;
    // A narrow optical core can cross only one or two coarse steps. Resolve
    // its profile with two half-interval midpoints and one density sample.
    // Attenuate the second contribution through the first half.
    if(!halo && chord>=.125*baseStep && span.y-span.x<=2.*baseStep){
     float halfSegment=fogTransmission(extinctionPerM,localDensity,chord*.5);
     vec3 first=eye+ray*(lo+.25*chord),second=eye+ray*(lo+.75*chord);
     vec3 scattered=incident(i,first,localDensity,false)+halfSegment*incident(i,second,localDensity,false);
     radiance+=transmission*foreground*(1.-halfSegment)*.92*scattered;
    }else{
     radiance+=transmission*foreground*(1.-fogTransmission(extinctionPerM,localDensity,chord))*.92*
        incident(i,opticalPosition,localDensity,halo);
    }
   }
  }
  transmission*=segment;
  subdivision++;
  if(subdivision>=subdivisions){baseIndex++;subdivision=0;}
 }
 volumeOutput=vec4(radiance,transmission);
}`;

export const FOG_FRAGMENT = `${fogIntegrator}
out vec4 volumeOutput;
void main(){integrateFog(volumeOutput);}`;

export const FOG_COMPOSITE_FRAGMENT = `${fogIntegrator}
out vec4 compositeOutput;
uniform sampler2D sceneColor,volume;
uniform vec2 volumeSize;
uniform float nearPlane,farPlane;
float viewDepth(float z){return (nearPlane*farPlane)/(farPlane-(farPlane-nearPlane)*z);}
void main(){
 vec4 color=texture(sceneColor,vUv);
 if(amount<=0.){compositeOutput=color;return;}
 float center=viewDepth(texture(sceneDepth,vUv).r);
 vec2 p=vUv*volumeSize-.5,base=floor(p),fraction=fract(p);
 vec4 fog=vec4(0.);float total=0.;
 for(int y=0;y<2;y++)for(int x=0;x<2;x++){
  vec2 uv=(base+vec2(float(x),float(y))+.5)/volumeSize;
  float neighbor=viewDepth(texture(sceneDepth,uv).r);
  float spatial=(x==0?1.-fraction.x:fraction.x)*(y==0?1.-fraction.y:fraction.y);
  float weight=spatial*exp(-abs(neighbor-center)/max(.03,center*.012));
  fog+=texture(volume,uv)*weight;total+=weight;
 }
 // All neighboring samples can lie behind a thin foreground surface. Integrate
 // this pixel up to its actual depth instead of discarding the medium in front.
 if(total>.00001)fog/=total;else integrateFog(fog);
 compositeOutput=vec4(color.rgb*fog.a+fog.rgb,color.a);
}`;

const now = () => globalThis.performance?.now?.() ?? Date.now();
const emptyShadow = () => {
  const texture = new THREE.DataTexture(new Uint32Array([0xffffffff]), 1, 1, THREE.DepthFormat, THREE.UnsignedIntType);
  texture.internalFormat = 'DEPTH_COMPONENT24'; texture.compareFunction = THREE.LessEqualCompare;
  texture.minFilter = texture.magFilter = THREE.LinearFilter;
  texture.needsUpdate = true; return texture;
};

export class VolumetricFog {
  constructor(renderer, { quality = 'desktop' } = {}) {
    if (!FOG_QUALITY[quality]) throw new Error('Unknown fog quality');
    this.renderer = renderer; this.quality = quality; this.budget = FOG_QUALITY[quality]; this.disposed = false;
    // Local preview haze is an explicit maintained room setting, not a single
    // startup puff that disappears while the control remains unchanged. At
    // unit visibility this is 15% equivalent density; show-owned atmosphere
    // below replaces it completely with accepted emitted/mixed quantities.
    this.field = new FogField({ grid: this.budget.grid, initialHaze: .15, retainedHaze: .15 });
    this.field.burst(); this.amount = 0.5;
    this.packed = new Uint16Array(this.field.data.length);
    this.density = new THREE.Data3DTexture(this.packed, ...this.budget.grid);
    Object.assign(this.density, { format: THREE.RedFormat, type: THREE.HalfFloatType, internalFormat: 'R16F',
      minFilter: THREE.LinearFilter, magFilter: THREE.LinearFilter, unpackAlignment: 1 });
    this.dummyShadow = emptyShadow();
    this.target = new THREE.WebGLRenderTarget(1, 1, { type: THREE.HalfFloatType, format: THREE.RGBAFormat,
      depthBuffer: false, stencilBuffer: false, minFilter: THREE.LinearFilter, magFilter: THREE.LinearFilter });
    this.lights = []; this.updateMs = 0; this.uploads = 0; this.width = 1; this.height = 1;
    const vectors = n => Array.from({ length: n }, () => new THREE.Vector3());
    this.uniforms = {
      sceneDepth: { value: null }, densityTexture: { value: this.density }, inverseProjection: { value: new THREE.Matrix4() },
      cameraWorld: { value: new THREE.Matrix4() }, eye: { value: new THREE.Vector3() }, room: { value: new THREE.Vector3(...this.field.room) },
      detailMin: {value: new THREE.Vector3()}, detailMax: {value: new THREE.Vector3()},
      amount: { value: this.amount }, time: { value: 0 }, mixedDensity:{value:0}, extinctionPerM:{value:DEFAULT_EXTINCTION_PER_M}, raySteps: { value: this.budget.steps }, lightCount: { value: 0 },
      lightPosition: { value: vectors(16) }, lightDirection: { value: vectors(16) }, lightColor: { value: vectors(16) },
      lightShape: { value: vectors(16) }, lightShadow: { value: new Int32Array(16).fill(-1) },
      lightProfile: {value: Array.from({length:16}, () => new THREE.Vector4())},
      lightStart: { value: new Float32Array(16) },
      ambientSky: { value: new THREE.Vector3() }, ambientGround: { value: new THREE.Vector3() },
      shadowMatrix: { value: Array.from({ length: 4 }, () => new THREE.Matrix4()) },
      shadowTexel: { value: Array.from({ length: 4 }, () => new THREE.Vector2(1, 1)) }, shadowBias: { value: new Float32Array(4) }
    };
    for (let i = 0; i < 4; i++) this.uniforms[`shadow${i}`] = { value: this.dummyShadow };
    this.compositeUniforms = { ...this.uniforms, sceneColor: { value: null }, volume: { value: this.target.texture },
      volumeSize: { value: new THREE.Vector2(1, 1) }, nearPlane: { value: 0.08 }, farPlane: { value: 80 } };
    const material = (fragmentShader, uniforms) => new THREE.ShaderMaterial({ vertexShader: vertex, fragmentShader, uniforms,
      glslVersion: THREE.GLSL3, depthTest: false, depthWrite: false, blending: THREE.NoBlending, toneMapped: false });
    this.volumeMaterial = material(FOG_FRAGMENT, this.uniforms);
    this.compositeMaterial = material(FOG_COMPOSITE_FRAGMENT, this.compositeUniforms);
    this.quad = new THREE.Mesh(new THREE.PlaneGeometry(2, 2), this.volumeMaterial); this.quad.frustumCulled = false;
    this.scene = new THREE.Scene(); this.scene.add(this.quad); this.camera = new THREE.Camera();
    this.upload();
  }

  upload() {
    const [nx, ny, nz] = this.field.grid, [rx, ry, rz] = this.field.room;
    const mixed = this.showAtmosphere ? this.showAtmosphere.sample?.['mixed-density'] || 0 : this.field.retainedHaze;
    // Positive half-floats retain numeric order. Comparing packed values also
    // conservatively includes cells rounded onto the detail threshold.
    const threshold = THREE.DataUtils.toHalfFloat(mixed + .10);
    let x0 = nx, y0 = ny, z0 = nz, x1 = -1, y1 = -1, z1 = -1;
    for (let i = 0; i < this.packed.length; i++) {
      const packed = THREE.DataUtils.toHalfFloat(Math.min(65504,this.field.data[i]));
      this.packed[i] = packed;
      if (packed < threshold) continue;
      const x = i % nx, y = Math.floor(i / nx) % ny, z = Math.floor(i / (nx * ny));
      x0 = Math.min(x0, x); y0 = Math.min(y0, y); z0 = Math.min(z0, z);
      x1 = Math.max(x1, x); y1 = Math.max(y1, y); z1 = Math.max(z1, z);
    }
    // One whole cell of padding exceeds the trilinear filter's half-cell
    // support beyond a cell boundary. Empty bounds disable subdivision.
    const lower = this.uniforms.detailMin.value, upper = this.uniforms.detailMax.value;
    if (x1 < 0) { lower.set(1, 1, 1); upper.set(-1, -1, -1); }
    else {
      lower.set((Math.max(0, x0 - 1) / nx - .5) * rx, Math.max(0, y0 - 1) / ny * ry,
        (Math.max(0, z0 - 1) / nz - .5) * rz);
      upper.set((Math.min(nx, x1 + 2) / nx - .5) * rx, Math.min(ny, y1 + 2) / ny * ry,
        (Math.min(nz, z1 + 2) / nz - .5) * rz);
    }
    this.density.needsUpdate = true; this.uploads++;
  }
  setRoom(room) {
    if (!this.field.setRoom(room)) return;
    this.uniforms.room.value.fromArray(room);
    if (this.showAtmosphere) {
      this.showAtmosphere.suspend();
      this.field.clear();
    } else {
      this.field.burst();
    }
    this.upload();
  }
  get extinctionPerM() {return this.showAtmosphere?.extinctionPerM ?? DEFAULT_EXTINCTION_PER_M;}
  setAmount(amount) { if (!Number.isFinite(amount)) throw new Error('Invalid fog amount'); this.amount = Math.max(0, Math.min(1, amount)); }
  setAmbient({color, intensity = 0, groundColor = color} = {}) {
    if (!Number.isFinite(intensity) || intensity < 0) throw new Error('Invalid atmosphere ambient intensity');
    const set = (uniform, value) => {
      if (!value) return uniform.value.set(0, 0, 0);
      if (![value.r, value.g, value.b].every(v => Number.isFinite(v) && v >= 0)) throw new Error('Invalid atmosphere ambient color');
      uniform.value.set(value.r, value.g, value.b).multiplyScalar(intensity);
    };
    set(this.uniforms.ambientSky, color); set(this.uniforms.ambientGround, groundColor);
  }
  setAtmosphere(identity, sample) {
    if (sample != null) {
      if (!this.showAtmosphere) {this.showAtmosphere = new ShowAtmosphere(); this.field.clear(0); this.upload();}
      this.showAtmosphere.accept(identity,sample);
    } else if (this.showAtmosphere) {
      this.showAtmosphere = null;
      this.field.clear(this.field.initialHaze); this.field.burst(); this.upload();
    }
  }
  burst(options) {
    if (this.showAtmosphere) return false;
    this.field.burst(options); return true;
  }
  setSize(width, height) {
    if (![width, height].every(v => Number.isFinite(v) && v > 0)) return;
    this.width = Math.max(1, Math.floor(width * this.budget.scale)); this.height = Math.max(1, Math.floor(height * this.budget.scale));
    this.target.setSize(this.width, this.height); this.compositeUniforms.volumeSize.value.set(this.width, this.height);
  }
  update(dt, lights = []) {
    if (this.disposed) return;
    const start = now(); this.lights = lights.slice(0, 16);
    const changed = this.showAtmosphere ? this.showAtmosphere.paint(this.field,dt) : this.field.advance(dt);
    if (changed) this.upload();
    this.updateMs = now() - start;
  }
  render(sceneTarget, camera, outputTarget = null) {
    if (this.disposed) return;
    if (!sceneTarget?.depthTexture || !sceneTarget?.texture) throw new Error('Fog requires opaque scene color and depth');
    const renderer = this.renderer, previousTarget = renderer.getRenderTarget(), autoClear = renderer.autoClear;
    const u = this.uniforms, c = this.compositeUniforms;
    u.sceneDepth.value = sceneTarget.depthTexture; u.inverseProjection.value.copy(camera.projectionMatrixInverse);
    u.cameraWorld.value.copy(camera.matrixWorld); u.eye.value.setFromMatrixPosition(camera.matrixWorld);
    const atmosphere = this.showAtmosphere;
    const visibleAmount = atmosphere && !atmosphere.localVolume && !atmosphere.sample?.['mixed-density'] ? 0 : this.amount;
    u.extinctionPerM.value = this.extinctionPerM;
    u.mixedDensity.value = atmosphere ? atmosphere.sample?.['mixed-density'] || 0 : this.field.retainedHaze;
    u.amount.value = visibleAmount; u.time.value = this.field.time; u.lightCount.value = this.lights.length;
    u.lightShadow.value.fill(-1); let shadows = 0;
    for (let slot = 0; slot < 4; slot++) u[`shadow${slot}`].value = this.dummyShadow;
    this.lights.forEach((light, i) => {
      u.lightPosition.value[i].copy(light.virtualPosition || light.position); u.lightDirection.value[i].copy(light.direction).normalize();
      u.lightColor.value[i].set(light.color.r, light.color.g, light.color.b);
      u.lightShape.value[i].set(Math.cos(light.angle), Math.max(0, light.intensity ?? 1), Math.max(0, light.distance ?? 0));
      const penumbra = Math.max(0, Math.min(1, light.penumbra ?? .55));
      const haloAngle = Math.max(light.angle, Math.min(1.54, light.angle + .23));
      const tangent = Math.tan(light.angle);
      u.lightProfile.value[i].set(Math.cos(light.angle * (1 - penumbra)), tangent,
        Math.tan(haloAngle) - tangent, Math.cos(haloAngle));
      u.lightStart.value[i] = Math.max(0, light.apexDistance ?? 0);
      if (light.shadow?.map?.depthTexture && shadows < this.budget.shadows) {
        if (light.shadow.map.depthTexture.compareFunction !== THREE.LessEqualCompare)
          throw new Error('Fog requires forward-depth PCF comparison shadow textures');
        const slot = shadows++; u.lightShadow.value[i] = slot;
        u[`shadow${slot}`].value = light.shadow.map.depthTexture;
        u.shadowMatrix.value[slot].copy(light.shadow.matrix);
        u.shadowTexel.value[slot].set(1 / light.shadow.map.width, 1 / light.shadow.map.height);
        u.shadowBias.value[slot] = light.shadow.bias ?? -0.0002;
      }
    });
    this.shadowCount = shadows;
    c.sceneColor.value = sceneTarget.texture; c.sceneDepth.value = sceneTarget.depthTexture;
    c.nearPlane.value = camera.near; c.farPlane.value = camera.far; c.amount.value = visibleAmount;
    try {
      renderer.autoClear = true;
      if (visibleAmount > 0) { renderer.setRenderTarget(this.target); this.quad.material = this.volumeMaterial; renderer.render(this.scene, this.camera); }
      renderer.setRenderTarget(outputTarget); this.quad.material = this.compositeMaterial; renderer.render(this.scene, this.camera);
    } finally { renderer.setRenderTarget(previousTarget); renderer.autoClear = autoClear; }
  }
  stats() { return { quality: this.quality, targetSize: [this.width, this.height], raySteps: this.budget.steps,
    maximumRaySteps: this.budget.steps > 36 ? 72 : this.budget.steps * Math.floor(72 / this.budget.steps),
    lightCount: this.lights.length, shadowCount: this.shadowCount ?? 0, cpuUpdateMs: this.updateMs, uploads: this.uploads, atmosphere:this.showAtmosphere?.stats(), ...this.field.stats() }; }
  dispose() {
    if (this.disposed) return; this.disposed = true;
    this.target.dispose(); this.density.dispose(); this.dummyShadow.dispose();
    this.volumeMaterial.dispose(); this.compositeMaterial.dispose(); this.quad.geometry.dispose(); this.scene.clear(); this.lights = [];
  }
}
