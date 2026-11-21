// A deliberately bounded WebGL2 room preview. Fixture output is authoritative;
// the fog field and camera are local visual state, never lighting commands.
const VERTEX = `#version 300 es
in vec2 position;
out vec2 uv;
void main(){uv=position*.5+.5;gl_Position=vec4(position,0.,1.);}`;
const FRAGMENT = `#version 300 es
precision highp float;
precision highp sampler3D;
in vec2 uv;
out vec4 fragColor;
uniform vec2 resolution;
uniform vec3 camera;
uniform vec3 target;
uniform vec3 room;
uniform int lightCount;
uniform vec3 lightPos[16];
uniform vec3 lightDir[16];
uniform vec3 lightColor[16];
uniform float lightCone[16];
uniform int trussCount;
uniform vec3 trussPos[8];
// length, cos(yaw), sin(yaw); the long axis is local X.
uniform vec3 trussShape[8];
uniform sampler3D fog;
uniform float fogAmount;
uniform int steps;

vec2 roomBox(vec3 ro,vec3 rd){
  vec3 bmin=vec3(-room.x*.5,0.,-room.z*.5);
  vec3 bmax=vec3(room.x*.5,room.y,room.z*.5);
  vec3 inv=1./(rd+vec3(.00001));
  vec3 lo=(bmin-ro)*inv,hi=(bmax-ro)*inv;
  vec3 a=min(lo,hi),b=max(lo,hi);
  return vec2(max(max(a.x,a.y),a.z),min(min(b.x,b.y),b.z));
}
float cone(vec3 p,int i){
  vec3 delta=p-lightPos[i];
  float distance=length(delta);
  float alignment=dot(delta/max(distance,.001),lightDir[i]);
  float width=lightCone[i];
  float mask=smoothstep(cos(width),cos(width*.65),alignment);
  return mask/(1.+distance*distance*.025);
}
vec3 lighting(vec3 p,vec3 normal){
  vec3 result=vec3(0.);
  for(int i=0;i<16;i++){
    if(i>=lightCount)break;
    float lambert=max(dot(normal,normalize(lightPos[i]-p)),.05);
    result+=lightColor[i]*cone(p,i)*lambert*1.7;
  }
  return result;
}
float trussHit(vec3 ro,vec3 rd,int i,out vec3 normal){
  vec3 shape=trussShape[i],delta=ro-trussPos[i];
  vec3 axisX=vec3(shape.y,0.,-shape.z),axisZ=vec3(shape.z,0.,shape.y);
  vec3 origin=vec3(dot(delta,axisX),delta.y,dot(delta,axisZ));
  vec3 direction=vec3(dot(rd,axisX),rd.y,dot(rd,axisZ));
  vec3 halfSize=vec3(shape.x*.5,.14,.14),inv=1./(direction+vec3(.0000001));
  vec3 a=(-halfSize-origin)*inv,b=(halfSize-origin)*inv;
  vec3 near=min(a,b),far=max(a,b);
  float enter=max(max(near.x,near.y),near.z),leave=min(min(far.x,far.y),far.z);
  normal=vec3(0.);
  if(leave<=max(enter,0.))return -1.;
  float distance=enter>0.?enter:leave;
  vec3 point=origin+direction*distance,face=abs(point/halfSize);
  if(face.x>=face.y&&face.x>=face.z)normal=axisX*sign(point.x);
  else if(face.y>=face.z)normal=vec3(0.,sign(point.y),0.);
  else normal=axisZ*sign(point.z);
  return distance;
}
void main(){
  vec3 forward=normalize(target-camera),right=normalize(cross(forward,vec3(0.,1.,0.))),up=cross(right,forward);
  vec2 screen=(uv-.5)*2.;screen.x*=resolution.x/resolution.y;
  vec3 rd=normalize(forward+right*screen.x*.48+up*screen.y*.48);
  vec2 hit=roomBox(camera,rd);
  vec3 color=vec3(.0015,.0022,.0015);
  if(hit.y>max(hit.x,0.)){
    float start=max(hit.x,0.),finish=hit.y;
    vec3 surface=camera+rd*finish;
    vec3 normal=vec3(0.,1.,0.);
    if(abs(surface.y-room.y)<.02)normal=vec3(0.,-1.,0.);
    else if(abs(surface.x-room.x*.5)<.02)normal=vec3(-1.,0.,0.);
    else if(abs(surface.x+room.x*.5)<.02)normal=vec3(1.,0.,0.);
    else if(abs(surface.z-room.z*.5)<.02)normal=vec3(0.,0.,-1.);
    else if(abs(surface.z+room.z*.5)<.02)normal=vec3(0.,0.,1.);
    bool metal=false;
    for(int i=0;i<8;i++){
      if(i>=trussCount)break;
      vec3 beamNormal;float distance=trussHit(camera,rd,i,beamNormal);
      if(distance>start&&distance<finish){
        finish=distance;surface=camera+rd*finish;normal=beamNormal;metal=true;
      }
    }
    float grid=0.;
    if(!metal&&normal.y>.5){
      vec2 edge=abs(fract(surface.xz+.5)-.5);
      grid=1.-smoothstep(.008,.025,min(edge.x,edge.y));
    }
    vec3 material=metal?vec3(.055,.066,.06):vec3(.006,.009,.005)+grid*.006;
    color=material+lighting(surface,normal)*(metal?.65:.44);
    float transmittance=1.;vec3 volume=vec3(0.);
    float stride=(finish-start)/float(steps);
    float d=start+stride*.5;
    for(int step=0;step<80;step++){
      if(step>=steps)break;
      vec3 p=camera+rd*d;
      vec3 coord=(p+vec3(room.x*.5,0.,room.z*.5))/room;
      float density=texture(fog,coord).r*fogAmount;
      float absorption=exp(-density*stride*.3);
      vec3 scattered=vec3(.003,.004,.003);
      for(int i=0;i<16;i++){
        if(i>=lightCount)break;
        float phase=.3+.7*pow(max(dot(rd,lightDir[i]),0.),3.);
        scattered+=lightColor[i]*cone(p,i)*(phase+ .25)*3.8;
      }
      volume+=transmittance*(1.-absorption)*scattered;
      transmittance*=absorption;
      d+=stride;
    }
    color=color*transmittance+volume;
    // Compact fixture bodies are ray/sphere intersections against the same rig.
    for(int i=0;i<16;i++){
      if(i>=lightCount)break;
      vec3 delta=camera-lightPos[i];float b=dot(delta,rd);
      float discriminant=b*b-dot(delta,delta)+.0225;
      if(discriminant>0.){
        float t=-b-sqrt(discriminant);
        if(t>start&&t<finish)color=vec3(.1,.12,.09)+lightColor[i]*.7;
      }
    }
  }
  // Filmic compression without treating the preview as photometric calibration.
  color=vec3(1.)-exp(-color*1.45);
  color=pow(max(color,vec3(0.)),vec3(1./2.2));
  float vignette=1.-.23*dot(uv-.5,uv-.5);
  fragColor=vec4(color*vignette,1.);
}`;

const clamp = (v, low, high) => Math.max(low, Math.min(high, v));
export class RoomPreview {
  constructor(canvas, status) {
    this.canvas = canvas; this.status = status; this.room = [12, 5, 10]; this.rig = []; this.trusses = []; this.output = {};
    this.fogAmount = .66; this.grid = [32, 16, 32]; this.density = new Float32Array(32 * 16 * 32); this.nextDensity = new Float32Array(this.density.length); this.bytes = new Uint8Array(this.density.length);
    for (let z = 0; z < 32; z++) for (let y = 0; y < 16; y++) for (let x = 0; x < 32; x++) {
      const i = x + y * 32 + z * 512;
      // Begin with a soft room haze and shaped low-lying clouds, not solid cones.
      this.density[i] = .12 + .16 * Math.exp(-y / 6) * (1 + Math.sin(x * .31 + z * .25) * Math.cos(z * .3));
      this.bytes[i] = Math.round(this.density[i] * 255);
    }
    this.positions = new Float32Array(48); this.directions = new Float32Array(48); this.colors = new Float32Array(48); this.cones = new Float32Array(16);
    this.trussPositions = new Float32Array(24); this.trussShapes = new Float32Array(24);
    this.lastFog = 0; this.burstRemaining = 2; this.pointers = new Map(); this.resetCamera(); this.setupInteraction();
    try { this.initialize(); } catch (error) { status.textContent = `Preview unavailable: ${error.message}`; return; }
    this.observer = new ResizeObserver(() => this.resize()); this.observer.observe(canvas);
    canvas.addEventListener('webglcontextlost', event => { event.preventDefault(); this.lost = true; status.textContent = 'Graphics paused · waiting for context'; });
    canvas.addEventListener('webglcontextrestored', () => { try { this.initialize(); this.lost = false; } catch (error) { status.textContent = error.message; } });
    this.frame = requestAnimationFrame(time => this.animate(time));
  }
  initialize() {
    const gl = this.canvas.getContext('webgl2', { antialias: false, alpha: false, powerPreference: 'low-power' });
    if (!gl) throw new Error('WebGL2 is not supported by this browser.');
    this.gl = gl;
    const shader = (type, text) => { const s = gl.createShader(type); gl.shaderSource(s, text); gl.compileShader(s); if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) { const error = gl.getShaderInfoLog(s); gl.deleteShader(s); throw new Error(error); } return s; };
    const program = gl.createProgram(), vertex = shader(gl.VERTEX_SHADER, VERTEX), fragment = shader(gl.FRAGMENT_SHADER, FRAGMENT);
    gl.attachShader(program, vertex); gl.attachShader(program, fragment); gl.linkProgram(program); gl.deleteShader(vertex); gl.deleteShader(fragment);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(program));
    this.program = program; gl.useProgram(program);
    this.uniforms = Object.fromEntries(['resolution', 'camera', 'target', 'room', 'lightCount', 'lightPos[0]', 'lightDir[0]', 'lightColor[0]', 'lightCone[0]', 'trussCount', 'trussPos[0]', 'trussShape[0]', 'fog', 'fogAmount', 'steps'].map(key => [key, gl.getUniformLocation(program, key)]));
    this.buffer = gl.createBuffer(); gl.bindBuffer(gl.ARRAY_BUFFER, this.buffer); gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1, -1, 3, -1, -1, 3]), gl.STATIC_DRAW);
    const location = gl.getAttribLocation(program, 'position'); gl.enableVertexAttribArray(location); gl.vertexAttribPointer(location, 2, gl.FLOAT, false, 0, 0);
    this.texture = gl.createTexture(); gl.activeTexture(gl.TEXTURE0); gl.bindTexture(gl.TEXTURE_3D, this.texture);
    gl.texParameteri(gl.TEXTURE_3D, gl.TEXTURE_MIN_FILTER, gl.LINEAR); gl.texParameteri(gl.TEXTURE_3D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
    for (const parameter of [gl.TEXTURE_WRAP_S, gl.TEXTURE_WRAP_T, gl.TEXTURE_WRAP_R]) gl.texParameteri(gl.TEXTURE_3D, parameter, gl.CLAMP_TO_EDGE);
    gl.texImage3D(gl.TEXTURE_3D, 0, gl.R8, ...this.grid, 0, gl.RED, gl.UNSIGNED_BYTE, this.bytes);
    gl.uniform1i(this.uniforms.fog, 0); this.resize(); this.status.textContent = 'Orbit to explore';
  }
  resize() {
    if (!this.gl) return;
    const bounds = this.canvas.getBoundingClientRect(), pixelRatio = Math.min(window.devicePixelRatio || 1, window.innerWidth < 850 ? 1.25 : 1.5);
    if (!bounds.width || !bounds.height) return;
    this.canvas.width = Math.round(bounds.width * pixelRatio); this.canvas.height = Math.round(bounds.height * pixelRatio); this.gl.viewport(0, 0, this.canvas.width, this.canvas.height);
  }
  resetCamera() { this.yaw = .35; this.pitch = .26; this.distance = 13; }
  dispose() { cancelAnimationFrame(this.frame); this.observer?.disconnect(); this.gl?.deleteTexture(this.texture); this.gl?.deleteBuffer(this.buffer); this.gl?.deleteProgram(this.program); }
  setFog(value) { this.fogAmount = clamp(value, 0, 1) * 2.2; }
  burst() { this.burstRemaining = Math.min(this.burstRemaining + 3, 8); }
  update(state) {
    if (!state) return;
    const room = state.rig?.room || {};
    this.room = [room.width || 12, room.height || 5, room.depth || 10];
    this.rig = (state.rig?.fixtures || []).slice(0, 16); this.output = state.output?.values || {};
    // Preview-only geometry is bounded independently of the complete saved rig.
    this.trusses = (state.rig?.trusses || []).filter(truss =>
      Number.isFinite(truss.length) && truss.length > 0 && Array.isArray(truss.position) &&
      truss.position.length === 3 && truss.position.every(Number.isFinite)).slice(0, 8);
    this.trussPositions.fill(0); this.trussShapes.fill(0);
    this.trusses.forEach((truss, index) => {
      const degrees = truss['rotation-deg']?.[1], yaw = (Number.isFinite(degrees) ? degrees : 0) * Math.PI / 180;
      this.trussPositions.set(truss.position, index * 3);
      this.trussShapes.set([truss.length, Math.cos(yaw), Math.sin(yaw)], index * 3);
    });
    this.positions.fill(0); this.colors.fill(0); this.directions.fill(0); this.cones.fill(.12);
    this.rig.forEach((fixture, index) => {
      const output = this.output[fixture.id] || {}, p = fixture.position || [0, 3, 0], color = output['emitted-color'] || [0, 0, 0];
      this.positions.set(p, index * 3); this.colors.set(color.slice(0, 3).map(v => clamp(Number(v) || 0, 0, 4)), index * 3);
      const calibration = fixture.calibration || {}, panRange = calibration['pan-range'] || [-180, 180], tiltRange = calibration['tilt-range'] || [-90, 90];
      const pan = (output['pan-deg'] ?? (panRange[0] + (output.pan ?? .5) * (panRange[1] - panRange[0]))) * Math.PI / 180;
      const tilt = (output['tilt-deg'] ?? (tiltRange[0] + (output.tilt ?? .5) * (tiltRange[1] - tiltRange[0]))) * Math.PI / 180;
      // Tilt zero points down from a hanging head. Pan rotates around room Y.
      this.directions.set([Math.sin(pan) * Math.sin(tilt), -Math.cos(tilt), Math.cos(pan) * Math.sin(tilt)], index * 3);
      this.cones[index] = clamp((fixture['beam-angle'] || 12) * Math.PI / 360 + (output.zoom || 0) * .3, .02, .7);
    });
    const limits = [];
    if ((state.rig?.fixtures?.length || 0) > 16) limits.push('16 fixtures');
    if ((state.rig?.trusses?.length || 0) > 8) limits.push('8 trusses');
    if (limits.length) this.status.textContent = `Previewing first ${limits.join(' and ')}`;
    else if (this.status.textContent.startsWith('Previewing first ')) this.status.textContent = 'Orbit to explore';
  }
  diffuse(dt) {
    const [nx, ny, nz] = this.grid, a = this.density, b = this.nextDensity, strideZ = nx * ny;
    // A coarse semi-Lagrangian drift plus bounded six-neighbour diffusion.
    // The local visual field retains its shape between renderer snapshots.
    const drift = Math.min(.3, dt * 1.4), spread = Math.min(.12, dt * .7), decay = Math.exp(-dt * .035);
    for (let z = 0; z < nz; z++) for (let y = 0; y < ny; y++) for (let x = 0; x < nx; x++) {
      const i = x + y * nx + z * strideZ, center = a[i];
      const left = a[x ? i - 1 : i], right = a[x < nx - 1 ? i + 1 : i], below = a[y ? i - nx : i], above = a[y < ny - 1 ? i + nx : i], back = a[z ? i - strideZ : i], front = a[z < nz - 1 ? i + strideZ : i];
      b[i] = clamp((center + spread * (left + right + below + above + back + front - 6 * center) + drift * (below - center)) * decay + .00015 * dt, 0, 1);
      if (this.burstRemaining > 0) {
        const dx = (x / nx - .18) / .09, dy = (y / ny - .06) / .08, dz = (z / nz - .36) / .12;
        b[i] = Math.min(1, b[i] + Math.exp(-(dx * dx + dy * dy + dz * dz)) * dt * 2.8);
      }
      this.bytes[i] = Math.round(b[i] * 255);
    }
    this.density = b; this.nextDensity = a; this.burstRemaining = Math.max(0, this.burstRemaining - dt);
    this.gl.bindTexture(this.gl.TEXTURE_3D, this.texture); this.gl.texSubImage3D(this.gl.TEXTURE_3D, 0, 0, 0, 0, ...this.grid, this.gl.RED, this.gl.UNSIGNED_BYTE, this.bytes);
  }
  animate(time) {
    this.frame = requestAnimationFrame(nextTime => this.animate(nextTime));
    if (document.hidden || this.lost || !this.canvas.getClientRects().length) { this.lastFog = time; return; }
    // A volumetric preview need not consume every display refresh (120 Hz iPads).
    // Keep the show-control thread responsive and cap this independent view at 30.
    if (this.lastDraw && time - this.lastDraw < 1000 / 30) return;
    this.lastDraw = time;
    const gl = this.gl, u = this.uniforms;
    if (time - this.lastFog > 100) { this.diffuse(Math.min(.2, (time - this.lastFog) / 1000)); this.lastFog = time; }
    const center = [0, this.room[1] * .32, 0], distance = this.distance * Math.max(this.room[0], this.room[2]) / 12;
    const camera = [Math.sin(this.yaw) * Math.cos(this.pitch) * distance, center[1] + Math.sin(this.pitch) * distance, Math.cos(this.yaw) * Math.cos(this.pitch) * distance];
    gl.useProgram(this.program); gl.uniform2f(u.resolution, this.canvas.width, this.canvas.height); gl.uniform3fv(u.camera, camera); gl.uniform3fv(u.target, center); gl.uniform3fv(u.room, this.room);
    gl.uniform1i(u.lightCount, this.rig.length); gl.uniform3fv(u['lightPos[0]'], this.positions); gl.uniform3fv(u['lightDir[0]'], this.directions); gl.uniform3fv(u['lightColor[0]'], this.colors); gl.uniform1fv(u['lightCone[0]'], this.cones);
    gl.uniform1i(u.trussCount, this.trusses.length); gl.uniform3fv(u['trussPos[0]'], this.trussPositions); gl.uniform3fv(u['trussShape[0]'], this.trussShapes);
    gl.uniform1f(u.fogAmount, this.fogAmount); gl.uniform1i(u.steps, window.innerWidth < 600 ? 40 : 64); gl.drawArrays(gl.TRIANGLES, 0, 3);
  }
  setupInteraction() {
    this.canvas.addEventListener('pointerdown', event => { this.pointers.set(event.pointerId, [event.clientX, event.clientY]); this.canvas.setPointerCapture(event.pointerId); });
    this.canvas.addEventListener('pointermove', event => {
      const previous = this.pointers.get(event.pointerId); if (!previous) return;
      if (this.pointers.size === 1) { this.yaw -= (event.clientX - previous[0]) * .008; this.pitch = clamp(this.pitch + (event.clientY - previous[1]) * .006, -.05, 1.1); }
      else { const other = [...this.pointers.entries()].find(([key]) => key !== event.pointerId)?.[1]; if (other) { const oldDistance = Math.hypot(previous[0] - other[0], previous[1] - other[1]), newDistance = Math.hypot(event.clientX - other[0], event.clientY - other[1]); this.distance = clamp(this.distance + (oldDistance - newDistance) * .035, 7, 40); } }
      this.pointers.set(event.pointerId, [event.clientX, event.clientY]);
    });
    for (const event of ['pointerup', 'pointercancel', 'lostpointercapture']) this.canvas.addEventListener(event, e => this.pointers.delete(e.pointerId));
    this.canvas.addEventListener('wheel', event => { event.preventDefault(); this.distance = clamp(this.distance + event.deltaY * .015, 7, 40); }, { passive: false });
    this.canvas.addEventListener('keydown', event => { if (!['ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown', '+', '-', 'Home'].includes(event.key)) return; event.preventDefault(); if (event.key === 'Home') this.resetCamera(); if (event.key === 'ArrowLeft') this.yaw -= .1; if (event.key === 'ArrowRight') this.yaw += .1; if (event.key === 'ArrowUp') this.pitch = clamp(this.pitch + .1, -.05, 1.1); if (event.key === 'ArrowDown') this.pitch = clamp(this.pitch - .1, -.05, 1.1); if (event.key === '+') this.distance = clamp(this.distance - 1, 7, 40); if (event.key === '-') this.distance = clamp(this.distance + 1, 7, 40); });
  }
}
