// One dedicated WebGL2 context executes immutable Afterglow fixture-field plans.
// The show host owns time, target order, composition and physical publication.
const maxVertices = 262144, maxGroups = 4, maxResultFloats = 1048576, maxSourceBytes = 131072;
const identifier = /^[A-Za-z_][A-Za-z_0-9]{0,127}$/;
const fragment = '#version 300 es\nprecision highp float; out vec4 dummy; void main(){dummy=vec4(0.0);}';
const problem = (code, message) => Object.assign(new Error(message), {code});
const finiteFloat = n => typeof n === 'number' && Number.isFinite(Math.fround(n));

function vector(input, length, label) {
  if (!(input instanceof Float32Array || Array.isArray(input)) || input.length !== length)
    throw problem('invalid-input', `Expected ${length} finite floats for ${label}`);
  for (let i = 0; i < length; i++)
    if (!finiteFloat(input[i])) throw problem('invalid-input', 'Non-finite ' + label);
  return input instanceof Float32Array ? input : new Float32Array(input);
}

function layout(plan, maxStride) {
  if (typeof plan?.source !== 'string' || plan.source.length > maxSourceBytes || !plan.source.startsWith('#version 300 es\n'))
    throw problem('invalid-plan', 'Expected a bounded GLSL ES 3.00 vertex shader');
  if (!Array.isArray(plan.outputs) || !plan.outputs.length || plan.outputs.length > maxStride)
    throw problem('invalid-plan', 'Invalid output field list');
  let offset = 0;
  const varyings = new Set();
  for (const field of plan.outputs) {
    if (!field || !identifier.test(field.varying) || typeof field.varying !== 'string' ||
        varyings.has(field.varying) || field.offset !== offset || ![1,2,3,4].includes(field.components))
      throw problem('invalid-plan', 'Output fields must be distinct and contiguous');
    offset += field.components; varyings.add(field.varying);
  }
  if (offset !== plan.stride || offset > maxStride) throw problem('invalid-plan', 'Unsupported output stride');
  return [...varyings];
}

function shader(gl, type, source) {
  const object = gl.createShader(type);
  if (!object) throw problem('allocation', 'Could not allocate a shader');
  gl.shaderSource(object, source); gl.compileShader(object);
  if (gl.getShaderParameter(object, gl.COMPILE_STATUS)) return object;
  const log = (gl.getShaderInfoLog(object) || '').slice(0, 512);
  gl.deleteShader(object); throw problem('invalid-plan', 'Shader compilation failed: ' + log);
}

function linkedProgram(gl, plan, varyings) {
  let vertex, pixel;
  const program = gl.createProgram();
  if (!program) throw problem('allocation', 'Could not allocate a program');
  try {
    vertex = shader(gl, gl.VERTEX_SHADER, plan.source);
    pixel = shader(gl, gl.FRAGMENT_SHADER, fragment);
    gl.attachShader(program, vertex); gl.attachShader(program, pixel);
    gl.transformFeedbackVaryings(program, varyings, gl.INTERLEAVED_ATTRIBS); gl.linkProgram(program);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS))
      throw problem('invalid-plan', 'Could not link feedback varyings: ' + (gl.getProgramInfoLog(program) || '').slice(0, 512));
    return program;
  } catch (error) { gl.deleteProgram(program); throw error; }
  finally { if (vertex) gl.deleteShader(vertex); if (pixel) gl.deleteShader(pixel); }
}

function inspect(gl, program, fields) {
  const types = [gl.FLOAT, gl.FLOAT_VEC2, gl.FLOAT_VEC3, gl.FLOAT_VEC4];
  fields.forEach((field, i) => {
    const actual = gl.getTransformFeedbackVarying(program, i);
    if (actual?.name !== field.varying || actual.size !== 1 || actual.type !== types[field.components - 1])
      throw problem('invalid-plan', 'Linked shader output differs from the field layout');
  });
  const uniforms = new Map(), count = gl.getProgramParameter(program, gl.ACTIVE_UNIFORMS);
  if (count > 128) throw problem('invalid-plan', 'Too many shader uniforms');
  for (let i = 0; i < count; i++) {
    const uniform = gl.getActiveUniform(program, i), width = types.indexOf(uniform.type) + 1;
    if (!width || uniform.size !== 1) throw problem('invalid-plan', 'Unsupported uniform type');
    uniforms.set(uniform.name, {width, location: gl.getUniformLocation(program, uniform.name)});
  }
  return uniforms;
}

function releaseGroup(gl, group) {
  for (const buffer of group.buffers) gl.deleteBuffer(buffer);
  if (group.vao) gl.deleteVertexArray(group.vao);
  if (group.program) gl.deleteProgram(group.program);
}

function bindAttribute(gl, group, location, width, data) {
  const buffer = gl.createBuffer();
  if (!buffer) throw problem('allocation', 'Could not allocate a fixture buffer');
  group.buffers.push(buffer);
  gl.bindBuffer(gl.ARRAY_BUFFER, buffer); gl.bufferData(gl.ARRAY_BUFFER, data, gl.STATIC_DRAW);
  gl.enableVertexAttribArray(location); gl.vertexAttribPointer(location, width, gl.FLOAT, false, 0, 0);
}

function setUniforms(gl, group, values) {
  if (!values || typeof values !== 'object' || Array.isArray(values) || Object.keys(values).length > 128)
    throw problem('invalid-input', 'Expected a bounded map of GLSL uniforms');
  // Drivers may optimize out a declared uniform. Validate supplied inactive
  // values too, and require a fresh value for every active uniform each job.
  for (const [name, value] of Object.entries(values)) {
    if (!identifier.test(name) || ![1,2,3,4].includes(value?.length)) throw problem('invalid-input', 'Invalid uniform');
    vector(value, group.uniforms.get(name)?.width ?? value.length, name);
  }
  for (const [name, {width, location}] of group.uniforms) {
    if (!Object.hasOwn(values, name)) throw problem('invalid-input', 'Missing uniform ' + name);
    gl[`uniform${width}fv`](location, values[name]);
  }
}

export function createComputeExecutor({canvas, onAvailability = () => {}} = {}) {
  const gl = canvas?.getContext('webgl2', {antialias:false, alpha:false, depth:false, stencil:false});
  if (!gl) throw problem('unsupported', 'WebGL2 transform feedback is unavailable');
  const capabilities = {backend:'webgl2-transform-feedback', maxVertices, maxGroups, maxResultFloats,
    maxStride: Math.min(64, gl.getParameter(gl.MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS))};
  const groups = new Map();
  let scratch, staging, capacity = 0, pending, busy = false, suspended = false, disposed = false, lost = false;
  function ready() {
    if (disposed || suspended || lost || gl.isContextLost()) throw problem('unavailable', 'Compute execution is inactive');
    if (busy) throw problem('busy', 'One compute job is already in flight');
  }
  function check(stage) {
    const error = gl.getError();
    if (error !== gl.NO_ERROR) throw problem('gpu-error', `${stage} failed with GL error ${error}`);
  }
  function clear() {
    pending?.cancel();
    for (const group of groups.values()) releaseGroup(gl, group);
    groups.clear();
    if (scratch) gl.deleteBuffer(scratch);
    if (staging) gl.deleteBuffer(staging);
    scratch = staging = null; capacity = 0;
  }
  function contextLost(event) {
    event.preventDefault(); lost = true; clear(); onAvailability(false, 'context-lost');
  }
  function contextRestored() {
    lost = false;
    if (!disposed && !suspended) onAvailability(true, 'context-restored');
  }
  canvas.addEventListener('webglcontextlost', contextLost);
  canvas.addEventListener('webglcontextrestored', contextRestored);

  function prepare({id, plan, positions, surfaceUvs} = {}) {
    ready();
    if (typeof id !== 'string' || !id.length || id.length > 256) throw problem('invalid-input', 'Invalid prepared group ID');
    const count = positions?.length / 3;
    if (!Number.isInteger(count) || count < 1 || count > maxVertices) throw problem('invalid-input', 'Invalid fixture count');
    const xyz = vector(positions, count * 3, 'positions'), uv = surfaceUvs == null ? null : vector(surfaceUvs, count * 2, 'surface UVs');
    const varyings = layout(plan, capabilities.maxStride);
    const group = {count, stride:plan.stride, buffers:[]};
    try {
      group.program = linkedProgram(gl, plan, varyings);
      group.uniforms = inspect(gl, group.program, plan.outputs);
      group.vao = gl.createVertexArray();
      if (!group.vao) throw problem('allocation', 'Could not allocate vertex array');
      gl.bindVertexArray(group.vao);
      bindAttribute(gl, group, 0, 3, xyz);
      if (gl.getAttribLocation(group.program, 'v_uv') >= 0) {
        if (!uv) throw problem('invalid-input', 'Charted shader requires fixture surface UVs');
        bindAttribute(gl, group, 1, 2, uv);
      }
      check('prepare');
    } catch (error) { releaseGroup(gl, group); throw error; }
    finally { gl.bindVertexArray(null); gl.bindBuffer(gl.ARRAY_BUFFER, null); }
    // Publish only a fully linked replacement. The oldest unused group is the
    // first to leave the bounded cache. The host can send prepare again.
    const key = groups.has(id) ? id : groups.size === maxGroups ? groups.keys().next().value : null;
    if (key !== null) {releaseGroup(gl, groups.get(key)); groups.delete(key);}
    groups.set(id, group);
    return {id, count, stride:group.stride, evicted: key !== id ? key : null};
  }

  function completion() {
    const fence = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0);
    if (!fence) throw problem('gpu-error', 'Could not fence the compute job');
    gl.flush();
    return new Promise((resolve, reject) => {
      const deadline = performance.now() + 2500;
      let timer;
      const finish = error => {
        clearTimeout(timer); gl.deleteSync(fence); pending = null;
        if (error) reject(error); else resolve();
      };
      pending = {cancel: () => finish(problem('cancelled', 'Compute job cancelled'))};
      const poll = () => {
        const state = gl.clientWaitSync(fence, 0, 0);
        if (state === gl.ALREADY_SIGNALED || state === gl.CONDITION_SATISFIED) finish();
        else if (state === gl.WAIT_FAILED || performance.now() >= deadline) finish(problem('gpu-timeout', 'GPU readback did not complete'));
        else timer = setTimeout(poll, 2);
      };
      timer = setTimeout(poll, 0);
    });
  }

  async function run({id, uniforms = {}, start, count} = {}) {
    ready();
    const group = groups.get(id);
    if (!group) throw problem('not-prepared', 'No retained plan and rig for this group');
    if (!Number.isSafeInteger(start) || start < 0 || !Number.isSafeInteger(count) || count < 1 ||
        start + count > group.count || count * group.stride > maxResultFloats) throw problem('invalid-input', 'Invalid fixture shard');
    groups.delete(id); groups.set(id, group); busy = true;
    try {
      const floats = count * group.stride;
      gl.useProgram(group.program); setUniforms(gl, group, uniforms); gl.bindVertexArray(group.vao);
      if (!scratch) scratch = gl.createBuffer();
      if (!staging) staging = gl.createBuffer();
      if (!scratch || !staging) throw problem('allocation', 'Could not allocate readback storage');
      gl.bindBuffer(gl.TRANSFORM_FEEDBACK_BUFFER, scratch);
      gl.bindBuffer(gl.COPY_WRITE_BUFFER, staging);
      if (capacity < floats) {
        gl.bufferData(gl.TRANSFORM_FEEDBACK_BUFFER, floats * 4, gl.STREAM_COPY);
        gl.bufferData(gl.COPY_WRITE_BUFFER, floats * 4, gl.STREAM_READ);
        check('allocate'); capacity = floats;
      }
      gl.bindBufferBase(gl.TRANSFORM_FEEDBACK_BUFFER, 0, scratch);
      gl.enable(gl.RASTERIZER_DISCARD); gl.beginTransformFeedback(gl.POINTS);
      try { gl.drawArrays(gl.POINTS, start, count); }
      finally { gl.endTransformFeedback(); gl.disable(gl.RASTERIZER_DISCARD); }
      gl.bindBufferBase(gl.TRANSFORM_FEEDBACK_BUFFER, 0, null);
      gl.bindBuffer(gl.COPY_READ_BUFFER, scratch);
      gl.copyBufferSubData(gl.COPY_READ_BUFFER, gl.COPY_WRITE_BUFFER, 0, 0, floats * 4);
      gl.bindBuffer(gl.COPY_READ_BUFFER, null);
      check('draw and stage');
      await completion();
      const values = new Float32Array(floats);
      gl.getBufferSubData(gl.COPY_WRITE_BUFFER, 0, values); check('readback');
      if (!values.every(Number.isFinite)) throw problem('invalid-result', 'Shader returned a non-finite value');
      return values;
    } finally {
      gl.bindBufferBase(gl.TRANSFORM_FEEDBACK_BUFFER, 0, null); gl.bindBuffer(gl.TRANSFORM_FEEDBACK_BUFFER, null);
      gl.bindBuffer(gl.COPY_READ_BUFFER, null); gl.bindBuffer(gl.COPY_WRITE_BUFFER, null);
      gl.bindVertexArray(null); gl.useProgram(null); busy = false;
    }
  }
  return {capabilities, prepare, run,
    release(id) {
      if (busy) throw problem('busy', 'Cannot release a running plan');
      const group = groups.get(id);
      if (group) { releaseGroup(gl, group); groups.delete(id); }
    },
    suspend() {suspended = true; clear();},
    resume() {if (disposed) throw problem('unavailable', 'Compute executor was disposed'); suspended = false;},
    dispose() {
      if (disposed) return;
      disposed = true; clear(); canvas.removeEventListener('webglcontextlost', contextLost);
      canvas.removeEventListener('webglcontextrestored', contextRestored);
    }};
}
