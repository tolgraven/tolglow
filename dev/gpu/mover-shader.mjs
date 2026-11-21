// Browser benchmark only. Captures vertex outputs directly into a float buffer.
function compile(gl, source, type) {
  const shader = gl.createShader(type);
  gl.shaderSource(shader, source);
  gl.compileShader(shader);
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(shader));
  return shader;
}

function calibration(axis) {
  if (Array.isArray(axis)) return [axis[0] / 255, axis[1] / (180 * 255)];
  const width = axis['max-deg'] - axis['min-deg'];
  return [-axis['min-deg'] / width, 1 / width];
}

function configure(gl, fixtures, vertexSource) {
  const program = gl.createProgram();
  gl.attachShader(program, compile(gl, vertexSource, gl.VERTEX_SHADER));
  gl.attachShader(program, compile(gl, '#version 300 es\nprecision highp float;\nout vec4 color; void main(){color=vec4(0.0);}', gl.FRAGMENT_SHADER));
  gl.transformFeedbackVaryings(program, ['axes'], gl.INTERLEAVED_ATTRIBS);
  gl.linkProgram(program);
  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(program));
  gl.useProgram(program);
  const n = fixtures.length;
  const rig = new Float32Array(n * 13);
  for (let i = 0; i < n; ++i) {
    const f = fixtures[i], angles = f['rotation-deg'].map(deg => -deg * Math.PI / 180);
    rig.set([...f.position, ...angles.map(Math.cos), ...angles.map(Math.sin),
      ...calibration(f.calibration.pan), ...calibration(f.calibration.tilt)], i * 13);
  }
  gl.bindVertexArray(gl.createVertexArray());
  gl.bindBuffer(gl.ARRAY_BUFFER, gl.createBuffer());
  gl.bufferData(gl.ARRAY_BUFFER, rig, gl.STATIC_DRAW);
  for (const [location, size, offset] of [[1,3,0], [2,3,3], [3,3,6], [4,4,9]]) {
    gl.enableVertexAttribArray(location);
    gl.vertexAttribPointer(location, size, gl.FLOAT, false, 52, 4 * offset);
  }
  const input = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, input);
  gl.bufferData(gl.ARRAY_BUFFER, n * 16, gl.STREAM_DRAW);
  gl.enableVertexAttribArray(0);
  gl.vertexAttribPointer(0, 4, gl.FLOAT, false, 0, 0);
  const output = gl.createBuffer();
  gl.bindBufferBase(gl.TRANSFORM_FEEDBACK_BUFFER, 0, output);
  gl.bufferData(gl.TRANSFORM_FEEDBACK_BUFFER, n * 16, gl.STREAM_COPY);
  gl.bindBufferBase(gl.TRANSFORM_FEEDBACK_BUFFER, 0, null);
  const staging = gl.createBuffer();
  gl.bindBuffer(gl.COPY_WRITE_BUFFER, staging);
  gl.bufferData(gl.COPY_WRITE_BUFFER, n * 16, gl.STREAM_READ);
  gl.enable(gl.RASTERIZER_DISCARD);
  return { input, output, staging };
}

async function waitForReadback(gl) {
  const fence = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0);
  if (!fence) throw new Error('Unable to fence submitted work');
  gl.flush();
  const deadline = performance.now() + 5000;
  try {
    for (;;) {
      await new Promise(resolve => setTimeout(resolve, 0));
      const status = gl.clientWaitSync(fence, 0, 0);
      if (status === gl.WAIT_FAILED || gl.isContextLost()) throw new Error(`Readback failed (status ${status}, GL error ${gl.getError()}, context lost ${gl.isContextLost()})`);
      if (performance.now() > deadline) throw new Error('Readback exceeded 5 seconds');
      if (status !== gl.TIMEOUT_EXPIRED) return;
    }
  } finally { gl.deleteSync(fence); }
}

function pack(fixtures, axes, universeCount) {
  const bytes = new Uint8Array(512 * universeCount);
  for (let i = 0; i < fixtures.length; ++i) {
    const f = fixtures[i];
    const start = (f.universe - 4) * 512 + f.address - 1;
    for (let j = 0; j < 2; ++j) {
      const spec = j === 0 ? f.channels.pan : f.channels.tilt;
      let v = axes[i * 4 + j] * 255;
      const pivot = spec['inverted-from'];
      if (pivot != null && v >= pivot) v = 255 - (v - pivot);
      const coarse = Math.trunc(v);
      bytes[start + spec.offset - 1] = coarse;
      bytes[start + spec['fine-offset'] - 1] = Math.round((v - coarse) * 255);
    }
  }
  return bytes;
}

function distribution(samples) {
  const sorted = [...samples].sort((a,b) => a-b);
  function at(p) { return sorted[Math.ceil(sorted.length*p)-1]; }
  return { p50: at(.5), p95: at(.95), p99: at(.99), max: sorted.at(-1) };
}

async function capture(gl, buffers, fixtures, frame, resetHistory, target, axes, universeCount) {
  const { input, output, staging } = buffers;
  const start = performance.now();
  for (let i = 0; i < fixtures.length; ++i) {
    target.set(frame.aims[i], i * 4);
    target[i * 4 + 3] = resetHistory ? -1 : axes[i * 4];
  }
  gl.bindBuffer(gl.ARRAY_BUFFER, input);
  gl.bufferSubData(gl.ARRAY_BUFFER, 0, target);
  const uploaded = performance.now();
  gl.bindBufferBase(gl.TRANSFORM_FEEDBACK_BUFFER, 0, output);
  gl.beginTransformFeedback(gl.POINTS);
  gl.drawArrays(gl.POINTS, 0, fixtures.length);
  gl.endTransformFeedback();
  gl.bindBufferBase(gl.TRANSFORM_FEEDBACK_BUFFER, 0, null);
  gl.bindBuffer(gl.COPY_READ_BUFFER, output);
  gl.bindBuffer(gl.COPY_WRITE_BUFFER, staging);
  gl.copyBufferSubData(gl.COPY_READ_BUFFER, gl.COPY_WRITE_BUFFER, 0, 0, axes.byteLength);
  gl.bindBuffer(gl.COPY_READ_BUFFER, null);
  const pending = waitForReadback(gl); // Creates a fence and flushes before the first yield.
  const submitted = performance.now();
  await pending;
  const ready = performance.now();
  gl.bindBuffer(gl.COPY_READ_BUFFER, staging);
  gl.getBufferSubData(gl.COPY_READ_BUFFER, 0, axes);
  gl.bindBuffer(gl.COPY_READ_BUFFER, null);
  const copied = performance.now();
  const actual = pack(fixtures, axes, universeCount);
  const end = performance.now();
  if (gl.getError() !== gl.NO_ERROR) throw new Error('WebGL error after transform feedback');
  return { actual, timing: { uploadMs: uploaded-start, submitMs: submitted-uploaded, fenceWaitMs: ready-submitted,
    readbackMs: copied-ready, packMs: end-copied, totalMs: end-start } };
}

function compare(measured, fixtures, frame, sample, axes, actual) {
  const expected = Object.values(frame.universes).flat();
  let frameDifferences = 0;
  for (let i = 0; i < expected.length; ++i) {
    if (actual[i] !== expected[i]) ++frameDifferences;
    measured.cpuDigest = Math.imul(measured.cpuDigest ^ expected[i], 16777619) >>> 0;
    measured.shaderDigest = Math.imul(measured.shaderDigest ^ actual[i], 16777619) >>> 0;
  }
  measured.byteMismatches += frameDifferences;
  if (!frameDifferences) ++measured.exactFrames;
  for (let i = 0; i < fixtures.length; ++i) {
    const f = fixtures[i];
    if (!Number.isFinite(axes[i*4]) || !Number.isFinite(axes[i*4+1]) || axes[i*4+3] !== 1)
      throw new Error(`Invalid aim for head ${i} frame ${sample}`);
    if (!frame.axes[i][2]) ++measured.unreachableHeadFrames;
    const [panOffset, panScale] = calibration(f.calibration.pan);
    if (Math.abs(frame.axes[i][0] - (panOffset + panScale * frame.axes[i][3])) > 359 * Math.abs(panScale)) {
      ++measured.wrappedPanHeadFrames;
    }
    if (Boolean(axes[i*4+2]) !== frame.axes[i][2]) ++measured.reachabilityMismatches;
    for (let j = 0; j < 2; ++j) {
      const err = Math.abs(axes[i*4+j] - frame.axes[i][j]);
      if (err > measured.maxAxisError) {
        measured.maxAxisError = err;
        measured.maxErrorSample = { frame: sample, head: i, axis: j === 0 ? 'pan' : 'tilt',
          cpu: frame.axes[i][j], shader: axes[i*4+j] };
      }
      const channel = j === 0 ? f.channels.pan : f.channels.tilt;
      const base = (f.universe - 4)*512 + f.address - 1;
      const c = base + channel.offset - 1, k = base + channel['fine-offset'] - 1;
      if (expected[c] !== actual[c]) ++measured.coarseMismatches;
      if (expected[k] !== actual[k]) ++measured.fineMismatches;
      const coarseDelta = expected[c] - actual[c], fineDelta = expected[k] - actual[k];
      const delta = coarseDelta * 255 + fineDelta;
      measured.maxWordError = Math.max(measured.maxWordError, Math.abs(coarseDelta * 256 + fineDelta));
      measured.maxPackedStepError = Math.max(measured.maxPackedStepError, Math.abs(delta));
    }
  }
}

export async function measure({ fixtures, frames }, vertexSource) {
  const gl = document.createElement('canvas').getContext('webgl2', { powerPreference: 'high-performance' });
  if (!gl) throw new Error('WebGL2 is required for the moving-head shader experiment');
  // This benchmark owns the context exclusively and drops all of its objects on exit.
  try {
    const n = fixtures.length;
    const buffers = configure(gl, fixtures, vertexSource);
    const target = new Float32Array(n * 4), axes = new Float32Array(n * 4);
    const debug = gl.getExtension('WEBGL_debug_renderer_info');
    const renderer = gl.getParameter(debug ? debug.UNMASKED_RENDERER_WEBGL : gl.RENDERER);
    const timing = [];
    const measured = { renderer, webgl: gl.getParameter(gl.VERSION), heads: n, frames: frames.length,
      warmup: 8, cpuDigest: 2166136261, shaderDigest: 2166136261, universes: Object.keys(frames[0].universes).length, byteMismatches: 0,
      coarseMismatches: 0, fineMismatches: 0, maxAxisError: 0, maxPackedStepError: 0, maxWordError: 0,
      reachabilityMismatches: 0, unreachableHeadFrames: 0, wrappedPanHeadFrames: 0, exactFrames: 0, comparisons: n * frames.length * 2,
      framesOver25ms: 0, maxErrorSample: null };
    for (let sample = -measured.warmup; sample < frames.length; ++sample) {
      const frame = frames[sample < 0 ? 0 : sample];
      const result = await capture(gl, buffers, fixtures, frame, sample <= 0, target, axes, measured.universes);
      if (sample < 0) continue;
      timing.push(result.timing);
      if (result.timing.totalMs > 25) ++measured.framesOver25ms;
      compare(measured, fixtures, frame, sample, axes, result.actual);
    }
    measured.timingMs = Object.fromEntries(Object.keys(timing[0]).map(key =>
      [key, distribution(timing.map(frame => frame[key]))]));
    // A tolerance gate makes catastrophic angle, wrap, layout, and range regressions fail.
    // Exact byte differences remain visible; feed physical DMX only under a separate exact gate.
    measured.passed = measured.maxAxisError < 1e-5 && measured.maxPackedStepError <= 1 &&
      measured.reachabilityMismatches === 0 &&
      measured.byteMismatches === measured.coarseMismatches + measured.fineMismatches;
    return measured;
  } finally { gl.getExtension('WEBGL_lose_context')?.loseContext(); }
}
