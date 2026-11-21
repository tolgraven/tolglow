/**
 * Optional WebGL2 numeric experiment. Never owns physical lighting output.
 * Input is afterglow.render.ir/emit-glsl metadata serialized as JSON.
 * CPU fallback, frame/epoch acceptance and scheduling belong to the caller.
 */
export class ComputeError extends Error {
  constructor(code, message, details = {}) {
    super(message);
    this.name = "ComputeError";
    this.code = code;
    this.details = details;
  }
}

export class UnsupportedError extends ComputeError {
  constructor(message, details = {}) {
    super("unsupported", message, details);
    this.name = "UnsupportedError";
  }
}

const FLOAT_MAX = 3.4028234663852886e38;
const TYPE_WIDTH = Object.freeze({ number: 1, vec2: 2, vec3: 3, vec4: 4 });
const identifier = (name) => typeof name === "string" && /^[A-Za-z_][A-Za-z0-9_]*$/.test(name);
const numericType = (type) => typeof type === "string" ? type.replace(/^:/, "") : type;
const supportedType = (type) => typeof type === "string" && Object.prototype.hasOwnProperty.call(TYPE_WIDTH, type);
const finiteFloat = (value) => typeof value === "number" && Number.isFinite(value) && Math.abs(value) <= FLOAT_MAX;

const vertexSource = `#version 300 es
precision highp float;
precision highp int;
layout(location = 0) in vec3 a_position;
uniform ivec2 u_dimensions;
out vec3 v_position;
void main() {
  int x = gl_VertexID % u_dimensions.x;
  int y = gl_VertexID / u_dimensions.x;
  vec2 center = vec2(float(x) + 0.5, float(y) + 0.5);
  gl_Position = vec4(center / vec2(u_dimensions) * 2.0 - 1.0, 0.0, 1.0);
  gl_PointSize = 1.0;
  v_position = a_position;
}`;

function compileShader(gl, type, source) {
  const shader = gl.createShader(type);
  if (!shader) throw new ComputeError("allocation", "Could not allocate a shader");
  gl.shaderSource(shader, source);
  gl.compileShader(shader);
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    const log = gl.getShaderInfoLog(shader);
    gl.deleteShader(shader);
    throw new ComputeError("shader-compile", "Shader compilation failed", { log });
  }
  return shader;
}

/** Create an isolated runner. Pass a dedicated canvas, never a shared renderer. */
export function createComputeRunner(canvas, options = {}) {
  if (!canvas) {
    if (typeof document !== "undefined") canvas = document.createElement("canvas");
    else if (typeof OffscreenCanvas !== "undefined") canvas = new OffscreenCanvas(1, 1);
    else throw new UnsupportedError("No canvas implementation is available");
  }
  const gl = canvas.getContext("webgl2", {
    antialias: false, alpha: false, depth: false, stencil: false,
    preserveDrawingBuffer: false, premultipliedAlpha: false,
    powerPreference: options.powerPreference || "high-performance",
  });
  if (!gl) throw new UnsupportedError("WebGL2 is unavailable");
  if (!gl.getExtension("EXT_color_buffer_float")) {
    throw new UnsupportedError("EXT_color_buffer_float is required for numeric RGBA32F output");
  }
  const maxElements = options.maxElements ?? 1_000_000;
  const timeoutMs = options.timeoutMs ?? 2000;
  const pollIntervalMs = options.pollIntervalMs ?? 0;
  if (!Number.isSafeInteger(maxElements) || maxElements <= 0 ||
      !Number.isFinite(timeoutMs) || timeoutMs <= 0 ||
      !Number.isFinite(pollIntervalMs) || pollIntervalMs < 0) {
    throw new ComputeError("invalid-options", "Invalid resource limit or timeout");
  }

  const maxTextureSize = gl.getParameter(gl.MAX_TEXTURE_SIZE);
  const debug = gl.getExtension("WEBGL_debug_renderer_info");
  const renderer = gl.getParameter(debug ? debug.UNMASKED_RENDERER_WEBGL : gl.RENDERER);
  const vendor = gl.getParameter(debug ? debug.UNMASKED_VENDOR_WEBGL : gl.VENDOR);
  const diagnostics = Object.freeze({
    version: gl.getParameter(gl.VERSION), shadingLanguage: gl.getParameter(gl.SHADING_LANGUAGE_VERSION),
    renderer, vendor, maxTextureSize,
    acceleration: /swiftshader|llvmpipe|softpipe|software|lavapipe/i.test(renderer) ? "software" : "unverified",
    floatTarget: "RGBA32F", readback: "PIXEL_PACK_BUFFER + fenceSync + getBufferSubData",
  });

  let disposed = false, lost = false, busy = false;
  let program = null, metadata = null, locations = null, activeFence = null;
  let texture = null, targetWidth = 0, targetHeight = 0, packBytes = 0;
  const framebuffer = gl.createFramebuffer();
  const vao = gl.createVertexArray();
  const positionsBuffer = gl.createBuffer();
  const packBuffer = gl.createBuffer();
  if (!framebuffer || !vao || !positionsBuffer || !packBuffer) {
    for (const buffer of [positionsBuffer, packBuffer]) if (buffer) gl.deleteBuffer(buffer);
    if (framebuffer) gl.deleteFramebuffer(framebuffer);
    if (vao) gl.deleteVertexArray(vao);
    throw new ComputeError("allocation", "Could not allocate WebGL resources");
  }
  gl.bindVertexArray(vao);
  gl.bindBuffer(gl.ARRAY_BUFFER, positionsBuffer);
  gl.enableVertexAttribArray(0);
  gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 0, 0);
  gl.bindVertexArray(null);
  gl.bindBuffer(gl.ARRAY_BUFFER, null);

  function onContextLost(event) {
    event.preventDefault?.();
    lost = true;
  }
  canvas.addEventListener?.("webglcontextlost", onContextLost);

  function assertReady() {
    if (disposed) throw new ComputeError("disposed", "The compute runner has been disposed");
    if (lost || gl.isContextLost()) throw new ComputeError("context-lost", "WebGL context was lost; recreate the runner after restoration");
  }

  function checkError(stage) {
    const error = gl.getError();
    if (error !== gl.NO_ERROR) throw new ComputeError("webgl-error", `WebGL failed during ${stage}`, { error });
  }

  function compile(nextMetadata) {
    assertReady();
    if (busy) throw new ComputeError("busy", "Cannot replace a program while a frame is in flight");
    if (!nextMetadata || typeof nextMetadata.source !== "string" ||
        !supportedType(numericType(nextMetadata.type)) ||
        !nextMetadata.uniforms || typeof nextMetadata.uniforms !== "object") {
      throw new ComputeError("invalid-shader", "Expected emitted shader source, result type and uniform metadata");
    }
    const bindings = Object.entries(nextMetadata.uniforms).map(([key, binding]) => {
      const type = numericType(binding?.type);
      if (!binding || !identifier(binding.name) || !supportedType(type)) {
        throw new ComputeError("invalid-shader", "Invalid parameter binding", { key });
      }
      return Object.freeze({ key, name: binding.name, type, width: TYPE_WIDTH[type] });
    });
    const inputs = nextMetadata.inputs || {};
    const timeName = inputs["time-ms"] || "u_time_ms";
    const elapsedName = inputs["elapsed-ms"] || "u_elapsed_ms";
    if (!identifier(timeName) || !identifier(elapsedName) ||
        (inputs.position && inputs.position !== "v_position")) {
      throw new ComputeError("invalid-shader", "The point runner expects v_position and valid clock uniform names");
    }
    const musicalBindings = ["beat", "elapsed-beats", "beats-per-bar", "bars-per-phrase"]
      .filter((key) => Object.prototype.hasOwnProperty.call(inputs, key))
      .map((key) => {
        if (!identifier(inputs[key])) throw new ComputeError("invalid-shader", "Invalid musical clock uniform", { key });
        return { key, name: inputs[key] };
      });
    let vertex = null, fragment = null, nextProgram = null;
    try {
      vertex = compileShader(gl, gl.VERTEX_SHADER, vertexSource);
      fragment = compileShader(gl, gl.FRAGMENT_SHADER, nextMetadata.source);
      nextProgram = gl.createProgram();
      if (!nextProgram) throw new ComputeError("allocation", "Could not allocate a shader program");
      gl.attachShader(nextProgram, vertex);
      gl.attachShader(nextProgram, fragment);
      gl.linkProgram(nextProgram);
      if (!gl.getProgramParameter(nextProgram, gl.LINK_STATUS)) {
        throw new ComputeError("shader-link", "Shader linking failed", { log: gl.getProgramInfoLog(nextProgram) });
      }
      const nextLocations = {
        dimensions: gl.getUniformLocation(nextProgram, "u_dimensions"),
        time: gl.getUniformLocation(nextProgram, timeName),
        elapsed: gl.getUniformLocation(nextProgram, elapsedName),
        musical: musicalBindings.map((binding) => ({ ...binding, location: gl.getUniformLocation(nextProgram, binding.name) })),
        bindings: bindings.map((binding) => ({ ...binding, location: gl.getUniformLocation(nextProgram, binding.name) })),
      };
      if (program) gl.deleteProgram(program);
      program = nextProgram;
      nextProgram = null;
      locations = nextLocations;
      // Copy interface fields so later caller mutation cannot change a linked program's bindings.
      metadata = Object.freeze({ type: numericType(nextMetadata.type) });
      return { type: metadata.type, parameters: bindings, diagnostics };
    } finally {
      if (vertex) gl.deleteShader(vertex);
      if (fragment) gl.deleteShader(fragment);
      if (nextProgram) gl.deleteProgram(nextProgram);
    }
  }

  function target(width, height) {
    if (width !== targetWidth || height !== targetHeight) {
      if (texture) gl.deleteTexture(texture);
      texture = gl.createTexture();
      if (!texture) throw new ComputeError("allocation", "Could not allocate a float target");
      gl.bindTexture(gl.TEXTURE_2D, texture);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
      gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
      gl.texStorage2D(gl.TEXTURE_2D, 1, gl.RGBA32F, width, height);
      gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
      gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, texture, 0);
      gl.drawBuffers([gl.COLOR_ATTACHMENT0]);
      if (gl.checkFramebufferStatus(gl.FRAMEBUFFER) !== gl.FRAMEBUFFER_COMPLETE) {
        throw new UnsupportedError("RGBA32F framebuffer is incomplete on this context");
      }
      targetWidth = width;
      targetHeight = height;
    } else gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
    gl.bindBuffer(gl.PIXEL_PACK_BUFFER, packBuffer);
    const bytes = width * height * 4 * Float32Array.BYTES_PER_ELEMENT;
    if (packBytes !== bytes) {
      gl.bufferData(gl.PIXEL_PACK_BUFFER, bytes, gl.STREAM_READ);
      packBytes = bytes;
    }
  }

  function waitForFence(fence, signal) {
    const started = performance.now();
    return new Promise((resolve, reject) => {
      function poll() {
        try {
          assertReady();
          if (signal?.aborted) throw new ComputeError("aborted", "GPU result was cancelled by its caller");
          if (performance.now() - started >= timeoutMs) throw new ComputeError("timeout", "GPU fence exceeded the configured deadline");
          const status = gl.clientWaitSync(fence, 0, 0);
          if (status === gl.ALREADY_SIGNALED || status === gl.CONDITION_SATISFIED) return resolve();
          if (status === gl.WAIT_FAILED) throw new ComputeError("fence-failed", "GPU fence wait failed");
          setTimeout(poll, pollIntervalMs);
        } catch (error) { reject(error); }
      }
      // Yield the browser thread so queued GPU work can progress; never gl.finish().
      setTimeout(poll, pollIntervalMs);
    });
  }

  async function run({ positions, params = {}, timeMs = 0, elapsedMs = 0, beat, elapsedBeats, meter = {}, frameId = null, epoch = null, signal } = {}) {
    assertReady();
    if (!program) throw new ComputeError("not-compiled", "Compile an emitted shader before running it");
    if (busy) throw new ComputeError("busy", "Only one frame may be in flight per runner");
    if (signal?.aborted) throw new ComputeError("aborted", "GPU result was cancelled by its caller");
    if (!(positions instanceof Float32Array) || positions.length === 0 || positions.length % 3 !== 0 ||
        positions.length / 3 > maxElements || !positions.every(Number.isFinite)) {
      throw new ComputeError("invalid-input", "Positions must be finite Float32Array xyz triples within maxElements");
    }
    if (!finiteFloat(timeMs) || !finiteFloat(elapsedMs)) throw new ComputeError("invalid-input", "Clock inputs must fit finite float32 values");
    const musicalInputs = { beat, "elapsed-beats": elapsedBeats,
      "beats-per-bar": meter?.beatsPerBar, "bars-per-phrase": meter?.barsPerPhrase };
    const musicalValues = locations.musical.map(({ key }) => {
      const value = musicalInputs[key];
      if (!finiteFloat(value) || ((key === "beats-per-bar" || key === "bars-per-phrase") && value <= 0)) {
        throw new ComputeError("invalid-input", "Missing or invalid requested musical clock/meter input", { key });
      }
      return value;
    });
    const count = positions.length / 3;
    const width = Math.min(maxTextureSize, Math.ceil(Math.sqrt(count)));
    const height = Math.ceil(count / width);
    if (height > maxTextureSize) throw new UnsupportedError("Fixture count exceeds the float target dimensions");
    // Validate before submitting anything. Domain constraints beyond finite values
    // (sqrt/pow/division etc.) remain the compiler/caller's responsibility.
    const parameterValues = locations.bindings.map((binding) => {
      const value = params[binding.key];
      const valid = binding.width === 1 ? finiteFloat(value) :
        (Array.isArray(value) || value instanceof Float32Array) && value.length === binding.width && Array.from(value).every(finiteFloat);
      if (!valid) throw new ComputeError("invalid-input", "Missing or invalid parameter", { key: binding.key, type: binding.type });
      return value;
    });

    busy = true;
    const started = performance.now();
    let fence = null;
    try {
      target(width, height);
      gl.useProgram(program);
      gl.bindVertexArray(vao);
      gl.bindBuffer(gl.ARRAY_BUFFER, positionsBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, positions, gl.STREAM_DRAW);
      gl.uniform2i(locations.dimensions, width, height);
      if (locations.time !== null) gl.uniform1f(locations.time, timeMs);
      if (locations.elapsed !== null) gl.uniform1f(locations.elapsed, elapsedMs);
      locations.musical.forEach((binding, index) => {
        if (binding.location !== null) gl.uniform1f(binding.location, musicalValues[index]);
      });
      locations.bindings.forEach((binding, index) => {
        if (binding.location === null) return;
        const value = parameterValues[index];
        if (binding.width === 1) gl.uniform1f(binding.location, value);
        else gl[`uniform${binding.width}fv`](binding.location, value);
      });
      const uploaded = performance.now();
      gl.viewport(0, 0, width, height);
      for (const capability of [gl.BLEND, gl.DEPTH_TEST, gl.STENCIL_TEST, gl.SCISSOR_TEST, gl.CULL_FACE, gl.DITHER]) gl.disable(capability);
      gl.colorMask(true, true, true, true);
      gl.drawArrays(gl.POINTS, 0, count);
      gl.readBuffer(gl.COLOR_ATTACHMENT0);
      gl.readPixels(0, 0, width, height, gl.RGBA, gl.FLOAT, 0);
      fence = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0);
      if (!fence) throw new ComputeError("fence-failed", "Could not allocate a GPU completion fence");
      activeFence = fence;
      gl.flush();
      checkError("dispatch/readback submission");
      const submitted = performance.now();
      await waitForFence(fence, signal);
      const ready = performance.now();
      const readback = new Float32Array(width * height * 4);
      gl.bindBuffer(gl.PIXEL_PACK_BUFFER, packBuffer);
      gl.getBufferSubData(gl.PIXEL_PACK_BUFFER, 0, readback);
      const values = readback.subarray(0, count * 4);
      checkError("readback copy");
      const completed = performance.now();
      if (!values.every(Number.isFinite)) throw new ComputeError("nonfinite-output", "Shader output contains NaN or infinity; use validated domains or CPU fallback");
      return {
        values, count, type: metadata.type, frameId, epoch,
        timings: { uploadMs: uploaded - started, submitMs: submitted - started,
          fenceWaitMs: ready - submitted, readbackMs: completed - ready, totalMs: completed - started },
        diagnostics,
      };
    } finally {
      if (fence && activeFence === fence) {
        gl.deleteSync(fence);
        activeFence = null;
      }
      if (!disposed && !gl.isContextLost()) {
        gl.bindBuffer(gl.PIXEL_PACK_BUFFER, null);
        gl.bindBuffer(gl.ARRAY_BUFFER, null);
        gl.bindVertexArray(null);
        gl.bindFramebuffer(gl.FRAMEBUFFER, null);
      }
      busy = false;
    }
  }

  function dispose() {
    if (disposed) return;
    disposed = true;
    canvas.removeEventListener?.("webglcontextlost", onContextLost);
    if (activeFence) { gl.deleteSync(activeFence); activeFence = null; }
    if (program) gl.deleteProgram(program);
    if (texture) gl.deleteTexture(texture);
    gl.deleteBuffer(packBuffer);
    gl.deleteBuffer(positionsBuffer);
    gl.deleteVertexArray(vao);
    gl.deleteFramebuffer(framebuffer);
  }

  return Object.freeze({ compile, run, dispose, diagnostics, canvas });
}
