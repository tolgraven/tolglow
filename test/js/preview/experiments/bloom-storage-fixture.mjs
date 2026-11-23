import {AdditiveBlending, DataTexture, FloatType, GLSL3, HalfFloatType, NoBlending, RGBAFormat,
  ShaderMaterial, WebGLRenderer, WebGLRenderTarget} from 'three';
import {ShaderPass} from 'three/addons/postprocessing/ShaderPass.js';
import {CopyShader} from 'three/addons/shaders/CopyShader.js';

export const width = 64;
export const height = 32;

function decodeHalf(bits) {
  const sign = bits & 0x8000 ? -1 : 1;
  const exponent = (bits >> 10) & 31;
  const fraction = bits & 1023;
  return sign * (exponent ? (1024 + fraction) * 2 ** (exponent - 25) : fraction * 2 ** -24);
}

export function bloomOperands() {
  const scene = new Float32Array(width * height * 4);
  const bloom = new Float32Array(scene.length);
  // Midpoints on even and odd mantissas, neighbors, exponent rollover, and
  // negative and subnormal sums. The rest are positive HDR values.
  const controls = [
    [0x3c00, 0x1000], [0x3c01, 0x1000], [0x3c00, 0x1001], [0x3c00, 0x0fff],
    [0x3fff, 0x1000], [0xbc01, 0x9000], [0x0401, 0x8400], [0x0400, 0x0001],
    [0x3c00, 0x3c00], [0x3c00, 0x0000]
  ];
  let state = 0x74d52ea1;
  function next() {
    state ^= state << 13;
    state ^= state >>> 17;
    state ^= state << 5;
    return state >>> 0;
  }
  for (let i = 0; i < scene.length; i++) {
    const sceneExponent = 10 + next() % 10;
    const bloomExponent = Math.max(1, sceneExponent - next() % 14);
    const sceneBits = (sceneExponent << 10) | (next() & 1023);
    const bloomBits = (bloomExponent << 10) | (next() & 1023);
    const [a, b] = controls[i] ?? [sceneBits, bloomBits];
    scene[i] = decodeHalf(a);
    bloom[i] = decodeHalf(b);
  }
  return {scene, bloom};
}

// The CPU oracle quantizes the magnitude by steps of one binary16 ULP. The
// GLSL toward-zero policy below uses bit fields instead of this arithmetic.
function roundHalf(value, nearest) {
  if (value === 0) return value;
  const magnitude = Math.abs(value);
  const step = 2 ** Math.max(-24, Math.floor(Math.log2(magnitude)) - 10);
  const units = magnitude / step;
  const lower = Math.floor(units);
  const fraction = units - lower;
  const above = nearest && (fraction > 0.5 || (fraction === 0.5 && lower % 2 === 1));
  return Math.sign(value) * (lower + Number(above)) * step;
}

const candidateShader = `
  uniform highp sampler2D tDiffuse;
  uniform highp sampler2D bloomTexture;
  uniform int conversion;
  out vec4 result;

  // This fixture supplies finite sums in the binary16 range.
  uint truncateHalf(float value) {
    uint bits = floatBitsToUint(value);
    uint signBit = (bits >> 16u) & 0x8000u;
    int exponent = int((bits >> 23u) & 255u) - 127;
    uint mantissa = bits & 0x7fffffu;
    if (exponent < -24) return signBit;
    if (exponent < -14) return signBit | ((mantissa | 0x800000u) >> uint(-exponent - 1));
    return signBit | (uint(exponent + 15) << 10u) | (mantissa >> 13u);
  }

  vec2 quantizeTowardZero(vec2 value) {
    return unpackHalf2x16(truncateHalf(value.x) | (truncateHalf(value.y) << 16u));
  }

  void main() {
    ivec2 pixel = ivec2(gl_FragCoord.xy);
    vec4 sum = texelFetch(tDiffuse, pixel, 0) + texelFetch(bloomTexture, pixel, 0);
    if (conversion == 1) {
      sum = vec4(unpackHalf2x16(packHalf2x16(sum.xy)), unpackHalf2x16(packHalf2x16(sum.zw)));
    } else if (conversion == 2) {
      sum = vec4(quantizeTowardZero(sum.xy), quantizeTowardZero(sum.zw));
    }
    result = sum;
  }
`;

export function compareBloomStorage({scene, bloom}) {
  const renderer = new WebGLRenderer({canvas: new OffscreenCanvas(width, height), antialias: false});
  const gl = renderer.getContext();
  renderer.autoClear = false;
  const copy = new ShaderPass(CopyShader);
  const input = [scene, bloom].map(values => {
    const texture = new DataTexture(new Float32Array(values), width, height, RGBAFormat, FloatType);
    texture.needsUpdate = true;
    return {texture};
  });
  const half = new WebGLRenderTarget(width, height, {type: HalfFloatType, depthBuffer: false});
  const full = new WebGLRenderTarget(width, height, {type: FloatType, depthBuffer: false});
  const candidate = new ShaderPass(new ShaderMaterial({glslVersion: GLSL3, blending: NoBlending,
    uniforms: {tDiffuse: {value: null}, bloomTexture: {value: input[1].texture}, conversion: {value: 0}},
    vertexShader: CopyShader.vertexShader.replace('varying vec2 vUv;', 'out vec2 vUv;'), fragmentShader: candidateShader}));

  function read(target) {
    renderer.setRenderTarget(target);
    const values = new Float32Array(width * height * 4);
    gl.readPixels(0, 0, width, height, gl.RGBA, gl.FLOAT, values);
    const error = gl.getError();
    if (error !== gl.NO_ERROR) throw new Error(`Bloom storage WebGL error 0x${error.toString(16)}`);
    return values;
  }
  function render(target, conversion) {
    candidate.uniforms.conversion.value = conversion;
    candidate.render(renderer, target, input[0]);
    return read(target);
  }
  function compare(a, b) {
    let changed = 0;
    let maximum = 0;
    let firstMismatch = null;
    for (let i = 0; i < a.length; i++) {
      if (a[i] === b[i]) continue;
      if (!Number.isFinite(a[i]) || !Number.isFinite(b[i])) throw new Error(`Non-finite output at component ${i}`);
      if (firstMismatch === null) firstMismatch = {component: i, scene: scene[i], bloom: bloom[i], a: a[i], b: b[i]};
      changed++;
      maximum = Math.max(maximum, Math.abs(a[i] - b[i]));
    }
    return {changed, maximum, firstMismatch};
  }
  try {
    if (!gl.getExtension('EXT_color_buffer_float')) throw new Error('Bloom storage requires EXT_color_buffer_float');
    copy.material.blending = NoBlending;
    copy.render(renderer, full, input[1]);
    const copySource = read(full);
    copy.render(renderer, half, input[0]);
    const initialStorage = read(half);
    copy.material.blending = AdditiveBlending;
    copy.material.premultipliedAlpha = true;
    copy.render(renderer, half, input[1]);
    const additiveBlend = gl.isEnabled(gl.BLEND) &&
      [gl.BLEND_EQUATION_RGB, gl.BLEND_EQUATION_ALPHA].every(parameter => gl.getParameter(parameter) === gl.FUNC_ADD) &&
      [gl.BLEND_SRC_RGB, gl.BLEND_DST_RGB, gl.BLEND_SRC_ALPHA, gl.BLEND_DST_ALPHA]
        .every(parameter => gl.getParameter(parameter) === gl.ONE);
    const blend = read(half);

    const gpuSum = render(full, 0);
    const cpuSum = scene.map((value, i) => Math.fround(value + bloom[i]));
    const cpuNearest = cpuSum.map(value => roundHalf(value, true));
    const cpuTowardZero = cpuSum.map(value => roundHalf(value, false));
    const paths = {blend, direct: render(half, 0), nearest: render(full, 1), towardZero: render(full, 2)};
    const comparisons = {};
    const names = Object.keys(paths);
    for (const [i, name] of names.entries()) {
      for (const other of names.slice(i + 1)) comparisons[`${name}/${other}`] = compare(paths[name], paths[other]);
      comparisons[`${name}/cpuNearest`] = compare(paths[name], cpuNearest);
      comparisons[`${name}/cpuTowardZero`] = compare(paths[name], cpuTowardZero);
    }
    const debug = gl.getExtension('WEBGL_debug_renderer_info');
    return {renderer: gl.getParameter(debug ? debug.UNMASKED_RENDERER_WEBGL : gl.RENDERER), glVersion: gl.getParameter(gl.VERSION),
      components: scene.length, controls: {additiveBlend, initialStorage: compare(initialStorage, scene),
        copySource: compare(copySource, bloom), gpuSum: compare(gpuSum, cpuSum), cpuPolicies: compare(cpuNearest, cpuTowardZero)}, comparisons};
  } finally {
    renderer.setRenderTarget(null);
    for (const {texture} of input) texture.dispose();
    copy.dispose();
    candidate.dispose();
    half.dispose();
    full.dispose();
    renderer.dispose();
  }
}
