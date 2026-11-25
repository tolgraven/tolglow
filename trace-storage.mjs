// Renderer-local storage pilot for a bounded scalar history. Beats and labels
// are supplied separately from the packed values used by the SVG path.
export const maxSamples = 256;
export const historyViewHeight = 44;
export const historyVerticalSpan = 38;
export const pixelBudget = 0.25;

export const formats = Object.freeze({
  full: {bytes: 8, maxError: 0, allocate: n => new Float64Array(n)},
  unorm8: {bytes: 1, maxError: 0.5 / 255, scale: 255, allocate: n => new Uint8Array(n)},
  unorm16: {bytes: 2, maxError: 0.5 / 65535, scale: 65535, allocate: n => new Uint16Array(n)},
  fp16: {bytes: 2, maxError: 2 ** -12, allocate: n => new Float16Array(n)},
});

export function quantizationBound(format, rasterHeight) {
  return formats[format].maxError * historyVerticalSpan / historyViewHeight * rasterHeight;
}

function schemaIsEligible(schema) {
  return schema?.type === 'number' && !schema.choices?.length &&
    schema.bounds?.length === 2 && schema.bounds[0] === 0 && schema.bounds[1] === 1 &&
    (schema.dmxBits == null || schema.dmxBits === 8) &&
    schema.semanticType == null && schema.unit == null && schema.units == null &&
    schema.componentSchemas == null;
}

function isNormalized(value) {
  return Number.isFinite(value) && value >= 0 && value <= 1;
}

function selectFormat(samples, {mode = 'full', usage, schema, rasterHeight}) {
  if (mode === 'full') return {format: 'full', reason: 'full-selected'};
  if (mode !== 'auto') return {format: 'full', reason: 'unknown-mode'};
  if (usage !== 'observed-history' || !schemaIsEligible(schema)) {
    return {format: 'full', reason: 'unqualified-schema-or-usage'};
  }
  if (!(rasterHeight > 0 && Number.isFinite(rasterHeight))) {
    return {format: 'full', reason: 'unknown-raster-height'};
  }
  if (samples.length === 0 || samples.length > maxSamples) {
    return {format: 'full', reason: 'unqualified-samples'};
  }
  for (const value of samples) {
    if (!isNormalized(value)) return {format: 'full', reason: 'unqualified-samples'};
  }
  // On this fixed interval UNORM16 has the same storage cost and a tighter
  // bound than binary16. It also requires no Float16Array support.
  if (quantizationBound('unorm8', rasterHeight) <= pixelBudget) {
    return {format: 'unorm8', reason: 'within-raster-budget'};
  }
  if (quantizationBound('unorm16', rasterHeight) <= pixelBudget) {
    return {format: 'unorm16', reason: 'unorm8-raster-budget-exceeded'};
  }
  return {format: 'full', reason: 'raster-budget-exceeded'};
}

// Direct codec access is for side-by-side qualification on eligible samples.
export function encodeInto(samples, format, target) {
  const scale = formats[format].scale;
  if (scale) {
    for (let i = 0; i < samples.length; i++) target[i] = Math.round(samples[i] * scale);
  } else {
    target.set(samples);
  }
  return target;
}

export function buildTrace(samples, options = {}) {
  const selection = selectFormat(samples, options);
  const {format} = selection;
  const values = format === 'full'
    ? samples
    : encodeInto(samples, format, formats[format].allocate(samples.length));
  return {...selection, values, latestValue: samples.at(-1)};
}

export function graphPoints(values, format, beats, window) {
  const read = formats[format].scale
    ? index => values[index] / formats[format].scale
    : index => values[index];
  const start = beats.at(-1) - window;
  const points = [];
  let i = 0;
  while (i < beats.length && beats[i] < start) i++;
  if (i > 0 && i < beats.length && start < beats[i]) {
    const a = read(i - 1);
    const b = read(i);
    if (Number.isFinite(a) && Number.isFinite(b)) {
      const progress = (start - beats[i - 1]) / (beats[i] - beats[i - 1]);
      points.push([0, plotY(a + progress * (b - a))]);
    }
  }
  for (; i < beats.length; i++) {
    const value = read(i);
    points.push(Number.isFinite(value) ? [100 * ((beats[i] - start) / window), plotY(value)] : null);
  }
  return points;
}

function plotY(value) {
  return 41 - historyVerticalSpan * Math.max(0, Math.min(1, value));
}

export function graphPath(values, format, beats, window) {
  let path = '';
  let pen = 'M';
  for (const point of graphPoints(values, format, beats, window)) {
    if (point) {
      path += `${pen}${point[0]},${point[1]}`;
      pen = 'L';
    } else {
      pen = 'M';
    }
  }
  return path;
}
