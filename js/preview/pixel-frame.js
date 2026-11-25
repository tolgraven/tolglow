// Diffused pixel strips cross the render boundary as one owned Float32 RGBA
// block instead of one map per LED. Texel order follows the rig: layouts in
// first-appearance order, each sorted by pixel index. Colour is the committed
// emitted colour; alpha carries the shutter state that gates interpolation.
import {emittedColor} from './math.js';

export const PIXEL_TEXTURE_WIDTH = 256;

// A row that must not blend with its neighbour in time.
export const HOLD = -1;

export const diffusedPixel = fixture => Boolean(fixture?.['pixel-layout-id'] && fixture['diffuser-radius-m']);

export function pixelTextureSize(count) {
  return [PIXEL_TEXTURE_WIDTH, Math.max(1, Math.ceil(count / PIXEL_TEXTURE_WIDTH))];
}

/** Stable texel assignment for every diffused pixel of a rig. */
export function pixelOrder(fixtures = []) {
  const groups = new Map(), others = [];
  for (const fixture of fixtures) {
    if (!diffusedPixel(fixture)) {others.push(String(fixture.id)); continue;}
    const layout = fixture['pixel-layout-id'], group = groups.get(layout);
    if (group) group.push(fixture);
    else groups.set(layout, [fixture]);
  }
  const strips = [], ids = [];
  for (const [layout, group] of groups) {
    group.sort((a, b) => a['pixel-index'] - b['pixel-index']);
    strips.push({layout, start: ids.length, pixels: group});
    for (const fixture of group) ids.push(String(fixture.id));
  }
  return {ids, strips, others, size: pixelTextureSize(ids.length)};
}

const level = value => Number.isFinite(value) ? value : null;

// Interpolation keeps a row only while it stays continuously lit through the
// same shutter. Equal keys on both samples permit a blend; HOLD never does.
export function holdKey(row) {
  if (!row || row['cut?'] || row.strobe > 0 || row.channels?.strobe > 0) return HOLD;
  const shutter = row.shutter, channel = row.channels?.shutter;
  if (shutter === undefined && channel === undefined) return 0;
  const a = shutter === undefined ? 0 : level(shutter), b = channel === undefined ? 0 : level(channel);
  if (a === null || b === null || a < 0 || b < 0 || a > 1 || b > 1) return HOLD;
  return 1 + Math.round(a * 255) + 256 * Math.round(b * 255)
    + 65536 * (Number(shutter !== undefined) + 2 * Number(channel !== undefined));
}

/** Pack committed rows into a fresh block the caller owns and may transfer. */
export function packPixels(order, values = {}) {
  const [width, height] = order.size, block = new Float32Array(width * height * 4);
  for (let i = 0; i < order.ids.length; i++) {
    const row = values[order.ids[i]], rgb = emittedColor(row), at = i * 4;
    block[at] = rgb[0]; block[at + 1] = rgb[1]; block[at + 2] = rgb[2]; block[at + 3] = holdKey(row);
  }
  return block;
}

/** The rows that still travel as maps once pixels have their own block. */
export function withoutPixels(order, values = {}) {
  const rest = {};
  for (const id of order.others) if (Object.hasOwn(values, id)) rest[id] = values[id];
  return rest;
}

// CPU reference for the sleeve shader. `at` is a continuous pixel coordinate
// within one strip whose last index is `last`.
export function washedColor(colors, at, last) {
  const lower = Math.min(last, Math.floor(at)), mix = at - lower;
  const a = colors[Math.max(0, lower - 1)], b = colors[lower];
  const c = colors[Math.min(last, lower + 1)], d = colors[Math.min(last, lower + 2)];
  return b.map((value, axis) =>
    (1 - mix) * (.16 * a[axis] + .68 * value + .16 * c[axis]) +
    mix * (.16 * value + .68 * c[axis] + .16 * d[axis]));
}
