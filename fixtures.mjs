import {maxSamples} from './trace-storage.mjs';

export const schema = {type: 'number', bounds: [0, 1]};
export const zooms = [0.5, 1, 2, 4, 8, 16];
export const profiles = [
  {name: 'Push', width: 101, height: 40, dpr: 1},
  {name: 'Studio default', width: 220, height: 58, dpr: 1},
  {name: 'Studio monitor', width: 220, height: 70, dpr: 1},
  {name: 'Studio effect card', width: 220, height: 90, dpr: 1},
  {name: 'Studio detail', width: 320, height: 105, dpr: 1},
  {name: 'Studio detail 2×', width: 320, height: 105, dpr: 2},
  {name: 'Studio detail 3×', width: 320, height: 105, dpr: 3},
];

const phase = i => i * Math.PI * 2 / 49;
const tiny = 2 ** -21;
export const traces = [
  ['linear ramp', i => i / (maxSamples - 1)],
  ['modulated sine', i => 0.5 + 0.499 * Math.sin(phase(i)) * Math.cos(phase(i) * 0.23)],
  ['pulse', i => i % 23 < 3 ? 0.991456 : 0.008543],
  ['DMX8 steps', i => Math.floor((i % 96) / 12) * 31 / 255],
  ['near UNORM8 midpoint', i => 0.5 + tiny * Math.sin(phase(i))],
  ['near binary16 midpoint', i => 0.75 + 2 ** -12 + tiny * Math.sin(phase(i))],
  ['low amplitude, off midpoint', i => 0.74144 + 0.0003 * Math.sin(phase(i))],
  ['endpoints and gain changes', i => i < 66 ? 0 : i > 189 ? 1 : 0.45 + 0.45 * Math.sin(phase(i))],
].map(([name, fn]) => ({
  name,
  samples: Array.from({length: maxSamples}, (_, i) => fn(i)),
  beats: Float64Array.from({length: maxSamples}, (_, i) => 80.125 + i * 0.201 + 0.009 * Math.sin(i)),
}));

export function options(profile, mode = 'auto') {
  return {mode, usage: 'observed-history', schema, rasterHeight: profile.height * profile.dpr};
}
