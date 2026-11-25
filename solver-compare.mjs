// Matched CPU solver comparison: master FogField vs #631 FogField.
import {FogField as Master, FOG_QUALITY} from './fog-field-master.mjs';
import {FogField as Branch} from '../js/preview/fog-field.js';
const scenario = (F, grid, haze) => {
  const f = new F({grid, room: [12, 5, 10], initialHaze: haze});
  f.burst({position: [-1.5, .35, 1], direction: [.7, .3, -.2], duration: 1.5, strength: 2.5});
  f.burst();
  return f;
};
const med = xs => [...xs].sort((x, y) => x - y)[xs.length >> 1];
const results = {};
for (const [tier, {grid}] of Object.entries(FOG_QUALITY)) for (const haze of [0.07, 0]) {
  const a = scenario(Master, grid, haze), b = scenario(Branch, grid, haze);
  for (let i = 0; i < 300; i++) { a.step(.05); b.step(.05); }
  let maxD = 0, maxT = 0;
  for (let i = 0; i < a.data.length; i++) {
    maxD = Math.max(maxD, Math.abs(a.data[i] - b.data[i]));
    maxT = Math.max(maxT, Math.abs(a.temperature[i] - b.temperature[i]));
  }
  const bytesEqual = Buffer.compare(Buffer.from(a.data.buffer), Buffer.from(b.data.buffer)) === 0
    && Buffer.compare(Buffer.from(a.temperature.buffer), Buffer.from(b.temperature.buffer)) === 0;
  const time = (f, n) => { const t = performance.now(); for (let i = 0; i < n; i++) f.step(.05); return (performance.now() - t) / n; };
  const n = tier === 'high' ? 20 : 60, ma = [], mb = [];
  for (let r = 0; r < 15; r++) { ma.push(time(a, n)); mb.push(time(b, n)); }
  results[`${tier}/haze=${haze}`] = {grid, cells: a.data.length, bytesEqual, maxDensityDiff: maxD, maxTempDiff: maxT,
    masterStepMs: +med(ma).toFixed(3), branchStepMs: +med(mb).toFixed(3), speedup: +(med(ma) / med(mb)).toFixed(2)};
}
console.log(JSON.stringify({node: process.version, results}, null, 2));
