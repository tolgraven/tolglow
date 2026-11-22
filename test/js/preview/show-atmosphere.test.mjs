import test from 'node:test';
import assert from 'node:assert/strict';
import { ShowAtmosphere } from '../../../js/preview/show-atmosphere.js';
import { FogField } from '../../../js/preview/fog-field.js';

const fogger = (id, position, local, emitted, flow = 2) => ({
  'fixture-id': id,
  position,
  direction: [1, 0.2, 0],
  'local-m3': local,
  'emitted-m3': emitted,
  'flow-m3-s': flow
});
const sample = (time, sources, mixed = 0, volume = 120) => ({
  'sample-time-ms': time,
  epoch: 1,
  'volume-m3': volume,
  'mixed-m3': mixed,
  'mixed-density': mixed / volume,
  sources
});
const near = (actual, expected) =>
  assert.ok(Math.abs(actual - expected) < 1e-5, `${actual} != ${expected}`);
const field = () => new FogField({ room: [10, 3, 4], grid: [20, 12, 16] });

test('accepted equivalent output is distributed once, independent of draw and show update rates', () => {
  const model = new ShowAtmosphere(),
    volume = field();
  const empty = sample(0, [fogger('a', [-2, 0.3, 0], 0, 0)]);
  model.accept('venue', empty);
  model.paint(volume, 0);
  near(volume.stats().massEstimate, 0);
  const live = sample(500, [fogger('a', [-2, 0.3, 0], 0.8, 1), fogger('b', [2, 0.3, 0], 0.1, 0.2)], 0.3);
  model.accept('venue', live);
  model.paint(volume, 0);
  near(volume.stats().massEstimate, 1.2);
  assert.ok(volume.stats().maximumDensity > 10 * volume.stats().meanDensity);
  for (let i = 0; i < 80; i++) {
    model.accept('venue', live);
    model.paint(volume, 1 / 120);
  }
  near(volume.stats().massEstimate, 1.2);
  assert.equal(model.accept('venue', empty), false);
  const mixed = sample(1000, [fogger('a', [-2, 0.3, 0], 0, 1, 0)], 1.2);
  model.accept('venue', mixed);
  model.paint(volume, 0);
  near(volume.stats().maximumDensity, 0.01);
  near(volume.stats().massEstimate, 1.2);
  assert.equal(model.stats().localVolumeM3, 0);
});

test('epoch, visibility and grid changes restore only mass still present in the latest sample', () => {
  const model = new ShowAtmosphere(),
    a = field();
  model.accept('old show', sample(100, [fogger('left', [-2, 0.2, 0], 10, 200)]));
  model.paint(a, 0);
  model.suspend();
  const latest = sample(90000, [fogger('left', [-2, 0.2, 0], 0.35, 1500, 0)], 3);
  model.accept('old show', latest);
  model.paint(a, 0);
  near(a.stats().massEstimate, 3.35);
  const b = new FogField({ grid: [14, 10, 8], room: [10, 3, 4] });
  model.paint(b, 0);
  near(b.stats().massEstimate, 3.35);
  model.accept('new show', sample(0, []));
  model.paint(b, 0);
  near(b.stats().massEstimate, 0);
  assert.equal(model.stats().foggerCount, 0);
});

test('one cubic metre of room haze halves in concentration at twice the venue volume', () => {
  for (const [w, v] of [
    [10, 120],
    [20, 240]
  ]) {
    const a = new ShowAtmosphere(),
      f = new FogField({ grid: [18, 8, 8], room: [w, 3, 4] });
    a.accept('venue', sample(0, [], 1, v));
    a.paint(f, 0);
    near(f.stats().meanDensity, 1 / v);
    near(f.stats().massEstimate, 1);
  }
});
