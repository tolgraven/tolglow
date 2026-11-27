import test from 'node:test';
import assert from 'node:assert/strict';
import * as THREE from 'three';
import {prominence, rankLights, assignSlots, screenCoverage, viewFromMatrix, LOD_DEFAULTS} from '../../../js/preview/light-lod.js';
import {TIERS, lightBudget, MAX_FULL_LIGHTS} from '../../../js/preview/render-quality.js';
import {FOG_QUALITY, fogRasterScale} from '../../../js/preview/fog-field.js';
import {roomExit} from '../../../js/preview/beams.js';

// Audience camera looking at the stage from the front of a 12 × 5 × 10 m room.
function audienceView(position = [0, 1.7, 6.5], target = [0, 1.6, -2]) {
  const camera = new THREE.PerspectiveCamera(49, 16 / 9, .08, 100);
  camera.position.fromArray(position); camera.lookAt(...target); camera.updateMatrixWorld();
  const matrix = new THREE.Matrix4().multiplyMatrices(camera.projectionMatrix, camera.matrixWorldInverse);
  return viewFromMatrix(matrix.elements, position, Math.tan(camera.fov * Math.PI / 360));
}

const head = (id, x, level, extra = {}) => ({id, level, position: [x, 3.5, -1], direction: [0, -1, 0],
  angle: .15, length: 3.5, ...extra});

test('prominence ranks emission, screen size, visibility and beams aimed at the viewer', () => {
  const view = audienceView();
  assert.ok(prominence(head('a', 0, 1), view) > prominence(head('b', 0, .3), view));
  // Behind the camera: culled entirely.
  assert.equal(prominence(head('behind', 0, 1, {position: [0, 3.5, 9.5], length: .5, direction: [0, 0, 1]}), view), 0);
  assert.equal(screenCoverage({position: [0, 2, 30], direction: [0, 0, 1], angle: .1, length: 1}, view), 0);
  // A nearer beam covers more of the view than an identical distant one.
  const near = head('near', 0, 1, {position: [0, 3.5, 3]}), far = head('far', 0, 1, {position: [0, 3.5, -4.5]});
  assert.ok(prominence(near, view) > prominence(far, view));
  // Aimed toward the eye (looking into the lens) beats an equal beam aimed away.
  const toward = head('toward', 0, 1, {direction: new THREE.Vector3(0, -1.8, 7.5).normalize().toArray()});
  const away = head('away', 0, 1, {direction: new THREE.Vector3(0, -1.8, -7.5).normalize().toArray()});
  assert.ok(prominence(toward, view) > prominence(away, view) * 1.3);
  assert.equal(prominence(head('dark', 0, 0), view), 0);
});

test('rankLights keeps the budget, pins the selection and drops non-emitters', () => {
  const view = audienceView();
  const lights = Array.from({length: 40}, (_, i) => head(String(i).padStart(2, '0'), -5 + i * .25, .2 + (i % 7) / 10));
  for (const budget of [8, 16]) {
    const result = rankLights(lights, view, budget);
    assert.equal(result.full.length, budget);
    assert.equal(result.full.length + result.faked.length, 40);
    assert.equal(new Set([...result.full, ...result.faked]).size, 40);
    // Highest score first, and every full light outranks every faked one.
    const scores = result.full.map(id => result.scores.get(id));
    assert.deepEqual(scores, [...scores].sort((a, b) => b - a));
    const lowestFull = Math.min(...scores);
    assert.ok(result.faked.every(id => result.scores.get(id) <= lowestFull));
  }
  const pinned = lights.map(light => light.id === '00' ? {...light, level: .01, pinned: true} : light);
  assert.equal(rankLights(pinned, view, 8).full.includes('00'), true);
  const dark = lights.map((light, i) => i < 35 ? {...light, level: 0} : light);
  const result = rankLights(dark, view, 8);
  assert.equal(result.full.length, 5); assert.equal(result.faked.length, 0);
  // Deterministic regardless of candidate order.
  assert.deepEqual(rankLights([...lights].reverse(), view, 8).full, rankLights(lights, view, 8).full);
  assert.equal(rankLights(lights, view, 0).full.length, 0);
  // Out-of-view emitters only take slots no visible light wants.
  const hidden = head('hidden', 0, 1, {position: [0, 3.5, 9.5], direction: [0, 0, 1], length: .4});
  const spare = rankLights([hidden, lights[0], lights[1]], view, 4);
  assert.deepEqual(spare.full.slice(0, 2).sort(), ['00', '01']); assert.equal(spare.full[2], 'hidden');
  assert.equal(spare.scores.get('hidden'), 0);
  assert.equal(rankLights([hidden, ...lights], view, 8).full.includes('hidden'), false);
});

test('hysteresis keeps residents through small changes and swaps on clear wins', () => {
  const view = audienceView();
  const a = head('a', -1, 1), b = head('b', 1, .9);
  let result = rankLights([a, b], view, 1, null, 0);
  assert.deepEqual(result.full, ['a']);
  // b slightly overtakes a: within the margin, a stays.
  result = rankLights([{...a, level: .9}, {...b, level: 1}], view, 1, result.state, 5);
  assert.deepEqual(result.full, ['a']);
  // b clearly overtakes a (beyond the margin): b is promoted.
  result = rankLights([{...a, level: .5}, {...b, level: 1}], view, 1, result.state, 6);
  assert.deepEqual(result.full, ['b']);
  // Within its hold time, a newly promoted light is not demoted even by a clear win.
  result = rankLights([{...a, level: 1}, {...b, level: .2}], view, 1, result.state, 6 + LOD_DEFAULTS.hold / 2);
  assert.deepEqual(result.full, ['b']);
  result = rankLights([{...a, level: 1}, {...b, level: .2}], view, 1, result.state, 6 + LOD_DEFAULTS.hold * 2);
  assert.deepEqual(result.full, ['a']);
  // A resident that stops emitting frees its slot at once.
  result = rankLights([{...a, level: 0}, {...b, level: .2}], view, 1, result.state, 6 + LOD_DEFAULTS.hold * 2.1);
  assert.deepEqual(result.full, ['b']);
});

test('noisy emission around a rank boundary does not flicker between tiers', () => {
  const view = audienceView();
  // 24 lights with near-equal levels; each frame adds ±5% noise.
  let seed = 7;
  const random = () => { seed = (seed * 1664525 + 1013904223) >>> 0; return seed / 2 ** 32; };
  for (const budget of [8, 16]) {
    let state = null, previous = null, swaps = 0;
    for (let frame = 0; frame < 600; frame++) {
      const lights = Array.from({length: 24}, (_, i) => head('h' + i, -5 + i * .43, .6 * (1 + .1 * (random() - .5))));
      const result = rankLights(lights, view, budget, state, frame / 60);
      state = result.state;
      const set = new Set(result.full);
      if (previous) swaps += [...set].filter(id => !previous.has(id)).length;
      previous = set;
    }
    assert.equal(swaps, 0, `budget ${budget} swapped ${swaps} lights over 600 noisy frames`);
  }
});

test('slots persist and the shadow-mapped slots take the most prominent lights', () => {
  let slots = assignSlots(null, ['a', 'b', 'c'], 4, 2);
  assert.deepEqual(slots, ['a', 'b', 'c', null]);
  // Same set in a different order: shadow group {d,a}, plain {b}.
  slots = assignSlots(slots, ['d', 'a', 'b'], 4, 2);
  assert.deepEqual(slots.slice(0, 2).sort(), ['a', 'd']);
  assert.equal(slots[0], 'a');
  assert.ok(slots.slice(2).includes('b'));
  assert.equal(slots.filter(Boolean).length, 3);
  // A plain-slot light promoted into the shadow group leaves its plain slot.
  slots = assignSlots(['a', 'b', 'c', 'd'], ['c', 'a', 'b', 'd'], 4, 2);
  assert.deepEqual(slots.slice(0, 2).sort(), ['a', 'c']);
  assert.deepEqual(slots.slice(2).sort(), ['b', 'd']);
  assert.equal(new Set(slots).size, 4);
  // Unchanged choice: unchanged slots.
  assert.deepEqual(assignSlots(slots, ['c', 'a', 'b', 'd'], 4, 2), slots);
  assert.deepEqual(assignSlots(['a', 'b'], [], 2, 1), [null, null]);
});

test('tiers select light budgets, fog modes and bounded fog rasters', () => {
  assert.equal(lightBudget(TIERS.desktop), 8);
  assert.equal(lightBudget(TIERS.high), 16);
  assert.equal(lightBudget(TIERS.low), 4);
  assert.equal(lightBudget(TIERS.desktop, 16), 16);
  assert.equal(lightBudget(TIERS.desktop, 40), MAX_FULL_LIGHTS);
  assert.equal(lightBudget(TIERS.desktop, -2), 0);
  // Low has no raymarch; desktop and high keep it.
  assert.equal(FOG_QUALITY[TIERS.low.fog].mode, 'haze');
  assert.equal(FOG_QUALITY.desktop.mode, 'raymarch');
  assert.equal(FOG_QUALITY.high.mode, 'raymarch');
  // The raymarch raster follows the tier scale until its pixel cap, then stops growing.
  assert.equal(fogRasterScale(1280, 720, FOG_QUALITY.desktop), .5);
  const capped = fogRasterScale(2560, 1440, FOG_QUALITY.desktop);
  assert.ok(capped < .5 && Math.abs(2560 * capped * 1440 * capped - FOG_QUALITY.desktop.pixels) < 1);
  const fullscreen = fogRasterScale(3024, 1964, FOG_QUALITY.desktop);
  assert.ok(Math.abs(3024 * fullscreen * 1964 * fullscreen - FOG_QUALITY.desktop.pixels) < 1);
});

test('faked cones end at the first room surface along the beam', () => {
  const room = [12, 5, 10];
  assert.equal(roomExit([0, 3.5, 0], [0, -1, 0], room), 3.5);
  assert.ok(Math.abs(roomExit([0, 2, 0], [1, 0, 0], room) - 6) < 1e-9);
  assert.ok(Math.abs(roomExit([0, 4, -4], [0, 0, -1], room) - 1) < 1e-9);
  assert.ok(roomExit([0, 4, 0], [.6, -.8, 0], room) - 5 < 1e-9);
});
