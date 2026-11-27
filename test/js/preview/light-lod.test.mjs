import test from 'node:test';
import assert from 'node:assert/strict';
import * as THREE from 'three';
import {prominence, rankLights, assignSlots, crossfadeSlots, holdEmission, screenCoverage, viewFromMatrix, LOD_DEFAULTS} from '../../../js/preview/light-lod.js';
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
  assert.ok(Math.abs(roomExit([0, 4, 0], [.6, -.8, 0], room) - 5) < 1e-9);
});

test('shadow slots keep their lights while near-equal scores cross the shadow boundary', () => {
  const view = audienceView();
  let seed = 11;
  const random = () => { seed = (seed * 1664525 + 1013904223) >>> 0; return seed / 2 ** 32; };
  const options = {...LOD_DEFAULTS, shadowSlots: 4};
  for (const [count, budget] of [[12, 8], [24, 16]]) {
    let state = null, slots = null, shadowChanges = 0, slotChanges = 0, previousShadow = null;
    for (let frame = 0; frame < 600; frame++) {
      const lights = Array.from({length: count}, (_, i) => head('h' + i, -5 + i * (10 / count), .6 * (1 + .1 * (random() - .5))));
      const result = rankLights(lights, view, budget, state, frame / 60, options);
      state = result.state;
      const next = assignSlots(slots, result.full, budget, 4);
      if (slots) slotChanges += next.filter((id, i) => id !== slots[i]).length;
      const shadow = new Set(next.slice(0, 4));
      if (previousShadow) shadowChanges += [...shadow].filter(id => !previousShadow.has(id)).length;
      previousShadow = shadow; slots = next;
    }
    assert.equal(shadowChanges, 0, `${count} lights / ${budget}: shadow group changed ${shadowChanges} times`);
    assert.equal(slotChanges, 0, `${count} lights / ${budget}: slot occupants changed ${slotChanges} times`);
  }
  // A clear win still takes a shadow slot once the hold has passed.
  const a = head('a', -2, 1), b = head('b', 0, .95), c = head('c', 2, .3);
  let result = rankLights([a, b, c], view, 3, null, 0, {...LOD_DEFAULTS, shadowSlots: 1});
  assert.equal(result.full[0], 'a');
  result = rankLights([{...a, level: .9}, b, c], view, 3, result.state, 5, {...LOD_DEFAULTS, shadowSlots: 1});
  assert.equal(result.full[0], 'a');
  result = rankLights([{...a, level: .4}, b, {...c, level: 1}], view, 3, result.state, 6, {...LOD_DEFAULTS, shadowSlots: 1});
  assert.equal(result.full[0], 'c');
  // NaN geometry scores zero instead of corrupting the order.
  assert.equal(prominence(head('nan', NaN, 1), view), 0);
});

test('slot handover cross-fades instead of popping and never smooths a blackout', () => {
  const lit = new Set(['a', 'b']);
  // A light that just turned on (not previously drawn faked) is full at once.
  assert.deepEqual(crossfadeSlots(null, ['a'], lit, .1, .3), [{id: 'a', weight: 1}]);
  let slots = crossfadeSlots(null, ['a'], lit, .1, .3, new Set(['a']));
  assert.deepEqual(slots, [{id: 'a', weight: 0}]);
  slots = crossfadeSlots(slots, ['a'], lit, .15, .3);
  assert.equal(slots[0].weight, .5);
  slots = crossfadeSlots(slots, ['a'], lit, 1, .3);
  assert.equal(slots[0].weight, 1);
  // b replaces a: a fades out first, keeping the slot, then b fades in.
  const weights = [];
  for (let frame = 0; frame < 12; frame++) {
    slots = crossfadeSlots(slots, ['b'], lit, .05, .3, new Set(['b']));
    weights.push([slots[0].id, +slots[0].weight.toFixed(3)]);
  }
  assert.deepEqual(weights.slice(0, 5).map(w => w[0]), ['a', 'a', 'a', 'a', 'a']);
  assert.ok(weights[4][1] < weights[0][1]);
  assert.equal(weights.at(-1)[0], 'b'); assert.ok(weights.at(-1)[1] > .5);
  for (let i = 1; i < weights.length; i++) {
    const [id, w] = weights[i], [previousId, previous] = weights[i - 1];
    if (id === previousId) assert.ok(Math.abs(w - previous) <= .17, 'weights move by at most one step per frame');
  }
  // An occupant that stops emitting is released at once.
  slots = crossfadeSlots([{id: 'a', weight: 1}], ['a'], new Set(['b']), .01, .3);
  assert.deepEqual(slots, [{id: null, weight: 0}]);
  // A light moving between slots finishes fading out of the old slot first.
  slots = crossfadeSlots([{id: 'a', weight: 1}, {id: null, weight: 0}], [null, 'a'], lit, .05, .3);
  assert.equal(slots[0].id, 'a'); assert.equal(slots[1].id, null);
});

// The scene owner's per-frame LOD loop, reduced to the pure functions.
function simulate({frames, levelAt, count = 12, budget = 8, shadows = 4, fps = 60}) {
  const view = audienceView();
  let envelope = null, state = null, desired = null, slots = null, faked = new Set();
  const history = [];
  for (let frame = 0; frame < frames; frame++) {
    const now = frame / fps;
    const levels = new Map(Array.from({length: count}, (_, i) => ['h' + i, levelAt(i, frame)]));
    envelope = holdEmission(envelope, levels, now);
    const candidates = [...envelope].map(([id, {peak}]) => head(id, -5 + Number(id.slice(1)) * (10 / count), peak));
    const ranked = rankLights(candidates, view, budget, state, now, {...LOD_DEFAULTS, shadowSlots: shadows});
    state = ranked.state;
    desired = assignSlots(desired, ranked.full, budget, shadows);
    slots = crossfadeSlots(slots, desired, new Set(envelope.keys()), 1 / fps, .3, faked);
    const weight = new Map(slots.filter(s => s.id != null).map(s => [s.id, s.weight]));
    faked = new Set([...levels].filter(([id, level]) => level > 0 && !(weight.get(id) >= 1)).map(([id]) => id));
    history.push({levels, weight, slots: slots.map(s => s.id)});
  }
  return history;
}

test('a strobing key light stays a full light on its on-frames', () => {
  for (const hz of [10, 20]) {
    const period = 60 / hz;
    // h5 is the brightest head and strobes with a 50 % duty cycle; the rest hold.
    const history = simulate({frames: 600, levelAt: (i, frame) => i === 5 ? (frame % period < period / 2 ? 1 : 0) : .3 + .02 * i});
    const on = history.filter(frame => frame.levels.get('h5') > 0);
    const full = on.filter(frame => frame.weight.get('h5') >= 1).length;
    assert.ok(full / on.length >= .95, `${hz} Hz strobe: full on ${full} of ${on.length} on-frames`);
    // While dark it keeps its slot but the slot carries no light (output is zero upstream).
    assert.ok(history.filter(frame => frame.levels.get('h5') === 0).every(frame => frame.slots.includes('h5')));
  }
});

test('a light dark for longer than the hold releases its slot', () => {
  const history = simulate({frames: 240, levelAt: (i, frame) => i === 5 ? (frame < 60 ? 1 : 0) : .3 + .02 * i});
  assert.ok(history[59].slots.includes('h5'));
  const released = history.findIndex((frame, index) => index > 60 && !frame.slots.includes('h5'));
  assert.ok(released > 60 && (released - 60) / 60 <= LOD_DEFAULTS.hold + 2 / 60, `released at frame ${released}`);
  // A spare light takes the freed slot and is lit by then.
  assert.equal(history.at(-1).slots.filter(Boolean).length, 8);
});
