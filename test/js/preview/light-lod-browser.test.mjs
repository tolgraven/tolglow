import test from 'node:test';
import assert from 'node:assert/strict';
import {existsSync, readFileSync} from 'node:fs';
import {chromium} from 'playwright-core';

// Renderer-level light LOD invariants on the generated preview bundle (#782).
async function probe() {
  const {SceneRenderer} = await import('/preview.js');
  const engine = new SceneRenderer(new OffscreenCanvas(1, 1), null, {worker: true, autoStart: false,
    reducedMotion: true, coarsePointer: false, displayHz: 60, viewport: {width: 240, height: 150, pixelRatio: 1}});
  const wait = ms => new Promise(resolve => setTimeout(resolve, ms));
  const rig = count => ({room: {width: 12, height: 5, depth: 10}, trusses: [],
    fixtures: Array.from({length: count}, (_, i) => ({id: 'h' + String(i).padStart(2, '0'), channels: {pan: 1, tilt: 2},
      position: [-5 + (i % 12) * .9, 3.5, -3 + Math.floor(i / 12) * 1.5], 'zoom-range-deg': [10, 30]}))});
  let frame = 0;
  const snapshot = (r, level = i => .4 + (i % 5) * .1) => ({'show-id': 'lod', epoch: 1, revision: 1, 'frame-number': ++frame,
    'time-ms': 25 * frame, rig: r, output: {values: Object.fromEntries(r.fixtures.map((f, i) =>
      [f.id, {direction: [0, -1, .1], 'emitted-color': [level(i), level(i) * .5, level(i) * .2]}]))}});
  try {
    if (engine.error) throw Error(engine.error);
    engine.setQuality('desktop'); engine.setFog(.6); engine.setLightingMode('hold');
    const twelve = rig(12);
    engine.update(snapshot(twelve)); for (let i = 0; i < 4; i++) engine.draw(0);
    // Static camera and output: no re-lighting, no atlas re-accumulation.
    const relights = engine.relights || 0, redraws = engine.footprints.redraws;
    for (let i = 0; i < 10; i++) engine.draw(0);
    const staticFrames = {relights: (engine.relights || 0) - relights, redraws: engine.footprints.redraws - redraws};
    const desktop = engine.getTelemetry().lightLod;
    engine.restoreContext(); const restoredDirty = engine.footprints.dirty; engine.draw(0);
    const restoredRedraw = engine.footprints.redraws - redraws - staticFrames.redraws;
    // Cross-fade: make a faked light far brighter; after the hold it is promoted
    // through a fade, with a resident fading out first.
    const before = engine.slotStates.map(slot => slot.id);
    const faked = engine.fixtures.find(entry => entry.beam && !entry.light).id;
    await wait(700);
    engine.update(snapshot(twelve, i => twelve.fixtures[i].id === faked ? 1 : .3));
    const weights = [];
    for (let i = 0; i < 40 && weights.length < 60; i++) {
      engine.draw(0);
      weights.push(engine.slotStates.map(slot => [slot.id, slot.weight]));
      if (engine.slotStates.some(slot => slot.id === faked && slot.weight >= 1)) break;
      await wait(25);
    }
    const promoted = engine.slotStates.find(slot => slot.id === faked);
    // A blackout releases every slot at once, with no fade.
    engine.update(snapshot(twelve, () => 0)); engine.draw(0);
    const blackout = {occupied: engine.slotStates.filter(slot => slot.id != null).length,
      intensity: Math.max(...engine.spots.map(light => light.intensity))};
    // More lit fixtures than meshes in the Low tier: haze fakes every beam and
    // meshless fixtures still get beams and footprints.
    engine.setQuality('low');
    const forty = rig(40);
    engine.update(snapshot(forty)); for (let i = 0; i < 3; i++) engine.draw(0);
    const low = {...engine.getTelemetry().lightLod, meshes: engine.fixtures.length, active: engine.activeLights,
      mode: engine.fog.mode, spots: engine.spots.length};
    return {staticFrames, desktop, restoredDirty, restoredRedraw, before, faked, weights, promoted, blackout, low};
  } finally { engine.dispose(); }
}

test('light LOD skips static work, cross-fades handovers and covers meshless fixtures', {timeout: 120000}, async t => {
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!existsSync(executablePath)) return t.skip('Set CHROMIUM_PATH for the WebGL scene test');
  const asset = new URL('../../../resources/studio/preview.js', import.meta.url);
  const browser = await chromium.launch({executablePath, args: ['--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
  try {
    const page = await browser.newPage(), errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.route('http://localhost/lod', route => route.fulfill({contentType: 'text/html', body: '<p>lod</p>'}));
    await page.route('http://localhost/preview.js', route => route.fulfill({contentType: 'text/javascript', body: readFileSync(asset)}));
    await page.goto('http://localhost/lod');
    const result = await page.evaluate(probe);
    t.diagnostic(JSON.stringify({...result, weights: result.weights.length}));
    assert.deepEqual(errors, []);
    assert.deepEqual(result.staticFrames, {relights: 0, redraws: 0});
    // Desktop: 8 full lights; the other 4 lit fixtures are faked with footprints.
    assert.equal(result.desktop.budget, 8); assert.equal(result.desktop.fakedBeams, 4); assert.equal(result.desktop.footprints, 4);
    assert.equal(result.restoredDirty, true); assert.equal(result.restoredRedraw, 1);
    // The promoted light faded in; no slot weight jumped from 0 to 1 or 1 to 0 in one frame.
    assert.ok(result.promoted && result.promoted.weight >= 1, JSON.stringify(result.promoted));
    const fadedIn = result.weights.map(frame => frame.find(([id]) => id === result.faked)?.[1]).filter(w => w != null);
    assert.ok(fadedIn.some(w => w > 0 && w < 1), JSON.stringify(fadedIn));
    for (let i = 1; i < result.weights.length; i++) {
      result.weights[i].forEach(([id, weight], slot) => {
        const [previousId, previous] = result.weights[i - 1][slot];
        if (id === previousId && id != null) assert.ok(Math.abs(weight - previous) < 1, `slot ${slot} jumped`);
      });
    }
    assert.deepEqual(result.blackout, {occupied: 0, intensity: 0});
    assert.equal(result.low.mode, 'haze'); assert.equal(result.low.meshes, 32); assert.equal(result.low.spots, 4);
    assert.equal(result.low.active, 40); assert.equal(result.low.fakedBeams, 40); assert.equal(result.low.footprints, 36);
  } finally { await browser.close(); }
});
