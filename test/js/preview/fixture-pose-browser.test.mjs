import test from 'node:test';
import assert from 'node:assert/strict';
import {existsSync, readFileSync} from 'node:fs';
import {chromium} from 'playwright-core';
import {measureShadowCache} from './shadow-cache-fixture.mjs';

test('fixture lens, shadow light, yoke and fog share the calibrated end-stop pose', {timeout: 90000}, async t => {
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!existsSync(executablePath)) return t.skip('Set CHROMIUM_PATH for the WebGL scene test');
  const oracle = JSON.parse(readFileSync(new URL('./fixtures/aim-oracle.json', import.meta.url)));
  const sample = oracle.cases.find(row => row.fixture.id === 'end-stop');
  const asset = new URL('../../../resources/studio/preview.js', import.meta.url);
  const browser = await chromium.launch({executablePath, args: ['--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
  try {
    const page = await browser.newPage();
    await page.route('http://localhost/pose', route => route.fulfill({contentType:'text/html',
      body:'<canvas style="width:320px;height:180px"></canvas><output></output>'}));
    await page.route('http://localhost/preview.js', route => route.fulfill({contentType:'text/javascript', body:readFileSync(asset)}));
    await page.goto('http://localhost/pose');
    const observation = await page.evaluate(async ({fixture, output, pose}) => {
      const {RoomPreview} = await import('/preview.js');
      const engine = new RoomPreview(document.querySelector('canvas'), document.querySelector('output'), {worker:false});
      try {
        engine.update({'show-id':'pose', epoch:1, revision:1, 'frame-number':1, 'time-ms':25,
          rig:{room:{width:12, height:5, depth:10}, fixtures:[fixture], trusses:[]},
          output:{values:{[fixture.id]:{...output, pose, 'emitted-color':[1,.5,.2]}}}});
        await engine.whenReady;
        engine.pause();
        const [entry] = engine.fixtures, light = entry.light;
        const beam = light.target.position.clone().sub(light.position).normalize();
        const lens = beam.clone().set(0,-1,0).applyQuaternion(entry.head.getWorldQuaternion(entry.head.quaternion.clone()));
        return {fog:engine.lightEntries[0].direction.toArray(), beam:beam.toArray(), lens:lens.toArray(),
          yoke:entry.yoke.rotation.y * 180 / Math.PI, shadow:light.castShadow, frames:engine.getTelemetry().frames};
      } finally {engine.dispose();}
    }, sample);
    assert.ok(observation.frames > 0);
    assert.equal(observation.shadow, true);
    for (const key of ['fog','beam','lens']) {
      observation[key].forEach((actual, axis) => assert.ok(Math.abs(actual-sample.expected[axis]) < 1e-8, key));
    }
    assert.ok(Math.abs(observation.yoke - 30) < 1e-8);
    const shadows = await page.evaluate(measureShadowCache);
    for (const key of ['fixed', 'camera', 'fog', 'color', 'zoom', 'aim', 'unlit', 'truss', 'mount', 'restore']) {
      assert.equal(shadows[key].identical, true, key);
      assert.deepEqual(shadows[key].forced, ['fill', 'key'], key);
    }
    for (const key of ['fixed', 'camera', 'fog', 'color']) assert.deepEqual(shadows[key].cached, [], key);
    assert.deepEqual(shadows.zoom.cached, ['key']);
    for (const key of ['aim', 'unlit', 'truss', 'mount', 'restore']) assert.deepEqual(shadows[key].cached, ['fill', 'key'], key);
    assert.equal(shadows.mapsStayedResident, true);
    assert.equal(shadows.glError, 0);
  } finally {await browser.close();}
});
