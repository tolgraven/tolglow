import test from 'node:test';
import assert from 'node:assert/strict';
import {existsSync} from 'node:fs';
import {createServer} from 'node:http';
import {build} from 'esbuild';
import {chromium} from 'playwright-core';

// The Room's full-light budget through the actual worker proxy and worker, and
// on the main-thread renderer (#782): the budget changes the engine's full
// lights, Auto returns to the tier default and a hidden preview applies a
// change when it resumes.
async function probe(worker) {
  const fixtures = Array.from({length: 20}, (_, index) => ({id: `head-${String(index).padStart(2, '0')}`,
    position: [-5 + (index % 10) * 1.1, 3.6, -2 + Math.floor(index / 10) * 2], 'zoom-range-deg': [10, 30]}));
  let frame = 0;
  const snapshot = () => ({'show-id': 'budget', epoch: 1, revision: 1, 'frame-number': ++frame, 'time-ms': 25 * frame,
    rig: {room: {width: 12, height: 5, depth: 10}, fixtures, trusses: []},
    output: {values: Object.fromEntries(fixtures.map((fixture, index) =>
      [fixture.id, {'emitted-color': [.3 + (index % 4) * .15, .2, .1], direction: [0, -1, .1]}]))}});
  const canvas = document.createElement('canvas'); canvas.style.cssText = 'width:320px;height:200px'; document.body.append(canvas);
  const engine = new window.RoomPreview(canvas, null, {worker});
  const state = async () => {
    engine.update(snapshot()); await engine.draw(); engine.update(snapshot()); await engine.draw();
    const {lightLod, lightCap, thread} = engine.getTelemetry();
    return {thread, budget: lightLod?.budget, full: lightLod?.full, faked: lightLod?.fakedBeams, lightCap};
  };
  try {
    engine.setQuality('desktop'); engine.update(snapshot()); await engine.whenReady;
    const result = {auto: await state()};
    for (const n of [12, 4, 16, 0]) {engine.setLightBudget(n); result[n] = await state();}
    engine.setLightBudget(null); result.autoAgain = await state();
    engine.setQuality('high'); result.highAuto = await state();
    engine.setQuality('desktop');
    // Hidden: the window loses focus, the budget changes, then focus returns.
    window.dispatchEvent(new Event('blur'));
    const hiddenVisible = engine.visible;
    engine.setLightBudget(12);
    window.dispatchEvent(new Event('focus'));
    result.resumed = await state(); result.hiddenVisible = hiddenVisible;
    return result;
  } finally {engine.dispose(); canvas.remove();}
}

test('the full-light budget reaches the worker and main-thread renderers, including after a hidden change', {timeout: 180000}, async t => {
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!existsSync(executablePath)) return t.skip('Set CHROMIUM_PATH to exercise the actual worker and WebGL renderer');
  const bundle = await build({entryPoints: {preview: new URL('../../../js/preview/index.js', import.meta.url).pathname,
    'preview-worker': new URL('../../../js/preview/worker.js', import.meta.url).pathname}, outdir: '/virtual', bundle: true,
    format: 'esm', platform: 'browser', write: false});
  const files = new Map(bundle.outputFiles.map(file => [file.path.split('/').at(-1), file.text]));
  const server = createServer((request, response) => {
    const file = files.get(request.url.slice(1)); response.setHeader('Content-Type', file ? 'text/javascript' : 'text/html');
    response.end(file || `<!doctype html><script type="module">import {RoomPreview} from '/preview.js'; window.RoomPreview=RoomPreview;</script>`);
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  let browser;
  try {
    browser = await chromium.launch({executablePath, headless: true,
      args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
    const page = await browser.newPage(), errors = [];
    page.on('pageerror', error => errors.push(error.message));
    page.on('console', message => {if (message.type() === 'error') errors.push(message.text());});
    await page.goto(`http://127.0.0.1:${server.address().port}/`);
    await page.waitForFunction(() => window.RoomPreview);
    await page.bringToFront(); await page.evaluate(() => window.focus());
    for (const worker of [true, false]) {
      const result = await page.evaluate(probe, worker);
      t.diagnostic(JSON.stringify({worker, ...result}));
      if (worker) assert.equal(result.auto.thread, 'worker');
      // 20 lit heads: the desktop default is 8 full lights and 12 faked beams.
      assert.deepEqual(result.auto, {...result.auto, budget: 8, full: 8, faked: 12, lightCap: 8});
      assert.deepEqual(result[12], {...result.auto, budget: 12, full: 12, faked: 8, lightCap: 12});
      assert.deepEqual(result[4], {...result.auto, budget: 4, full: 4, faked: 16, lightCap: 4});
      assert.deepEqual(result[16], {...result.auto, budget: 16, full: 16, faked: 4, lightCap: 16});
      assert.equal(result[0].lightCap, 0); assert.equal(result[0].faked, 20);
      assert.deepEqual(result.autoAgain, result.auto);
      assert.equal(result.highAuto.budget, 16);
      assert.equal(result.hiddenVisible, false);
      assert.deepEqual(result.resumed, {...result.auto, budget: 12, full: 12, faked: 8, lightCap: 12});
    }
    assert.deepEqual(errors, []);
  } finally {await browser?.close(); await new Promise(resolve => server.close(resolve));}
});
