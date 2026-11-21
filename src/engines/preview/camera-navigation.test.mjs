import test from 'node:test';
import assert from 'node:assert/strict';
import {existsSync} from 'node:fs';
import {createServer} from 'node:http';
import {build} from 'esbuild';
import {chromium} from 'playwright-core';

test('real camera input, active-light tour and editor suspension agree in worker and fallback', {timeout: 120000}, async t => {
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!existsSync(executablePath)) return t.skip('Set CHROMIUM_PATH for real camera input');
  const bundle = await build({entryPoints: {preview: new URL('./index.js', import.meta.url).pathname,
    'preview-worker': new URL('./worker.js', import.meta.url).pathname}, outdir: '/virtual', bundle: true,
    format: 'esm', platform: 'browser', write: false});
  const files = new Map(bundle.outputFiles.map(file => [file.path.split('/').at(-1), file.text]));
  const server = createServer((request, response) => {
    const file = files.get(request.url.slice(1)); response.setHeader('Content-Type', file ? 'text/javascript' : 'text/html');
    response.end(file || `<!doctype html><style>canvas{width:240px;height:160px;touch-action:none}</style>
      <canvas tabindex="0"></canvas><input aria-label="REPL"><p id="status"></p>
      <script type="module">import {RoomPreview} from '/preview.js'; window.RoomPreview=RoomPreview;</script>`);
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  let browser;
  try {
    browser = await chromium.launch({executablePath, headless: true,
      args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
    for (const worker of [true, false]) {
      const page = await browser.newPage(), errors = [];
      page.on('pageerror', error => errors.push(error.message));
      await page.goto(`http://127.0.0.1:${server.address().port}/`);
      await page.waitForFunction(() => window.RoomPreview);
      await page.evaluate(async worker => {
        window.snapshot = {'show-id': 'camera-test', epoch: 1, revision: 1, 'frame-number': 1,
          rig: {room: {width: 12, height: 5, depth: 10}, trusses: [], fixtures: [
            {id: 'head-a', position: [-2, 3.5, -2]}, {id: 'head-b', position: [2, 3.5, -2]}]},
          output: {values: {'head-a': {'emitted-color': [.3, .1, 0]}, 'head-b': {'emitted-color': [0, 0, 0]}}}};
        window.engine = new RoomPreview(document.querySelector('canvas'), document.querySelector('#status'), {worker});
        engine.setFog(0); engine.update(snapshot); await engine.whenReady;
        window.position = () => engine.camera.position.toArray();
        window.nextFrames = async (count = 3) => {for (let i = 0; i < count; i++) await new Promise(requestAnimationFrame);};
      }, worker);
      assert.equal(await page.evaluate(() => engine.getTelemetry().thread || 'main'), worker ? 'worker' : 'main');

      // Real key events are scoped to the focused canvas; text input is untouched.
      await page.locator('canvas').focus();
      const start = await page.evaluate(() => position());
      await page.keyboard.down('w');
      await page.waitForFunction(start => Math.hypot(...position().map((v, i) => v - start[i])) > .03, start);
      await page.keyboard.up('w');
      assert.equal(await page.evaluate(() => engine.getNavigationState().mode), 'walk');
      await page.locator('input').fill(''); await page.locator('input').focus();
      const typingStart = await page.evaluate(() => position());
      await page.keyboard.type('wasd'); await page.evaluate(() => nextFrames());
      assert.deepEqual(await page.evaluate(() => position()), typingStart);
      assert.equal(await page.locator('input').inputValue(), 'wasd');

      const target = await page.evaluate(() => engine.controls.target.toArray());
      await page.mouse.move(100, 80); await page.mouse.down(); await page.mouse.move(145, 95, {steps: 4}); await page.mouse.up();
      assert.notDeepEqual(await page.evaluate(() => engine.controls.target.toArray()), target);
      await page.evaluate(() => engine.setInteractionActive(true));
      const editPose = await page.evaluate(() => ({position: position(), target: engine.controls.target.toArray()}));
      await page.keyboard.down('w'); await page.mouse.move(100, 80); await page.mouse.down();
      await page.mouse.move(150, 100); await page.mouse.up(); await page.evaluate(() => nextFrames()); await page.keyboard.up('w');
      assert.deepEqual(await page.evaluate(() => ({position: position(), target: engine.controls.target.toArray()})), editPose);
      await page.evaluate(() => engine.setInteractionActive(false));

      await page.evaluate(() => engine.setNavigationMode('tour'));
      await page.waitForFunction(() => engine.getNavigationState()['tour-fixture'] === 'head-a');
      const beforeUpdate = await page.evaluate(() => position());
      await page.evaluate(() => {snapshot = {...snapshot, revision: 2, output: {values: {'head-b': {'emitted-color': [.2, .2, .2]}}}}; engine.update(snapshot);});
      const afterUpdate = await page.evaluate(() => position());
      assert.ok(Math.hypot(...afterUpdate.map((v, i) => v - beforeUpdate[i])) < 1, 'Snapshot update cannot teleport the camera');
      await page.waitForFunction(() => engine.getNavigationState()['tour-fixture'] === 'head-b');
      await page.evaluate(() => {snapshot = {...snapshot, revision: 3, output: {values: {}}}; engine.update(snapshot);});
      await page.waitForFunction(() => engine.getNavigationState()['tour-waiting']);
      await page.locator('canvas').focus(); await page.keyboard.press('d');
      assert.equal(await page.evaluate(() => engine.getNavigationState().mode), 'walk', 'Manual input takes over immediately');

      await page.emulateMedia({reducedMotion: 'reduce'});
      await page.evaluate(() => engine.setNavigationMode('tour')); await page.evaluate(() => nextFrames());
      const reducedPose = await page.evaluate(() => position()); await page.evaluate(() => nextFrames(5));
      assert.deepEqual(await page.evaluate(() => position()), reducedPose);
      await page.emulateMedia({reducedMotion: 'no-preference'});
      await page.evaluate(() => engine.setNavigationMode('walk'));
      await page.locator('canvas').focus(); await page.keyboard.down('w');
      await page.evaluate(() => nextFrames()); await page.locator('input').focus();
      const blurPose = await page.evaluate(() => position()); await page.evaluate(() => nextFrames());
      assert.deepEqual(await page.evaluate(() => position()), blurPose); await page.keyboard.up('w');

      const hidden = await page.evaluate(async () => {
        engine.moveCamera('forward', true); await nextFrames(); engine.canvas.style.display = 'none';
        await nextFrames(); const pose = position(); engine.canvas.style.display = ''; await nextFrames();
        return {pose, after: position(), held: engine.navigation.buttons.size};
      });
      assert.deepEqual(hidden.pose, hidden.after); assert.equal(hidden.held, 0);
      const resize = await page.evaluate(() => {engine.canvas.style.width = '320px'; engine.resize(); return engine.camera.aspect;});
      assert.equal(resize, 2);
      const cleanup = await page.evaluate(async () => {
        engine.moveCamera('forward', true); const navigator = engine.navigation; engine.dispose();
        const pose = position(); await nextFrames(); return {pose, after: position(), disposed: navigator.disposed, held: navigator.buttons.size};
      });
      assert.deepEqual(cleanup.pose, cleanup.after); assert.equal(cleanup.held, 0); assert.ok(cleanup.disposed);
      assert.deepEqual(errors, []); await page.close();
    }
  } finally {await browser?.close(); await new Promise(resolve => server.close(resolve));}
});
