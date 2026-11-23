import test from 'node:test';
import assert from 'node:assert/strict';
import {existsSync} from 'node:fs';
import {createServer} from 'node:http';
import {build} from 'esbuild';
import {chromium} from 'playwright-core';

const distance = (a, b) => Math.hypot(...a.map((v, i) => v - b[i]));

function assertGroundTravel(before, after, axis, sign, input) {
  const delta = after.map((value, index) => value - before[index]);
  assert.ok(delta[axis] * sign > .03, `${input} should move in the expected direction: ${delta}`);
  for (const [index, movement] of delta.entries()) {
    if (index !== axis) assert.ok(Math.abs(movement) < .001, `${input} should stay on its ground axis: ${delta}`);
  }
}

function speedBetween(frames, start, end) {
  return 1000 * distance(frames[start].position, frames[end].position) / (frames[end].time - frames[start].time);
}

function dispatchTouch(session, type, points = []) {
  return session.send('Input.dispatchTouchEvent', {
    type, touchPoints: points.map(([id, x, y]) => ({id, x, y})),
  });
}

async function exerciseCamera(page) {
  const canvas = page.locator('canvas');
  const touch = await page.context().newCDPSession(page);
  await canvas.focus();
  await page.evaluate(() => setTestPose());
  await page.evaluate(() => traceKey('keydown'));
  await page.keyboard.down('w');
  const acceleration = await page.evaluate(() => cameraTrace);
  await page.evaluate(() => traceKey('keyup'));
  await page.keyboard.up('w');
  const braking = await page.evaluate(() => cameraTrace);
  const initial = speedBetween(acceleration, 0, 2), cruising = speedBetween(acceleration, 6, 9);
  const justReleased = speedBetween(braking, 0, 2), slowing = speedBetween(braking, 6, 9);
  assert.ok(initial > 0 && initial < cruising * .85, `W accelerates gradually: ${initial} → ${cruising} m/s`);
  assert.ok(justReleased > .05 && slowing < justReleased * .65, `W brakes gradually: ${justReleased} → ${slowing} m/s`);
  assertGroundTravel(acceleration[0].position, braking.at(-1).position, 2, -1, 'w');

  for (const [keys, axis, sign] of [
    [['s', 'j', 'ArrowDown'], 2, 1], [['k', 'ArrowUp'], 2, -1],
    [['a', 'h', 'ArrowLeft'], 0, -1], [['d', 'l', 'ArrowRight'], 0, 1],
  ]) {
    for (const key of keys) {
      const before = await page.evaluate(() => setTestPose());
      await page.keyboard.down(key);
      await page.waitForFunction(({before, axis}) => Math.abs(position()[axis] - before[axis]) > .03, {before, axis}, {timeout: 3000});
      await page.keyboard.up(key);
      assertGroundTravel(before, await page.evaluate(() => position()), axis, sign, key);
      assert.equal(await page.evaluate(() => pose().mode), 'walk', key);
    }
  }

  await page.evaluate(() => setTestPose());
  const orbitStart = await page.evaluate(() => pose());
  await page.mouse.move(100, 80); await page.mouse.down();
  await page.mouse.move(147, 99, {steps: 4}); await page.mouse.up();
  const orbitEnd = await page.evaluate(() => pose());
  assert.ok(distance(orbitStart.position, orbitEnd.position) > .05, 'Mouse drag should move the Orbit camera around its target');
  assert.ok(Math.abs(orbitEnd.radius - orbitStart.radius) < .001);
  assert.equal(orbitEnd.mode, 'orbit');
  assert.deepEqual(orbitEnd.target, orbitStart.target);

  await page.evaluate(() => setTestPose('walk'));
  const walkStart = await page.evaluate(() => pose());
  await page.mouse.move(100, 80); await page.mouse.down();
  await page.mouse.move(145, 95, {steps: 4}); await page.mouse.up();
  await page.evaluate(() => nextFrames());
  const mouseLook = await page.evaluate(() => pose());
  assert.deepEqual(mouseLook.position, walkStart.position);
  assert.ok(distance(mouseLook.rotation, walkStart.rotation) > .01, 'Mouse drag should look around in Explore');

  await page.evaluate(() => setTestPose('walk'));
  const touchStart = await page.evaluate(() => pose());
  await dispatchTouch(touch, 'touchStart', [[1, 95, 90]]);
  await dispatchTouch(touch, 'touchMove', [[1, 125, 101]]);
  await page.evaluate(() => nextFrames());
  const touchLook = await page.evaluate(() => pose());
  assert.deepEqual(touchLook.position, touchStart.position);
  assert.ok(distance(touchLook.rotation, touchStart.rotation) > .01, 'One finger should look around in Explore');
  await dispatchTouch(touch, 'touchCancel');

  for (const [mode, points, axis, sign] of [
    ['orbit', [[1, 90, 60], [2, 150, 60]], 2, -1],
    ['walk', [[1, 140, 108], [2, 200, 108]], 0, 1],
  ]) {
    await page.evaluate(mode => setTestPose(mode), mode);
    await dispatchTouch(touch, 'touchStart', [[1, 90, 108]]);
    const oneFinger = await page.evaluate(() => pose());
    await dispatchTouch(touch, 'touchStart', [[1, 90, 108], [2, 150, 108]]);
    await page.evaluate(() => nextFrames());
    assert.deepEqual(await page.evaluate(() => pose()), oneFinger, 'A second stationary finger must not jump the view');
    await dispatchTouch(touch, 'touchMove', points);
    await page.evaluate(() => nextFrames(5));
    const swiped = await page.evaluate(() => pose());
    assertGroundTravel(oneFinger.position, swiped.position, axis, sign, 'Two-finger swipe');
    assert.equal(swiped.mode, 'walk');
    assert.ok(distance(swiped.rotation, oneFinger.rotation) < .001, 'Two-finger swipes should translate without steering');
    await dispatchTouch(touch, 'touchCancel');
    const cancelled = await page.evaluate(() => pose());
    await page.evaluate(() => nextFrames());
    assert.deepEqual(await page.evaluate(() => pose()), cancelled, 'Cancelled touch must leave no queued movement');
  }

  await page.evaluate(() => setTestPose());
  const pinchStart = await page.evaluate(() => pose());
  await dispatchTouch(touch, 'touchStart', [[1, 100, 90], [2, 140, 90]]);
  await dispatchTouch(touch, 'touchMove', [[1, 68, 90], [2, 172, 90]]);
  await page.evaluate(() => nextFrames(5));
  await dispatchTouch(touch, 'touchEnd');
  const pinched = await page.evaluate(() => pose());
  assert.ok(pinched.radius < pinchStart.radius - .1, 'A symmetrical touch pinch should dolly in');
  assert.deepEqual(pinched.target, pinchStart.target);
  assert.equal(pinched.mode, 'orbit');

  for (const [dx, dy, axis, sign] of [[0, -60, 2, -1], [60, 0, 0, 1]]) {
    const before = await page.evaluate(() => setTestPose());
    await page.mouse.move(120, 90); await page.mouse.wheel(dx, dy);
    await page.evaluate(() => nextFrames(5));
    const trackpad = await page.evaluate(() => pose());
    assertGroundTravel(before, trackpad.position, axis, sign, 'Trackpad swipe');
    assert.equal(trackpad.mode, 'walk');
  }
  await page.evaluate(() => setTestPose());
  const trackpadStart = await page.evaluate(() => pose());
  await page.keyboard.down('Control'); await page.mouse.wheel(0, -60); await page.keyboard.up('Control');
  await page.evaluate(() => nextFrames(5));
  const trackpadPinch = await page.evaluate(() => pose());
  assert.ok(trackpadPinch.radius < trackpadStart.radius - .1, 'Control-modified wheel should dolly in');
  assert.equal(trackpadPinch.mode, 'orbit');

  // Drawing must still reach the real editor input owner while navigation is suspended.
  await page.evaluate(() => {
    setTestPose('walk');
    window.editEvents = [];
    engine.canvas.addEventListener('tolglow-editor', event => editEvents.push(event.detail.type));
    engine.update({...snapshot, editor: {tool: 'draw', plane: 'xy', 'plane-value': -1}});
  });
  assert.equal(await page.evaluate(() => engine.getNavigationState().interacting), true);
  const editing = await page.evaluate(() => pose());
  await page.keyboard.down('w'); await page.mouse.move(100, 80); await page.mouse.down();
  await page.mouse.move(150, 100); await page.mouse.up(); await page.mouse.wheel(60, -60);
  await dispatchTouch(touch, 'touchStart', [[1, 90, 108], [2, 150, 108]]);
  await dispatchTouch(touch, 'touchMove', [[1, 120, 60], [2, 180, 60]]);
  await dispatchTouch(touch, 'touchCancel');
  await page.evaluate(() => nextFrames()); await page.keyboard.up('w');
  assert.deepEqual(await page.evaluate(() => pose()), editing);
  assert.deepEqual(await page.evaluate(() => editEvents), ['begin', 'move', 'end', 'begin', 'move', 'cancel']);
  await page.evaluate(() => engine.update(snapshot));
  assert.equal(await page.evaluate(() => engine.getNavigationState().interacting), false);
  await touch.detach();
}

test('real camera input, active-light tour and editor suspension agree in worker and fallback', {timeout: 120000}, async t => {
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!existsSync(executablePath)) return t.skip('Set CHROMIUM_PATH for real camera input');
  const bundle = await build({entryPoints: {preview: new URL('../../../js/preview/index.js', import.meta.url).pathname,
    'preview-worker': new URL('../../../js/preview/worker.js', import.meta.url).pathname}, outdir: '/virtual', bundle: true,
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
      const page = await browser.newPage({hasTouch: true}), errors = [];
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
        window.pose = () => ({position: position(), target: engine.controls.target.toArray(),
          rotation: engine.camera.quaternion.toArray(), radius: engine.camera.position.distanceTo(engine.controls.target),
          mode: engine.getNavigationState().mode});
        window.setTestPose = (mode = 'orbit') => {
          engine.setNavigationMode(mode);
          engine.camera.position.set(0, 1.6, 2.7);
          engine.controls.target.set(0, 1.2, -2);
          engine.controls.enableDamping = false;
          engine.controls.update();
          engine.controls.enableDamping = true;
          return position();
        };
        window.nextFrames = async (count = 3) => {for (let i = 0; i < count; i++) await new Promise(requestAnimationFrame);};
        window.traceKey = type => {
          window.cameraTrace = new Promise(resolve => {
            engine.canvas.addEventListener(type, async () => {
              const frames = [];
              for (let i = 0; i < 10; i++) {
                const time = await new Promise(requestAnimationFrame);
                frames.push({position: position(), time});
              }
              resolve(frames);
            }, {once: true});
          });
        };
      }, worker);
      assert.equal(await page.evaluate(() => engine.getTelemetry().thread || 'main'), worker ? 'worker' : 'main');

      await exerciseCamera(page);

      // Real key events are scoped to the focused canvas; text input is untouched.
      await page.locator('input').fill(''); await page.locator('input').focus();
      const typingStart = await page.evaluate(() => position());
      await page.keyboard.type('wasdhjkl'); await page.keyboard.press('ArrowLeft'); await page.evaluate(() => nextFrames());
      assert.deepEqual(await page.evaluate(() => position()), typingStart);
      assert.equal(await page.locator('input').inputValue(), 'wasdhjkl');
      assert.equal(await page.locator('input').evaluate(input => input.selectionStart), 7);

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
      // Actual scrolling drives IntersectionObserver, independently of window
      // focus. No snapshots cross the worker boundary during suspension.
      await page.evaluate(() => {
        document.body.style.height = '300vh'; window.scrollTo(0, document.body.scrollHeight);
      });
      await page.waitForFunction(() => !engine.visible && !engine.rendering);
      const idle = await page.evaluate(async () => {
        const frames = engine.getTelemetry().frames, sequence = engine.channel?.sequence;
        for (let n = 2; n <= 20; n++) engine.update({...snapshot, 'frame-number': n});
        const drew = await engine.draw();
        window.dispatchEvent(new Event('blur'));
        window.scrollTo(0, 0); await nextFrames(5);
        return {frames, current: engine.getTelemetry().frames, sequence, currentSequence: engine.channel?.sequence,
          navigationPending: engine.navigation.frame, visible: engine.visible, drew};
      });
      assert.equal(idle.frames, idle.current); assert.equal(idle.sequence, idle.currentSequence);
      assert.equal(idle.navigationPending, null); assert.equal(idle.visible, false); assert.equal(idle.drew, false);
      await page.evaluate(() => window.dispatchEvent(new Event('focus')));
      await page.waitForFunction(() => engine.getTelemetry().frameNumber === 20);
      await page.evaluate(() => engine.canvas.requestFullscreen());
      await page.waitForFunction(() => document.fullscreenElement && engine.visible);
      await page.evaluate(() => document.exitFullscreen());
      await page.waitForFunction(() => !document.fullscreenElement && engine.visible);
      const cleanup = await page.evaluate(async () => {
        engine.moveCamera('forward', true); const navigator = engine.navigation; engine.dispose();
        const pose = position(); await nextFrames(); return {pose, after: position(), disposed: navigator.disposed, held: navigator.buttons.size};
      });
      assert.deepEqual(cleanup.pose, cleanup.after); assert.equal(cleanup.held, 0); assert.ok(cleanup.disposed);
      // A second owner can mount offscreen without initializing WebGL or
      // starting a camera loop. Full initialization waits for intersection.
      const dormant = await page.evaluate(async worker => {
        const canvas = document.createElement('canvas'); canvas.style.display = 'none'; document.body.append(canvas);
        const renderer = new RoomPreview(canvas, null, {worker}); renderer.update(snapshot);
        await nextFrames(3); const telemetry = renderer.getTelemetry();
        const result = {visible: renderer.visible, gpu: !!renderer.renderer, sent: renderer.channel?.sequence || 0,
          navigationPending: renderer.navigation.frame, draws: telemetry.frames || 0, draw: await renderer.draw()};
        renderer.dispose(); return result;
      }, worker);
      assert.deepEqual(dormant, {visible: false, gpu: false, sent: 0, navigationPending: null, draws: 0, draw: false});
      assert.deepEqual(errors, []); await page.close();
    }
  } finally {await browser?.close(); await new Promise(resolve => server.close(resolve));}
});
