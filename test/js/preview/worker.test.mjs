import test from 'node:test';
import assert from 'node:assert/strict';
import {existsSync} from 'node:fs';
import {createServer} from 'node:http';
import {build} from 'esbuild';
import {chromium} from 'playwright-core';

test('actual worker renders while input runs, fences epochs, resizes and safely falls back', {timeout: 120000}, async t => {
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!existsSync(executablePath)) return t.skip('Set CHROMIUM_PATH to exercise the actual worker and WebGL renderer');
  const bundle = await build({entryPoints: {preview: new URL('../../../js/preview/index.js', import.meta.url).pathname,
    'preview-worker': new URL('../../../js/preview/worker.js', import.meta.url).pathname}, outdir: '/virtual', bundle: true,
    format: 'esm', platform: 'browser', write: false});
  const files = new Map(bundle.outputFiles.map(file => [file.path.split('/').at(-1), file.text]));
  const server = createServer((request, response) => {
    const file = files.get(request.url.slice(1)); response.setHeader('Content-Type', file ? 'text/javascript' : 'text/html');
    response.end(file || `<!doctype html><canvas style="width:360px;height:240px"></canvas><p id="status"></p><output id="heartbeat"></output>
      <script type="module">import {RoomPreview} from '/preview.js'; window.RoomPreview=RoomPreview;</script>`);
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
    const first = await page.evaluate(async () => {
      const fixtures = Array.from({length: 8}, (_, index) => ({id: `head-${index}`, position: [-3.5 + index, 3.7, -2]}));
      window.snapshot = {'show-id': 'worker-test', epoch: 1, revision: 1, 'frame-number': 1,
        rig: {room: {width: 12, height: 5, depth: 10}, fixtures, trusses: []},
        output: {values: Object.fromEntries(fixtures.map((fixture, index) => [fixture.id,
          {'emitted-color': index % 2 ? [.15, .35, .5] : [.5, .12, .03], direction: [(index - 3.5) * -.2, -1, .4], focus: .2}]))}};
      window.engine = new RoomPreview(document.querySelector('canvas'), document.querySelector('#status'));
      // Begin with every fixture black. The first cue must reuse the warmed
      // light/shadow shader layout, not compile a different lit-scene program.
      engine.update({...snapshot, output: {values: {}}}); await engine.whenReady; return engine.getTelemetry();
    });
    assert.equal(first.lightingMode, 'hold');
    assert.equal(first.thread, 'worker'); assert.equal(first.ready, true); assert.equal(first.failed, null);
    assert.equal(first.fixtureCount, 8); assert.ok(first.shadowCount > 0);
    assert.equal(first.activeLights, 0);

    const heartbeat = await page.evaluate(async () => {
      let previous = performance.now(), maximumGap = 0, ticks = 0, revision = 1;
      let animation, previousPaint = 0, maximumPaintGap = 0, paints = 0;
      const paint = now => {
        if (previousPaint) maximumPaintGap = Math.max(maximumPaintGap, now - previousPaint);
        previousPaint = now; document.querySelector('#heartbeat').textContent = String(++paints);
        animation = requestAnimationFrame(paint);
      };
      animation = requestAnimationFrame(paint);
      const timer = setInterval(() => {
        const now = performance.now(); maximumGap = Math.max(maximumGap, now - previous); previous = now; ticks++;
        engine.update({...snapshot, revision: ++revision, 'frame-number': revision});
      }, 20);
      await new Promise(resolve => setTimeout(resolve, 10000)); clearInterval(timer); cancelAnimationFrame(animation);
      return {maximumGap, ticks, maximumPaintGap, paints, telemetry: engine.getTelemetry()};
    });
    // These broad software-CI bounds detect saturation/backlog, not a physical
    // device frame-rate promise. Actual Studio latency has a separate gate.
    assert.ok(heartbeat.ticks > 100, `Only ${heartbeat.ticks} UI callbacks in ten seconds`);
    assert.ok(heartbeat.maximumGap < 1000, `UI callback blocked ${heartbeat.maximumGap}ms`);
    assert.ok(heartbeat.paints > 20 && heartbeat.maximumPaintGap < 1000,
      `Compositor backlog: ${heartbeat.paints} animation frames, largest gap ${heartbeat.maximumPaintGap}ms`);
    assert.ok(heartbeat.telemetry.frames >= first.frames + 5, `Warmup left only ${heartbeat.telemetry.frames} frames in ten seconds`);
    assert.ok(heartbeat.telemetry.frameIntervalMs <= Math.max(1000, heartbeat.telemetry.workerFrameMs * 2.2),
      'Cadence must recover after shader compilation');
    assert.equal(heartbeat.telemetry.failed, null);
    assert.equal(heartbeat.telemetry.activeLights, 8);
    assert.equal(heartbeat.telemetry.programCount, first.programCount, 'A cue launch must not compile a new light-count shader');

    const fenced = await page.evaluate(async () => {
      engine.setLightingMode('interpolate'); engine.pause(); await engine.draw();
      // Change show epoch after a real render starts, while its shader work is
      // still in progress. The old image must close without presentation.
      const discarded = engine.discardedFrames;
      const nextStart = new Promise(resolve => {
        const listener = event => {
          if (event.data.type !== 'render-start') return;
          engine.worker.removeEventListener('message', listener);
          snapshot = {...snapshot, epoch: 2}; engine.update(snapshot); resolve();
        };
        engine.worker.addEventListener('message', listener);
      });
      const draw = engine.draw(); await nextStart; await draw;
      return {discarded: engine.discardedFrames - discarded, telemetry: engine.getTelemetry()};
    });
    assert.equal(fenced.telemetry.lightingMode, 'interpolate');
    assert.ok(fenced.discarded >= 1); assert.equal(fenced.telemetry.epoch, 2); assert.equal(fenced.telemetry.ready, true);
    const atmosphere = await page.evaluate(async () => {
      const sample={'sample-time-ms':2500,epoch:2,'extinction-per-m':2.5,'reference-path-m':5,'fog-level':.060587,'volume-m3':600,'mixed-m3':3,'mixed-density':.005,
        sources:[{'fixture-id':'jet',position:[0,1,0],direction:[1,0,0],'local-m3':.5,'emitted-m3':3.5,'flow-m3-s':0}]};
      engine.update({...snapshot,epoch:2,'frame-number':150,output:{...snapshot.output,atmosphere:sample}});
      await engine.draw(); return engine.getTelemetry();
    });
    assert.equal(atmosphere.fogAmount,1);
    assert.equal(atmosphere.fog.atmosphere.foggerCount,1);
    assert.equal(atmosphere.fog.atmosphere.extinctionPerM,2.5);
    assert.equal(atmosphere.fog.atmosphere.referencePathM,5);
    assert.equal(atmosphere.fog.atmosphere.fogLevel,.060587);
    assert.ok(Math.abs(atmosphere.fog.massEstimate - 3.5)<.00001);
    const resized = await page.evaluate(async () => {
      engine.canvas.style.width = '480px'; engine.canvas.style.height = '300px'; engine.resize(); await engine.draw();
      const telemetry = engine.getTelemetry(); const canvas = engine.canvas; engine.dispose();
      return {telemetry, disposed: engine.disposed, terminated: engine.worker === null,
        channelClosed: engine.channel.closed, ownerRemoved: !canvas.tolglowPreview, drawAfterDispose: await engine.draw()};
    });
    const scale = resized.telemetry.renderScale;
    assert.ok(scale >= .5 && scale <= 1);
    assert.deepEqual(resized.telemetry.displayPixels, [480, 300]);
    assert.equal(resized.telemetry.displayRange, 'sdr');
    assert.deepEqual(resized.telemetry.pixels, [Math.round(360 * scale), Math.round(225 * scale)]);
    assert.ok(resized.disposed && resized.terminated && resized.channelClosed && resized.ownerRemoved);
    assert.equal(resized.drawAfterDispose, false);
    assert.deepEqual(errors, []);

    const fallback = await page.evaluate(async () => {
      const canvas = document.createElement('canvas'); canvas.style.cssText = 'width:160px;height:100px'; document.body.append(canvas);
      const calls = [], getContext = canvas.getContext.bind(canvas); canvas.getContext = (...args) => {calls.push(args[0]); return getContext(...args);};
      const url = URL.createObjectURL(new Blob([`throw new Error('deliberate startup failure');`], {type: 'text/javascript'}));
      const renderer = new RoomPreview(canvas, document.querySelector('#status'), {workerFactory: () => new Worker(url)});
      renderer.setLightingMode('interpolate'); renderer.setNavigationMode('walk'); renderer.setNavigationSpeed(3); renderer.setInteractionActive(true);
      const navigationStates = [], secondaryStates = [];
      const unsubscribe = renderer.onNavigationChange(state => navigationStates.push(state));
      const unsubscribeSecondary = renderer.onNavigationChange(state => secondaryStates.push(state));
      renderer.update(snapshot); await renderer.whenReady; URL.revokeObjectURL(url);
      const telemetry = renderer.getTelemetry(), navigation = renderer.getNavigationState();
      unsubscribe(); unsubscribeSecondary(); renderer.dispose();
      return {telemetry, navigation, observed: navigationStates.at(-1), navigationDisposed: renderer.fallback.navigation.disposed,
        secondaryObserved: secondaryStates.at(-1), cameraShared: renderer.camera === renderer.fallback.camera,
        calls, ownerRemoved: !canvas.tolglowPreview, drawAfterDispose: await renderer.draw()};
    });
    assert.equal(fallback.telemetry.lightingMode, 'interpolate');
    assert.equal(fallback.telemetry.thread, 'main'); assert.equal(fallback.telemetry.ready, true);
    assert.match(fallback.telemetry.fallbackReason, /deliberate startup failure/);
    assert.equal(fallback.calls[0], 'webgl2'); assert.equal(fallback.ownerRemoved, true);
    assert.equal(fallback.drawAfterDispose, false);
    assert.equal(fallback.navigation.mode, 'walk'); assert.equal(fallback.navigation.speed, 3);
    assert.equal(fallback.navigation.interacting, true); assert.equal(fallback.navigationDisposed, true);
    assert.deepEqual(fallback.observed, fallback.navigation);
    assert.deepEqual(fallback.secondaryObserved, fallback.navigation); assert.equal(fallback.cameraShared, true);
    assert.deepEqual(errors, []);
    t.diagnostic(JSON.stringify({scope: 'Chromium SwiftShader, not physical GPU/Safari', first, heartbeat, fenced, resized, fallback}));
  } finally {await browser?.close(); await new Promise(resolve => server.close(resolve));}
});
