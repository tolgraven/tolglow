import test from 'node:test';
import assert from 'node:assert/strict';
import {fileURLToPath} from 'node:url';
import {runInNewContext} from 'node:vm';
import {build} from 'esbuild';

// Keep the real worker, SceneRenderer lifecycle/quality policy and DisplayClock.
// Replace only graphics allocation, raster work and resize at the GPU boundary.
const bundle = await build({stdin: {
  resolveDir: fileURLToPath(new URL('../../../js/preview/', import.meta.url)),
  contents: `import {HemisphereLight} from 'three';
    import {SceneRenderer} from './scene-renderer.js';
    import {TIERS} from './render-quality.js';
    import './worker.js';
    SceneRenderer.prototype.initialize = function () {
      graphics.engine = this;
      this.softwareRenderer = graphics.software;
      this.quality = this.autoQuality(); this.options = TIERS[this.quality];
      this.frameInterval = 1000 / this.targetHz;
      this.diffuseBounce = new HemisphereLight();
      this.renderer = {getContext: () => graphics.gl, info: {memory: {}, programs: []}, dispose() {}};
    };
    SceneRenderer.prototype.rebuild = function () {
      this.lightIds = []; this.spots = []; this.diffuserTubes = []; this.fixtureSamples = [];
    };
    SceneRenderer.prototype.draw = function (dt) {graphics.draw(dt); this.draws++;};
    SceneRenderer.prototype.resize = function () {graphics.resize();};
    SceneRenderer.prototype.restoreContext = function () {
      this.lost = false; this.restorePending = false;
    };`
}, bundle: true, format: 'iife', platform: 'browser', write: false});

const BLINK = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/155.0.0.0 Safari/537.36';
const WEBKIT = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.1 Safari/605.1.15';

function worker({hz = 60, work = 0, software = false, quality = software ? 'auto' : 'mobile',
  yieldMs, readyAfterYields = Infinity, userAgent = BLINK, animationFrames = true} = {}) {
  let now = 0, nextId = 0, stateId = 0, readyThrough = Infinity, serial = 0, closed = false, yields = 0;
  const fences = new Set(), bitmaps = [], listeners = new Map(), resizes = [];
  const rafs = new Map(), timers = new Map(), messages = [], draws = [];
  const timer = (callback, delay) => {
    const id = ++nextId; timers.set(id, {callback, at: now + delay}); return id;
  };
  const graphics = {software, resize() {resizes.push(fences.size);},
    draw(dt) {draws.push({at: now, dt}); now += work;}, gl: {
    SYNC_GPU_COMMANDS_COMPLETE: 1, ALREADY_SIGNALED: 2, CONDITION_SATISFIED: 3, WAIT_FAILED: 4, TIMEOUT_EXPIRED: 5,
    fenceSync() {
      const fence = ++serial; fences.add(fence);
      assert.ok(fences.size <= (software ? 1 : 2), 'GPU ownership must be bounded');
      return fence;
    },
    clientWaitSync(fence) {return fence <= readyThrough ? this.CONDITION_SATISFIED : this.TIMEOUT_EXPIRED;},
    deleteSync(fence) {assert.ok(fences.delete(fence), 'each fence must be deleted exactly once');},
    flush() {}
  }};
  const host = {graphics, AbortController, console, performance: {now: () => now, timeOrigin: 0},
    navigator: {userAgent},
    requestAnimationFrame(callback) {
      // Chrome refuses animation frames in a worker without a parent frame sink.
      if (!animationFrames) throw Object.assign(new Error('unsupported'), {name: 'NotSupportedError'});
      // The refresh sampler owns its own short burst beside the draw wakeup.
      const sampler = callback.name === 'bound tick';
      assert.equal([...rafs.values()].filter(pending => (pending.name === 'bound tick') === sampler).length, 0,
        'only one display wakeup may be outstanding per owner');
      const id = ++nextId; rafs.set(id, callback); return id;
    },
    cancelAnimationFrame: id => rafs.delete(id), setTimeout: timer, clearTimeout: id => timers.delete(id),
    postMessage: message => messages.push(message), close: () => {closed = true;},
    OffscreenCanvas: class {
      addEventListener(type, listener) {
        if (!listeners.has(type)) listeners.set(type, []);
        listeners.get(type).push(listener);
      }
      transferToImageBitmap() {
        const bitmap = {closed: false, close() {
          assert.equal(this.closed, false, 'each bitmap must be closed exactly once');
          this.closed = true;
        }};
        bitmaps.push(bitmap);
        return bitmap;
      }
    }
  };
  if (yieldMs != null) host.scheduler = {yield() {
    yields++; now += yieldMs;
    if (yields >= readyAfterYields) readyThrough = Infinity;
    return Promise.resolve();
  }};
  host.self = host;
  runInNewContext(bundle.outputFiles[0].text, host);
  const send = data => host.onmessage({data});
  const state = {generation: 1, displayHz: hz, viewport: {width: 640, height: 360, pixelRatio: 1},
    quality, lightingMode: 'hold', paused: false, actions: [],
    camera: {position: [0, 2, 8], target: [0, 2, 0], up: [0, 1, 0], fov: 49, name: 'audience'}};
  const update = (outputOnly = false) => send({type: 'state', id: ++stateId, outputOnly, state: {...state, receivedAt: now}});
  const frames = () => messages.filter(message => message.type === 'frame');
  const healthy = () => assert.deepEqual(messages.filter(message => message.type === 'error'), []);
  // Drain actual async worker continuations, without a wall-clock timer or GPU.
  const settle = () => new Promise(resolve => setImmediate(resolve));
  async function advance(time) {
    for (;;) {
      const due = [...timers].filter(([, entry]) => entry.at <= time).sort((a, b) => a[1].at - b[1].at)[0];
      if (!due) break;
      timers.delete(due[0]); now = Math.max(now, due[1].at); due[1].callback(); await settle();
    }
    now = Math.max(now, time);
  }
  send({type: 'activity', active: true, generation: 1}); update(); healthy();
  return {draws, messages, frames, healthy, update, advance, state, bitmaps, resizes, fences, send,
    get engine() {return graphics.engine;},
    get closed() {return closed;},
    get yields() {return yields;},
    get timerWaits() {return [...timers.values()].map(entry => entry.at - now);},
    set fenceReady(value) {readyThrough = value ? Infinity : 0;},
    completeThrough(fence) {readyThrough = fence;},
    acknowledge(frameId = frames().at(-1)?.frameId) {
      const frame = frames().find(frame => frame.frameId === frameId);
      if (frame && !frame.bitmap.closed) frame.bitmap.close();
      send({type: 'frame-ack', frameId}); healthy();
    },
    contextEvent(type) {for (const listener of listeners.get(type) ?? []) listener({preventDefault() {}});},
    async refresh(timestamp) {
      await advance(timestamp + 2);
      const callbacks = [...rafs.values()]; rafs.clear();
      for (const callback of callbacks) callback(timestamp);
      await settle(); healthy();
    },
    dispose() {
      if (closed) return;
      send({type: 'dispose'}); healthy();
      // A worker that presents stays for the animation frame that commits its blank image.
      for (let frame = 0; !closed && frame < 2; frame++) {
        const callbacks = [...rafs.values()]; rafs.clear();
        for (const callback of callbacks) callback(now);
      }
      assert.ok(closed); assert.equal(fences.size, 0); assert.equal(rafs.size, 0); assert.equal(timers.size, 0);
    }
  };
}

test('actual worker keeps 60/120 Hz phase with late callbacks and completed frames', async t => {
  for (const [hz, work] of [[60, 12], [120, 6]]) {
    const w = worker({hz, work}); t.after(() => w.dispose());
    for (let frame = 1; frame <= hz; frame++) {
      w.update(); // Ordinary upstream state must neither reset phase nor add a draw.
      await w.refresh(frame * 1000 / hz);
      w.acknowledge();
    }
    assert.equal(w.draws.length, hz); assert.equal(w.frames().length, hz);
    assert.ok(Math.abs(w.engine.clock - (hz - 1) / hz) < 1e-9);
    assert.equal(w.engine.quality, 'mobile'); assert.equal(w.engine.renderScale, 1);
  }
});

test('jittered 60/120 Hz timestamps retain every attainable worker refresh', async t => {
  const jitter = [0, -1.6, .8, -.9, 1.1, 0, -1.4, .3];
  for (const hz of [60, 120]) {
    const w = worker({hz, work: 1}); t.after(() => w.dispose());
    for (let frame = 1; frame <= hz * 3; frame++) {
      await w.refresh(frame * 1000 / hz + jitter[(frame - 1) % jitter.length]);
      w.acknowledge();
    }
    assert.equal(w.frames().length, hz * 3, `${hz} Hz must tolerate early timestamp jitter`);
  }
});

test('worker timestamp tolerance cannot accumulate into rate creep on 144/240 Hz displays', async t => {
  const jitter = [0, -1.8, 1.8, -.8, .8, 0];
  for (const hz of [144, 240]) {
    const w = worker({hz, work: 1}); t.after(() => w.dispose());
    for (let frame = 1; frame <= hz * 10; frame++) {
      const timestamp = frame * 1000 / hz + jitter[(frame - 1) % jitter.length];
      await w.refresh(timestamp); w.acknowledge();
      assert.ok(w.frames().length <= Math.floor(timestamp * 120 / 1000) + 1,
        `${hz} Hz callbacks must stay within one frame of the 120 Hz cadence`);
    }
    assert.equal(w.engine.targetHz, 120);
    assert.ok(w.frames().length >= 1198, 'preserving the cap must not halve attainable throughput');
    assert.ok(w.frames().length <= 1201);
  }
});

test('hardware prepares two frames but transfers only one until the matching canvas ack', async t => {
  const w = worker(); t.after(() => w.dispose());
  w.fenceReady = false;
  await w.refresh(1000 / 60);
  await w.refresh(2 * 1000 / 60);
  assert.equal(w.draws.length, 2); assert.equal(w.fences.size, 2); assert.equal(w.frames().length, 0);
  w.fenceReady = true; await w.advance(45);
  assert.equal(w.frames().length, 1);
  assert.equal(w.frames()[0].rendering, true);
  w.acknowledge(999);
  for (let frame = 3; frame <= 5; frame++) await w.refresh(frame * 1000 / 60);
  assert.equal(w.draws.length, 2); assert.equal(w.frames().length, 1);
  w.acknowledge(1);
  assert.equal(w.frames()[1].frameId, 2);
  await w.refresh(100);
  assert.equal(w.draws.length, 3); assert.equal(w.frames().length, 2);
  w.acknowledge(2);
  assert.equal(w.frames()[2].frameId, 3);
  w.acknowledge(3);
});

test('changed scene state drains both fences before acknowledging its resize and pause', async t => {
  const w = worker(); t.after(() => w.dispose());
  w.fenceReady = false;
  await w.refresh(1000 / 60);
  await w.refresh(2 * 1000 / 60);
  const acks = () => w.messages.filter(message => message.type === 'state-ack').map(message => message.id);
  w.state.viewport = {width: 900, height: 600, pixelRatio: 2};
  w.state.paused = true;
  w.update();
  w.completeThrough(1); await w.advance(45);
  assert.deepEqual(acks(), [1]);
  assert.equal(w.engine.platform.viewport.width, 640);
  w.acknowledge(1);
  await w.refresh(3 * 1000 / 60);
  assert.equal(w.draws.length, 2);
  w.fenceReady = true; await w.advance(65);
  assert.deepEqual(acks(), [1, 2]);
  assert.equal(w.engine.platform.viewport.width, 900);
  assert.equal(w.frames()[1].stateId, 1);
  w.acknowledge(2);
  await w.refresh(5 * 1000 / 60);
  assert.equal(w.draws.length, 2);
  w.state.actions = [{id: 1, type: 'draw'}];
  w.update(); await w.refresh(100);
  assert.equal(w.frames()[2].stateId, 3);
  assert.equal(w.frames()[2].actionThrough, 1);
  w.acknowledge(3);
  await w.refresh(7 * 1000 / 60);
  assert.equal(w.draws.length, 3, 'explicit paused draw must happen once');
  assert.deepEqual([...new Set(w.resizes)], [0]);
});

test('emitted output advances the next draw while its predecessor is still on the GPU', async t => {
  const w = worker(); t.after(() => w.dispose());
  const snapshot = {'show-id': 'test', epoch: 1, revision: 1, 'time-ms': 0, 'frame-number': 10,
    rig: {fixtures: []}, output: {values: {}}, 'spatial-volumes': []};
  w.state.snapshot = snapshot;
  w.update();
  w.fenceReady = false;
  await w.refresh(1000 / 60);
  const {rig: _rig, 'spatial-volumes': _volumes, ...sample} = snapshot;
  w.state.snapshot = {...sample, 'time-ms': 25, 'frame-number': 11};
  w.update(true);
  assert.equal(w.messages.at(-1).id, 3);
  assert.equal(w.engine.snapshot.rig, snapshot.rig);
  await w.refresh(2 * 1000 / 60);
  w.fenceReady = true; await w.advance(45);
  assert.deepEqual(w.frames().map(frame => [frame.stateId, frame.showFrame]), [[2, 10]]);
  w.acknowledge(1);
  assert.deepEqual(w.frames().map(frame => [frame.stateId, frame.showFrame]), [[2, 10], [3, 11]]);
  w.acknowledge(2);
  assert.deepEqual([...new Set(w.resizes)], [0]);
});

test('old visibility generations and disposal retire both untransferred bitmaps', async t => {
  const w = worker(); t.after(() => w.dispose());
  w.fenceReady = false;
  await w.refresh(1000 / 60);
  await w.refresh(2 * 1000 / 60);
  w.update();
  w.send({type: 'activity', active: false, generation: 2});
  w.fenceReady = true; await w.advance(70);
  assert.equal(w.frames().length, 0);
  assert.ok(w.bitmaps.every(bitmap => bitmap.closed));
  assert.equal(w.fences.size, 0);
  const acks = w.messages.filter(message => message.type === 'state-ack');
  assert.equal(acks.at(-1).id, 2, 'invalidating a deferred generation releases its state mailbox');
  w.state.generation = 3;
  w.send({type: 'activity', active: true, generation: 3});
  w.update(); w.fenceReady = false;
  await w.refresh(5 * 1000 / 60);
  await w.refresh(6 * 1000 / 60);
  assert.equal(w.fences.size, 2);
  w.dispose();
  assert.ok(w.bitmaps.every(bitmap => bitmap.closed));
});

test('context loss retires both frames and restoration applies the deferred state', async t => {
  const w = worker(); t.after(() => w.dispose());
  w.fenceReady = false;
  await w.refresh(1000 / 60);
  await w.refresh(2 * 1000 / 60);
  w.update();
  w.contextEvent('webglcontextlost');
  assert.ok(w.bitmaps.every(bitmap => bitmap.closed));
  assert.equal(w.fences.size, 0);
  assert.deepEqual(w.timerWaits, []);
  w.contextEvent('webglcontextrestored');
  w.fenceReady = true; await w.refresh(4 * 1000 / 60);
  assert.equal(w.frames().length, 1);
  assert.equal(w.frames()[0].stateId, 2);
  w.acknowledge();
});

test('short hardware completion can yield tasks without entering timer polling', async t => {
  const w = worker({yieldMs: .2, readyAfterYields: 2}); t.after(() => w.dispose());
  w.fenceReady = false; await w.refresh(1000 / 60);
  assert.equal(w.yields, 2); assert.equal(w.frames().length, 1);
  assert.deepEqual(w.timerWaits, []);
});

test('hardware completion bounds cooperative yields to sixteen attempts and four milliseconds', async t => {
  for (const [hz, yieldMs, expectedYields] of [[60, .1, 16], [60, 1.1, 4], [120, .1, 16], [120, 1.1, 4]]) {
    const w = worker({hz, yieldMs}); t.after(() => w.dispose());
    w.fenceReady = false; await w.refresh(1000 / hz);
    assert.equal(w.yields, expectedYields); assert.equal(w.frames().length, 0);
    assert.equal(w.timerWaits.length, 1);
    assert.ok(Math.abs(w.timerWaits[0] - 2) < 1e-9);
    await w.advance(30);
    assert.equal(w.frames().length, 0); assert.equal(w.yields, expectedYields);
    w.fenceReady = true; await w.advance(50);
    assert.equal(w.yields, expectedYields, 'timer fallback must not restart the yield burst');
    assert.equal(w.frames().length, 1);
  }
});

test('software and browsers without scheduler.yield use timer polling directly', async t => {
  for (const options of [{software: true, yieldMs: .1}, {}]) {
    const w = worker(options); t.after(() => w.dispose());
    w.fenceReady = false; await w.refresh(1000 / 60);
    assert.equal(w.yields, 0); assert.equal(w.frames().length, 0);
    assert.equal(w.timerWaits.length, 1);
    assert.ok(Math.abs(w.timerWaits[0] - 2) < 1e-9);
    w.fenceReady = true; await w.advance(50);
    assert.equal(w.frames().length, 1); assert.equal(w.yields, 0);
  }
});

test('software worker retains the explicit low-tier cap and post-completion idle interval', async t => {
  const w = worker({software: true, work: 2}); t.after(() => w.dispose());
  for (let frame = 1; frame <= 60; frame++) {
    await w.refresh(frame * 1000 / 60); w.acknowledge();
  }
  assert.equal(w.engine.targetHz, 10); assert.equal(w.engine.quality, 'low');
  assert.ok(w.frames().length >= 8 && w.frames().length <= 10);
  for (let i = 1; i < w.draws.length; i++) assert.ok(w.draws[i].at - w.draws[i - 1].at >= 100);
});

test('Auto measures the completed display cycle through bitmap acknowledgement', async t => {
  const w = worker({work: 2, quality: 'auto'}); t.after(() => w.dispose());
  await w.refresh(1000 / 60); w.acknowledge();
  await w.refresh(2 * 1000 / 60);
  await w.advance(2 * 1000 / 60 + 13); w.acknowledge();
  assert.equal(w.frames().at(-1).telemetry.workerFrameMs, 2);
  assert.ok(Math.abs(w.engine.measuredFrameMs - 13) < 1e-9,
    'delivery lateness and bitmap acknowledgement must count against Auto\'s display budget');
});

// A presentation surface the worker owns after the proxy hands the canvas over.
function surfaceFor(w, identity = '[null,null,null]') {
  const shown = [];
  const canvas = {width: 0, height: 0, getContext: type => type === 'bitmaprenderer'
    ? {transferFromImageBitmap(bitmap) {shown.push(bitmap);}} : null};
  const first = w.frames().at(-1);
  w.send({type: 'surface', canvas, identity, frameId: first.frameId, bitmap: first.bitmap});
  w.healthy();
  return shown;
}

test('a handed-over surface presents every completed frame without a page acknowledgement', async t => {
  const w = worker(); t.after(() => w.dispose());
  await w.refresh(1000 / 60);
  assert.equal(w.frames().length, 1); assert.ok(w.frames()[0].bitmap);
  const shown = surfaceFor(w);
  assert.equal(shown.length, 1, 'the first image is shown by the worker that rendered it');
  assert.equal(w.frames().at(-1).presented, true);
  // The page never answers again. Sixty refreshes still yield sixty images.
  for (let frame = 2; frame <= 61; frame++) await w.refresh(frame * 1000 / 60);
  assert.equal(w.draws.length, 61); assert.equal(shown.length, 61);
  const reports = w.frames().slice(1);
  assert.ok(reports.every(frame => !frame.bitmap && frame.telemetry.presentation === 'worker'));
  assert.deepEqual(reports.map(frame => frame.frameId), Array.from({length: 61}, (_, i) => i + 1));
  assert.ok(w.bitmaps.every(bitmap => bitmap.closed), 'each presented bitmap is released');
  assert.equal(w.fences.size, 0);
});

test('a surface keeps GPU ownership bounded and closes images from a show the page has left', async t => {
  const w = worker(); t.after(() => w.dispose());
  await w.refresh(1000 / 60);
  const shown = surfaceFor(w);
  w.fenceReady = false;
  for (let frame = 2; frame <= 6; frame++) await w.refresh(frame * 1000 / 60);
  assert.equal(w.fences.size, 2, 'two submitted draws at most, with or without acknowledgements');
  assert.equal(w.draws.length, 3);
  // The page moved to another show while both draws were on the GPU.
  w.send({type: 'identity', identity: '["runtime","other",2]'});
  assert.equal(shown.at(-1), null, 'the old image is cleared at once');
  w.fenceReady = true; await w.advance(140);
  const late = w.frames().slice(-2);
  assert.deepEqual(late.map(frame => frame.presented), [false, false]);
  assert.equal(shown.filter(Boolean).length, 1);
  assert.ok(w.bitmaps.every(bitmap => bitmap.closed));
  w.send({type: 'identity', identity: '[null,null,null]'});
  await w.refresh(200);
  assert.equal(w.frames().at(-1).presented, true);
  w.send({type: 'dispose'}); w.healthy();
  assert.equal(shown.at(-1), null, 'disposal blanks the canvas only the worker can reach');
  assert.equal(w.fences.size, 0);
  assert.equal(w.closed, false, 'the blank image is committed at the next display frame, so the worker outlives it');
  w.dispose();
});

test('a presenting worker without animation frames still ends after blanking its surface', async t => {
  const w = worker({animationFrames: false}); t.after(() => w.dispose());
  await w.advance(1000 / 60);
  const shown = surfaceFor(w);
  w.send({type: 'dispose'}); w.healthy();
  assert.equal(shown.at(-1), null);
  assert.equal(w.closed, false);
  await w.advance(1000);
  assert.equal(w.closed, true);
  assert.deepEqual(w.timerWaits, []);
});

test('a worker whose animation frames are unsupported renders on timers', async t => {
  const w = worker({animationFrames: false}); t.after(() => w.dispose());
  for (let frame = 1; frame <= 6; frame++) {
    await w.advance(frame * 1000 / 60);
    if (w.frames().at(-1)?.bitmap && !w.frames().at(-1).bitmap.closed) w.acknowledge();
  }
  assert.ok(w.draws.length >= 5, `only ${w.draws.length} timer-paced draws`);
  assert.equal(w.engine.targetHz, 60);
});

test('the worker samples the display itself when the page reports a starved callback rate', async t => {
  const w = worker({hz: 15}); t.after(() => w.dispose());
  assert.equal(w.engine.targetHz, 15);
  for (let frame = 1; frame <= 45; frame++) {
    await w.refresh(frame * 1000 / 60);
    if (w.frames().at(-1)?.bitmap) w.acknowledge();
  }
  assert.equal(w.engine.displayHz, 60);
  assert.equal(w.engine.targetHz, 60);
  // A later page sample cannot lower what this thread measured on the panel.
  w.update();
  assert.equal(w.engine.targetHz, 60);
});

test('headless and embedded Blink builds sample the display from the worker as Chrome does', async t => {
  for (const product of ['HeadlessChrome/155.0.0.0', 'Chromium/155.0.0.0 Chrome/155.0.0.0', 'Chrome/155.0.0.0 Mobile']) {
    const w = worker({hz: 15, userAgent: BLINK.replace('Chrome/155.0.0.0', product)}); t.after(() => w.dispose());
    for (let frame = 1; frame <= 45; frame++) {
      await w.refresh(frame * 1000 / 60);
      if (w.frames().at(-1)?.bitmap && !w.frames().at(-1).bitmap.closed) w.acknowledge();
    }
    assert.equal(w.engine.displayHz, 60, product);
  }
});

test('timer-paced worker animation frames cannot raise the display rate the page measured', async t => {
  // WebKit services worker animation frames from a 15 ms timer on every panel.
  const w = worker({hz: 60, userAgent: WEBKIT}); t.after(() => w.dispose());
  for (let frame = 1; frame <= 45; frame++) {
    await w.refresh(frame * 15);
    if (w.frames().at(-1)?.bitmap && !w.frames().at(-1).bitmap.closed) w.acknowledge();
  }
  assert.equal(w.engine.displayHz, 60);
  assert.equal(w.engine.targetHz, 60);
});

test('Auto lowers its internal scale while a saturated GPU always has a draw outstanding', async t => {
  const w = worker({quality: 'auto', work: 2}); t.after(() => w.dispose());
  await w.refresh(1000 / 60);
  surfaceFor(w);
  // From here each draw takes fifty milliseconds to complete and a second one
  // is always queued behind it, as on a large rig at full resolution.
  w.fenceReady = false;
  let time = 1000 / 60, done = 1;
  await w.refresh(time += 1000 / 60);
  for (let step = 0; step < 60; step++) {
    await w.refresh(time += 50);
    if (w.engine.renderScale < 1) break;
    assert.equal(w.fences.size, 2, 'the queue stays full until Auto reacts');
    w.completeThrough(++done); await w.advance(time + 3);
  }
  assert.ok(w.engine.renderScale < 1, 'sustained overload must reduce the scene raster');
  assert.equal(w.engine.resizePending, true);
  assert.equal(w.fences.size, 1, 'a draw was still outstanding when the decision was taken');
  // The new raster waits for the submitted draws. Nothing else is submitted meanwhile.
  const submitted = w.draws.length;
  await w.refresh(time += 50);
  assert.equal(w.draws.length, submitted);
  w.fenceReady = true; await w.advance(time += 10);
  assert.equal(w.fences.size, 0);
  await w.refresh(time += 50);
  assert.equal(w.draws.length, submitted + 1);
});
