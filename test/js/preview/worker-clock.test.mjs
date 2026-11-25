import test from 'node:test';
import assert from 'node:assert/strict';
import {fileURLToPath} from 'node:url';
import {runInNewContext} from 'node:vm';
import {build} from 'esbuild';

// Keep the real worker, SceneRenderer lifecycle/quality policy and DisplayClock.
// Replace only graphics allocation, raster work and resize at the GPU boundary.
const bundle = await build({stdin: {
  resolveDir: fileURLToPath(new URL('../../../js/preview/', import.meta.url)),
  contents: `import {SceneRenderer} from './scene-renderer.js';
    import {TIERS} from './render-quality.js';
    import './worker.js';
    SceneRenderer.prototype.initialize = function () {
      graphics.engine = this;
      this.softwareRenderer = graphics.software;
      this.quality = this.autoQuality(); this.options = TIERS[this.quality];
      this.frameInterval = 1000 / this.targetHz;
      this.renderer = {getContext: () => graphics.gl, info: {memory: {}, programs: []}, dispose() {}};
    };
    SceneRenderer.prototype.draw = function (dt) {graphics.draw(dt); this.draws++;};
    SceneRenderer.prototype.resize = function () {};`
}, bundle: true, format: 'iife', platform: 'browser', write: false});

function worker({hz = 60, work = 0, software = false, quality = software ? 'auto' : 'mobile',
  yieldMs, readyAfterYields = Infinity} = {}) {
  let now = 0, nextId = 0, stateId = 0, fenceReady = true, fences = 0, closed = false, yields = 0;
  const rafs = new Map(), timers = new Map(), messages = [], draws = [];
  const timer = (callback, delay) => {
    const id = ++nextId; timers.set(id, {callback, at: now + delay}); return id;
  };
  const graphics = {software, draw(dt) {draws.push({at: now, dt}); now += work;}, gl: {
    SYNC_GPU_COMMANDS_COMPLETE: 1, ALREADY_SIGNALED: 2, CONDITION_SATISFIED: 3, WAIT_FAILED: 4, TIMEOUT_EXPIRED: 5,
    fenceSync() {assert.equal(++fences, 1, 'only one GPU frame may be outstanding'); return {};},
    clientWaitSync() {return fenceReady ? this.CONDITION_SATISFIED : this.TIMEOUT_EXPIRED;},
    deleteSync() {fences--;}, flush() {}
  }};
  const host = {graphics, AbortController, console, performance: {now: () => now, timeOrigin: 0},
    requestAnimationFrame(callback) {
      assert.equal(rafs.size, 0, 'only one display wakeup may be outstanding');
      const id = ++nextId; rafs.set(id, callback); return id;
    },
    cancelAnimationFrame: id => rafs.delete(id), setTimeout: timer, clearTimeout: id => timers.delete(id),
    postMessage: message => messages.push(message), close: () => {closed = true;},
    OffscreenCanvas: class {addEventListener() {} transferToImageBitmap() {return {close() {}};}}
  };
  if (yieldMs != null) host.scheduler = {yield() {
    yields++; now += yieldMs;
    if (yields >= readyAfterYields) fenceReady = true;
    return Promise.resolve();
  }};
  host.self = host;
  runInNewContext(bundle.outputFiles[0].text, host);
  const send = data => host.onmessage({data});
  const state = {generation: 1, displayHz: hz, viewport: {width: 640, height: 360, pixelRatio: 1},
    quality, lightingMode: 'hold', paused: false, actions: [],
    camera: {position: [0, 2, 8], target: [0, 2, 0], up: [0, 1, 0], fov: 49, name: 'audience'}};
  const update = () => send({type: 'state', id: ++stateId, state: {...state}, receivedAt: now});
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
  return {draws, messages, frames, healthy, update, advance,
    get engine() {return graphics.engine;},
    get yields() {return yields;},
    get timerWaits() {return [...timers.values()].map(entry => entry.at - now);},
    set fenceReady(value) {fenceReady = value;},
    acknowledge(frameId = frames().at(-1)?.frameId) {send({type: 'frame-ack', frameId}); healthy();},
    async refresh(timestamp) {
      await advance(timestamp + 2);
      const callbacks = [...rafs.values()]; rafs.clear();
      for (const callback of callbacks) callback(timestamp);
      await settle(); healthy();
    },
    dispose() {
      send({type: 'dispose'}); healthy();
      assert.ok(closed); assert.equal(fences, 0); assert.equal(rafs.size, 0); assert.equal(timers.size, 0);
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

test('GPU completion and matching bitmap acknowledgement gate the next worker render', async t => {
  const w = worker(); t.after(() => w.dispose());
  w.fenceReady = false;
  await w.refresh(1000 / 60); w.update();
  await w.refresh(2 * 1000 / 60);
  assert.equal(w.draws.length, 1); assert.equal(w.frames().length, 0);
  assert.equal(w.messages.filter(message => message.type === 'state-ack').length, 1);
  w.fenceReady = true; await w.advance(45);
  assert.equal(w.frames().length, 1);
  assert.equal(w.messages.filter(message => message.type === 'state-ack').length, 2);
  w.acknowledge(999);
  for (let frame = 3; frame <= 5; frame++) await w.refresh(frame * 1000 / 60);
  assert.equal(w.draws.length, 1); assert.equal(w.frames().length, 1);
  w.acknowledge();
  await w.refresh(100);
  assert.equal(w.draws.length, 2); assert.equal(w.frames().length, 2);
  await w.refresh(7 * 1000 / 60);
  assert.equal(w.draws.length, 2, 'late acknowledgement must not cause a catch-up burst');
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
