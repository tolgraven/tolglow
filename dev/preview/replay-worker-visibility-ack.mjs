import assert from 'node:assert/strict';
import {fileURLToPath} from 'node:url';
import {runInNewContext} from 'node:vm';
import {build} from 'esbuild';

const resolveDir = fileURLToPath(new URL('../../js/preview/', import.meta.url));
const bundle = async (contents, define) => (await build({stdin: {resolveDir, contents},
  bundle: true, format: 'iife', platform: 'browser', write: false, define})).outputFiles[0].text;

// Same GPU boundary as test/js/preview/worker-clock.test.mjs.
// The worker, proxy, state channel, activity handling, fences, Auto policy and
// telemetry are all from the source checkout.
const [workerBundle, proxyBundle] = await Promise.all([
  bundle(`import {SceneRenderer} from './scene-renderer.js';
    import {TIERS} from './render-quality.js';
    import './worker.js';
    SceneRenderer.prototype.initialize = function () {
      graphics.engine = this;
      this.softwareRenderer = false;
      this.quality = this.autoQuality();
      this.options = TIERS[this.quality];
      this.frameInterval = 1000 / this.targetHz;
      this.renderer = {getContext: () => graphics.gl, info: {memory: {}, programs: []}, dispose() {}};
    };
    SceneRenderer.prototype.draw = function (dt) {graphics.draw(dt); this.draws++;};
    SceneRenderer.prototype.resize = function () {this.resizePending = false;};`),
  bundle(`import {WorkerRoomPreview} from './worker-proxy.js';
    globalThis.createPreview = (...args) => new WorkerRoomPreview(...args);`,
    {'import.meta.url': JSON.stringify('file:///cpu-replay/preview.js')})
]);

function replay() {
  let now = 5000;
  let phase = 'warmup';
  let heldFences = 0;
  let number = 0;
  let observeIntersection;
  const trace = [];
  const toWorker = [];
  const fromWorker = [];
  const frames = [];
  const drawn = [];
  const bitmaps = [];
  const presented = [];
  const measure = () => ({now: () => now, timeOrigin: 0});
  const record = (event, data = {}) => {
    if (phase !== 'warmup') trace.push({ms: now, event, ...data});
  };
  const readTelemetry = telemetry => ({
    quality: telemetry.quality, renderScale: telemetry.renderScale,
    targetRefreshHz: telemetry.targetRefreshHz, readyForFrameMs: telemetry.readyForFrameMs,
    measuredFrameMs: telemetry.measuredFrameMs, workerFrameMs: telemetry.workerFrameMs,
    rafDeliveryMs: telemetry.rafDeliveryMs, gpuCompletionWaitMs: telemetry.gpuCompletionWaitMs
  });

  const makeClock = (worker) => {
    const rafs = new Map();
    const timers = new Map();
    return {rafs, timers,
      requestAnimationFrame(callback) {
        if (worker) assert.equal(rafs.size, 0);
        const id = ++number;
        rafs.set(id, callback);
        return id;
      },
      cancelAnimationFrame(id) {rafs.delete(id);},
      setTimeout(callback, delay) {
        const id = ++number;
        timers.set(id, {at: now + delay, callback});
        return id;
      },
      clearTimeout(id) {timers.delete(id);}
    };
  };
  const workerClock = makeClock(true);
  const pageClock = makeClock(false);
  const graphics = {draw(dt) {drawn.push({at: now, dt}); now += 2;}, gl: {
    SYNC_GPU_COMMANDS_COMPLETE: 1, CONDITION_SATISFIED: 2,
    fenceSync() {assert.equal(++heldFences, 1); return {};},
    clientWaitSync() {return this.CONDITION_SATISFIED;},
    deleteSync() {heldFences--;},
    flush() {}
  }};
  const workerHost = {...workerClock, graphics, AbortController, console, performance: measure(),
    postMessage(message, transferList) {
      assert.notEqual(message.type, 'error', message.message);
      if (message.type === 'frame') {
        assert.equal(transferList[0], message.bitmap);
        assert.equal(heldFences, 0);
        frames.push(message);
        record('worker transferred bitmap', {frameId: message.frameId, generation: message.generation, stateId: message.stateId,
          ...readTelemetry(message.telemetry)});
      }
      fromWorker.push(message);
    },
    close() {},
    OffscreenCanvas: class {
      addEventListener() {}
      transferToImageBitmap() {
        const bitmap = {id: bitmaps.length + 1, width: 1000, height: 500, closeCount: 0,
          close() {this.closeCount++; record('proxy closed bitmap', {frameId: this.id});}};
        bitmaps.push(bitmap);
        return bitmap;
      }
    }
  };
  workerHost.self = workerHost;
  runInNewContext(workerBundle, workerHost);

  const device = {postMessage(message) {toWorker.push(message);}, terminate() {}};
  const document = Object.assign(new EventTarget(), {hidden: false, hasFocus: () => true});
  const windowEvents = new EventTarget();
  const canvas = Object.assign(new EventTarget(), {
    ownerDocument: document, style: {}, width: 1000, height: 500,
    getBoundingClientRect: () => ({width: 1000, height: 500}),
    getContext(kind) {
      assert.equal(kind, 'bitmaprenderer');
      return {transferFromImageBitmap(bitmap) {
        if (bitmap) {presented.push(bitmap.id); record('proxy presented bitmap', {frameId: bitmap.id});}
      }};
    }
  });
  const pageHost = {...pageClock, AbortController, CustomEvent, URL, console, document, devicePixelRatio: 1,
    performance: measure(),
    matchMedia: () => Object.assign(new EventTarget(), {matches: false}),
    ResizeObserver: class {observe() {} disconnect() {}},
    IntersectionObserver: class {
      constructor(callback) {observeIntersection = callback;}
      observe() {}
      disconnect() {}
    }
  };
  pageHost.window = pageHost;
  for (const fn of ['addEventListener', 'removeEventListener']) pageHost[fn] = windowEvents[fn].bind(windowEvents);
  runInNewContext(proxyBundle, pageHost);
  const preview = pageHost.createPreview(canvas, {}, {workerFactory: () => device});
  assert.equal(preview.fallback, undefined);

  const drainWorker = () => {
    while (toWorker.length) {
      const message = toWorker.shift();
      record('worker received ' + message.type, {active: message.active, generation: message.generation ?? message.state?.generation,
        stateId: message.id, frameId: message.frameId});
      workerHost.onmessage({data: message});
      if (message.type === 'state') {
        const ack = fromWorker.find(item => item.type === 'state-ack' && item.id === message.id);
        assert.ok(ack, 'the state was applied and acknowledged by the worker');
        record('worker applied state', {stateId: message.id, generation: message.state.generation,
          displayWakeup: graphics.engine.displayClock.pending});
      }
    }
  };
  const drainPage = () => {
    while (fromWorker.length) device.onmessage({data: fromWorker.shift()});
  };
  const setHidden = (hidden) => {
    document.hidden = hidden;
    record(hidden ? 'document hidden' : 'document shown');
    document.dispatchEvent(new Event('visibilitychange'));
    drainWorker();
  };
  const advance = (when) => {assert.ok(now <= when, `clock cannot move backward from ${now} to ${when}`); now = when;};
  const refresh = async (timestamp) => {
    advance(timestamp + 2);
    const callbacks = [...workerClock.rafs.values()];
    workerClock.rafs.clear();
    record('worker display callback', {timestamp, count: callbacks.length});
    callbacks.forEach(callback => callback(timestamp));
    await new Promise(resolve => setImmediate(resolve));
  };
  const snapshot = () => {
    const {lastReadyMs, measuredFrameMs, quality, renderScale, overBudget, underBudget,
      scaleChangedAt, displayClock, targetHz} = graphics.engine;
    return {lastReadyMs, measuredFrameMs, quality, renderScale, overBudget, underBudget, scaleChangedAt,
      displayWakeup: displayClock.pending, targetHz};
  };
  const consume = (time) => {advance(time); drainPage(); drainWorker(); drainPage();};
  const stableFrame = async (time, ackDelay) => {await refresh(time); consume(time + ackDelay);};

  drainPage();
  observeIntersection([{isIntersecting: true, intersectionRatio: 1}]);
  drainWorker();
  drainPage();
  assert.equal(preview.generation, 1);
  assert.equal(graphics.engine.qualityChoice, 'auto');
  assert.equal(graphics.engine.targetHz, 60);

  return {preview, record, trace, frames, drawn, presented, advance, refresh, snapshot, stableFrame,
    setHidden, consume, readTelemetry, drainWorker, drainPage, toWorker, fromWorker,
    startTrace() {phase = 'probe';},
    cleanup() {
      preview.dispose();
      workerHost.onmessage({data: {type:'dispose'}});
      assert.equal(heldFences, 0);
      assert.equal(workerClock.rafs.size + workerClock.timers.size, 0);
      assert.equal(pageClock.rafs.size + pageClock.timers.size, 0);
      assert.ok(bitmaps.every(bitmap => bitmap.closeCount === 1));
    }
  };
}

async function runScenario(name, order, tierFloor = false) {
  const w = replay();
  let probeTime;
  const startingQuality = 'desktop';
  if (tierFloor) {
    for (let time = 5000; time <= 5400; time += 100) await w.stableFrame(time, 80);
    assert.equal(w.snapshot().renderScale, 0.5);
    for (let time = 6600; time <= 6720; time += 20) await w.stableFrame(time, 16);
    assert.equal(w.snapshot().overBudget, 7);
    probeTime = 6740;
  } else {
    for (let time = 5000; time <= 5060; time += 20) await w.stableFrame(time, 16);
    assert.equal(w.snapshot().overBudget, 3);
    assert.equal(w.snapshot().renderScale, 1);
    probeTime = 5080;
  }
  assert.equal(w.snapshot().quality, startingQuality);
  w.startTrace();
  const before = w.snapshot();
  await w.refresh(probeTime);
  const stale = w.frames.at(-1);
  assert.equal(stale.generation, 1);
  assert.equal(stale.telemetry.workerFrameMs, 2);

  if (order !== 'continuous') {
    w.advance(probeTime + 4);
    w.setHidden(true);
    if (order === 'show-before-ack') {
      w.advance(probeTime + 5);
      w.setHidden(false);
      assert.equal(w.preview.generation, 3);
      assert.ok(w.fromWorker.some(item => item.type === 'state-ack' && item.id === stale.stateId + 1));
      assert.equal(w.snapshot().displayWakeup, false, 'old ticket still blocks after new visible state');
    }
  }

  const presentedBefore = w.presented.length;
  const beforeAck = w.snapshot();
  w.advance(probeTime + 7);
  w.drainPage();
  assert.equal(w.toWorker.length, 1);
  assert.equal(w.toWorker[0].type, 'frame-ack');
  assert.equal(w.toWorker[0].frameId, stale.frameId);
  assert.equal(stale.bitmap.closeCount, 1);
  assert.equal(w.presented.length - presentedBefore, order === 'continuous' ? 1 : 0);
  assert.equal(w.preview.getTelemetry().discardedFrames, order === 'continuous' ? 0 : 1);
  w.drainWorker();
  const afterAck = w.snapshot();

  if (order === 'show-before-ack') {
    assert.equal(afterAck.lastReadyMs, probeTime + 7);
    assert.equal(afterAck.measuredFrameMs, probeTime + 7);
    assert.equal(afterAck.displayWakeup, true, 'matching stale ACK released the slot');
    assert.equal(afterAck.renderScale, tierFloor ? .65 : .5);
    assert.equal(afterAck.quality, tierFloor ? 'mobile' : startingQuality);
  } else {
    assert.equal(afterAck.renderScale, before.renderScale);
    assert.equal(afterAck.quality, before.quality);
    assert.equal(afterAck.measuredFrameMs, order === 'continuous' ? 7 : 16);
    assert.equal(afterAck.displayWakeup, order === 'continuous');
  }

  if (order === 'ack-before-show') {
    w.advance(probeTime + 8);
    w.setHidden(false);
    w.drainPage();
    assert.equal(w.snapshot().displayWakeup, true, 'current state woke the released slot');
  }

  const count = w.frames.length;
  await w.refresh(probeTime + 20);
  assert.equal(w.frames.length, count + 1);
  const resumed = w.frames.at(-1);
  assert.equal(resumed.frameId, stale.frameId + 1);
  if (order !== 'continuous') {
    assert.equal(resumed.generation, 3);
    assert.equal(w.drawn.at(-1).dt, 0, 'draw cadence starts fresh across the activity generation');
  }
  assert.equal(resumed.telemetry.readyForFrameMs, afterAck.lastReadyMs);
  assert.equal(resumed.telemetry.measuredFrameMs, afterAck.measuredFrameMs);
  assert.equal(resumed.telemetry.targetRefreshHz, 60);
  assert.equal(resumed.telemetry.workerFrameMs, 2);
  w.consume(probeTime + 27);
  assert.equal(w.presented.at(-1), resumed.frameId);
  assert.equal(w.snapshot().lastReadyMs, 7);
  assert.equal(w.snapshot().measuredFrameMs, 7);
  const afterFreshAck = w.snapshot();
  const proxyTelemetry = w.preview.getTelemetry();
  const report = {name, before, beforeAck, afterAck,
    resumed: {frameId: resumed.frameId, generation: resumed.generation, dt: w.drawn.at(-1).dt,
      ...w.readTelemetry(resumed.telemetry), discardedFrames: proxyTelemetry.discardedFrames},
    afterFreshAck, trace: w.trace};
  w.cleanup();
  return report;
}

for (const scenario of [
  ['continuous same-frame ACK', 'continuous'],
  ['hide, stale ACK while hidden, show', 'ack-before-show'],
  ['hide, show, apply state, stale ACK at scale 1', 'show-before-ack'],
  ['hide, show, apply state, stale ACK at scale floor', 'show-before-ack', true],
  ['continuous same-frame ACK at scale floor', 'continuous', true]
]) {
  console.log(JSON.stringify(await runScenario(...scenario)));
}
