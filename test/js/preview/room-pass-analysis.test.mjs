import test from 'node:test';
import assert from 'node:assert/strict';
import {beginPassSample, compareBrackets, heldMediumInvalid, stationarityOf} from '../../../dev/preview/room-pass-analysis.mjs';
import {measureRoom} from '../../../dev/preview/room-cadence.mjs';

async function withPresentationBoundary(run) {
  const worker = new EventTarget(), presenter = {transferFromImageBitmap() {}};
  const original = presenter.transferFromImageBitmap;
  const engine = {worker, presenter, bitmapPresenter: true, ready: true, draw() {},
    telemetry: {thread: 'worker', visible: true, targetRefreshHz: 60, quality: 'low', renderScale: .5, pixels: [100, 50]},
    getTelemetry() {return this.telemetry;}};
  const canvas = {tolglowPreview: engine, width: 100, height: 50, getBoundingClientRect: () => ({width: 100, height: 50})};
  const document = Object.assign(new EventTarget(), {hidden: false, hasFocus: () => true, querySelector: () => canvas});
  const window = new EventTarget();
  const globals = {document, window, devicePixelRatio: 1, navigator: {userAgent: 'boundary-test'},
    screen: {width: 1512, height: 982, availLeft: 0, availTop: 0, colorDepth: 30, isExtended: false},
    requestAnimationFrame: callback => setTimeout(() => callback(performance.now()), 1), cancelAnimationFrame: clearTimeout};
  const previous = new Map(Object.keys(globals).map(key => [key, Object.getOwnPropertyDescriptor(globalThis, key)]));
  for (const [key, value] of Object.entries(globals)) Object.defineProperty(globalThis, key, {value, configurable: true, writable: true});
  let frame = 0;
  const present = () => {
    const telemetry = {...engine.telemetry, passProbe: {mode: 'full', frame: ++frame, raymarched: true,
      submitMs: {fog: 1}, gpuMs: {}, policy: {}, maximumDensity: .15,
      timerQueriesEnabled: false, timerQueriesStarted: 0, timerApiCalls: 0, timerSupported: false,
      sentAt: performance.timeOrigin + performance.now()}};
    engine.telemetry = telemetry;
    presenter.transferFromImageBitmap({});
    const event = new Event('message'); event.data = {type: 'frame', telemetry}; worker.dispatchEvent(event);
  };
  try {await run({present, presenter, original, window, document, canvas, engine});}
  finally {
    window.finishRoomPassSample?.();
    for (const [key, descriptor] of previous) {
      if (descriptor) Object.defineProperty(globalThis, key, descriptor); else delete globalThis[key];
    }
  }
}

test('prepared diagnostics share cadence boundaries and exclude surrounding presentations', async () => {
  await withPresentationBoundary(async ({present, presenter, original, window}) => {
    beginPassSample({mode: 'full', defer: true});
    present(); assert.equal(presenter.transferFromImageBitmap, original);
    const pending = measureRoom(0);
    present(); present();
    const measured = await pending;
    present();
    assert.equal(measured.presented.frames, 2);
    assert.equal(measured.passDiagnostic.accepted, measured.presented.frames);
    assert.equal(measured.passDiagnostic.queryFrames.length, 2);
    assert.equal(measured.passDiagnostic.timings.fogSubmitMs.samples, 2);
    assert.equal(presenter.transferFromImageBitmap, original);
    assert.equal(window.startRoomPassSample, undefined);
    assert.equal(window.finishRoomPassSample, undefined);
  });
});

test('legacy immediate sampling remains available and early failure clears preparation', async () => {
  await withPresentationBoundary(async ({present, window, document, presenter, original}) => {
    beginPassSample('full'); present();
    assert.equal(window.finishRoomPassSample().accepted, 1);
    beginPassSample({mode: 'full', defer: true});
    assert.throws(() => beginPassSample('full'), /already prepared/);
    document.hidden = true;
    await assert.rejects(measureRoom(0), /foreground/);
    assert.equal(window.startRoomPassSample, undefined);
    assert.equal(window.finishRoomPassSample, undefined);
    assert.equal(presenter.transferFromImageBitmap, original);
  });
});

test('a headless browser sample is recorded but cannot qualify as a foreground display', async () => {
  await withPresentationBoundary(async ({present}) => {
    const sample = async () => {
      const pending = measureRoom(0);
      present(); present();
      return (await pending).invalid;
    };
    assert.deepEqual(await sample(), []);
    navigator.userAgent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) HeadlessChrome/155.0.0.0 Safari/537.36';
    assert.deepEqual(await sample(), ['headless browser has no foreground display']);
  });
});

test('a canvas resize during a sample invalidates it and frame gaps count against the configured interval', async () => {
  await withPresentationBoundary(async ({present, canvas, engine}) => {
    engine.telemetry = {...engine.telemetry, targetRefreshHz: 120};
    const pending = measureRoom(40);
    present(); await new Promise(resolve => setTimeout(resolve, 15)); present();
    canvas.width = 120;
    const measured = await pending;
    assert.deepEqual(measured.invalid, ['canvas size or display scale changed during the sample']);
    assert.deepEqual(measured.startSize.backing, [100, 50]);
    assert.deepEqual(measured.backing, [120, 50]);
    // One 15 ms gap is under the 60 Hz threshold but misses a 120 Hz refresh.
    assert.equal(measured.presented.over25ms, 0);
    assert.equal(measured.presented.overOneAndHalfTargetInterval, 1);
    assert.equal(measured.screen.width, 1512);
  });
});

test('a resize reverted before the end of a sample still invalidates it', async () => {
  await withPresentationBoundary(async ({present, canvas, window}) => {
    const pending = measureRoom(1100);
    present(); canvas.width = 140;
    await new Promise(resolve => setTimeout(resolve, 1050));
    canvas.width = 100; window.dispatchEvent(new Event('resize')); present();
    const measured = await pending;
    assert.deepEqual(measured.backing, [100, 50]);
    assert.deepEqual(measured.invalid, ['canvas size or display scale changed during the sample']);
    assert.ok(measured.sizeChecks.changedChecks >= 1);
    assert.equal(measured.sizeChecks.windowResizes, 1);
  });
});

// Block medians of worker frame time (ms) recorded in ten 2-second blocks of
// 20-second samples by the headless ANGLE Metal runs on the M4 Pro (#685).
const recorded = {
  quietNull2First: [11.19, 11.13, 11.08, 11.26, 11.27, 11.23, 11.29, 11.32, 11.41, 11.35],
  contendedNull1First: [13.70, 13.35, 11.22, 14.11, 12.88, 13.49, 13.96, 14.08, 13.44, 12.56],
  timerLastFull: [12.59, 11.39, 12.43, 11.11, 13.45, 11.74, 12.78, 10.57, 13.27, 11.56],
  attributionCachedFrame: [3.63, 3.59, 3.63, 3.62, 3.57, 3.64, 3.65, 3.60, 3.37, 3.29],
  attributionFirst: [11.18, 11.23, 11.36, 11.29, 11.27, 11.38, 11.16, 11.37, 11.49, 11.40],
  attributionSkipFog: [8.95, 8.87, 8.84, 8.72, 8.88, 8.96, 8.85, 8.84, 8.91, 8.86],
  attributionCachedShadows: [14.26, 11.67, 12.27, 11.56, 13.01, 10.36, 12.91, 11.13, 12.21, 11.88],
  attributionLast: [11.19, 11.24, 11.31, 11.25, 11.30, 11.14, 11.17, 11.11, 11.23, 11.21]};
// 120 frames per block, spread evenly over the 20-second sample.
const frames = blocks => {
  const values = blocks.flatMap(ms => Array(120).fill(ms));
  return {values, offsets: values.map((_, i) => (i + .5) * 20000 / values.length)};
};
const stationarity = {blocks: 10, tolerance: .05, floorMs: .25};

test('a sample is stationary only when its 2-second block medians stay within the stated spread', () => {
  const quiet = frames(recorded.quietNull2First), contended = frames(recorded.contendedNull1First);
  const q = stationarityOf(quiet.values, quiet.offsets, 20000, stationarity);
  assert.equal(q.stationary, true); assert.deepEqual(q.blockMediansMs, recorded.quietNull2First);
  assert.ok(q.spread < .03);
  const c = stationarityOf(contended.values, contended.offsets, 20000, stationarity);
  assert.equal(c.stationary, false); assert.match(c.reason, /^block medians spread 21\.5% \/ 2\.89 ms \(limit 5% or 0\.25 ms\)$/);
  // A whole-sample median can look ordinary while its blocks switch levels.
  const t = frames(recorded.timerLastFull);
  assert.equal(stationarityOf(t.values, t.offsets, 20000, stationarity).stationary, false);
  // Sub-millisecond jitter on a short pass stays under the 0.25 ms floor; the
  // recorded cached-frame blocks (0.36 ms) do not.
  const short = frames([3.6, 3.65, 3.55, 3.62, 3.58, 3.6, 3.64, 3.56, 3.61, 3.59]);
  const s = stationarityOf(short.values, short.offsets, 20000, stationarity);
  assert.ok(s.spread > .025 && s.spreadMs <= .25); assert.equal(s.stationary, true);
  assert.equal(stationarityOf(short.values, short.offsets, 20000, {blocks: 10, tolerance: .02}).stationary, false);
  const cached = frames(recorded.attributionCachedFrame);
  assert.equal(stationarityOf(cached.values, cached.offsets, 20000, stationarity).stationary, false);
  // Equal-count blocks when frame offsets are unavailable; missing or sparse data fails closed.
  assert.equal(stationarityOf(quiet.values, null, null, stationarity).stationary, true);
  assert.equal(stationarityOf([...quiet.values.slice(1), NaN], quiet.offsets, 20000, stationarity).reason, 'frame times missing');
  assert.equal(stationarityOf(quiet.values.slice(0, 40), null, null, stationarity).reason, 'fewer than five frames in a block');
});

const sample = (name, pass, {density, hz = 60, blocks}) => {
  const {values, offsets} = frames(blocks);
  const sorted = [...values].sort((a, b) => a - b), p50 = (sorted[599] + sorted[600]) / 2;
  return {name: name+'-'+pass, scenario: name, pass, policySamples: [{fogMeanDensity: density}, {fogMeanDensity: density}],
    presented: {hz}, passTimingsMs: {workerFrameMs: {p50}, gpuFenceWaitMs: {p50: 2}, readyForFrameMs: {p50: 16}},
    stationarity: stationarityOf(values, offsets, 20000, stationarity)};
};
const flat = ms => Array(10).fill(ms);
const tolerance = {meanDensity: .01, roomHz: .02, workerFrameP50Ms: .02};

test('a middle is resolved only when stationary and further from the bracket mean than the brackets differ', () => {
  const [c] = compareBrackets([sample('dense', 'full', {density: .07, blocks: recorded.attributionFirst}),
    sample('dense', 'present-cached-frame', {density: .07, blocks: flat(3.6)}),
    sample('dense', 'skip-fog-raymarch', {density: .07, blocks: recorded.attributionSkipFog}),
    sample('dense', 'cached-shadows', {density: .07, blocks: recorded.attributionCachedShadows}),
    sample('dense', 'full', {density: .07, blocks: recorded.attributionLast})], tolerance);
  assert.equal(c.bracketsAgree, true);
  assert.deepEqual(c.middles.map(m => [m.pass, m.stationary, m.reading]), [
    ['present-cached-frame', true, 'resolved'], ['skip-fog-raymarch', true, 'resolved'],
    ['cached-shadows', false, 'not resolved']]);
  // A −1.3% middle inside a 1.5% bracket difference is not a result.
  const small = compareBrackets([sample('dense', 'full', {density: .07, blocks: flat(11.2)}),
    sample('dense', 'full-no-timers', {density: .07, blocks: flat(11.2 * 1.0075 * (1 - .013))}),
    sample('dense', 'full', {density: .07, blocks: flat(11.2 * 1.015)})], tolerance)[0];
  assert.equal(small.bracketsAgree, true);
  assert.equal(small.middles[0].reading, 'not resolved');
});

test('a held null comparison agrees at its brackets and its middle is not resolved', () => {
  const [c] = compareBrackets(['full', 'full', 'full'].map(p => sample('dense', p, {density: .069, blocks: recorded.quietNull2First})), tolerance);
  assert.equal(c.bracketsAgree, true);
  assert.deepEqual(c.disagreements, []);
  assert.equal(c.brackets.meanDensity.relative, 0);
  assert.deepEqual(c.middles.map(m => [m.pass, m.relativeToBracketMean.meanDensity, m.relativeToBracketMean.workerFrameP50Ms, m.reading]),
    [['full', 0, 0, 'not resolved']]);
  // Nearly identical brackets must not let noise resolve: the threshold is at
  // least the bracket tolerance and the middle's own block spread.
  const [close] = compareBrackets([11.20, 11.26, 11.21].map(ms => sample('dense', 'full', {density: .069, blocks: flat(ms)})), tolerance);
  assert.equal(close.bracketsAgree, true);
  assert.ok(close.middles[0].relativeToBracketMean.workerFrameP50Ms > Math.abs(close.brackets.workerFrameP50Ms.relative));
  assert.equal(close.middles[0].resolutionThreshold, .02);
  assert.equal(close.middles[0].reading, 'not resolved');
  const spreadMiddle = compareBrackets([sample('dense', 'full', {density: .069, blocks: flat(11.2)}),
    sample('dense', 'full', {density: .069, blocks: [11.0, 11.5, 11.0, 11.5, 11.0, 11.5, 11.0, 11.5, 11.0, 11.5].map(ms => ms * 1.0)}),
    sample('dense', 'full', {density: .069, blocks: flat(11.2)})], tolerance)[0];
  assert.ok(spreadMiddle.middles[0].resolutionThreshold > .04);
  // Missing stationarity evidence is not stationary.
  const bare = ['full', 'full', 'full'].map(p => {const s = sample('dense', p, {density: .069, blocks: flat(11.2)}); delete s.stationarity; return s;});
  assert.equal(compareBrackets(bare, tolerance)[0].bracketsAgree, false);
});

test('a decaying medium, a frame-cost drift or a contended bracket attributes nothing', () => {
  const decayed = compareBrackets([sample('dense', 'full', {density: .069, blocks: flat(11.2)}),
    sample('dense', 'simple-fixed-loop', {density: .04, blocks: flat(11.2)}),
    sample('dense', 'full', {density: .021, blocks: flat(11.2)})], tolerance)[0];
  assert.equal(decayed.bracketsAgree, false);
  assert.deepEqual(decayed.disagreements, ['meanDensity -69.57% (limit 1%)']);
  // Shaped on the held timer run: its last bracket contended (blocks recorded).
  const timer = compareBrackets([sample('dense', 'full', {density: .07, blocks: flat(11.21)}),
    sample('dense', 'full-no-timers', {density: .07, blocks: flat(11.55)}),
    sample('dense', 'full', {density: .07, blocks: recorded.timerLastFull})], tolerance)[0];
  assert.equal(timer.bracketsAgree, false);
  assert.match(timer.disagreements[0], /^workerFrameP50Ms 7\.81% \(limit 2%\)$/);
  assert.deepEqual(timer.bracketStationary, [true, false]);
  assert.equal(timer.middles[0].reading, 'not resolved');
  // A contended bracket fails even when its median happens to match.
  const contended = compareBrackets([sample('dense', 'full', {density: .05, blocks: flat(12.21)}),
    sample('dense', 'full', {density: .05, blocks: flat(12.2)}),
    sample('dense', 'full', {density: .05, blocks: recorded.timerLastFull})], tolerance)[0];
  assert.deepEqual(contended.disagreements, []);
  assert.equal(contended.bracketsAgree, false);
  const missing = sample('dense', 'full', {density: .05, blocks: flat(10)}); delete missing.passTimingsMs;
  assert.deepEqual(compareBrackets([sample('dense', 'full', {density: .05, blocks: flat(10)}),
    sample('dense', 'skip-fog-raymarch', {density: .05, blocks: flat(8)}), missing], tolerance)[0].disagreements,
  ['workerFrameP50Ms unavailable']);
  // Sequences that are not bracketed by full renders are not compared.
  assert.deepEqual(compareBrackets([sample('fixed', 'full', {density: .05, blocks: flat(10)}),
    sample('fixed', 'cached-shadows', {density: .05, blocks: flat(9)})], tolerance), []);
});

test('a held sample is invalid unless every frame rendered the captured medium', () => {
  const capture = {meanDensity: .0703564941660362, time: 31.7};
  const medium = {first: capture.meanDensity, last: capture.meanDensity, minimum: capture.meanDensity, maximum: capture.meanDensity,
    minimumTime: 31.7, maximumTime: 31.7, heldFrames: 1200, brokenHoldFrames: 0};
  assert.equal(heldMediumInvalid(medium, capture, 1200), false);
  assert.equal(heldMediumInvalid({...medium, brokenHoldFrames: 1}, capture, 1200), true);
  assert.equal(heldMediumInvalid({...medium, heldFrames: 1199}, capture, 1200), true);
  assert.equal(heldMediumInvalid({...medium, minimum: .0703}, capture, 1200), true);
  assert.equal(heldMediumInvalid({...medium, maximumTime: 31.75}, capture, 1200), true);
  assert.equal(heldMediumInvalid({...medium, minimum: null}, capture, 1200), true);
  assert.equal(heldMediumInvalid(medium, null, 1200), true);
});
