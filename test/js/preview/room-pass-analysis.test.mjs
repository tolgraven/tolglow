import test from 'node:test';
import assert from 'node:assert/strict';
import {beginPassSample, compareBrackets} from '../../../dev/preview/room-pass-analysis.mjs';
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

const sample = (name, pass, {density, hz = 60, worker = 10}) => ({name: name+'-'+pass, scenario: name, pass,
  policySamples: [{fogMeanDensity: density}, {fogMeanDensity: density}], presented: {hz},
  passTimingsMs: {workerFrameMs: {p50: worker}, gpuFenceWaitMs: {p50: 2}, readyForFrameMs: {p50: 16}}});
const tolerance = {meanDensity: .01, roomHz: .05, workerFrameP50Ms: .1};

test('a held null comparison agrees at its brackets and reports a zero middle difference', () => {
  const [c] = compareBrackets(['full', 'full', 'full'].map(p => sample('dense', p, {density: .069})), tolerance);
  assert.equal(c.bracketsAgree, true);
  assert.deepEqual(c.disagreements, []);
  assert.equal(c.brackets.meanDensity.relative, 0);
  assert.deepEqual(c.middles.map(m => [m.pass, m.relativeToBracketMean.meanDensity, m.relativeToBracketMean.roomHz]), [['full', 0, 0]]);
});

test('a decaying medium or a frame-cost drift between the full brackets attributes nothing', () => {
  const decayed = compareBrackets([sample('dense', 'full', {density: .069}), sample('dense', 'simple-fixed-loop', {density: .04}),
    sample('dense', 'full', {density: .021})], tolerance)[0];
  assert.equal(decayed.bracketsAgree, false);
  assert.match(decayed.disagreements[0], /^meanDensity -69\.57% \(limit 1%\)$/);
  const drift = compareBrackets([sample('dense', 'full', {density: .05, hz: 60}), sample('dense', 'full', {density: .05}),
    sample('dense', 'full', {density: .05, hz: 54, worker: 12})], tolerance)[0];
  assert.deepEqual(drift.disagreements, ['roomHz -10.00% (limit 5%)', 'workerFrameP50Ms 20.00% (limit 10%)']);
  const missing = sample('dense', 'full', {density: .05}); delete missing.passTimingsMs;
  assert.deepEqual(compareBrackets([sample('dense', 'full', {density: .05}), sample('dense', 'skip-fog-raymarch', {density: .05}), missing],
    tolerance)[0].disagreements, ['workerFrameP50Ms unavailable']);
  // Sequences that are not bracketed by full renders are not compared.
  assert.deepEqual(compareBrackets([sample('fixed', 'full', {density: .05}), sample('fixed', 'cached-shadows', {density: .05})], tolerance), []);
});
