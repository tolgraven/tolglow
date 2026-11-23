import test from 'node:test';
import assert from 'node:assert/strict';
import {beginPassSample} from '../../../dev/preview/room-pass-analysis.mjs';
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
  try {await run({present, presenter, original, window, document});}
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
