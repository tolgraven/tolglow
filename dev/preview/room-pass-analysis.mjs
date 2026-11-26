// Browser-side collection uses completed, accepted bitmaps from the real proxy.
// When the worker presents on its own surface it reports the canvas submission.
export function beginPassSample(options) {
  const {mode, defer = false} = typeof options === 'string' ? {mode: options} : options;
  if (window.finishRoomPassSample) throw new Error('A Room pass collector is already prepared.');
  const engine = document.querySelector('#preview').tolglowPreview;
  if (!engine.worker || !(engine.bitmapPresenter || engine.surface)) throw new Error('Pass attribution requires a worker and bitmaprenderer.');
  // A deferred start can follow the handover, so the presenter is chosen then.
  let presenter = null, original = null;
  const values = new Map();
  let submitted, accepted = 0, raymarched = 0, started = false;
  const policies = new Set(), queryFrames = [];
  let maximumDensity = 0, detailedFrames = 0, missingPolicyFrames = 0;
  const add = (name, ms) => {
    if (!Number.isFinite(ms)) return;
    if (!values.has(name)) values.set(name, []);
    values.get(name).push(ms);
  };
  const transfer = function(bitmap) {
    const start = performance.now(), result = original.call(this, bitmap);
    submitted = performance.now() - start;
    return result;
  };
  const collect = ({data}) => {
    const t = data?.telemetry, p = t?.passProbe;
    if (!p || p.mode !== mode || t !== engine.telemetry) return;
    accepted++; if (p.raymarched) raymarched++;
    if (p.policy) policies.add(JSON.stringify(p.policy)); else missingPolicyFrames++;
    maximumDensity = Math.max(maximumDensity, p.maximumDensity);
    if (p.detailActive) detailedFrames++;
    for (const [phase, ms] of Object.entries(p.submitMs)) add(phase+'SubmitMs', ms);
    for (const [phase, ms] of Object.entries(p.gpuMs)) add(phase+'GpuMs', ms);
    add('workerFrameMs', t.workerFrameMs); add('gpuFenceWaitMs', t.gpuCompletionWaitMs);
    add('readyForFrameMs', t.readyForFrameMs); add('rafDeliveryMs', t.rafDeliveryMs);
    add('bitmapExportMs', t.bitmapTransferMs); add('domSubmitMs', engine.surface ? t.presentSubmitMs : submitted);
    add('workerPostThroughDomMs', performance.timeOrigin + performance.now() - p.sentAt);
    queryFrames.push({frame: p.frame, enabled: p.timerQueriesEnabled, started: p.timerQueriesStarted,
      apiCalls: p.timerApiCalls, supported: p.timerSupported, extensionRequested: p.timerExtensionRequested, workerFrameMs: t.workerFrameMs,
      gpuFenceWaitMs: t.gpuCompletionWaitMs, readyForFrameMs: t.readyForFrameMs, rafDeliveryMs: t.rafDeliveryMs});
  };
  window.startRoomPassSample = () => {
    if (started) return;
    started = true;
    if (!engine.surface) {
      presenter = engine.presenter; original = presenter.transferFromImageBitmap;
      presenter.transferFromImageBitmap = transfer;
    }
    engine.worker.addEventListener('message', collect);
  };
  window.finishRoomPassSample = () => {
    if (started) {
      if (presenter) presenter.transferFromImageBitmap = original;
      engine.worker.removeEventListener('message', collect);
    }
    delete window.startRoomPassSample;
    delete window.finishRoomPassSample;
    const timings = Object.fromEntries([...values].map(([key, xs]) => {
      xs.sort((a,b) => a-b);
      const at = p => xs[Math.ceil(xs.length * p)-1];
      return [key, {samples:xs.length, p50:at(.5), p95:at(.95), max:xs.at(-1)}];
    }));
    return {timings, accepted, raymarched, queryFrames, traversal: {policies: [...policies].map(p => JSON.parse(p)),
      maximumDensity, detailedFrames, missingPolicyFrames}};
  };
  if (!defer) window.startRoomPassSample();
}
