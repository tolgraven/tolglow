// Browser-side collection uses completed, accepted bitmaps from the real proxy.
export function beginPassSample(options) {
  const {mode, defer = false} = typeof options === 'string' ? {mode: options} : options;
  if (window.finishRoomPassSample) throw new Error('A Room pass collector is already prepared.');
  const engine = document.querySelector('#preview').tolglowPreview;
  if (!engine.worker || !engine.bitmapPresenter) throw new Error('Pass attribution requires a worker and bitmaprenderer.');
  const presenter = engine.presenter, original = presenter.transferFromImageBitmap, values = new Map();
  let submitted, accepted = 0, raymarched = 0, started = false;
  const policies = new Set(), queryFrames = [];
  let maximumDensity = 0, detailedFrames = 0, missingPolicyFrames = 0;
  // Medium seen by the accepted frames: first/last/min/max field mean density
  // and fog time, plus whether every frame rendered a held field.
  const medium = {first: null, last: null, minimum: Infinity, maximum: -Infinity,
    minimumTime: Infinity, maximumTime: -Infinity, heldFrames: 0, brokenHoldFrames: 0};
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
    if (Number.isFinite(p.meanDensity)) {
      medium.first ??= p.meanDensity; medium.last = p.meanDensity;
      medium.minimum = Math.min(medium.minimum, p.meanDensity); medium.maximum = Math.max(medium.maximum, p.meanDensity);
    }
    if (Number.isFinite(p.fieldTime)) {
      medium.minimumTime = Math.min(medium.minimumTime, p.fieldTime); medium.maximumTime = Math.max(medium.maximumTime, p.fieldTime);
    }
    if (p.fogHold) {medium.heldFrames++; if (p.fogHold.broken) medium.brokenHoldFrames++;}
    for (const [phase, ms] of Object.entries(p.submitMs)) add(phase+'SubmitMs', ms);
    for (const [phase, ms] of Object.entries(p.gpuMs)) add(phase+'GpuMs', ms);
    add('workerFrameMs', t.workerFrameMs); add('gpuFenceWaitMs', t.gpuCompletionWaitMs);
    add('readyForFrameMs', t.readyForFrameMs); add('rafDeliveryMs', t.rafDeliveryMs);
    add('bitmapExportMs', t.bitmapTransferMs); add('domSubmitMs', submitted);
    add('workerPostThroughDomMs', performance.timeOrigin + performance.now() - p.sentAt);
    queryFrames.push({frame: p.frame, enabled: p.timerQueriesEnabled, started: p.timerQueriesStarted,
      apiCalls: p.timerApiCalls, supported: p.timerSupported, extensionRequested: p.timerExtensionRequested, workerFrameMs: t.workerFrameMs,
      gpuFenceWaitMs: t.gpuCompletionWaitMs, readyForFrameMs: t.readyForFrameMs, rafDeliveryMs: t.rafDeliveryMs});
  };
  window.startRoomPassSample = () => {
    if (started) return;
    started = true; presenter.transferFromImageBitmap = transfer;
    engine.worker.addEventListener('message', collect);
  };
  window.finishRoomPassSample = () => {
    if (started) {
      presenter.transferFromImageBitmap = original;
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
      maximumDensity, detailedFrames, missingPolicyFrames},
      medium: Object.fromEntries(Object.entries(medium).map(([key, value]) => [key, Number.isFinite(value) || typeof value !== 'number' ? value : null]))};
  };
  if (!defer) window.startRoomPassSample();
}

// Node side. Groups each scenario's pass samples, compares the first and last
// `full` samples, and reports every middle sample against their mean. A
// sequence whose brackets disagree beyond the tolerance attributes nothing.
export function compareBrackets(scenarios, tolerance) {
  const groups = new Map();
  for (const sample of scenarios) if (sample.pass) {
    const key = sample.scenario ?? sample.name;
    if (!groups.has(key)) groups.set(key, []);
    groups.get(key).push(sample);
  }
  const average = xs => {
    const finite = xs.filter(Number.isFinite);
    return finite.length ? finite.reduce((a, b) => a + b, 0) / finite.length : null;
  };
  const metrics = s => ({meanDensity: average((s.policySamples || []).map(p => p.fogMeanDensity)),
    roomHz: s.presented?.hz ?? null, workerFrameP50Ms: s.passTimingsMs?.workerFrameMs?.p50 ?? null,
    gpuFenceWaitP50Ms: s.passTimingsMs?.gpuFenceWaitMs?.p50 ?? null,
    readyForFrameP50Ms: s.passTimingsMs?.readyForFrameMs?.p50 ?? null});
  const relative = (value, reference) => !Number.isFinite(value) || !Number.isFinite(reference) ? null
    : reference === 0 ? (value === 0 ? 0 : null) : (value - reference) / reference;
  const comparisons = [];
  for (const [scenario, samples] of groups) {
    if (samples.length < 3 || samples[0].pass !== 'full' || samples.at(-1).pass !== 'full') continue;
    const first = metrics(samples[0]), last = metrics(samples.at(-1));
    const brackets = Object.fromEntries(Object.keys(first).map(key =>
      [key, {first: first[key], last: last[key], relative: relative(last[key], first[key])}]));
    const disagreements = Object.entries(tolerance).filter(([key, limit]) => {
      const r = brackets[key]?.relative;
      return r == null || Math.abs(r) > limit;
    }).map(([key]) => key+' '+(brackets[key]?.relative == null ? 'unavailable'
      : (100 * brackets[key].relative).toFixed(2)+'% (limit '+(100 * tolerance[key])+'%)'));
    const middles = samples.slice(1, -1).map(sample => {
      const m = metrics(sample);
      return {name: sample.name, pass: sample.pass, values: m, relativeToBracketMean: Object.fromEntries(Object.keys(m).map(key =>
        [key, relative(m[key], first[key] == null || last[key] == null ? null : (first[key] + last[key]) / 2)]))};
    });
    comparisons.push({scenario, first: samples[0].name, last: samples.at(-1).name, tolerance, brackets,
      disagreements, bracketsAgree: !disagreements.length, middles});
  }
  return comparisons;
}
