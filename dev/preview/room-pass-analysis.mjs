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

const median = xs => {
  const sorted = [...xs].sort((a, b) => a - b), middle = sorted.length / 2;
  return sorted.length % 2 ? sorted[Math.floor(middle)] : (sorted[middle - 1] + sorted[middle]) / 2;
};

// Node side. A held sample is valid only if every accepted frame rendered the
// captured mean density and fog time and no frame reported a broken hold.
export function heldMediumInvalid(medium, capture, accepted) {
  if (!medium || !capture) return true;
  return medium.heldFrames !== accepted || medium.brokenHoldFrames !== 0 ||
    [medium.minimum, medium.maximum].some(d => d !== capture.meanDensity) ||
    [medium.minimumTime, medium.maximumTime].some(t => t !== capture.time);
}

// Node side. Contention comes and goes within a sample, so a sample median can
// land anywhere between a quiet and a contended level. Split the accepted
// frames into equal time blocks (2 s for ten blocks of a 20 s sample) and
// require the block medians to spread by at most `tolerance` of their median,
// or `floorMs`, whichever is larger, so sub-millisecond jitter on very short
// frames does not count as contention.
export function stationarityOf(values, offsetsMs, durationMs, {blocks = 10, tolerance, floorMs = 0}) {
  const result = reason => ({blocks, tolerance, floorMs, blockMediansMs, spread, spreadMs, stationary: !reason, reason: reason || null});
  let blockMediansMs = [], spread = null, spreadMs = null;
  if (!Array.isArray(values) || values.some(v => !Number.isFinite(v))) return result('frame times missing');
  const timed = Array.isArray(offsetsMs) && offsetsMs.length === values.length && Number.isFinite(durationMs) && durationMs > 0;
  const groups = Array.from({length: blocks}, () => []);
  values.forEach((value, i) => {
    const block = timed ? Math.floor(offsetsMs[i] / (durationMs / blocks)) : Math.floor(i * blocks / values.length);
    groups[Math.max(0, Math.min(blocks - 1, block))].push(value);
  });
  if (groups.some(group => group.length < 5)) return result('fewer than five frames in a block');
  blockMediansMs = groups.map(median);
  const center = median(blockMediansMs);
  spreadMs = Math.max(...blockMediansMs) - Math.min(...blockMediansMs); spread = spreadMs / center;
  return result(spreadMs > Math.max(tolerance * center, floorMs)
    ? 'block medians spread '+(100 * spread).toFixed(1)+'% / '+spreadMs.toFixed(2)+' ms (limit '+(100 * tolerance)+'% or '+floorMs+' ms)' : null);
}

// Node side. Groups each scenario's pass samples, compares the first and last
// `full` samples, and reports every middle sample against their mean. A
// sequence whose brackets disagree beyond the tolerance attributes nothing,
// and a middle is resolved only when it is stationary and lies further from
// the bracket mean than the bracket gap, the bracket tolerance and its own
// block spread. Missing stationarity evidence counts as not stationary.
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
  const stationary = s => s.stationarity?.stationary === true;
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
    const bracketsAgree = !disagreements.length && [samples[0], samples.at(-1)].every(stationary);
    const bracketMean = key => first[key] == null || last[key] == null ? null : (first[key] + last[key]) / 2;
    const middles = samples.slice(1, -1).map(sample => {
      const m = metrics(sample);
      const relativeToBracketMean = Object.fromEntries(Object.keys(m).map(key => [key, relative(m[key], bracketMean(key))]));
      const difference = relativeToBracketMean.workerFrameP50Ms, gap = brackets.workerFrameP50Ms.relative;
      const threshold = gap == null ? null : Math.max(Math.abs(gap), tolerance.workerFrameP50Ms ?? 0, sample.stationarity?.spread ?? Infinity);
      const resolved = bracketsAgree && stationary(sample) && difference != null && threshold != null &&
        Math.abs(difference) > threshold;
      return {name: sample.name, pass: sample.pass, values: m, stationary: stationary(sample), relativeToBracketMean,
        resolutionThreshold: threshold, resolved, reading: resolved ? 'resolved' : 'not resolved'};
    });
    comparisons.push({scenario, first: samples[0].name, last: samples.at(-1).name, tolerance, brackets,
      bracketStationary: [stationary(samples[0]), stationary(samples.at(-1))], disagreements, bracketsAgree, middles});
  }
  return comparisons;
}
