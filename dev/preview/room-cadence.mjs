// Evaluate inside the real, foreground Studio tab. The worker listener counts
// accepted bitmap presentations after the proxy processed each message. Main
// rendering counts completed film passes. Neither path requests an extra draw.
export async function measureRoom(durationMs) {
  const canvas = document.querySelector('#preview');
  const engine = canvas?.tolglowPreview;
  if (!engine?.ready || !document.hasFocus() || document.hidden) throw new Error('Room must be ready and in the foreground.');
  const initial = engine.getTelemetry();
  const worker = initial.thread === 'worker' ? engine.worker : null;
  const renderer = engine.fallback || engine;
  const frames = [], display = [], qualities = new Set(), sizes = new Set();
  let raf, qualityTimer;
  const inspect = () => {
    const t = engine.getTelemetry();
    qualities.add(t.quality);
    sizes.add(JSON.stringify(t.pixels));
  };
  const mark = () => frames.push(performance.now());
  const onBitmap = event => {
    if (event.data?.type === 'frame' && event.data.telemetry === engine.telemetry) mark();
  };
  const original = renderer.draw;
  if (worker) worker.addEventListener('message', onBitmap);
  else {
    renderer.draw = function(...args) {
      const before = this.draws;
      const result = original.apply(this, args);
      if (this.draws > before) mark();
      return result;
    };
  }
  const boundary = performance.now();
  const observeRaf = time => {
    if (time >= boundary) display.push(time);
    raf = requestAnimationFrame(observeRaf);
  };
  const invalid = [];
  const focusChange = () => { if (document.hidden || !document.hasFocus()) invalid.push('backgrounded'); };
  document.addEventListener('visibilitychange', focusChange);
  window.addEventListener('blur', focusChange);
  try {
    inspect(); raf = requestAnimationFrame(observeRaf); qualityTimer = setInterval(inspect, 1000);
    await new Promise(resolve => setTimeout(resolve, durationMs));
  } finally {
    if (worker) worker.removeEventListener('message', onBitmap);
    else renderer.draw = original;
    window.removeEventListener('blur', focusChange);
    document.removeEventListener('visibilitychange', focusChange);
    cancelAnimationFrame(raf); clearInterval(qualityTimer);
  }
  const elapsedMs = performance.now() - boundary;
  const describe = times => {
    const gaps = times.slice(1).map((time, i) => time - times[i]).sort((a, b) => a - b);
    const quantile = p => gaps.length ? gaps[Math.ceil(gaps.length * p) - 1] : null;
    return {frames: times.length, hz: times.length * 1000 / elapsedMs,
      p50Ms: quantile(.5), p95Ms: quantile(.95), maxMs: gaps.at(-1) ?? null,
      over25ms: gaps.filter(ms => ms > 25).length};
  };
  const end = engine.getTelemetry(), rect = canvas.getBoundingClientRect();
  const fields = ['renderer', 'deviceRenderer', 'softwareRenderer', 'targetRefreshHz', 'quality',
    'frameCPUms', 'gpuTimeMs', 'gpuCompletionWaitMs', 'workerFrameMs', 'sceneSamples', 'edgeFilter',
    'renderScale', 'displayRange', 'fixtureCount', 'visibleFixtures', 'activeLights', 'shadowCount',
    'workLight', 'fogAmount', 'fog', 'camera', 'lightIds', 'fallbackReason', 'frameNumber'];
  const project = t => Object.fromEntries(fields.map(k => [k, t[k] ?? null]));
  if (end.failed || !end.visible || (worker && end.thread !== 'worker')) invalid.push('renderer interrupted');
  if (frames.length < 2) invalid.push('fewer than two completed renderer frames');
  return {durationMs: elapsedMs, invalid, rendererThread: worker ? 'worker' : 'main',
    frameMeasurement: worker ? 'accepted DOM bitmap' : 'completed main-thread film pass',
    presented: describe(frames), windowRaf: describe(display), frameOffsetsMs: frames.map(t => t - boundary),
    qualities: [...qualities], sceneSizes: [...sizes].map(s => JSON.parse(s)), backing: [canvas.width, canvas.height],
    css: [rect.width, rect.height], dpr: devicePixelRatio, displayPixels: end.displayPixels,
    browser: navigator.userAgent, start: project(initial), end: project(end)};
}
