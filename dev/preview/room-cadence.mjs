// Evaluate inside the real, foreground Studio tab. The worker listener counts
// accepted bitmap presentations after the proxy processed each message. Main
// rendering counts completed film passes. Neither path requests an extra draw.
export async function measureRoom(durationMs) {
  const canvas = document.querySelector('#preview');
  const engine = canvas?.tolglowPreview;
  if (!engine?.ready || !document.hasFocus() || document.hidden) {
    window.finishRoomPassSample?.();
    throw new Error('Room must be ready and in the foreground.');
  }
  const initial = engine.getTelemetry();
  const size = () => {
    const rect = canvas.getBoundingClientRect();
    return {backing: [canvas.width, canvas.height], css: [rect.width, rect.height], dpr: devicePixelRatio};
  };
  const startSize = size();
  // A resize reverted before the end must still count: check every second
  // and count window resize events, not only the start and end sizes.
  let sizeChanges = 0, windowResizes = 0;
  const checkSize = () => {if (JSON.stringify(size()) !== JSON.stringify(startSize)) sizeChanges++;};
  const onResize = () => {windowResizes++;};
  const worker = initial.thread === 'worker' ? engine.worker : null;
  const renderer = engine.fallback || engine;
  const frames = [], display = [], qualities = new Set(), sizes = new Set(), scales = new Set(), samples = [];
  let raf, qualityTimer, passDiagnostic, ended;
  const inspect = () => {
    checkSize();
    const t = engine.getTelemetry();
    qualities.add(t.quality);
    sizes.add(JSON.stringify(t.pixels)); scales.add(t.renderScale);
    samples.push({atMs: performance.now() - boundary, tier: t.quality, scale: t.renderScale,
      scene: t.pixels, targetHz: t.targetRefreshHz, measuredDisplayHz: t.displayHz ?? null, tierMaxHz: t.tierMaxHz ?? null, intervalMs: t.frameIntervalMs,
      workerFrameMs: t.workerFrameMs ?? null, cpuSubmitMs: t.frameCPUms ?? null,
      fogMaximumDensity: t.fog?.maximumDensity ?? null, fogMeanDensity: t.fog?.meanDensity ?? null,
      fogActiveSources: t.fog?.activeSources ?? null,
      gpuFenceWaitMs: t.gpuCompletionWaitMs ?? null, gpuTimerMs: t.gpuTimeMs ?? null});
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
  // Headless Chrome reports focus and visibility without presenting to a display.
  if (/\bHeadlessChrome\b/.test(navigator.userAgent)) invalid.push('headless browser has no foreground display');
  const focusChange = () => { if (document.hidden || !document.hasFocus()) invalid.push('backgrounded'); };
  document.addEventListener('visibilitychange', focusChange);
  window.addEventListener('blur', focusChange);
  window.addEventListener('resize', onResize);
  try {
    // Prepared diagnostic collection shares this synchronous browser boundary.
    // Node-side API reads and OS qualification before/after cannot add frames.
    window.startRoomPassSample?.();
    inspect(); raf = requestAnimationFrame(observeRaf); qualityTimer = setInterval(inspect, 1000);
    await new Promise(resolve => setTimeout(resolve, durationMs));
  } finally {
    ended = performance.now();
    if (worker) worker.removeEventListener('message', onBitmap);
    else renderer.draw = original;
    window.removeEventListener('blur', focusChange);
    window.removeEventListener('resize', onResize);
    document.removeEventListener('visibilitychange', focusChange);
    cancelAnimationFrame(raf); clearInterval(qualityTimer);
    passDiagnostic = window.finishRoomPassSample?.();
  }
  const elapsedMs = ended - boundary;
  // over25ms is a 60 Hz threshold. overOneAndHalfTargetInterval counts gaps
  // longer than 1.5 configured frame intervals, so one missed 120 Hz refresh counts.
  const describe = (times, targetMs = null) => {
    const gaps = times.slice(1).map((time, i) => time - times[i]).sort((a, b) => a - b);
    const quantile = p => gaps.length ? gaps[Math.ceil(gaps.length * p) - 1] : null;
    return {frames: times.length, hz: times.length * 1000 / elapsedMs,
      p50Ms: quantile(.5), p95Ms: quantile(.95), maxMs: gaps.at(-1) ?? null,
      over25ms: gaps.filter(ms => ms > 25).length,
      overOneAndHalfTargetInterval: targetMs ? gaps.filter(ms => ms > 1.5 * targetMs).length : null};
  };
  const end = engine.getTelemetry(), rect = canvas.getBoundingClientRect(), endSize = size();
  if (JSON.stringify(startSize) !== JSON.stringify(endSize) || sizeChanges || windowResizes)
    invalid.push('canvas size or display scale changed during the sample');
  const fields = ['renderer', 'deviceRenderer', 'softwareRenderer', 'targetRefreshHz', 'quality',
    'frameCPUms', 'gpuTimeMs', 'gpuCompletionWaitMs', 'workerFrameMs', 'sceneSamples', 'edgeFilter',
    'renderScale', 'displayRange', 'fixtureCount', 'visibleFixtures', 'activeLights', 'shadowCount',
    'workLight', 'fogAmount', 'fog', 'camera', 'target', 'lightIds', 'fallbackReason', 'frameNumber'];
  const project = t => Object.fromEntries(fields.map(k => [k, t[k] ?? null]));
  if (end.failed || !end.visible || (worker && end.thread !== 'worker')) invalid.push('renderer interrupted');
  if (frames.length < 2) invalid.push('fewer than two completed renderer frames');
  inspect();
  const targets = [...new Set(samples.map(s => s.targetHz))];
  const targetHz = targets.length === 1 ? targets[0] : null;
  const presented = describe(frames, targetHz ? 1000 / targetHz : null), windowRaf = describe(display);
  const cadence = {configuredHz: targets, roomHz: presented.hz, displayHz: windowRaf.hz,
    roomOverConfigured: targetHz ? presented.hz / targetHz : null,
    roomOverDisplay: windowRaf.hz ? presented.hz / windowRaf.hz : null,
    withinFivePercentOfConfigured: targetHz ? presented.hz >= .95 * targetHz : null};
  return {durationMs: elapsedMs, invalid, rendererThread: worker ? 'worker' : 'main',
    sampleBoundaryEpochMs: {start: performance.timeOrigin + boundary, end: performance.timeOrigin + ended},
    frameMeasurement: worker ? 'accepted DOM bitmap' : 'completed main-thread film pass',
    presented, windowRaf, cadence, policySamples: samples, scales: [...scales], frameOffsetsMs: frames.map(t => t - boundary),
    qualities: [...qualities], sceneSizes: [...sizes].map(s => JSON.parse(s)), backing: [canvas.width, canvas.height],
    css: [rect.width, rect.height], dpr: devicePixelRatio, displayPixels: end.displayPixels, startSize,
    sizeChecks: {changedChecks: sizeChanges, windowResizes},
    // Geometry of the screen holding the window; browsers expose no display name here.
    screen: {width: screen.width, height: screen.height, availLeft: screen.availLeft ?? null, availTop: screen.availTop ?? null,
      colorDepth: screen.colorDepth, isExtended: screen.isExtended ?? null},
    browser: navigator.userAgent, start: project(initial), end: project(end), passDiagnostic};
}
