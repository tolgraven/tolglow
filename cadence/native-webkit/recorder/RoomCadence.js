// Evaluate inside the real, foreground Studio tab. The worker listener counts
// accepted bitmap presentations after the proxy processed each message. Main
// rendering counts completed film passes. Neither path requests an extra draw.
async function measureRoom(durationMs) {
  const canvas = document.querySelector('#preview');
  const engine = canvas?.tolglowPreview;
  if (!engine?.ready || !document.hasFocus() || document.hidden) {
    window.finishRoomPassSample?.();
    throw new Error('Room must be ready and in the foreground.');
  }
  const initial = engine.getTelemetry();
  const worker = initial.thread === 'worker' ? engine.worker : null;
  const renderer = engine.fallback || engine;
  const frames = [], display = [], qualities = new Set(), sizes = new Set(), scales = new Set(), samples = [];
  const workerTimings = [];
  let raf, qualityTimer, passDiagnostic, ended;
  const inspect = () => {
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
    if (event.data?.type !== 'frame' || event.data.telemetry !== engine.telemetry) return;
    mark();
    const t = event.data.telemetry;
    workerTimings.push([t.workerFrameMs, t.gpuCompletionWaitMs, t.bitmapTransferMs,
      t.readyForFrameMs, t.rafDeliveryMs, t.frameCPUms]);
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
    document.removeEventListener('visibilitychange', focusChange);
    cancelAnimationFrame(raf); clearInterval(qualityTimer);
    passDiagnostic = window.finishRoomPassSample?.();
  }
  const elapsedMs = ended - boundary;
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
    'workLight', 'fogAmount', 'fog', 'camera', 'target', 'lightIds', 'fallbackReason', 'frameNumber'];
  const project = t => Object.fromEntries(fields.map(k => [k, t[k] ?? null]));
  if (end.failed || !end.visible || (worker && end.thread !== 'worker')) invalid.push('renderer interrupted');
  if (frames.length < 2) invalid.push('fewer than two completed renderer frames');
  inspect();
  const presented = describe(frames), windowRaf = describe(display);
  const targets = [...new Set(samples.map(s => s.targetHz))];
  const targetHz = targets.length === 1 ? targets[0] : null;
  const cadence = {configuredHz: targets, roomHz: presented.hz, displayHz: windowRaf.hz,
    roomOverConfigured: targetHz ? presented.hz / targetHz : null,
    roomOverDisplay: windowRaf.hz ? presented.hz / windowRaf.hz : null,
    withinFivePercentOfConfigured: targetHz ? presented.hz >= .95 * targetHz : null};
  return {durationMs: elapsedMs, invalid, rendererThread: worker ? 'worker' : 'main',
    sampleBoundaryEpochMs: {start: performance.timeOrigin + boundary, end: performance.timeOrigin + ended},
    frameMeasurement: worker ? 'accepted DOM bitmap' : 'completed main-thread film pass',
    presented, windowRaf, cadence, policySamples: samples, scales: [...scales], frameOffsetsMs: frames.map(t => t - boundary),
    qualities: [...qualities], sceneSizes: [...sizes].map(s => JSON.parse(s)), backing: [canvas.width, canvas.height],
    css: [rect.width, rect.height], dpr: devicePixelRatio, displayPixels: end.displayPixels,
    browser: navigator.userAgent, start: project(initial), end: project(end), passDiagnostic, workerTimings};
}

(async () => {
  const samples = [], startup = [], changed = [];
  const receipt = {type: 'room-cadence', samples, startup, changed, expanded: globalThis.roomCadenceExpanded};
  const report = phase => {
    receipt.phase = phase;
    window.webkit.messageHandlers.recordRoomCadence.postMessage(JSON.stringify(receipt));
  };
  const delay = milliseconds => new Promise(resolve => setTimeout(resolve, milliseconds));
  async function until(label, condition, timeout=60000) {
    const expires = performance.now() + timeout;
    while (performance.now() < expires) {
      const result = condition();
      if (result) return result;
      await delay(150);
    }
    throw new Error('Timed out waiting for '+label);
  }
  function control(label) {
    const item = document.querySelector('[aria-label="'+label+'"]');
    if (!item) throw new Error('Missing Room control '+label);
    return item;
  }
  function slider(label, value) {
    const input = control(label);
    Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value').set.call(input, String(value));
    input.dispatchEvent(new Event('input', {bubbles: true}));
  }
  async function api(route, options) {
    const response = await fetch('/api/'+route, options);
    if (!response.ok) throw new Error('Loopback API request failed '+route+' '+response.status);
    return response.json();
  }
  async function cue(id) {
    const state = await api('state');
    const result = await api('command', {method: 'POST', headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({op: 'cue/launch', 'cue-id': id, launch: {mode: 'persistent'},
        'runtime-id': state['runtime-id'], 'show-id': state['show-id'], epoch: state.epoch,
        'expected-epoch': state.epoch, 'command-id': crypto.randomUUID(), source: {type: 'repl', 'session-id': 'native-room-cadence'}})});
    if ((result.ack || result).status !== 'accepted') throw new Error('The demo refused '+id);
  }
  async function head() {
    const {output} = await api('state');
    return output?.values?.['head-1']?.pan;
  }
  const visibilityChanged = () => {
    if (document.hidden || !document.hasFocus()) changed.push(Date.now());
  };
  document.addEventListener('visibilitychange', visibilityChanged);
  window.addEventListener('blur', visibilityChanged);
  let releasePlumes;
  try {
    report('installed');
    const currentCanvas = () => {
      const node = document.querySelector('#preview');
      return node?.tolglowPreview ? node : null;
    };
    let canvas = await until('production Room canvas', currentCanvas);
    let preview = canvas.tolglowPreview;
    let startupCount = 0;
    await until('Room first presentation in a key window', () => {
      const t = preview.getTelemetry();
      const r = canvas.getBoundingClientRect();
      const inView = r.top < innerHeight && r.bottom > 0 && r.left < innerWidth && r.right > 0;
      if (startupCount++ % 4 === 0 && startup.length < 40) startup.push({
        ready: preview.ready, visible: t.visible, thread: t.thread, documentFocused: document.hasFocus(),
        documentHidden: document.hidden, inView});
      return !document.hidden && inView && document.hasFocus() && preview.ready && t.visible;
    });
    if (receipt.expanded) {
      const width = canvas.getBoundingClientRect().width;
      const expand = document.querySelector('.preview-expand-control');
      if (!expand) throw new Error('Missing normal Room expansion control');
      if (expand.getAttribute('aria-pressed') !== 'true') expand.click();
      canvas = await until('expanded Room', () => {
        const candidate = currentCanvas();
        return document.querySelector('.preview-expand-control')?.getAttribute('aria-pressed') === 'true' &&
          candidate && candidate.getBoundingClientRect().width > width + 10 ? candidate : null;
      });
      preview = canvas.tolglowPreview;
      canvas.scrollIntoView({behavior:'instant', block:'center', inline:'nearest'});
    }
    report('presenting');
    const selects = {quality: control('Preview quality'), updates: control('Preview fixture updates')};
    selects.quality.value = 'auto';
    selects.updates.value = 'interpolate';
    selects.quality.dispatchEvent(new Event('change', {bubbles:true}));
    selects.updates.dispatchEvent(new Event('change', {bubbles:true}));
    slider('Preview work light', 0);
    const queue = [['fixed-haze', 'ember', .25, false], ['moving-haze', 'sweep', .7, false],
      ['moving-plumes', null, .7, true]];
    for (const [name, id, amount, plumes] of queue) {
      report('prepare-'+name);
      if (id) await cue(id);
      slider('Preview fog density', amount);
      await until('lit Room with requested fog and preview settings', () => {
        const t = preview.getTelemetry();
        receipt.lastSettings = {visible:t.visible, activeLights:t.activeLights, fogAmount:t.fogAmount,
          workLight:t.workLight, lightingMode:t.lightingMode, documentFocused:document.hasFocus(), canvas:[canvas.width,canvas.height]};
        return t.visible && t.activeLights === 8 && t.fogAmount === amount && t.workLight === 0 && t.lightingMode === 'interpolate';
      });
      const counts = {accepted: 0, rejected: 0};
      if (plumes) {
        const addPlume = () => counts[preview.burst({position: [-1,.25,.4], direction: [1,.5,-.25], duration: 1.5, strength: 4})
          ? 'accepted' : 'rejected']++;
        addPlume();
        releasePlumes = setInterval(addPlume, 4000);
      }
      await delay(15000);
      const headBefore = await head();
      counts.accepted = counts.rejected = 0;
      const measured = await measureRoom(20000);
      const headAfter = await head();
      clearInterval(releasePlumes);
      const steadyField = (key) => JSON.stringify(measured.start[key]) === JSON.stringify(measured.end[key]);
      if (!steadyField('camera') || !steadyField('target')) measured.invalid.push('camera changed');
      if (selects.quality.value !== 'auto' || selects.updates.value !== 'interpolate' ||
          !steadyField('fogAmount') || measured.end.fogAmount !== amount ||
          !steadyField('workLight') || measured.end.workLight !== 0)
        measured.invalid.push('Room controls changed');
      if (!measured.end.deviceRenderer?.startsWith('Apple GPU') || measured.end.softwareRenderer ||
          measured.rendererThread !== 'worker') measured.invalid.push('hardware WebKit worker route not established');
      if (name !== 'fixed-haze' && (!Number.isFinite(headBefore) || !Number.isFinite(headAfter) || headBefore === headAfter))
        measured.invalid.push('moving heads not established');
      if (plumes && (counts.accepted < 4 || counts.rejected ||
          !measured.policySamples.some(t => t.fogMaximumDensity > .35))) measured.invalid.push('active plume case not established');
      samples.push({...measured, name, headBefore, headAfter, ...(plumes ? {plumes: counts} : {})});
    }
  } catch (error) {
    receipt.error = String(error);
  } finally {
    clearInterval(releasePlumes);
    document.removeEventListener('visibilitychange', visibilityChanged);
    window.removeEventListener('blur', visibilityChanged);
    report('finished');
  }
})();
