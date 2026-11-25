// Synthetic Room load, evaluated in any page that can fetch a preview bundle.
// It builds a rig of moving heads and diffused pixel strips, feeds committed
// output at a fixed rate and reports accepted presentations with the worker's
// own timings. No show, server or fixture output is involved. The engine is the
// production bundle under `base`; only the worker URL is rebound to a Blob.
//
//   const load = await installRoomLoad({base, heads: 64, quality: 'desktop',
//     displayHz: 120, busy: {work: 22, period: 25}});
//   console.log(await load.measure(8000, 4000)); load.dispose();
//
// Presentation times come from the worker when it owns the canvas, otherwise
// from the page task that showed the bitmap.
// `rafHz` is the page's animation callback rate and `longTasks` its blocked
// time. A hidden or background page throttles both; say so with the numbers.
export async function installRoomLoad({base = '/studio/', heads = 8, pixels = 0, stripLength = 200, feedHz = 40, quality = 'auto', fog = .3, workLight = .12, lightingMode = 'hold', mainThread = false, surface = true, displayHz = null, busy = null, workerPrefix = ''}) {
  window.tolglowRoomLoad?.dispose();
  const [mainSrc, workerSrc] = await Promise.all(['preview.js', 'preview-worker.js'].map(f => fetch(base + f).then(r => { if (!r.ok) throw new Error(f + ' ' + r.status); return r.text(); })));
  const workerUrl = URL.createObjectURL(new Blob([workerPrefix + workerSrc], {type: 'text/javascript'}));
  const needle = 'new URL("./preview-worker.js",import.meta.url)';
  if (!mainSrc.includes(needle)) throw new Error('worker URL pattern not found');
  const moduleUrl = URL.createObjectURL(new Blob([mainSrc.replace(needle, JSON.stringify(workerUrl))], {type: 'text/javascript'}));
  const mod = await import(moduleUrl);
  document.body.innerHTML = '<canvas id="preview" style="position:fixed;inset:0;width:100vw;height:100vh;display:block;background:#000"></canvas><span id="status" style="position:fixed;left:8px;top:8px;color:#8f8;font:12px monospace;z-index:2"></span>';
  document.body.style.margin = '0';
  const canvas = document.querySelector('#preview'), status = document.querySelector('#status');
  const room = {width: 16, height: 6, depth: 12};
  const fixtures = [], trusses = [];
  const rows = Math.max(1, Math.ceil(heads / 8));
  for (let r = 0; r < rows; r++) trusses.push({id: 'truss-' + r, position: [0, 5.2, -4.5 + r * (9 / Math.max(1, rows - 1 || 1))], length: 14, 'rotation-deg': [0, 0, 0]});
  for (let i = 0; i < heads; i++) {
    const r = Math.floor(i / 8), c = i % 8;
    fixtures.push({id: 'head-' + i, 'fixture-type': 'bench-head', position: [-6 + c * (12 / 7), 4.9, trusses[r].position[2]], 'rotation-deg': [0, 0, 0],
      channels: {pan: 1, tilt: 2}, 'beam-angle': 9, 'lens-radius-m': .07});
  }
  const strips = Math.ceil(pixels / stripLength);
  for (let s = 0; s < strips; s++) {
    const z = -5.5 + 11 * (s % 50) / 49, y = .4 + 1.1 * Math.floor(s / 50);
    for (let i = 0; i < stripLength && s * stripLength + i < pixels; i++)
      fixtures.push({id: `px-${s}-${i}`, 'fixture-type': 'bench-pixel', position: [-7 + 14 * i / (stripLength - 1), y, z], 'pixel-layout-id': 'strip-' + s, 'pixel-index': i, 'diffuser-radius-m': .02});
  }
  const rig = {room, fixtures, trusses, surfaces: []};
  const engine = new mod.RoomPreview(canvas, status, mainThread ? {worker: false} : {surface});
  // Optionally pin the reported display rate and occupy the page thread for
  // `busy.work` of every `busy.period` milliseconds, as a loaded Studio would.
  if (displayHz && engine.refresh) {engine.refresh.setActive(false); engine.refresh.setActive = () => {}; engine.displayHz = displayHz;}
  const busyTimer = busy && setInterval(() => {const end = performance.now() + busy.work; while (performance.now() < end);}, busy.period);
  let frame = 0, feedMs = [], feedTimer;
  const t0 = performance.now();
  const feed = () => {
    const started = performance.now(), t = (started - t0) / 1000, values = {};
    for (let i = 0; i < heads; i++) {
      const hue = (i / heads + t * .05) % 1, k = hue * 6, x = 1 - Math.abs(k % 2 - 1);
      const rgb = k < 1 ? [1, x, 0] : k < 2 ? [x, 1, 0] : k < 3 ? [0, 1, x] : k < 4 ? [0, x, 1] : k < 5 ? [x, 0, 1] : [1, 0, x];
      values['head-' + i] = {'emitted-color': rgb, pan: .5 + .12 * Math.sin(t * .7 + i * .4), tilt: .22 + .08 * Math.sin(t * .5 + i * .3), focus: .5};
    }
    for (let s = 0; s < strips; s++) for (let i = 0; i < stripLength && s * stripLength + i < pixels; i++) {
      const v = .5 + .5 * Math.sin(t * 3 + i * .08 + s * .5);
      values[`px-${s}-${i}`] = {'emitted-color': [v, .2 * v, 1 - v]};
    }
    engine.update({'runtime-id': 'bench', 'show-id': 'bench', epoch: 1, revision: 1, 'frame-number': ++frame, 'time-ms': Math.round(started - t0), master: 1, 'blackout?': false, rig, output: {values}});
    feedMs.push(performance.now() - started);
  };
  canvas.dispatchEvent(new CustomEvent('tolglow-preview-external', {detail: {owner: 'bench', active: true}}));
  engine.setQuality(quality); engine.setFog(fog); engine.setWorkLight(workLight); engine.setLightingMode(lightingMode);
  feed(); feedTimer = setInterval(feed, 1000 / feedHz);
  const q = (xs, p) => { if (!xs.length) return null; const s = [...xs].sort((a, b) => a - b); return +s[Math.min(s.length - 1, Math.ceil(s.length * p) - 1)].toFixed(2); };
  const load = window.tolglowRoomLoad = {
    engine, mod,
    dispose() { clearInterval(feedTimer); clearInterval(busyTimer); engine.dispose(); URL.revokeObjectURL(moduleUrl); URL.revokeObjectURL(workerUrl); },
    async measure(ms = 8000, warm = 4000) {
      await new Promise(r => setTimeout(r, warm));
      const frames = [], rafs = [], tele = [], long = [];
      const worker = engine.worker;
      const onMsg = e => { if (e.data?.type === 'frame' && e.data.presented !== false) { frames.push(e.data.telemetry.presentedAtMs ?? performance.timeOrigin + performance.now()); const t = e.data.telemetry; tele.push([t.workerFrameMs, t.gpuCompletionWaitMs, t.frameCPUms, t.bitmapTransferMs, t.rafDeliveryMs, t.readyForFrameMs]); } };
      let orig;
      if (worker) worker.addEventListener('message', onMsg);
      else { const r = engine.fallback || engine; orig = r.draw; r.draw = function (...a) { const b = this.draws, s = performance.now(), out = orig.apply(this, a); if (this.draws > b) { frames.push(performance.now()); tele.push([null, null, performance.now() - s, null, null, null]); } return out; }; }
      let raf, last = performance.now(); const tick = t => { rafs.push(t); raf = requestAnimationFrame(tick); };
      raf = requestAnimationFrame(tick);
      let po; try { po = new PerformanceObserver(l => { for (const e of l.getEntries()) long.push(e.duration); }); po.observe({entryTypes: ['longtask']}); } catch {}
      feedMs = []; const f0 = frame, start = performance.now();
      await new Promise(r => setTimeout(r, ms));
      const elapsed = performance.now() - start;
      cancelAnimationFrame(raf); po?.disconnect();
      if (worker) worker.removeEventListener('message', onMsg); else (engine.fallback || engine).draw = orig;
      const gaps = xs => xs.slice(1).map((t, i) => t - xs[i]);
      const col = i => tele.map(r => r[i]).filter(Number.isFinite);
      const t = engine.getTelemetry();
      return {heads, pixels, quality, fogAmount: fog, lightingMode, elapsedMs: Math.round(elapsed), focused: document.hasFocus(), hidden: document.hidden,
        presentedHz: +(frames.length * 1000 / elapsed).toFixed(2), frameGapP50: q(gaps(frames), .5), frameGapP95: q(gaps(frames), .95),
        rafHz: +(rafs.length * 1000 / elapsed).toFixed(2), fedHz: +((frame - f0) * 1000 / elapsed).toFixed(2),
        mainUpdateMsP50: q(feedMs, .5), mainUpdateMsP95: q(feedMs, .95), longTasks: long.length, longTaskMsTotal: Math.round(long.reduce((a, b) => a + b, 0)),
        workerFrameMs: [q(col(0), .5), q(col(0), .95)], gpuFenceWaitMs: [q(col(1), .5), q(col(1), .95)], cpuSubmitMs: [q(col(2), .5), q(col(2), .95)],
        bitmapMs: [q(col(3), .5), q(col(3), .95)], rafDeliveryMs: [q(col(4), .5), q(col(4), .95)], readyForFrameMs: [q(col(5), .5), q(col(5), .95)],
        thread: t.thread, presentation: t.presentation ?? 'proxy', tier: t.quality, scale: t.renderScale, scene: t.pixels, display: t.displayPixels, targetHz: t.targetRefreshHz, displayHz: t.displayHz,
        deviceRenderer: t.deviceRenderer, software: t.softwareRenderer, samples: t.sceneSamples, shadows: t.shadowCount, activeLights: t.activeLights, visibleFixtures: t.visibleFixtures,
        fog: t.fog && {quality: t.fog.quality, target: t.fog.targetSize, steps: t.fog.raySteps, cpuUpdateMs: +t.fog.cpuUpdateMs?.toFixed(2), uploads: t.fog.uploads},
        coalescedStates: t.coalescedStates, discardedFrames: t.discardedFrames, failed: t.failed, fallbackReason: t.fallbackReason, css: [innerWidth, innerHeight], dpr: devicePixelRatio};
    }
  };
  return load;
}
