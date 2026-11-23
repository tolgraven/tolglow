// Opens an owned Chromium window and serves one captured show frame on loopback.
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {readFile, writeFile} from 'node:fs/promises';
import {createServer} from 'node:http';
import {build} from 'esbuild';
import {chromium} from 'playwright-core';
import {bloomDisplayShaderSources} from './bloom-display-experiment.mjs';

const [capture, configPath, output] = process.argv.slice(2);
assert.ok(capture && configPath && output,
  'Usage: node dev/preview/measure-bloom-display.mjs show-frame.json dimensions.json receipt.json');
const input = await readFile(capture);
const config = JSON.parse(await readFile(configPath, 'utf8'));
const smoke = process.env.BLOOM_DISPLAY_SMOKE === '1';
assert.ok(smoke || process.platform === 'darwin', 'Physical qualification requires an unlocked Mac.');
const warmupMs = smoke ? 1000 : 20_000;
const sampleMs = smoke ? 5000 : 20_000;
for (const key of ['css', 'scene', 'fog', 'display']) {
  assert.ok(config[key]?.length === 2 && config[key].every(x => Number.isInteger(x) && x > 0), key);
}
assert.ok(['low', 'mobile', 'desktop', 'high'].includes(config.quality), 'Use a fixed quality.');
assert.ok(config.scale > 0 && config.scale <= 1, 'Use a fixed scale in (0, 1].');

const bundle = await build({entryPoints: {room: 'js/preview/index.js', 'preview-worker': 'dev/preview/bloom-display-worker.mjs'},
  bundle: true, format: 'esm', platform: 'browser', target: 'es2022', outdir: '/virtual', write: false, minify: true});
const assets = new Map(bundle.outputFiles.map(file => [file.path.split('/').at(-1), file.contents]));
const server = createServer((request, response) => {
  const asset = assets.get(request.url.slice(1));
  if (asset) response.writeHead(200, {'Content-Type': 'text/javascript'}).end(asset);
  else if (request.url === '/') response.writeHead(200, {'Content-Type': 'text/html'}).end(
    '<!doctype html><title>Room bloom A/B</title><style>body{margin:0;background:#000}canvas{display:block}</style><canvas id="preview"></canvas>');
  else response.writeHead(404).end();
});
await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));

const hash = bytes => createHash('sha256').update(bytes).digest('hex');
const summary = values => {
  const sorted = values.filter(Number.isFinite).sort((a, b) => a - b);
  const at = p => sorted[Math.ceil(p * sorted.length) - 1] ?? null;
  return {p50: at(.5), p95: at(.95), p99: at(.99), max: sorted.at(-1) ?? null};
};
function cadence(times, ms) {
  return {frames: times.length, hz: 1000 * times.length / ms, intervalMs: summary(times.slice(1).map((t, i) => t - times[i]))};
}

let browser;
try {
  const [cssWidth, cssHeight] = config.css;
  browser = await chromium.launch({headless: smoke, executablePath: process.env.CHROMIUM_PATH || undefined,
    args: smoke ? ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'] :
      [`--window-size=${cssWidth + 24},${cssHeight + 140}`]});
  const page = await browser.newPage({viewport: smoke ? {width: 400, height: 300} : null, reducedMotion: 'reduce'});
  await page.goto(`http://127.0.0.1:${server.address().port}/`);
  await page.bringToFront();
  const peerReady = page.waitForEvent('worker', {timeout: 90_000});
  await page.evaluate(async ({config, snapshot}) => {
    const {RoomPreview} = await import('/room.js');
    const canvas = document.querySelector('#preview');
    [canvas.style.width, canvas.style.height] = config.css.map(n => `${n}px`);
    if (innerWidth < config.css[0] || innerHeight < config.css[1])
      throw new Error('The entire fixed-size canvas must fit the foreground window.');
    const engine = new RoomPreview(canvas, null, {startupTimeout: 120_000});
    engine.setQuality(config.quality);
    engine.setLightingMode('hold');
    engine.setWorkLight(0);
    engine.update(snapshot);
    engine.controls.enableDamping = false;
    engine.camera.position.set(-0.63, 1.76, 4.26);
    engine.controls.target.set(0, 1.62, -2.05);
    engine.controls.update();
  }, {config, snapshot: JSON.parse(input)});
  const peer = await peerReady;
  const samples = [];
  let settings;
  for (const fused of [false, true, true, false]) {
    await peer.evaluate(({fused, scale}) => self.setBloomDisplayTrial(fused, scale), {fused, scale: config.scale});
    await page.waitForFunction(({fused, scale}) => {
      const t = document.querySelector('#preview').tolglowPreview?.getTelemetry();
      return t?.ready && t.bloomExperiment?.fused === fused && t.bloomExperiment.scale === scale;
    }, {fused, scale: config.scale}, {timeout: 120_000});
    await page.waitForTimeout(warmupMs);
    const sample = await page.evaluate(async ({sampleMs, fused, config}) => {
      const canvas = document.querySelector('#preview');
      const engine = canvas.tolglowPreview;
      const accepted = [];
      const raf = [];
      const settings = new Set();
      let request;
      let invalid;
      const verifyWindow = () => {
        if (document.hidden || !document.hasFocus()) invalid = 'Window lost foreground focus.';
        if (innerWidth < config.css[0] || innerHeight < config.css[1]) invalid = 'Canvas was clipped by the window.';
      };
      verifyWindow();
      const start = performance.now();
      const collect = ({data}) => {
        const t = data?.telemetry;
        // The proxy updates this reference only after it presents the bitmap.
        if (data?.type !== 'frame' || t !== engine.telemetry) return;
        if (t.bloomExperiment.fused !== fused) invalid = 'Trial mode changed.';
        const dims = [t.pixels, t.fog.targetSize, t.displayPixels];
        if (JSON.stringify(dims) !== JSON.stringify([config.scene, config.fog, config.display]))
          invalid = `Unexpected scene/fog/display sizes ${JSON.stringify(dims)}.`;
        settings.add(JSON.stringify({dims, dpr: devicePixelRatio, tier: t.quality, scale: t.renderScale, samples: t.sceneSamples,
          displayRange: t.displayRange, camera: t.camera, target: t.target, film: t.film, shadowCount: t.shadowCount,
          lights: t.lightIds, activeLights: t.activeLights, workLight: t.workLight, fogAmount: t.fogAmount,
          revision: t.revision, snapshotFrame: t.frameNumber, backend: t.deviceRenderer, software: t.softwareRenderer}));
        accepted.push({at: performance.now() - start, cpuSubmitMs: t.frameCPUms, fenceWaitMs: t.gpuCompletionWaitMs,
          bitmapTransferMs: t.bitmapTransferMs, workerFrameMs: t.workerFrameMs});
      };
      function observe(now) {raf.push(now - start); request = requestAnimationFrame(observe);}
      const worker = engine.worker;
      worker.addEventListener('message', collect);
      window.addEventListener('blur', verifyWindow);
      window.addEventListener('resize', verifyWindow);
      document.addEventListener('visibilitychange', verifyWindow);
      request = requestAnimationFrame(observe);
      try { await new Promise(resolve => setTimeout(resolve, sampleMs)); }
      finally {
        worker.removeEventListener('message', collect);
        window.removeEventListener('blur', verifyWindow);
        window.removeEventListener('resize', verifyWindow);
        document.removeEventListener('visibilitychange', verifyWindow);
        cancelAnimationFrame(request);
      }
      verifyWindow();
      const end = engine.getTelemetry();
      if (invalid || settings.size !== 1 || accepted.length < 2 || end.thread !== 'worker' ||
          !end.ready || end.failed || end.lost || !end.visible)
        throw new Error(invalid || 'Worker failed, settings drifted or too few bitmaps were accepted.');
      return {accepted, raf, durationMs: performance.now() - start, settings: JSON.parse([...settings][0])};
    }, {config, sampleMs, fused});
    settings ??= sample.settings;
    assert.deepEqual(sample.settings, settings, 'Render controls drifted across trials.');
    assert.equal(settings.tier, config.quality);
    assert.equal(settings.scale, config.scale);
    assert.ok(smoke || (!settings.software && /Apple|Metal/i.test(settings.backend)), settings.backend);
    const presented = cadence(sample.accepted.map(frame => frame.at), sample.durationMs);
    const windowRaf = cadence(sample.raf, sample.durationMs);
    const timings = Object.fromEntries(['cpuSubmitMs', 'fenceWaitMs', 'workerFrameMs', 'bitmapTransferMs']
      .map(key => [key, summary(sample.accepted.map(frame => frame[key]))]));
    samples.push({fused, presented, windowRaf, timings, ...sample});
    console.log(JSON.stringify({fused, presented, windowRaf, timings}));
  }
  const displayShaderSha256 = Object.fromEntries(Object.entries(bloomDisplayShaderSources())
    .map(([mode, sources]) => [mode, hash(JSON.stringify(sources))]));
  await writeFile(output, JSON.stringify({scope: smoke ? 'software smoke' : 'headed Mac worker replay',
    chromium: browser.version(), inputSha256: hash(input), roomSha256: hash(assets.get('room.js')),
    workerSha256: hash(assets.get('preview-worker.js')), displayShaderSha256, config, warmupMs, sampleMs, samples}, null, 2) + '\n');
} finally {
  try {await browser?.close();} finally {await new Promise(resolve => server.close(resolve));}
}
