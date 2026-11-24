// Run from the qualified checkout with show-frame.json already in <dir>.
// The shared Chrome development session stays open. Only this tab is closed.
import assert from 'node:assert/strict';
import {execFileSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {mkdir, readFile, writeFile} from 'node:fs/promises';
import {resolve, join} from 'node:path';
import {chromium} from 'playwright-core';

const dir = resolve(process.argv[2] || 'target/violet-stage');
const count = Number(process.argv[3] || 33);
assert.ok(Number.isInteger(count) && count >= 3 && count <= 120);
const input = await readFile(join(dir, 'show-frame.json'));
const snapshot = JSON.parse(input);
const bundle = await readFile('resources/studio/preview.js');
const viewport = {width: 960, height: 540};
await mkdir(join(dir, 'orbit'), {recursive: true});
const browser = await chromium.connectOverCDP('http://127.0.0.1:9223');
const errors = [], samples = [];
let session, targetId, page;
try {
  const context = browser.contexts()[0];
  session = await browser.newBrowserCDPSession();
  const opened = context.waitForEvent('page');
  ({targetId} = await session.send('Target.createTarget', {url: 'about:blank', background: true}));
  page = await opened;
  const ownPage = await context.newCDPSession(page);
  assert.equal((await ownPage.send('Target.getTargetInfo')).targetInfo.targetId, targetId);
  page.on('pageerror', error => errors.push(error.message));
  page.on('console', message => { if (message.type() === 'error') errors.push(message.text()); });
  await page.route('http://preview.test/scene', r => r.fulfill({contentType: 'text/html', body: '<title>Tolglow violet-stage capture</title>'}));
  await page.route('http://preview.test/preview.js', r => r.fulfill({contentType: 'text/javascript', body: bundle}));
  await page.goto('http://preview.test/scene');
  await page.evaluate(async ({snapshot, viewport}) => {
    const {SceneRenderer} = await import('/preview.js');
    const canvas = new OffscreenCanvas(viewport.width, viewport.height);
    const engine = new SceneRenderer(canvas, null, {worker: true, autoStart: false,
      reducedMotion: true, viewport: {...viewport, pixelRatio: 1}, displayHz: 60});
    window.referenceStage = engine;
    if (engine.error || !engine.renderer) throw Error(engine.error || 'WebGL2 scene unavailable');
    engine.setQuality('desktop');
    engine.setWorkLight(0);
    engine.update(snapshot);
    engine.resize();
  }, {snapshot, viewport});
  const distinctFrames = new Set();
  for (let i = 0; i < count; i++) {
    const {sample, png} = await page.evaluate(async ({i, count}) => {
      const scene = window.referenceStage;
      const progress = i / count;
      const angle = -.10 + .25 * Math.sin(Math.PI * 2 * progress);
      const center = [0, 1.62, -2.05], distance = 6.34;
      scene.controls.target.fromArray(center);
      scene.camera.position.set(center[0] + distance * Math.sin(angle), 1.66 + .10 * Math.cos(2 * Math.PI * progress),
                                center[2] + distance * Math.cos(angle));
      scene.controls.update();
      const started = performance.now();
      scene.draw(0);
      scene.renderer.getContext().finish();
      const completedDrawMs = performance.now() - started;
      const blob = await scene.canvas.convertToBlob({type: 'image/png'});
      const bytes = new Uint8Array(await blob.arrayBuffer());
      const png = bytes.toBase64();
      const t = scene.getTelemetry();
      if (!t.ready || t.failed) throw Error(t.failed || 'Studio did not draw a complete frame');
      const sample = {frame: i, camera: t.camera, target: t.target, renderer: t.renderer, gpu: t.deviceRenderer,
        workLight: t.workLight, completedDrawMs, edgeFilter: t.edgeFilter, shadows: t.shadowCount,
        scene: t.pixels, display: t.displayPixels, volume: t.fog.targetSize, raySteps: t.fog.raySteps,
        tier: t.quality, sceneSamples: t.sceneSamples, displayRange: t.displayRange, film: t.film,
        emitted: t.activeLights, frameNumber: t.frameNumber, atmosphere: t.fog.atmosphere};
      return {sample, png};
    }, {i, count});
    assert.deepEqual(sample.scene, [viewport.width, viewport.height]);
    assert.deepEqual(sample.display, sample.scene);
    assert.match(sample.gpu, /ANGLE Metal Renderer: Apple M4/);
    assert.equal(sample.tier, 'desktop');
    assert.equal(sample.workLight, 0);
    assert.equal(sample.emitted, 7);
    assert.equal(sample.frameNumber, snapshot['frame-number']);
    const image = Buffer.from(png, 'base64');
    sample.pngSha256 = createHash('sha256').update(image).digest('hex');
    distinctFrames.add(sample.pngSha256);
    await writeFile(join(dir, 'orbit', String(i).padStart(3, '0') + '.png'), image);
    if (i === 0) console.log(JSON.stringify(sample));
    samples.push(sample);
  }
  assert.equal(distinctFrames.size, count, 'Orbit must contain a different raster at each camera position');
  assert.deepEqual(errors, []);
  const report = {browser: browser.version(), revision: execFileSync('git', ['rev-parse', 'HEAD'], {encoding: 'utf8'}).trim(),
    mode: 'manually sampled OffscreenCanvas scene', bundleSha256: createHash('sha256').update(bundle).digest('hex'),
    inputSha256: createHash('sha256').update(input).digest('hex'), viewport, samples};
  await writeFile(join(dir, 'render.json'), JSON.stringify(report, null, 2) + '\n');
} finally {
  try {
    if (page) await page.evaluate(() => window.referenceStage?.dispose()).catch(() => {});
    if (targetId) await session.send('Target.closeTarget', {targetId});
  } finally { await browser.close(); }
}
