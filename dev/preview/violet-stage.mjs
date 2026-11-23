// Run after: lein run -m tolglow.qa.violet-stage <dir> && npm run preview:build
// node dev/preview/violet-stage.mjs <dir> [frame-count]
import assert from 'node:assert/strict';
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
const browser = await chromium.launch({headless: true, executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
const errors = [], samples = [];
try {
  const page = await browser.newPage({viewport, deviceScaleFactor: 1, reducedMotion: 'reduce'});
  page.on('pageerror', error => errors.push(error.message));
  page.on('console', message => { if (message.type() === 'error') errors.push(message.text()); });
  await page.route('http://preview.test/scene', r => r.fulfill({contentType: 'text/html', body: ''}));
  await page.route('http://preview.test/preview.js', r => r.fulfill({contentType: 'text/javascript', body: bundle}));
  await page.goto('http://preview.test/scene');
  await page.evaluate(async snapshot => {
    const {SceneRenderer} = await import('/preview.js');
    document.documentElement.style.margin = document.body.style.margin = '0';
    const canvas = document.createElement('canvas');
    canvas.style.cssText = 'display:block;width:100vw;height:100vh';
    document.body.append(canvas);
    const engine = new SceneRenderer(canvas, document.createElement('output'));
    window.referenceStage = engine;
    await engine.whenReady;
    if (!engine.ready) throw Error(engine.error || 'WebGL2 scene unavailable');
    engine.pause();
    engine.setQuality('desktop');
    engine.setWorkLight(0);
    engine.controls.enableDamping = false;
    engine.update(snapshot);
  }, snapshot);
  const distinctFrames = new Set();
  for (let i = 0; i < count; i++) {
    const {sample, png} = await page.evaluate(({i, count}) => {
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
      // Read in the draw task, before Chromium clears the non-preserved WebGL buffer.
      const png = scene.canvas.toDataURL('image/png').split(',')[1];
      const t = scene.getTelemetry();
      const sample = {frame: i, camera: t.camera, target: t.target, renderer: t.renderer, gpu: t.deviceRenderer,
        workLight: t.workLight, completedDrawMs, edgeFilter: t.edgeFilter, shadows: t.shadowCount,
        scene: t.pixels, display: t.displayPixels, volume: t.fog.targetSize, raySteps: t.fog.raySteps,
        tier: t.quality, sceneSamples: t.sceneSamples, displayRange: t.displayRange, film: t.film,
        emitted: t.activeLights, frameNumber: t.frameNumber, atmosphere: t.fog.atmosphere};
      return {sample, png};
    }, {i, count});
    assert.deepEqual(sample.scene, [viewport.width, viewport.height]);
    assert.deepEqual(sample.display, sample.scene);
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
  const report = {browser: browser.version(), bundleSha256: createHash('sha256').update(bundle).digest('hex'),
    inputSha256: createHash('sha256').update(input).digest('hex'), viewport, samples};
  await writeFile(join(dir, 'render.json'), JSON.stringify(report, null, 2) + '\n');
  await page.evaluate(() => window.referenceStage.dispose());
} finally { await browser.close(); }
