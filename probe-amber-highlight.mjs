// Offline material ablation using the same camera samples as capture.mjs.
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
await mkdir(dir, {recursive: true});
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
  for (let i = 0; i < count; i++) {
    const sample = await page.evaluate(({i, count}) => {
      const scene = window.referenceStage;
      const progress = i / count;
      const angle = -.10 + .25 * Math.sin(Math.PI * 2 * progress);
      const center = [0, 1.62, -2.05], distance = 6.34;
      scene.controls.target.fromArray(center);
      scene.camera.position.set(center[0] + distance * Math.sin(angle), 1.66 + .10 * Math.cos(2 * Math.PI * progress),
                                center[2] + distance * Math.cos(angle));
      scene.controls.update();
      if (!window.lipProbe) {
        const trims = [];
        scene.scene.traverse(object => {
          if (object.isMesh && Math.abs(object.position.y - .435) < 1e-8 &&
              Math.abs(object.position.z + 1.746) < 1e-8) trims.push(object);
        });
        if (trims.length !== 1) throw Error('Expected one stage-edge trim mesh');
        const [trim] = trims, installed = trim.material;
        if (installed !== scene.materials.stage) throw Error('Expected the installed painted stage material');
        window.lipProbe = {trim, installed};
      }
      const {trim, installed} = window.lipProbe;
      const x = 330, top = 355, width = 265, height = 56, y = 540 - top - height;
      const read = () => {
        const pixels = new Uint16Array(width * height * 4);
        scene.renderer.readRenderTargetPixels(scene.sceneTarget, x, y, width, height, pixels);
        const values = new Float16Array(pixels.buffer);
        let maximum = 0, total = 0, largestAt = null, lit = 0;
        for (let p = 0; p < width * height; p++) {
          const [r,g,b] = values.subarray(p*4,p*4+3);
          if (r > .01 && g > r*.008 && g < r*.75 && b < g*.65) {
            lit++;
            total += r;
            if (r > maximum) {
              maximum = r;
              largestAt = [x + p % width, top + height - 1 - Math.floor(p/width)];
            }
          }
        }
        return {maximum, total, lit, largestAt};
      };
      const readings = {};
      const variants = [['installed',installed,null],['original-polished-metal',scene.materials.metal,null],
        ['original-with-zero-fog',scene.materials.metal,0]];
      for (const [kind, material, fog] of variants) {
        trim.material = material;
        scene.setFog(fog);
        scene.draw(0);
        scene.renderer.getContext().finish();
        readings[kind] = read();
      }
      trim.material = installed;
      return {frame:i, camera:scene.getTelemetry().camera, gpu:scene.getTelemetry().deviceRenderer,
        trimPosition:trim.position.toArray(), readings};
    }, {i, count});
    assert.match(sample.gpu, /ANGLE Metal Renderer: Apple M4/);
    if (i === 0) console.log(JSON.stringify(sample));
    samples.push(sample);
  }
  assert.deepEqual(errors, []);
  const report = {browser: browser.version(), revision: execFileSync('git', ['rev-parse', 'HEAD'], {encoding: 'utf8'}).trim(),
    mode: 'manually sampled OffscreenCanvas scene', bundleSha256: createHash('sha256').update(bundle).digest('hex'),
    inputSha256: createHash('sha256').update(input).digest('hex'), viewport, samples};
  await writeFile(join(dir, 'amber-highlight-probe.json'), JSON.stringify(report, null, 2) + '\n');
} finally {
  try {
    if (page) await page.evaluate(() => {
      window.referenceStage?.dispose();
    }).catch(() => {});
    if (targetId) await session.send('Target.closeTarget', {targetId});
  } finally { await browser.close(); }
}
