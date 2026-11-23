// Browser boundary for lein run -m tolglow.qa.push-scene.
const assert = require('node:assert/strict');
const {readFileSync, writeFileSync} = require('node:fs');
const {join} = require('node:path');
const {chromium} = require('playwright-core');
const {createWorker} = require('../resources/devices/push-display/worker.cjs');

async function captureShow([url, directory, executablePath]) {
  const frames = JSON.parse(readFileSync(join(directory, 'frames.json')));
  let page;
  const errors = [], requests = [], samples = [];
  const worker = await createWorker({url, executablePath, noSandbox: true, captureTimeoutMs: 10000, startupTimeoutMs: 60000, sceneStartupTimeoutMs: 60000, sceneCaptureTimeoutMs: 60000},
    async options => {
      const browser = await chromium.launch({...options, args: [...options.args, '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
      return {
        close: () => browser.close(),
        async newContext(settings) {
          const context = await browser.newContext(settings);
          context.on('page', opened => {
            page = opened;
            opened.on('pageerror', e => errors.push(e.message));
            opened.on('console', m => {if (m.type() === 'error') errors.push(m.text());});
            opened.on('request', req => requests.push(req.url()));
          });
          return context;
        }
      };
    });
  try {
    console.log('Checking parameters-only startup');
    await worker.capture({'display-mode': 'parameters'});
    assert(!requests.some(x => x.endsWith('/preview.js')), 'unmounted scene fetched renderer');
    for (const [i, frame] of frames.entries()) {
      console.log('Drawing', i, frame['display-mode'], frame.scene.camera);
      const result = await worker.capture(frame);
      const sample = await page.evaluate(() => {
        const canvas = document.querySelector('.push-scene canvas'), e = canvas?.tolglowPreview;
        if (!e?.ready) throw Error(document.querySelector('.push-scene-status')?.textContent || 'no rendered scene');
        window.lastVenue = e;
        return {draws: e.draws, lights: e.lightEntries.length, width: canvas.clientWidth, height: canvas.clientHeight,
                drawingBuffer: [canvas.width, canvas.height], device: e.deviceRenderer, lighting: e.lighting.mode,
                mode: e.snapshot.editor?.tool || 'simulator', camera: e.snapshot.camera};
      });
      assert.equal(sample.lighting, 'hold'); assert.equal(sample.width, 960); assert.equal(sample.height, 160); assert(sample.lights >= 8);
      sample.captureMs = Math.round(result['capture-ms']); samples.push(sample);
      writeFileSync(join(directory, String(i).padStart(2, '0') + '.png'), Buffer.from(result.png, 'base64'));
    }
    await page.waitForTimeout(250);
    assert.equal(await page.evaluate(() => window.lastVenue.draws), samples.at(-1).draws, 'idle panel ran extra frames');
    // Headless Chromium does not background tabs. Drive the browser visibility
    // event while exercising the production renderer, context loss and new canvas.
    await page.evaluate(() => {
      Object.defineProperty(document, 'hidden', {configurable: true, value: true});
      document.dispatchEvent(new Event('visibilitychange'));
    });
    assert(await page.evaluate(() => window.lastVenue.disposed && !document.querySelector('.push-scene canvas')));
    await page.evaluate(() => {
      delete document.hidden;
      document.dispatchEvent(new Event('visibilitychange'));
      return window.tolglowPushDisplay.frameReady();
    });
    const recovered = await worker.capture(frames[0]);
    assert(await page.evaluate(() => {
      const next = document.querySelector('.push-scene canvas')?.tolglowPreview;
      return next && next !== window.lastVenue && next.ready && next.draws > 0 && !next.renderer.getContext().isContextLost();
    }), 'restored display failed to draw on a fresh WebGL surface');
    writeFileSync(join(directory, 'recovered.png'), Buffer.from(recovered.png, 'base64'));
    await page.evaluate(() => { window.lastVenue = document.querySelector('.push-scene canvas').tolglowPreview; });
    await worker.capture({'display-mode': 'parameters'});
    assert(await page.evaluate(() => !document.querySelector('.push-scene canvas') && window.lastVenue.disposed),
      'returning to parameters left the simulator mounted');
    assert.deepEqual(errors, []);
    writeFileSync(join(directory, 'measurements.json'), JSON.stringify(samples, null, 2));
    console.log('Captured', samples.length, 'device frames and verified idle and unmount.');
  } finally {await worker.close();}
}
captureShow(process.argv.slice(2)).catch(error => {console.error(error); process.exitCode = 1;});
