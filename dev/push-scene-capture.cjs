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
  const worker = await createWorker({url, executablePath, noSandbox: true, startupTimeoutMs: 60000, captureTimeoutMs: 60000},
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
        return {draws: e.draws, lights: e.lightEntries.length, width: canvas.width, height: canvas.height,
                mode: e.snapshot.editor?.tool || 'simulator', camera: e.snapshot.camera};
      });
      assert.equal(sample.width, 960); assert.equal(sample.height, 160); assert(sample.lights >= 8);
      sample.captureMs = Math.round(result['capture-ms']); samples.push(sample);
      writeFileSync(join(directory, String(i).padStart(2, '0') + '.png'), Buffer.from(result.png, 'base64'));
    }
    await page.waitForTimeout(250);
    assert.equal(await page.evaluate(() => window.lastVenue.draws), samples.at(-1).draws, 'idle panel ran extra frames');
    await worker.capture({'display-mode': 'parameters'});
    assert(await page.evaluate(() => !document.querySelector('.push-scene canvas') && window.lastVenue.disposed),
      'returning to parameters left the simulator mounted');
    assert.deepEqual(errors, []);
    writeFileSync(join(directory, 'measurements.json'), JSON.stringify(samples, null, 2));
    console.log('Captured', samples.length, 'device frames and verified idle and unmount.');
  } finally {await worker.close();}
}
captureShow(process.argv.slice(2)).catch(error => {console.error(error); process.exitCode = 1;});
