import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
import {setTimeout as delay} from 'node:timers/promises';
import {chromium} from 'playwright-core';

const [origin, directory] = process.argv.slice(2);
await mkdir(directory, {recursive: true});
const browser = await chromium.launch({executablePath: process.env.CHROMIUM_PATH || chromium.executablePath(),
  args: ['--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
try {
  const page = await browser.newPage({viewport: {width: 1024, height: 820}, deviceScaleFactor: 1});
  await page.addInitScript(() => {
    window.acceptedBitmaps = [];
    const proto = ImageBitmapRenderingContext.prototype;
    const transfer = proto.transferFromImageBitmap;
    proto.transferFromImageBitmap = function(bitmap) {
      const result = transfer.call(this, bitmap);
      if (this.canvas.id === 'preview' && bitmap) window.acceptedBitmaps.push(performance.now());
      return result;
    };
  });
  await page.goto(origin, {waitUntil: 'domcontentloaded'});
  await page.waitForFunction(() => document.querySelector('#connection-label')?.textContent === 'Studio connected');
  const panel = page.locator('#preview-panel');
  await panel.scrollIntoViewIfNeeded();
  await page.waitForFunction(() => /^Show compute · [0-9.]+ ms$/.test(document.querySelector('#frame-time')?.textContent));
  await page.waitForFunction(() => /Canvas · [0-9.]+ fps/.test(document.querySelector('#preview-panel')?.innerText), null, {timeout: 90000});
  const rate = await page.evaluate(() => {
    const times = window.acceptedBitmaps;
    const text = document.querySelector('#preview-panel').innerText;
    const reported = Number(text.match(/Canvas · ([0-9.]+) fps/)[1]);
    // Every displayed sample covers consecutive accepted images for at least one second.
    let closest = Infinity;
    for (let end = 1; end < times.length; ++end) {
      for (let start = end - 1; start >= 0; --start) {
        const interval = times[end] - times[start];
        if (interval >= 998) {
          closest = Math.min(closest, Math.abs(reported - (end - start) * 1000 / interval));
          break;
        }
      }
    }
    const telemetry = document.querySelector('#preview').tolglowPreview.getTelemetry();
    return {reported, closest, accepted:times.length, thread:telemetry.thread,
      backing:telemetry.displayPixels, scene:telemetry.pixels, device:telemetry.deviceRenderer};
  });
  assert.equal(rate.thread, 'worker');
  assert.ok(Number.isFinite(rate.closest) && rate.closest < Math.max(.2, rate.reported * .05),
    'Visible cadence must agree with an independently observed stream of accepted bitmaps: ' + JSON.stringify(rate));
  for (let frame = 1; frame <= 3; ++frame) {
    await panel.screenshot({path:directory + '/room-' + frame + '.png'});
    await delay(1250);
  }
  await writeFile(directory + '/readout.json', JSON.stringify(rate, null, 2));
  console.log(rate);
} finally {await browser.close();}
