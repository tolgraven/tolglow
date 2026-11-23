// Real Studio scene, Ring/Transit stream and show graph. An independent
// test-only pixel read checks what the host received against the presented frame.
import assert from 'node:assert/strict';
import {execFileSync, spawn} from 'node:child_process';
import {mkdir, writeFile} from 'node:fs/promises';
import {createInterface} from 'node:readline';
import {setTimeout as delay} from 'node:timers/promises';
import {chromium} from 'playwright-core';

const target = process.argv[2] || '/tmp/tolglow-browser-observations';
await mkdir(target, {recursive: true});
const classpath = execFileSync('lein', ['classpath'], {encoding:'utf8'}).trim();
const host = spawn('java', ['-cp',classpath,'clojure.main','-m','tolglow.qa.browser-observations'], {stdio:['pipe','pipe','inherit']});
const output = createInterface({input: host.stdout})[Symbol.asyncIterator]();
async function response(command) {
  if (command) host.stdin.write(command + '\n');
  for (;;) {
    const {done, value} = await output.next();
    if (done) throw new Error('Observation host exited');
    if (value.startsWith('OBSERVATION-QA ')) return JSON.parse(value.slice('OBSERVATION-QA '.length));
  }
}
function trackReadbacks(mode) {
  if (mode === 'main') Object.defineProperty(window, 'OffscreenCanvas', {value:undefined});
  const bitmap = window.createImageBitmap.bind(window);
  window.observedPixels = [];
  window.createImageBitmap = (canvas, ...region) => {
    if (canvas?.id === 'preview' && region[2] === 1 && region[3] === 1) {
      const sample = document.createElement('canvas');
      sample.width = sample.height = 1;
      const ctx = sample.getContext('2d');
      ctx.drawImage(canvas, ...region, 0, 0, 1, 1);
      window.observedPixels.push(Array.from(ctx.getImageData(0,0,1,1).data).slice(0,3));
    }
    return bitmap(canvas, ...region);
  };
}
async function latest() {
  for (let attempt = 0; attempt < 80; attempt++) {
    const {sample} = await response('read');
    if (sample) return sample;
    await delay(100);
  }
  throw new Error('Host graph returned no completed observation');
}
async function captureCamera(page, canvas, mode, camera) {
  await page.getByLabel('Preview camera', {exact:true}).selectOption(camera);
  const initial = await page.evaluate(() => window.observedPixels.length);
  await page.waitForFunction(n => window.observedPixels.length >= n + 2, initial);
  const sample = await latest();
  const pixels = await page.evaluate(() => window.observedPixels);
  assert.ok(pixels.some(p => p.every((v,i) => v === sample.value[i])), 'Host RGB differs from presented image');
  await canvas.screenshot({path:target + '/' + mode + '-' + camera + '.png'});
  return {camera,sample,reads:pixels.length};
}
const browser = await chromium.launch({executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args:['--no-sandbox','--use-angle=swiftshader','--enable-unsafe-swiftshader']});
try {
  const {port} = await response();
  const report = [];
  for (const mode of ['worker', 'main']) {
    const context = await browser.newContext({viewport:{width:1120,height:820},deviceScaleFactor:1});
    const page = await context.newPage(), faults = [];
    page.setDefaultTimeout(90000);
    page.on('pageerror', error => faults.push(error.message));
    await page.addInitScript(trackReadbacks, mode);
    await page.goto('http://127.0.0.1:' + port, {waitUntil:'domcontentloaded'});
    await page.locator('[data-view="perform"]').click();
    const canvas = page.locator('#preview');
    await canvas.scrollIntoViewIfNeeded();
    await page.waitForFunction(() => document.querySelector('#preview')?.tolglowPreview?.ready);
    assert.equal(await canvas.evaluate(c => c.tolglowPreview.worker ? 'worker' : 'main'), mode);
    assert.equal(await page.evaluate(() => window.observedPixels.length), 0);
    await response('acquire');
    await page.waitForFunction(() => window.observedPixels.length > 0);
    const measurements = [];
    for (const camera of ['audience','overview']) measurements.push(await captureCamera(page,canvas,mode,camera));
    await response('release');
    await page.waitForFunction(() => !document.querySelector('#preview').tolglowPreview.observedFrame);
    const released = await page.evaluate(() => window.observedPixels.length);
    await delay(600);
    assert.equal(await page.evaluate(() => window.observedPixels.length), released);
    assert.deepEqual(faults, []);
    report.push({renderer:mode,measurements,readsBeforeDemand:0,readsAfterRelease:0});
    await context.close();
  }
  await writeFile(target + '/observations.json', JSON.stringify(report,null,2)+'\n');
  console.log(JSON.stringify(report));
} finally {
  host.stdin.end('quit\n');
  await browser.close();
}
