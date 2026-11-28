// Room full-light budget in the optimized Studio (#782), against a disposable
// preview show with 20 heads. Arguments: the Ring origin and a capture directory.
// Choosing a budget in the Room settings changes the Room engine's full lights;
// a reload restores this browser's choice, and Auto quality caps it by tier. Set CHROMIUM_ARGS to choose the GPU
// backend; the default is software WebGL. Headless Chrome is not iPad Safari.
import assert from 'node:assert/strict';
import {mkdir, rm, writeFile} from 'node:fs/promises';
import {setTimeout as delay} from 'node:timers/promises';
import {chromium} from 'playwright-core';

const [origin, directory] = process.argv.slice(2);
assert.ok(origin && directory, 'Specify the preview origin and a capture directory');
await mkdir(directory, {recursive: true});

const response = await fetch(origin + '/api/command', {method: 'POST',
  headers: {accept: 'application/json', 'content-type': 'application/json'},
  body: JSON.stringify({op: 'cue/launch', 'cue-id': 'ember', launch: {mode: 'persistent'}})});
const launched = await response.json();
assert.ok(response.ok && launched.status !== 'rejected', JSON.stringify(launched));

const profile = directory + '/chrome-profile';
const context = await chromium.launchPersistentContext(profile, {
  executablePath: process.env.CHROMIUM_PATH || chromium.executablePath(), headless: false,
  viewport: {width: 1280, height: 900}, deviceScaleFactor: 1,
  args: ['--headless=new', ...(process.env.CHROMIUM_ARGS || '--use-angle=swiftshader --enable-unsafe-swiftshader').split(' ')]});
const report = {origin, renderer: null, steps: []};
try {
  const page = context.pages()[0] || await context.newPage();
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  const budget = page.locator('select[aria-label="Room full lights"]');
  const quality = page.locator('select[aria-label="Preview quality"]');
  const open = async () => {
    await page.goto(origin, {waitUntil: 'domcontentloaded'});
    await page.waitForFunction(() => document.querySelector('#connection-label')?.textContent === 'Studio connected');
    await page.locator('#preview-panel').scrollIntoViewIfNeeded();
    await page.waitForFunction(() => /Canvas · [0-9.]+ fps/.test(document.querySelector('#preview-panel')?.innerText), null, {timeout: 90000});
    await budget.waitFor();
  };
  const lod = () => page.evaluate(() => {
    const telemetry = document.querySelector('#preview').tolglowPreview.getTelemetry();
    return {...telemetry.lightLod, lightCap: telemetry.lightCap, activeLights: telemetry.activeLights, thread: telemetry.thread,
      quality: telemetry.quality, status: document.querySelector('#preview-panel .preview-status')?.textContent};
  });
  const settle = async (label, expected, tier) => {
    let reading;
    for (let attempt = 0; attempt < 60; ++attempt) {
      reading = await lod();
      if (reading.budget === expected && reading.full === expected && (!tier || reading.quality === tier)) break;
      await delay(250);
    }
    report.steps.push({label, select: await budget.inputValue(), quality: await quality.inputValue(), ...reading});
    assert.equal(reading.budget, expected, label + ': ' + JSON.stringify(reading));
    assert.equal(reading.full, expected, label + ': ' + JSON.stringify(reading));
    if (tier) assert.equal(reading.quality, tier, label + ': ' + JSON.stringify(reading));
    await page.locator('#preview-panel').screenshot({path: `${directory}/${label}.png`});
  };

  await open();
  report.renderer = await page.evaluate(() => document.querySelector('#preview').tolglowPreview.getTelemetry().deviceRenderer);
  assert.equal(await budget.inputValue(), 'auto');
  // A fixed tier, so Auto means the Desktop default of 8.
  await quality.selectOption('desktop');
  await settle('1-auto-desktop', 8, 'desktop');
  assert.equal((await lod()).activeLights, 20);
  await budget.selectOption('12'); await settle('2-twelve', 12);
  await budget.selectOption('4'); await settle('3-four', 4);
  report.stored = await page.evaluate(() => localStorage.getItem('tolglow.studio.room-view'));
  const {savedAt, ...stored} = JSON.parse(report.stored);
  assert.deepEqual(stored, {quality: 'desktop', 'light-budget': '4'});
  assert.ok(Number.isFinite(savedAt) && savedAt > 0, 'the copy carries its save time');

  // This browser keeps the choice across a reload.
  await open();
  assert.equal(await budget.inputValue(), '4'); assert.equal(await quality.inputValue(), 'desktop');
  await settle('4-reloaded', 4);
  await budget.selectOption('auto'); await settle('5-auto-again', 8);
  await quality.selectOption('high'); await settle('6-auto-high', 16, 'high');
  // Auto quality caps a chosen 16: this narrow Room panel runs Auto as Mobile.
  const label = () => budget.evaluate(select => select.selectedOptions[0].textContent);
  await budget.selectOption('16'); await quality.selectOption('auto'); await settle('7-sixteen-auto-mobile', 8, 'mobile');
  await page.waitForFunction(() => document.querySelector('select[aria-label="Room full lights"]').selectedOptions[0].textContent === '16 (8 now)');
  report.cappedLabel = await label();
  await page.locator('.preview-settings').screenshot({path: `${directory}/settings-row-capped.png`});
  await quality.selectOption('desktop'); await settle('8-sixteen-desktop', 16, 'desktop');
  await page.waitForFunction(() => document.querySelector('select[aria-label="Room full lights"]').selectedOptions[0].textContent === '16 full lights');
  report.uncappedLabel = await label();
  await page.locator('.preview-settings').screenshot({path: `${directory}/settings-row.png`});
  assert.deepEqual(errors, []);
} finally {
  await context.close();
  await rm(profile, {recursive: true, force: true});
  await writeFile(directory + '/report.json', JSON.stringify(report, null, 2));
  console.log(JSON.stringify(report, null, 2));
}
