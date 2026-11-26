'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs/promises');
const path = require('node:path');
const {chromium} = require('playwright-core');
const {createWorker} = require('../../resources/devices/push-display/worker.cjs');
const executablePath = process.env.CHROMIUM || '/usr/bin/chromium';

async function capturePush(origin, directory) {
  const recording = await fs.readFile(path.join(directory, 'requests.ndjson'), 'utf8');
  const frames = recording.trim().split('\n').map(line => JSON.parse(line).view);
  assert.equal(frames.length, 48);
  let browser;
  const worker = await createWorker({url: origin + '/push-display', executablePath,
    startupTimeoutMs: 30000, captureTimeoutMs: 8000}, async options => {
    browser = await chromium.launch(options);
    return browser;
  });
  const errors = [];
  const page = browser.contexts()[0].pages()[0];
  page.on('pageerror', error => errors.push(error.message));
  try {
    await fs.mkdir(path.join(directory, 'frames'), {recursive: true});
    for (const [i, view] of frames.entries()) {
      const mode = i < 16 ? 'auto' : 'history';
      const result = await worker.capture({...view, 'parameter-view': mode, 'history-beats': i < 36 ? 4 : 1});
      if (i === 0) {
        assert.equal(await page.locator('.parameter-space').count(), 2);
        assert.equal(await page.locator('.parameter-color').count(), 1);
        assert.equal(await page.getByLabel('orientation in three dimensions').count(), 1);
        assert.equal(await page.locator('.push-component').first().innerText(), 'X');
      }
      if (i === 16 || i === 35 || i === 45) {
        assert.equal(await page.locator('.parameter-history').count(), 0);
        assert.equal(await page.locator('.parameter-space').count(), 2);
        assert.equal(await page.locator('.push-source-state[data-state="held"]').count(), i === 35 ? 3 : 0);
      }
      if (i === 33 || i === 42) {
        assert.equal(await page.locator('.parameter-history').count(), 3);
        assert.equal(await page.locator('.push-source-state[data-state="active"]').count(), 3);
        assert.ok((await page.locator('.parameter-history path').first().getAttribute('d')).includes('L'));
      }
      const png = Buffer.from(result.png, 'base64');
      await fs.writeFile(path.join(directory, 'frames', String(i).padStart(2, '0') + '.png'), png);
      if (i === 10) await fs.writeFile(path.join(directory, 'push-parameters.png'), png);
      if (i === 34) await fs.writeFile(path.join(directory, 'push-history.png'), png);
    }
    assert.deepEqual(errors, []);
    console.log('Captured 48 live show views through the 960 × 160 Push worker.');
  } finally {
    await worker.close();
  }
}

async function inspectStudio(origin, id, directory) {
  const client = await chromium.launch({executablePath});
  try {
    const page = await client.newPage({viewport: {width: 1024, height: 768}, hasTouch: true, deviceScaleFactor: 1});
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.goto(origin);
    const card = page.locator('article.live-effect-card[data-live-instance=' + JSON.stringify(id) + ']');
    await card.getByRole('button', {name: /^Expand /}).tap();
    const row = card.locator('[data-param="aim"]');
    await row.scrollIntoViewIfNeeded();
    await row.locator('.parameter-space').waitFor();
    await row.getByRole('button', {name: 'Edit Orbit aim components'}).tap();
    const dialog = row.getByRole('dialog', {name: 'Orbit aim components'});
    await dialog.getByRole('spinbutton', {name: 'Orbit aim X value'}).fill('0.42');
    await dialog.getByRole('button', {name: 'Done'}).tap();
    await row.locator('.parameter-space figcaption').filter({hasText: '0.42'}).waitFor();
    await row.getByRole('button', {name: 'History'}).tap();
    const zoom = row.getByLabel('Parameter history beat window');
    await zoom.selectOption('0.5');
    await row.getByRole('img', {name: 'Last 0.5 beats'}).waitFor();
    await page.waitForFunction(() => document.querySelector('[data-param="aim"] .parameter-history path')?.getAttribute('d')?.includes('L'));
    await zoom.selectOption('8');
    await row.getByRole('img', {name: 'Last 8 beats'}).waitFor();
    await row.getByRole('button', {name: '3D'}).tap();
    await row.locator('.parameter-space').waitFor();
    await card.evaluate(el => {
      const toolbar = document.querySelector('.topbar').getBoundingClientRect();
      window.scrollBy({top: el.getBoundingClientRect().top - toolbar.bottom - 10, behavior: 'instant'});
    });
    await page.screenshot({path: path.join(directory, 'studio-parameters.png')});
    assert.deepEqual(errors, []);
    console.log('Switched live Studio aim geometry to history, changed zoom and restored 3D.');
  } finally {
    await client.close();
  }
}

async function run(origin, id, directory) {
  assert.ok(origin && id && directory, 'Pass the local show URL, instance ID and capture directory');
  await capturePush(origin, directory);
  if (!process.env.TOLGLOW_PUSH_ONLY) await inspectStudio(origin, id, directory);
}
run(...process.argv.slice(2)).catch(error => { console.error(error); process.exitCode = 1; });
