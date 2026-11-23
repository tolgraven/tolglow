'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs/promises');
const path = require('node:path');
const {chromium} = require('playwright-core');
const {createWorker} = require('../../resources/devices/push-display/worker.cjs');

async function capturePush(origin, directory, executablePath) {
  const lines = (await fs.readFile(path.join(directory, 'capture.ndjson'), 'utf8')).trim().split('\n');
  let browser;
  const worker = await createWorker({url: origin + '/push-display', executablePath,
    startupTimeoutMs: 30000, captureTimeoutMs: 10000}, async options => (browser = await chromium.launch(options)));
  try {
    const page = browser.contexts()[0].pages()[0];
    for (const [i, line] of lines.entries()) {
      const {view} = JSON.parse(line);
      const result = await worker.capture({...view, "parameter-view": "controls"});
      const readouts = await page.locator('.dial-number').allTextContents();
      for (const value of i ? ['0.502', '0.50001'] : ['0.498', '0.49999']) assert(readouts.includes(value), JSON.stringify(readouts));
      await fs.writeFile(path.join(directory, 'push-' + i + '.png'), Buffer.from(result.png, 'base64'));
    }
  } finally { await worker.close(); }
}

async function main(origin, id, directory) {
  const executablePath = process.env.CHROMIUM || '/usr/bin/chromium';
  await capturePush(origin, directory, executablePath);
  const browser = await chromium.launch({executablePath});
  try {
    const page = await browser.newPage({viewport: {width: 1024, height: 768}, hasTouch: true});
    await page.goto(origin);
    const card = page.locator('article.live-effect-card[data-live-instance=' + JSON.stringify(id) + ']');
    await card.getByRole('button', {name: /^Expand /}).tap();
    const pan = card.locator('[data-param="pan"]');
    const level = card.locator('[data-param="level"]');
    await pan.locator('.dial-number').filter({hasText: '0.50001'}).waitFor();
    await level.locator('.dial-number').filter({hasText: '0.502'}).waitFor();
    const input = pan.getByRole('spinbutton');
    await input.click();
    const accepted = page.waitForResponse(response => response.request().method() === 'POST' && response.url().endsWith('/api/command'));
    await input.fill('0.123456789');
    assert.equal((await accepted).status(), 200);
    await input.press('Tab');
    await pan.locator('.dial-number').filter({hasText: '0.12346'}).waitFor();
    const modelValue = await (async () => {
      const deadline = Date.now() + 5000;
      let latest;
      do {
        const state = await (await page.request.get(origin + '/api/state')).json();
        latest = state.instances.find(row => row.id === id).params.pan;
        if (latest === .123456789) break;
        await new Promise(resolve => setTimeout(resolve, 100));
      } while (Date.now() < deadline);
      return latest;
    })();
    assert.equal(modelValue, .123456789);
    await card.scrollIntoViewIfNeeded();
    await card.screenshot({path: path.join(directory, 'studio.png')});
    console.log('Push readbacks and exact Studio input verified. Captured 8-bit and 16-bit readouts.');
  } finally { await browser.close(); }
}
main(...process.argv.slice(2)).catch(error => { console.error(error); process.exitCode = 1; });
