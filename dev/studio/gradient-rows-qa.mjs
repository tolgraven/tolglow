// Check a running gradient cue's parameter row from a show started by tolglow.qa.gradient-rows (#689).
import assert from 'node:assert/strict';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, id, directory] = process.argv.slice(2);
assert.ok(origin && id && directory, 'Pass the show URL, the instance ID and a capture directory');
const executablePath = process.env.CHROMIUM_PATH || process.env.CHROMIUM || '/usr/bin/chromium';
const userDataDir = join(directory, 'chrome-profile');

const context = await chromium.launchPersistentContext(userDataDir, {
  executablePath, headless: true, args: ['--headless=new', '--use-angle=metal'],
  viewport: {width: 1440, height: 1000}, deviceScaleFactor: 2});
try {
  const page = context.pages()[0] || await context.newPage();
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(origin + '/perform');
  const card = page.locator('article.live-effect-card[data-live-instance=' + JSON.stringify(id) + ']');
  await card.waitFor();
  await card.getByRole('button', {name: /^Expand /}).click();
  const row = card.locator('.param-row[data-param=palette]');
  await row.waitFor();
  await row.scrollIntoViewIfNeeded();
  await page.waitForTimeout(400);
  await row.screenshot({path: join(directory, 'studio-gradient-row.png')});
  assert.equal(await row.locator('output').first().innerText(), '10 stops');
  assert.equal(await row.locator('.param-gradient .parameter-color.gradient').count(), 1, 'the row draws the ramp');
  assert.equal(await row.locator('input[type=number]').count(), 0, 'a gradient has no number field');
  assert.equal(await row.getByRole('status').filter({hasText: 'Outputs sample'}).innerText(),
               'Outputs sample 8 evenly spaced stops of 10');
  await card.screenshot({path: join(directory, 'studio-gradient-card.png')});
  assert.deepEqual(errors, []);
  console.log('Gradient row shows the ramp and the reduction for ' + id + '.');
} finally { await context.close(); }
