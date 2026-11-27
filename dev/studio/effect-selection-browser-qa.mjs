// Browser half of tolglow.qa.effect-selection-browser (#749).
//
//   $W/bin/repl run tolglow.qa.effect-selection-browser 18890 target/effect-selection-qa
//
// The REPL has already soloed card B with one :mode :exclusive toggle while
// the Solo default is additive. This script checks Studio shows only B
// soloed, then uses Exclusive once, Alt-click and a plain click on the
// production build, waiting each time for the committed projection to update
// the cards. It writes stills and report.json. Headless desktop Chrome with
// the real GPU is not evidence for a foreground display or a real touchscreen.
import assert from 'node:assert/strict';
import {mkdirSync, writeFileSync} from 'node:fs';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, output, a, b, c] = process.argv.slice(2);
assert.ok(origin && output && a && b && c, 'usage: origin output-dir instance-a instance-b instance-c');
const executablePath = process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
mkdirSync(output, {recursive: true});
const report = {origin, instances: {a, b, c}, startedAt: new Date().toISOString(), checks: {}};
const pass = (name, detail = true) => { report.checks[name] = detail; console.log('ok', name, JSON.stringify(detail)); };

const context = await chromium.launchPersistentContext(resolve(output, 'chrome-profile'), {
  executablePath, headless: true,
  args: ['--headless=new', '--use-angle=metal', '--enable-gpu', '--ignore-gpu-blocklist', '--hide-scrollbars'],
  viewport: {width: 1440, height: 1000},
});
const page = context.pages()[0] || await context.newPage();
const problems = [];
page.on('pageerror', error => problems.push(error.message));
page.on('console', message => { if (message.type() === 'error') problems.push('console: ' + message.text()); });

const card = id => page.locator(`[data-live-instance="${id}"]`);
const soloButton = id => card(id).locator('button[data-selection-control="solo"]');

// Each card's Solo button state and suppression label, as Studio renders them.
const cards = () => page.evaluate(ids => Object.fromEntries(ids.map(id => {
  const node = document.querySelector(`[data-live-instance="${id}"]`);
  return [id, {solo: node?.querySelector('button[data-selection-control="solo"]')?.getAttribute('aria-pressed'),
               suppressed: node?.querySelector('[data-suppressed-by]')?.dataset.suppressedBy ?? null}];
})), [a, b, c]);

async function waitForSolo(expected, step) {
  const want = Object.fromEntries([a, b, c].map(id => [id, expected.includes(id) ? 'true' : 'false']));
  await page.waitForFunction(([ids, want]) => ids.every(id =>
    document.querySelector(`[data-live-instance="${id}"] button[data-selection-control="solo"]`)
      ?.getAttribute('aria-pressed') === want[id]), [[a, b, c], want], {timeout: 10000});
  const shown = await cards();
  for (const id of [a, b, c]) {
    assert.equal(shown[id].suppressed, expected.includes(id) ? null : 'solo', `${step}: ${id} suppression label`);
  }
  pass(step, shown);
  await page.screenshot({path: join(output, step.replace(/[^a-z0-9]+/gi, '-').toLowerCase() + '.png')});
}

try {
  await page.goto(origin + '/perform');
  await card(c).waitFor({timeout: 20000});
  await waitForSolo([b], 'Studio shows the REPL exclusive Solo on B only');

  const once = page.locator('button[data-selection-once="exclusive"]');
  await once.click();
  await page.locator('button[data-selection-once="exclusive"][aria-pressed="true"]').waitFor({timeout: 5000});
  pass('Exclusive once is armed');
  await page.locator('.effect-selection-tools').screenshot({path: join(output, 'toolbar-exclusive-once-armed.png')});
  await soloButton(c).click();
  await waitForSolo([c], 'Exclusive once then Solo on C replaces B');
  await page.locator('button[data-selection-once="exclusive"][aria-pressed="false"]').waitFor({timeout: 5000});
  pass('Exclusive once is consumed by one action');

  await soloButton(a).click({modifiers: ['Alt']});
  await waitForSolo([a], 'Alt-click Solo on A replaces C');

  await soloButton(c).click();
  await waitForSolo([a, c], 'A plain click on C still uses the additive default');

  assert.equal(await page.locator('select[aria-label="Solo default"]').inputValue(), 'additive');
  pass('the Solo default stays additive');
  // Phone width: the toolbar wraps without horizontal page scroll.
  await page.setViewportSize({width: 390, height: 844});
  await page.locator('.perform-section-tabs button', {hasText: 'Live'}).click();
  await page.locator('button[data-selection-once="exclusive"]').scrollIntoViewIfNeeded();
  const overflow = await page.evaluate(() => document.documentElement.scrollWidth - innerWidth);
  assert.ok(overflow <= 0, `horizontal page overflow ${overflow}px at 390px`);
  await page.screenshot({path: join(output, 'phone-390.png')});
  pass('no horizontal page scroll at 390px', {overflow});
  assert.deepEqual(problems, []);
  pass('no page errors');
  report.finishedAt = new Date().toISOString();
  writeFileSync(join(output, 'report.json'), JSON.stringify(report, null, 2));
} finally {
  await context.close();
}
