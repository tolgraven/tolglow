// Controlled checkboxes, the publication failure path and block stage edits
// in the Definitions editor (#732), in headless Chrome:
//   - Playwright's check()/uncheck() on #auto-publish, #builder-cue, the
//     reactive "Evaluate when inputs change" box and the Perform auto-launch
//     "Choose cues by visual activity" box; each must keep its new state at
//     once (check() fails when a click is undone for a frame).
//   - A publication refused while a newer edit was typed: the newer text stays
//     Unpublished changes and Publish valid edits sends it after the quiet time.
//   - Generated EDN reads Unpublished changes, not "Generated EDN".
//   - Two taps on one stage's Remove inside one task remove one stage.
//
//   node dev/studio/definition-controls-qa.mjs http://127.0.0.1:18721 target/qa/definition-controls
//
// Run against a production build on a capture-only host (preview output).
// Headless Chrome is not iPad Safari or the packaged app's WKWebView.
import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin = 'http://127.0.0.1:18721', output = 'target/qa/definition-controls'] = process.argv.slice(2);
await mkdir(output, {recursive: true});
const show = await (await fetch(origin + '/api/state', {headers: {accept: 'application/json'}})).json();
assert.equal(show.status.output.type, 'preview', 'a capture-only host');
const browser = await chromium.launch({
  executablePath: process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  args: ['--headless=new'],
});
const results = {};
const errors = [];
try {
  const page = await browser.newPage({viewport: {width: 1440, height: 900}});
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(origin + '/library');
  await page.locator('.definition-item').first().waitFor();

  // check() clicks, then requires the new state; a controlled box that shows
  // its old state for a frame makes it throw "did not change its state".
  const toggle = async (name, locator) => {
    await locator.scrollIntoViewIfNeeded();
    const start = await locator.isChecked();
    for (const step of start ? ['uncheck', 'check'] : ['check', 'uncheck', 'check']) {
      await locator[step]({timeout: 3000});
      assert.equal(await locator.isChecked(), step === 'check', `${name}: ${step}()`);
    }
    results[name] = 'check() and uncheck() kept their state';
  };

  await page.locator('#new-definition').click();
  await page.locator('#definition-status').filter({hasText: /^New definition$/}).waitFor();
  await toggle('#auto-publish', page.locator('#auto-publish'));
  await page.locator('#auto-publish').uncheck();

  await page.getByRole('button', {name: 'Visual builder', exact: true}).click();
  await toggle('#builder-cue', page.locator('#builder-cue'));
  const reactive = page.getByLabel('Evaluate when inputs change');
  if (await reactive.count()) {
    await toggle('reactive evaluation', reactive);
    await reactive.uncheck();
  }

  // Generated EDN is an edit: its status follows the published baseline.
  await page.locator('#builder-id').fill('generated-status-check');
  await page.getByRole('button', {name: /Generate EDN/}).click();
  await page.waitForFunction(() => document.querySelector('#definition-source')?.value.includes(':generated-status-check'),
                             null, {timeout: 5000});
  assert.equal(await page.locator('#definition-status').textContent(), 'Unpublished changes');
  assert.equal(await page.locator('#validation-result').textContent(), 'Generated EDN from the builder.');
  results.generatedStatus = await page.locator('#definition-status').textContent();

  // A refused publication with a newer edit typed meanwhile.
  await page.locator('#new-definition').click();
  await page.locator('#definition-status').filter({hasText: /^New definition$/}).waitFor();
  const valid = (await page.locator('#definition-source').inputValue()).replace(':after-hours', ':failure-path-check');
  const posts = [];
  let release;
  const held = new Promise(resolve => { release = resolve; });
  await page.route('**/api/definition', async route => {
    if (route.request().method() !== 'POST') return route.continue();
    const body = route.request().postData() || '';
    posts.push({at: Date.now(), body});
    if (posts.length === 1) await held;
    await route.continue();
  });
  await page.locator('#auto-publish').check();
  await page.locator('#definition-source').fill('{:id');
  for (let i = 0; i < 50 && posts.length < 1; i++) await page.waitForTimeout(100);
  assert.equal(posts.length, 1, 'Publish valid edits sent the first text');
  // Typed while that request is held: its own debounce fires and is dropped.
  await page.locator('#definition-source').fill(valid);
  await page.waitForTimeout(1200);
  assert.equal(posts.length, 1, 'nothing else is sent while a publication is in flight');
  const released = Date.now();
  release();
  await page.waitForFunction(() => document.querySelector('#validation-result')?.textContent.trim(), null, {timeout: 5000});
  const result = await page.locator('#validation-result').textContent();
  assert.match(result, /newer edits are not published/, `the failure names the newer edit as unpublished: ${result}`);
  assert.equal(await page.locator('#definition-status').textContent(), 'Unpublished changes');
  for (let i = 0; i < 50 && posts.length < 2; i++) await page.waitForTimeout(100);
  assert.equal(posts.length, 2, 'the newer edit went out after the failure');
  assert.match(posts[1].body, /failure-path-check/, 'the follow-up carries the newer text');
  const delay = posts[1].at - released;
  assert.ok(delay >= 800, `the follow-up waited the quiet time (${delay} ms)`);
  await page.locator('#definition-status').filter({hasText: /^Published$/}).waitFor({timeout: 10000});
  await page.unroute('**/api/definition');
  results.failurePath = {firstRefused: true, followUpDelayMs: delay, finalStatus: 'Published'};
  await page.locator('#auto-publish').uncheck();

  // Two taps on one Remove inside one task remove one stage.
  const id = 'stage-id-check-' + Date.now();
  await page.locator('#new-definition').click();
  await page.locator('#definition-status').filter({hasText: /^New definition$/}).waitFor();
  await page.getByLabel('Declarative EDN definition').fill(`{:id :${id} :version 1 :params {}\n :outputs {:color [0.8 0.7 0.4] :dimmer 0.65}}\n`);
  const panel = page.locator('.block-editor:visible');
  await panel.getByRole('button', {name: 'Assign blocks'}).click();
  await panel.getByLabel('Block binding').selectOption({label: 'outputs / color'});
  for (const name of ['Through water', 'Color gel', 'Through water']) {
    await panel.getByLabel('New block').selectOption({label: name});
    await panel.getByRole('button', {name: 'Add stage'}).click();
  }
  await panel.locator('.block-stage').nth(2).waitFor();
  await panel.getByRole('button', {name: 'Remove Color gel'}).evaluate(button => { button.click(); button.click(); });
  await page.waitForTimeout(200);
  const titles = await panel.locator('.block-stage header strong').allTextContents();
  assert.deepEqual(titles, ['1 · Through water', '2 · Through water'], 'one stage removed, and the right one');
  results.doubleRemove = titles;

  assert.deepEqual(errors, [], 'no page errors');
  await page.screenshot({path: join(output, 'definitions.png')});

  // A Perform checkbox: the auto-launch form's visual activity box.
  await page.goto(origin + '/perform');
  await page.locator('#auto-status').waitFor({state: 'attached'});
  await page.locator('details.auto-panel:has(#auto-status)').evaluate(details => { details.open = true; });
  await toggle('Choose cues by visual activity', page.getByLabel('Choose cues by visual activity'));
  assert.deepEqual(errors, [], 'no page errors');
  results.passed = true;
  console.log(JSON.stringify(results));
} finally {
  await writeFile(join(output, 'results.json'), JSON.stringify({results, errors}, null, 2) + '\n');
  await browser.close();
}
