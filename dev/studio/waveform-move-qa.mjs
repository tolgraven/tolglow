// Browser check for moving a running waveform (#468), started by tolglow.qa.waveform-move-studio.
//
//   lein run -m tolglow.qa.waveform-move-studio target/waveform-move-qa 19941
//
// Uses the production Studio build in headless Chrome with the real GPU. In the
// first activation's Level source editor it chooses Hue as the destination, has
// the REPL side occupy Hue, selects Move LFO and checks that Studio names the
// refusal and its reason and returns the Destination select to Level. It then
// moves the waveform to the second activation's Level. Headless desktop Chrome
// is not evidence for a foreground display or iPad Safari.
import assert from 'node:assert/strict';
import {existsSync, mkdirSync, writeFileSync} from 'node:fs';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, first, second, directory] = process.argv.slice(2);
assert.ok(origin && first && second && directory, 'Pass the show URL, both instance IDs and an output directory');
const executablePath = process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
mkdirSync(directory, {recursive: true});
const report = {origin, first, second, checks: {}};
const pass = (name, detail = true) => { report.checks[name] = detail; console.log('ok', name); };
const sleep = ms => new Promise(done => setTimeout(done, ms));
// Reagent re-renders on the next animation frame, so read values once they settle.
const settles = (label, locator, expected) =>
  until(label + ' to read ' + expected, async () => (await locator.inputValue()) === expected, 5000);

async function until(label, predicate, timeout = 15000) {
  const started = Date.now();
  for (;;) {
    const value = await predicate();
    if (value) return value;
    if (Date.now() - started > timeout) throw new Error('Timed out waiting for ' + label);
    await sleep(100);
  }
}

const context = await chromium.launchPersistentContext(resolve(directory, 'chrome-profile'), {
  executablePath, headless: true,
  args: ['--headless=new', '--use-angle=metal', '--enable-gpu', '--ignore-gpu-blocklist', '--hide-scrollbars'],
  viewport: {width: 1440, height: 1100}, deviceScaleFactor: 2,
});
const page = context.pages()[0] || await context.newPage();
const problems = [];
page.on('pageerror', error => problems.push(error.message));
page.on('console', message => { if (message.type() === 'error') problems.push('console: ' + message.text()); });

try {
  await page.goto(origin + '/perform', {waitUntil: 'domcontentloaded'});
  const card = id => page.locator('article.live-effect-card[data-live-instance=' + JSON.stringify(id) + ']');
  await card(first).waitFor({timeout: 30000});
  await card(first).getByRole('button', {name: 'Edit source for Level', exact: true}).click();
  const panel = card(first).getByRole('region', {name: 'Level source'});
  const destination = panel.getByLabel('Destination', {exact: true});
  await destination.waitFor();
  const shot = async name => {
    await panel.scrollIntoViewIfNeeded();
    await panel.screenshot({path: join(directory, name + '.png')});
  };

  const groups = await destination.evaluate(select => [...select.querySelectorAll('optgroup')].map(group => ({
    label: group.label, options: [...group.querySelectorAll('option')].map(option => option.textContent)})));
  assert.deepEqual(groups, [{label: 'This activation', options: ['Level', 'Hue', 'Layer priority']},
                            {label: 'Drift wash · ' + second, options: ['Hue', 'Level', 'Layer priority']}]);
  pass('destinations list this activation, then the other running activation', groups);

  await destination.selectOption(':hue');
  await settles('From', panel.getByLabel('From', {exact: true}), '0');
  await settles('To', panel.getByLabel('To', {exact: true}), '360');
  pass('choosing Hue offers its typed endpoints');

  // The REPL side attaches a waveform to Hue while Studio still shows it chosen.
  writeFileSync(join(directory, 'occupy'), '');
  await until('the REPL to occupy Hue', () => existsSync(join(directory, 'occupied')));
  await until('Studio to show the Hue waveform', async () =>
    (await card(first).getByRole('button', {name: 'Edit source for Hue', exact: true}).count()) > 0);
  assert.equal(await destination.inputValue(), ':hue', 'the stale choice stays selected until the move is sent');
  await shot('1-stale-choice');

  await panel.getByRole('button', {name: 'Move LFO', exact: true}).click();
  const alert = panel.getByRole('alert');
  const message = await until('the refusal', async () => {
    const text = (await alert.count()) ? await alert.first().innerText() : '';
    return text.includes('refused') && text;
  });
  assert.match(message, /^Moving the LFO to Hue was refused\. /);
  assert.match(message, /already bound to the destination parameter\. It stays/);
  assert.match(message, /It stays on Level\.$/);
  pass('the refusal names the destination, the reason and where the LFO stays', message);
  await until('the select to return to Level', async () => (await destination.inputValue()) === ':level');
  await panel.getByRole('button', {name: 'Update LFO', exact: true}).waitFor({timeout: 5000});
  await settles('From', panel.getByLabel('From', {exact: true}), '0.1');
  pass('the Destination select and endpoints return to the input the LFO is on');
  await shot('2-refused-and-reset');

  // The select value is the destination as EDN: [activation-id parameter].
  await destination.selectOption('[' + JSON.stringify(second) + ' :level]');
  await panel.getByRole('button', {name: 'Move LFO', exact: true}).waitFor({timeout: 5000});
  await settles('the other activation\'s Level To', panel.getByLabel('To', {exact: true}), '1');
  await shot('3-other-activation-chosen');
  await panel.getByRole('button', {name: 'Move LFO', exact: true}).click();
  await until('the second activation to show the LFO', async () => {
    const trigger = card(second).getByRole('button', {name: 'Edit source for Level', exact: true});
    return (await trigger.count()) > 0 && (await trigger.getAttribute('class')).includes('bound');
  });
  await until('the first activation to release Level', async () =>
    (await card(first).getByRole('button', {name: 'Attach source for Level', exact: true}).count()) > 0);
  pass('the waveform moved to the other activation');
  await card(second).scrollIntoViewIfNeeded();
  await page.screenshot({path: join(directory, '4-moved-across.png')});

  // The refused move's acknowledgement is an HTTP 422, which Chrome logs once.
  const refusal = 'console: Failed to load resource: the server responded with a status of 422 (Unprocessable Entity)';
  assert.equal(problems.filter(problem => problem === refusal).length, 1);
  assert.deepEqual(problems.filter(problem => problem !== refusal), []);
  pass('no page errors beyond the refused move\'s 422');
  report.passed = true;
} catch (error) {
  report.error = String(error.stack || error);
  console.error(report.error);
  await page.screenshot({path: join(directory, 'failure.png')}).catch(() => {});
  process.exitCode = 1;
} finally {
  writeFileSync(join(directory, 'report.json'), JSON.stringify(report, null, 2));
  await context.close();
}
