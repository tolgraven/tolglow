// Browser check for Studio's device session readout (#905, #907) against
// tolglow.qa.device-retirement-studio, a captured-output show whose Push 2
// session "qa-push" has a blocked display write.
//
//   $W/bin/repl run tolglow.qa.device-retirement-studio 20220 target/device-retirement-qa
//   CHROMIUM_PATH=... node dev/studio/device-retirement-qa.mjs http://127.0.0.1:20220 target/device-retirement-qa
//
// Uses the production Studio build in headless Chrome with the real GPU. It
// opens Connections, has the harness import the show, checks that only qa-push
// is listed as closing, compares the shown refusal with the REPL's connect
// refusal (state.json), releases the write, sees the session close and the
// closed count rise, and sees the refusal clear once the REPL connects qa-push
// again. The panel is read-only. It writes stills at desktop and phone widths
// plus report.json, then stops the show.
// Headless desktop Chrome is not evidence for a foreground display or iPad Safari.
import assert from 'node:assert/strict';
import {existsSync, mkdirSync, readFileSync, writeFileSync} from 'node:fs';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const [origin = 'http://127.0.0.1:20220', work = 'target/device-retirement-qa'] = process.argv.slice(2);
const output = join(work, 'out');
const executablePath = process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
mkdirSync(output, {recursive: true});
const report = {origin, startedAt: new Date().toISOString(), checks: {}};
const open = [];
const pass = (name, detail = true) => { report.checks[name] = detail; console.log('ok', name); };
const sleep = ms => new Promise(done => setTimeout(done, ms));

async function state(step) {
  const file = join(work, 'state.json');
  for (let i = 0; i < 400; i++) {
    if (existsSync(file)) {
      const data = JSON.parse(readFileSync(file, 'utf8'));
      if (data.step === step) return data;
    }
    await sleep(50);
  }
  throw new Error('Harness did not reach ' + step);
}

const touch = name => writeFileSync(join(work, name), '');

async function launch(name, {width, height, scale = 1, touch = false}) {
  const context = await chromium.launchPersistentContext(resolve(work, 'chrome-profile-' + name), {
    executablePath, headless: true,
    args: ['--headless=new', '--use-angle=metal', '--enable-gpu', '--ignore-gpu-blocklist', '--hide-scrollbars'],
    viewport: {width, height}, deviceScaleFactor: scale, hasTouch: touch,
  });
  open.push(context);
  const page = context.pages()[0] || await context.newPage();
  const problems = [];
  const rejected = [];
  page.on('pageerror', error => problems.push(error.message));
  page.on('console', message => { if (message.type() === 'error') problems.push('console: ' + message.text()); });
  page.on('response', response => { if (response.status() >= 400) rejected.push([new URL(response.url()).pathname, response.status()]); });
  return {page, problems, rejected};
}

async function openPanel(page) {
  await page.goto(origin + '/connections', {waitUntil: 'domcontentloaded'});
  const panel = page.locator('section[aria-label="Device sessions"]');
  await panel.waitFor({timeout: 20000});
  await panel.scrollIntoViewIfNeeded();
  return panel;
}

const readPanel = panel => panel.evaluate(section => ({
  chip: section.querySelector('.connection-chip')?.textContent,
  counts: Object.fromEntries([...section.querySelectorAll('.retirement-count')]
    .map(div => [div.querySelector('dt').textContent, Number(div.querySelector('dd').textContent)])),
  sessions: [...section.querySelectorAll('.retirement-session')].map(li => ({
    id: li.querySelector('.retirement-id')?.textContent,
    state: li.querySelector('.retirement-state')?.textContent,
  })),
  buttons: section.querySelectorAll('button').length,
  lastRefusal: section.querySelector('.retirement-refusal')?.textContent ?? null,
  skin: section.dataset.skin,
}));

async function until(panel, label, predicate) {
  for (let i = 0; i < 200; i++) {
    const view = await readPanel(panel);
    if (predicate(view)) return view;
    await sleep(50);
  }
  throw new Error('Timed out waiting for ' + label + ': ' + JSON.stringify(await readPanel(panel)));
}

try {
  const blocked = await state('blocked');
  const {page, problems, rejected} = await launch('desktop', {width: 1440, height: 1000});
  const panel = await openPanel(page);
  const before = await until(panel, 'the status', view => view.counts['Closed'] !== undefined);
  assert.deepEqual(before.sessions, []);
  pass('no retired sessions before the import', before);
  await panel.screenshot({path: join(output, 'device-sessions-before.png')});

  touch('import');
  const imported = await state('imported');
  const closing = await until(panel, 'qa-push closing',
    view => view.sessions.length === 1 && view.sessions[0].id === 'qa-push' && view.sessions[0].state === 'Closing'
         && view.counts['Closed'] === 1 && view.lastRefusal);
  assert.equal(closing.chip, '1 still closing');
  assert.equal(closing.counts['Closing'], 1);
  assert.equal(closing.counts['Refused connects'], 1);
  assert.equal(closing.buttons, 0, 'the panel is read-only');
  assert.equal(imported.retirement.pending, 1);
  assert.deepEqual(imported.retirement.closing, ['qa-push']);
  assert.ok(imported.packets > imported['packets-before-import'], 'captured output kept flowing through the import');
  pass('the import lists qa-push as closing; qa-midi has closed', {panel: closing, status: imported.retirement});
  // The REPL's refusal for the same connect, read by the harness.
  const repl = imported['repl-refusal'];
  assert.equal(repl.message, 'Previous device session is still closing');
  assert.deepEqual(repl.data, {'session-id': 'qa-push', code: 'device/closing'});
  assert.match(closing.lastRefusal, new RegExp(repl.message));
  assert.match(closing.lastRefusal, /:device\/closing/);
  assert.match(closing.lastRefusal, /qa-push/);
  pass('the REPL refusal is shown with its message, code and session', closing.lastRefusal);
  await panel.screenshot({path: join(output, 'device-sessions-closing.png')});

  touch('release');
  const released = await state('released');
  const closed = await until(panel, 'qa-push closed', view => view.sessions.length === 0);
  assert.equal(closed.chip, 'None closing');
  assert.equal(closed.counts['Closing'], 0);
  assert.equal(closed.counts['Closed'], 2);
  assert.ok(released['packets-at-release'] > imported.packets, 'captured output kept flowing while the write was blocked');
  pass('after release the session is closed and the closed count rose', {panel: closed, status: released.retirement});
  await panel.screenshot({path: join(output, 'device-sessions-released.png')});

  touch('reconnect');
  await state('reconnected');
  const cleared = await until(panel, 'the refusal to clear', view => !view.lastRefusal);
  pass('a successful connect of qa-push clears its refusal', cleared);
  await panel.screenshot({path: join(output, 'device-sessions-cleared.png')});
  assert.equal(closing.skin, 'ember');
  assert.deepEqual(problems, []);
  assert.deepEqual(rejected, []);
  pass('no page errors or rejected requests on desktop');

  const phone = await launch('phone', {width: 390, height: 844, scale: 2, touch: true});
  const phonePanel = await openPanel(phone.page);
  await until(phonePanel, 'the phone status', view => view.counts['Closed'] === 2);
  const overflow = await phone.page.evaluate(() => document.documentElement.scrollWidth - window.innerWidth);
  assert.ok(overflow <= 0, 'no horizontal page scroll at phone width: ' + overflow);
  await phonePanel.screenshot({path: join(output, 'device-sessions-phone.png')});
  assert.deepEqual(phone.problems, []);
  assert.deepEqual(phone.rejected, []);
  pass('phone width fits without horizontal scroll');

  touch('stop');
  const stopping = await state('stopping');
  assert.deepEqual(stopping.connected, ['qa-push']);
  pass('the REPL reconnect held qa-push until stop', stopping.connected);
  report.result = 'pass';
} catch (error) {
  report.result = 'fail';
  report.error = String(error.stack || error);
  console.error(error);
  process.exitCode = 1;
  for (const name of ['import', 'release', 'reconnect', 'stop']) touch(name);
} finally {
  for (const context of open) await context.close().catch(() => {});
  report.finishedAt = new Date().toISOString();
  writeFileSync(join(output, 'report.json'), JSON.stringify(report, null, 2));
}
