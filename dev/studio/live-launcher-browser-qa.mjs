// Browser check for the cue launcher panel (#228) against tolglow.qa.live-launcher-studio.
//
//   $W/bin/repl run tolglow.qa.live-launcher-studio 18850 target/live-launcher-qa
//   CHROMIUM_PATH=... node dev/studio/live-launcher-browser-qa.mjs http://127.0.0.1:18850 target/live-launcher-qa/out
//
// Uses the production Studio build in headless Chrome with the real GPU. It
// opens Connections, presses Show pads, then Set up in Live (the same host
// operation as show/setup-live-launcher! at the REPL, through the running Live
// bridge), checks the 8x8 grid, problems and warnings against
// /api/live-launcher (the projection show/live-launcher returns at the REPL),
// checks the Pad layout selector, and writes stills at desktop and phone
// widths plus report.json. Headless desktop Chrome is not evidence for a
// foreground display or iPad Safari.
import assert from 'node:assert/strict';
import {mkdirSync, writeFileSync} from 'node:fs';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const [origin = 'http://127.0.0.1:18850', output = 'target/live-launcher-qa/out'] = process.argv.slice(2);
const executablePath = process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
mkdirSync(output, {recursive: true});
const report = {origin, startedAt: new Date().toISOString(), checks: {}};
const open = [];
const pass = (name, detail = true) => { report.checks[name] = detail; console.log('ok', name); };

async function json(path) {
  const response = await fetch(origin + path, {headers: {Accept: 'application/json'}});
  assert.equal(response.status, 200, path);
  return response.json();
}

async function launch(name, {width, height, scale = 1, touch = false}) {
  const context = await chromium.launchPersistentContext(resolve(output, '..', 'chrome-profile-' + name), {
    executablePath, headless: true,
    args: ['--headless=new', '--use-angle=metal', '--enable-gpu', '--ignore-gpu-blocklist', '--hide-scrollbars'],
    viewport: {width, height}, deviceScaleFactor: scale, hasTouch: touch,
  });
  open.push(context);
  const page = context.pages()[0] || await context.newPage();
  const problems = [];
  page.on('pageerror', error => problems.push(error.message));
  page.on('console', message => { if (message.type() === 'error') problems.push('console: ' + message.text()); });
  return {page, problems};
}

async function showPads(page) {
  await page.goto(origin + '/connections', {waitUntil: 'domcontentloaded'});
  const panel = page.locator('section[aria-label="Cue launcher"]');
  await panel.waitFor({timeout: 20000});
  await panel.locator('#launcher-refresh').click();
  await panel.locator('.launcher-pads').waitFor({timeout: 10000});
  await panel.scrollIntoViewIfNeeded();
  return panel;
}

const panelText = panel => panel.evaluate(section => ({
  status: section.querySelector('[role=status]')?.textContent,
  rows: [...section.querySelectorAll('.launcher-row')].map(row =>
    [...row.querySelectorAll('.launcher-pad')].map(pad => ({
      note: Number(pad.querySelector('.launcher-note')?.textContent),
      label: pad.querySelector('.launcher-label')?.textContent ?? null,
    }))),
  errors: [...section.querySelectorAll('.connection-error')].map(e => e.textContent),
  warnings: [...section.querySelectorAll('p.muted')].map(e => e.textContent).filter(t => t.startsWith('Warning')),
  overflow: section.scrollWidth - section.clientWidth,
}));

try {
  const before = await json('/api/live-launcher');
  assert.equal(before.layout, 'push-64-pads');
  assert.equal(before.setup, undefined, 'nothing is set up before the browser asks');
  const desktop = await launch('desktop', {width: 1440, height: 1000});
  const panel = await showPads(desktop.page);
  await panel.locator('#launcher-setup').click();
  await panel.locator('[role=status]', {hasText: 'setup problem'}).waitFor({timeout: 15000});
  const projection = await json('/api/live-launcher');
  const playing = projection.pads.filter(p => p['cue-id']);
  assert.ok(playing.length > 2, 'the demo page maps several pads');
  pass('Set up in Live ran the host setup through the bridge', {playing: playing.length,
    setup: {ready: projection.setup['ready?'], applied: projection.setup.applied, problems: projection.setup.problems.length,
            warnings: projection.setup.warnings}});

  const shown = await panelText(panel);
  assert.equal(shown.rows.length, 8);
  assert.ok(shown.rows.every(r => r.length === 8));
  const byCell = Object.fromEntries(projection.pads.map(p => [p.cell.join(','), p]));
  shown.rows.forEach((row, y) => row.forEach((pad, x) => {
    const expected = byCell[[x, y].join(',')];
    assert.equal(pad.note, expected.note, `note at ${x},${y}`);
    assert.equal(pad.label, expected['cue-id'] ? (expected.label ?? expected['cue-id']) : null, `label at ${x},${y}`);
  }));
  assert.equal(shown.rows[7][0].note, 36, 'bottom-left pad is note 36');
  assert.equal(shown.rows[0][0].note, 64, 'top-left pad is note 64');
  assert.equal(shown.rows[0][7].note, 99, 'top-right pad is note 99');
  const notes = projection.setup.problems.filter(p => p.code === 'empty-pad').map(p => p.note);
  assert.deepEqual(shown.errors, ['empty-pad · notes ' + notes.join(', ')], 'the empty pads share one line');
  assert.deepEqual(shown.warnings, ['Warning · stale-pad · note 40']);
  assert.match(shown.status, /^Push 64 Pads · \d+ of 64 pads play cues · \d+ setup problems? in Live$/);
  assert.equal(shown.overflow, 0);
  const layout = await desktop.page.locator('#live-pad-layout').inputValue();
  assert.equal(layout, 'push-64-pads', 'the saved layout is selected');
  assert.deepEqual(desktop.problems, []);
  pass('Studio draws the same 64 pads, problems and warnings as the projection, top row first', {status: shown.status, errors: shown.errors, warnings: shown.warnings, layout});
  await panel.screenshot({path: join(output, 'launcher-desktop.png')});

  const phone = await launch('phone', {width: 390, height: 844, scale: 3, touch: true});
  const small = await showPads(phone.page);
  const narrow = await panelText(small);
  const pageOverflow = await phone.page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth);
  assert.equal(narrow.overflow, 0);
  assert.ok(pageOverflow <= 0, 'no horizontal page scroll at phone width: ' + pageOverflow);
  assert.deepEqual(phone.problems, []);
  pass('the panel fits a 390 px wide phone viewport', {pageOverflow, panelOverflow: narrow.overflow});
  await small.screenshot({path: join(output, 'launcher-phone.png')});
  report.result = 'pass';
} catch (error) {
  report.result = 'fail';
  report.error = String(error.stack || error);
  console.error(error);
  process.exitCode = 1;
} finally {
  for (const context of open) await context.close();
  report.finishedAt = new Date().toISOString();
  writeFileSync(join(output, 'report.json'), JSON.stringify(report, null, 2));
}
