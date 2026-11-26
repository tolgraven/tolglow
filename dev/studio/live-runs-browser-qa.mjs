// Browser check for the Live runs panel (#683) against tolglow.qa.live-runs-studio.
//
//   $W/bin/repl run tolglow.qa.live-runs-studio 18600 target/live-runs-qa
//   CHROMIUM_PATH=... node dev/studio/live-runs-browser-qa.mjs http://127.0.0.1:18600 target/live-runs-qa/out
//
// Uses the production Studio build in headless Chrome with the real GPU. It
// opens Connections, presses Show what ran, checks every listed take against
// /api/live-runs (the projection show/live-runs returns at the REPL), and
// writes stills at desktop and phone widths plus report.json. Headless
// desktop Chrome is not evidence for a foreground display or iPad Safari.
import assert from 'node:assert/strict';
import {mkdirSync, writeFileSync} from 'node:fs';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const [origin = 'http://127.0.0.1:18600', output = 'target/live-runs-qa/out'] = process.argv.slice(2);
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

async function showRuns(page) {
  await page.goto(origin + '/connections', {waitUntil: 'domcontentloaded'});
  const panel = page.locator('section[aria-label="Live runs"]');
  await panel.waitFor({timeout: 20000});
  await panel.locator('#live-runs-refresh').click();
  await panel.locator('ul.live-runs').waitFor({timeout: 10000});
  await panel.scrollIntoViewIfNeeded();
  return panel;
}

const panelText = panel => panel.evaluate(section => ({
  status: section.querySelector('[role=status]')?.textContent,
  cues: [...section.querySelectorAll('ul.live-runs > li')].map(li => ({
    name: li.querySelector('strong')?.textContent,
    group: li.querySelector(':scope > span')?.textContent ?? null,
    lanes: [...li.querySelectorAll(':scope > ul > li')].map(lane => ({
      track: lane.querySelector(':scope > span')?.textContent,
      takes: [...lane.querySelectorAll('ul > li')].map(t => t.textContent),
    })),
  })),
  overflow: section.scrollWidth - section.clientWidth,
}));

try {
  const projection = await json('/api/live-runs');
  assert.equal(projection.takes, 5, 'the QA show performed five runs, one still running');
  pass('the REPL projection served at /api/live-runs', {takes: projection.takes, running: projection.running, mirror: projection.mirror});

  const desktop = await launch('desktop', {width: 1440, height: 1000});
  const panel = await showRuns(desktop.page);
  const shown = await panelText(panel);
  assert.deepEqual(shown.cues.map(c => c.name), projection.channels.map(c => c.name));
  for (const [i, channel] of projection.channels.entries()) {
    assert.deepEqual(shown.cues[i].lanes.map(l => l.track), channel.lanes.map(l => l['track-name']));
    assert.deepEqual(shown.cues[i].lanes.map(l => l.takes.length), channel.lanes.map(l => l.takes.length));
  }
  assert.equal(shown.status, '2 launched cues · 5 takes (1 running) · 4 of 5 in Live');
  const flat = shown.cues.flatMap(c => c.lanes.flatMap(l => l.takes));
  assert.ok(flat.some(t => t.endsWith('in Live')) && flat.some(t => t.endsWith('recording')));
  assert.ok(shown.cues.some(c => c.lanes.length === 2), 'the overlapping run has its own lane');
  assert.equal(shown.overflow, 0);
  assert.deepEqual(desktop.problems, []);
  pass('Studio lists the same cues, lanes and takes as the projection', shown);
  await panel.screenshot({path: join(output, 'live-runs-desktop.png')});

  const phone = await launch('phone', {width: 390, height: 844, scale: 3, touch: true});
  const small = await showRuns(phone.page);
  const narrow = await panelText(small);
  const pageOverflow = await phone.page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth);
  assert.equal(narrow.overflow, 0);
  assert.ok(pageOverflow <= 0, 'no horizontal page scroll at phone width: ' + pageOverflow);
  assert.deepEqual(phone.problems, []);
  pass('the panel fits a 390 px wide phone viewport', {pageOverflow, panelOverflow: narrow.overflow});
  await small.screenshot({path: join(output, 'live-runs-phone.png')});
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
