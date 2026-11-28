// Browser check for capture to Live (#671, #680) against tolglow.qa.live-capture-studio.
//
//   $W/bin/repl run tolglow.qa.live-capture-studio 19680 target/live-capture-qa
//   CHROMIUM_PATH=... node dev/studio/live-capture-browser-qa.mjs http://127.0.0.1:19680 target/live-capture-qa
//
// Uses the production Studio build in headless Chrome with the real GPU. It
// opens Connections, presses Send to Live (the same host command as
// show/send-capture! at the REPL, through the running Live bridge to a
// simulated Set), checks the status line against GET /api/capture (the status
// show/capture-action! returns at the REPL) and the simulated Set's selected
// clip, presses Open clip in Live, and writes stills at desktop and phone
// widths plus report.json. Headless desktop Chrome is not evidence for a
// foreground display or iPad Safari.
import assert from 'node:assert/strict';
import {mkdirSync, readFileSync, writeFileSync} from 'node:fs';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const [origin = 'http://127.0.0.1:19680', work = 'target/live-capture-qa'] = process.argv.slice(2);
const output = join(work, 'out');
const executablePath = process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
mkdirSync(output, {recursive: true});
const report = {origin, startedAt: new Date().toISOString(), checks: {}};
const open = [];
const pass = (name, detail = true) => { report.checks[name] = detail; console.log('ok', name); };
const selected = () => readFileSync(join(work, 'selected.edn'), 'utf8');

async function json(path) {
  const response = await fetch(origin + path, {headers: {Accept: 'application/json'}});
  assert.equal(response.status, 200, path);
  return response.json();
}

async function launch(name, {width, height, scale = 1, touch = false}) {
  const context = await chromium.launchPersistentContext(resolve(work, 'chrome-profile-' + name), {
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

async function capturePanel(page) {
  await page.goto(origin + '/connections', {waitUntil: 'domcontentloaded'});
  const panel = page.locator('section[aria-label="Capture"]');
  await panel.waitFor({timeout: 20000});
  await panel.scrollIntoViewIfNeeded();
  return panel;
}

try {
  const before = await json('/api/capture');
  assert.equal(before.live.setup ?? null, null, 'nothing is set up before the browser asks');
  const desktop = await launch('desktop', {width: 1440, height: 1000});
  const panel = await capturePanel(desktop.page);
  await panel.locator('#capture-send:not([disabled])').waitFor({timeout: 15000});
  await panel.locator('#capture-send').click();
  await panel.locator('#capture-live-status', {hasText: 'last send wrote'}).waitFor({timeout: 30000});
  const status = await json('/api/capture');
  const clip = status.live.sent.clips[0]['clip-id'];
  assert.equal(status.live.setup['ready?'], true);
  assert.equal(status.live.focus.status, 'executed');
  assert.equal(status.live.playback['bound?'], true);
  // The host writes the simulated Set's state every 200 ms.
  for (let i = 0; i < 20 && !selected().includes(`:selected "${clip}"`); i++) await new Promise(r => setTimeout(r, 100));
  assert.match(selected(), new RegExp(`:selected "${clip}"`), 'the simulated Set selected the sent clip');
  const line = await panel.locator('#capture-live-status').textContent();
  const tracks = status.live.setup.tracks.map(t => `${t.name} (ch ${t.channel})`).join(', ');
  assert.equal(line, `Capture tracks ready in Live: ${tracks} · playback returns to the show · last send wrote ${status.live.sent.clips.length} clip${status.live.sent.clips.length === 1 ? '' : 's'} · clip open in Live`);
  const summary = await panel.locator('p[role=status]').first().textContent();
  assert.match(summary, /^Capture \d+ · /, 'the send also shows the capture summary');
  assert.ok(await panel.locator('#capture-download').isVisible(), 'the MIDI download is offered');
  pass('Send to Live ran the host command through the bridge and the panel shows its status', {line, summary, clip,
    setup: status.live.setup, tracks: selected()});
  await panel.screenshot({path: join(output, 'capture-desktop.png')});

  await panel.locator('#capture-focus').click();
  await desktop.page.waitForTimeout(500);
  await panel.locator('#capture-focus:not([disabled])').waitFor({timeout: 15000});
  const refocused = await json('/api/capture');
  assert.equal(refocused.live.focus.status, 'executed');
  assert.equal(refocused.live.focus['clip-id'], clip);
  assert.deepEqual(desktop.problems, []);
  pass('Open clip in Live focuses the sent clip', refocused.live.focus);

  const phone = await launch('phone', {width: 390, height: 844, scale: 3, touch: true});
  const small = await capturePanel(phone.page);
  await small.locator('#capture-live-status', {hasText: 'last send wrote'}).waitFor({timeout: 15000});
  const overflow = await small.evaluate(section => section.scrollWidth - section.clientWidth);
  const pageOverflow = await phone.page.evaluate(() => document.documentElement.scrollWidth - document.documentElement.clientWidth);
  assert.equal(overflow, 0);
  assert.ok(pageOverflow <= 0, 'no horizontal page scroll at phone width: ' + pageOverflow);
  assert.deepEqual(phone.problems, []);
  pass('the panel fits a 390 px wide phone viewport and reads the saved status on open', {pageOverflow, panelOverflow: overflow});
  await small.screenshot({path: join(output, 'capture-phone.png')});
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
