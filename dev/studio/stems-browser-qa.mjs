// Browser check for instrument stems (#675) against tolglow.qa.stems-studio.
//
//   lein run -m tolglow.qa.stems-studio 4670 target/stems-qa
//   CHROMIUM_PATH=... node dev/studio/stems-browser-qa.mjs http://127.0.0.1:4670 target/stems-qa/out
//
// Uses the production Studio build in headless Chrome with the real GPU. It
// waits for the paced recording to produce onsets on all four stems, checks
// the stem cards against /api/audio, routes the snare to a cue from a card and
// confirms the subscriber reached the show, then writes stills at desktop and
// phone widths plus report.json. Headless desktop Chrome is not evidence for a
// foreground display or iPad Safari.
import assert from 'node:assert/strict';
import {mkdirSync, writeFileSync} from 'node:fs';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const [origin = 'http://127.0.0.1:4670', output = 'target/stems-qa/out'] = process.argv.slice(2);
const executablePath = process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
mkdirSync(output, {recursive: true});
const report = {origin, startedAt: new Date().toISOString(), checks: {}};
const open = [];
const sleep = ms => new Promise(done => setTimeout(done, ms));
const pass = (name, detail = true) => { report.checks[name] = detail; console.log('ok', name); };

async function json(path) {
  const response = await fetch(origin + path, {headers: {Accept: 'application/json'}});
  assert.equal(response.status, 200, path);
  return response.json();
}

async function until(label, predicate, timeout = 30000) {
  const started = Date.now();
  for (;;) {
    const value = await predicate();
    if (value) return value;
    if (Date.now() - started > timeout) throw new Error('Timed out waiting for ' + label);
    await sleep(250);
  }
}

async function launch(name, {width, height, scale = 1, touch = false}) {
  const profile = resolve(output, '..', 'chrome-profile-' + name);
  const context = await chromium.launchPersistentContext(profile, {
    executablePath, headless: true,
    args: ['--headless=new', '--use-angle=metal', '--enable-gpu', '--ignore-gpu-blocklist', '--hide-scrollbars'],
    viewport: {width, height}, deviceScaleFactor: scale, hasTouch: touch,
  });
  open.push(context);
  const page = context.pages()[0] || await context.newPage();
  const problems = [];
  page.on('pageerror', error => problems.push(error.message));
  page.on('console', message => { if (message.type() === 'error') problems.push('console: ' + message.text()); });
  return {context, page, problems};
}

const cards = page => page.evaluate(() => [...document.querySelectorAll('.stem-card')].map(card => ({
  stem: card.querySelector('.stem-head strong')?.textContent,
  meta: card.querySelector('.stem-meta')?.textContent,
  level: Number(card.querySelector('meter')?.value),
  metrics: Object.fromEntries([...card.querySelectorAll('.stem-metrics div')].map(m => [m.querySelector('small').textContent, m.querySelector('strong').textContent])),
  routes: [...card.querySelectorAll('.stem-route span')].map(s => s.textContent),
  overflow: card.scrollWidth - card.clientWidth,
})));

async function openStems(page) {
  await page.goto(origin + '/connections', {waitUntil: 'domcontentloaded'});
  const panel = page.locator('.stem-panel');
  await panel.waitFor({timeout: 20000});
  await until('stem cards', async () => (await cards(page)).length === 4, 30000);
  return panel;
}

try {
  const audio = await until('onsets on every stem', async () => {
    const status = await json('/api/audio');
    const stems = status.features?.stems;
    return stems && ['kick', 'snare', 'bass', 'guitar'].every(id => stems[id]?.['onset-count'] >= 2) && status;
  }, 60000);
  pass('the paced recording produces onsets on all four stems', Object.fromEntries(
    Object.entries(audio.features.stems).map(([id, s]) => [id, {onsets: s['onset-count'], bleed: s['bleed-count'], channel: s.channel}])));

  const desktop = await launch('desktop', {width: 1440, height: 1400});
  const panel = await openStems(desktop.page);
  const shown = await cards(desktop.page);
  assert.deepEqual(shown.map(c => c.stem), ['kick', 'snare', 'bass', 'guitar']);
  assert.deepEqual(shown.map(c => c.meta), ['Ch 1 · Kick', 'Ch 2 · Snare', 'Ch 3 · Bass', 'Ch 4 · Guitar']);
  assert.ok(shown.every(c => c.overflow <= 1), 'no horizontal overflow inside a card');
  assert.ok(shown[0].routes.some(r => /launch Flash/i.test(r)), 'kick shows its cue route: ' + shown[0].routes);
  assert.ok(shown[3].routes.some(r => /Level → Breathe · level/.test(r)), 'guitar shows its level route: ' + shown[3].routes);
  assert.match(shown[0].metrics.Gate, /ms @ 120 BPM/);
  pass('cards show stems, gates and routes from the capture status and show data', shown);

  // The onset counts in Studio follow the server's.
  const before = Number(shown[0].metrics.Onsets);
  await until('the kick onset count to advance in Studio', async () => Number((await cards(desktop.page))[0].metrics.Onsets) > before, 10000);
  const server = (await json('/api/audio')).features.stems.kick['onset-count'];
  const studio = Number((await cards(desktop.page))[0].metrics.Onsets);
  assert.ok(Math.abs(server - studio) <= 4, `studio ${studio} vs server ${server}`);
  pass('onset counts advance live and match the server', {studio, server});

  // Route the snare from its card, through the same subscriber save as the REPL.
  const snare = desktop.page.locator('.stem-card').nth(1);
  await snare.getByLabel('snare cue').selectOption('ice');
  await snare.getByRole('button', {name: 'Launch on onset'}).click();
  await until('the snare route to appear on its card', async () =>
    (await cards(desktop.page))[1].routes.some(r => /launch Ice/i.test(r)), 15000);
  pass('a card routes snare onsets to a cue and lists the new subscriber');

  await panel.scrollIntoViewIfNeeded();
  await sleep(500);
  await panel.screenshot({path: join(output, 'stems-desktop.png')});
  await desktop.page.locator('.audio-connection').screenshot({path: join(output, 'stems-audio-panel.png')});
  assert.deepEqual(desktop.problems, []);
  await desktop.context.close();

  const phone = await launch('phone', {width: 390, height: 1600, scale: 2, touch: true});
  const phonePanel = await openStems(phone.page);
  const overflow = await phone.page.evaluate(() => document.documentElement.scrollWidth - innerWidth);
  assert.ok(overflow <= 1, 'no horizontal page scroll at phone width: ' + overflow);
  await phonePanel.scrollIntoViewIfNeeded();
  await sleep(400);
  await phonePanel.locator('.stem-card').first().screenshot({path: join(output, 'stems-phone.png')});
  assert.deepEqual(phone.problems, []);
  pass('phone width fits without horizontal scroll', {overflow});
  await phone.context.close();
  report.result = 'pass';
} catch (error) {
  report.result = 'fail';
  report.error = String(error && error.stack || error);
  console.error(error);
  process.exitCode = 1;
} finally {
  for (const context of open) await context.close().catch(() => {});
  report.finishedAt = new Date().toISOString();
  writeFileSync(join(output, 'report.json'), JSON.stringify(report, null, 2));
}
