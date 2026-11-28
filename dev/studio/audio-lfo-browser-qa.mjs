// Browser check for audio-shaped LFOs (#676) against tolglow.qa.audio-lfo-studio.
//
//   lein run -m tolglow.qa.audio-lfo-studio 4550 target/audio-lfo-qa/control
//   CHROMIUM_PATH=... node dev/studio/audio-lfo-browser-qa.mjs http://127.0.0.1:4550 target/audio-lfo-qa/out target/audio-lfo-qa/control
//
// Uses the production Studio build in headless Chrome with the real GPU. It
// waits for both LFOs to commit shapes from the paced input, checks the panel
// against /api/state, applies a configuration change through the panel, mutes
// the input to show the silence policy, and writes stills at desktop and
// phone widths plus report.json. Headless desktop Chrome is not evidence for
// a foreground display or iPad Safari.
import assert from 'node:assert/strict';
import {existsSync, mkdirSync, rmSync, writeFileSync} from 'node:fs';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const [origin = 'http://127.0.0.1:4550', output = 'target/audio-lfo-qa/out', control = 'target/audio-lfo-qa/control'] = process.argv.slice(2);
const executablePath = process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
mkdirSync(output, {recursive: true});
mkdirSync(control, {recursive: true});
const report = {origin, startedAt: new Date().toISOString(), checks: {}};
const open = [];
const sleep = ms => new Promise(done => setTimeout(done, ms));
const pass = (name, detail = true) => { report.checks[name] = detail; console.log('ok', name); };
const flag = (name, on) => { const file = join(control, name); if (on) writeFileSync(file, ''); else if (existsSync(file)) rmSync(file); };

async function state() {
  const response = await fetch(origin + '/api/state', {headers: {Accept: 'application/json'}});
  assert.equal(response.status, 200);
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

const cards = page => page.evaluate(() => [...document.querySelectorAll('.audio-lfo-card')].map(card => ({
  id: card.dataset.lfoId, status: card.dataset.status,
  name: card.querySelector('.audio-lfo-head strong')?.textContent,
  band: card.querySelector('.audio-lfo-band')?.textContent,
  metrics: Object.fromEntries([...card.querySelectorAll('.audio-lfo-metric')].map(m => [m.querySelector('small').textContent, m.querySelector('strong').textContent])),
  source: card.querySelector('.source polyline')?.getAttribute('points')?.split(' ').length ?? 0,
  current: card.querySelector('polyline.current')?.getAttribute('points')?.split(' ').length ?? 0,
  target: card.querySelector('polyline.target')?.getAttribute('points')?.split(' ').length ?? 0,
  caption: card.querySelector('.shape figcaption small')?.textContent,
  overflow: card.scrollWidth - card.clientWidth,
})));

try {
  const committed = await until('both LFOs to commit a shape', async () => {
    const lfos = (await state())['audio-lfos'] || [];
    return lfos.length === 2 && lfos.every(l => l.commits >= 2) && lfos;
  }, 60000);
  pass('owner committed shapes from the paced input', committed.map(l => ({id: l.id, commits: l.commits, status: l.status})));

  const desktop = await launch('desktop', {width: 1440, height: 1100});
  await desktop.page.goto(origin + '/connections', {waitUntil: 'domcontentloaded'});
  const panel = desktop.page.locator('.audio-lfo-panel');
  await panel.waitFor({timeout: 20000});
  await until('cards to render', async () => {
    const shown = await cards(desktop.page);
    return shown.length === 2 && shown.every(c => c.current > 8 && c.source > 8);
  });
  const shown = await cards(desktop.page);
  assert.deepEqual(shown.map(c => c.id), ['bass-envelope', 'synth-cycle']);
  assert.equal(shown[0].band, 'bass 60 Hz–250 Hz');
  assert.ok(shown.every(c => c.status === 'ok'), 'both LFOs are shaping');
  assert.ok(shown.every(c => c.overflow <= 1), 'no horizontal overflow inside a card');
  assert.match(shown[1].metrics.Pitch, /Hz$/);
  assert.equal(shown[1].metrics.Waveform, 'audio/synth-cycle');
  pass('panel shows both LFOs from the shared projection', shown);

  await panel.scrollIntoViewIfNeeded();
  await sleep(400);
  await panel.screenshot({path: join(output, 'audio-lfo-desktop.png')});

  // Picker parity: the waveform is in the catalog every LFO picker reads.
  const catalog = (await state())['parameter-waveforms'].map(w => w.id);
  assert.ok(catalog.includes('audio/bass-envelope') && catalog.includes('audio/synth-cycle'));
  pass('both waveforms are in the LFO picker catalog', catalog.filter(id => id.startsWith('audio/')));

  // A configuration change from the panel goes through the owner command.
  const card = desktop.page.locator('.audio-lfo-card[data-lfo-id="bass-envelope"]');
  await card.getByLabel('Morph over').selectOption('bars-2');
  await card.getByRole('button', {name: 'Apply'}).click();
  await until('the owner to commit the new morph time', async () => {
    const lfo = (await state())['audio-lfos'].find(l => l.id === 'bass-envelope');
    return lfo.config.morph.bars === 2;
  });
  pass('Apply sends :audio-lfo/configure and the projection follows');

  // Linked knobs (#676): link a new knob to the band from the card, turn it there and from the REPL side.
  // The link form's selects in order: field, from, to, knob.
  const knobForm = card.locator('.audio-lfo-knob-form select');
  const bandOf = async () => (await state())['audio-lfos'].find(l => l.id === 'bass-envelope').config;
  await knobForm.nth(0).selectOption('band');
  await knobForm.nth(1).selectOption('sub');
  await knobForm.nth(2).selectOption('full');
  await knobForm.nth(3).selectOption('new');
  await card.getByRole('button', {name: 'Link knob'}).click();
  const knob = await until('the new knob with a band link', async () => {
    const knobs = (await state()).knobs?.knobs || [];
    return knobs.find(k => k.links.some(l => l.destination?.kind === 'audio-lfo' && l.destination?.field === 'band'));
  });
  const slider = card.locator('.audio-lfo-knob input[type=range]').first();
  await slider.waitFor({timeout: 10000});
  await slider.fill('0.6');
  await until('the card turn to reach the band', async () => (await bandOf()).band === 'mid');
  await until('the card to show the new band', async () => (await cards(desktop.page))[0].band === 'mid 800 Hz–2.5 kHz');
  pass('a knob linked on the card turns the band through :knob/set', {knob: knob.id, band: 'mid'});

  writeFileSync(join(control, 'knob-turn'), '0.2');
  await until('the REPL turn to reach the band', async () => (await bandOf()).band === 'bass');
  await until('the open card to follow the REPL turn', async () => {
    const value = Number(await slider.inputValue());
    return Math.abs(value - 0.2) < 1e-6 && (await cards(desktop.page))[0].band === 'bass 60 Hz–250 Hz';
  });
  pass('a REPL turn moves the open card slider and band');

  await knobForm.nth(0).selectOption('morph');
  await knobForm.nth(1).selectOption('1');
  // Two bars is already eight beats, so top the link at four to see it move.
  await knobForm.nth(2).selectOption('4');
  await knobForm.nth(3).selectOption(knob.id);
  await card.getByRole('button', {name: 'Link knob'}).click();
  await until('the morph link on the same knob', async () =>
    ((await state()).knobs.knobs.find(k => k.id === knob.id)?.links.length || 0) === 2);
  await slider.fill('1');
  await until('band and morph at the top of the knob', async () => {
    const config = await bandOf();
    return config.band === 'full' && config.morph.beats === 4;
  });
  await until('Morph over to show the knob-set time', async () =>
    (await card.getByLabel('Morph over').locator('option:checked').textContent()) === '4 beats');
  await card.scrollIntoViewIfNeeded();
  await sleep(300);
  await card.screenshot({path: join(output, 'audio-lfo-knobs.png')});
  pass('one knob drives band and morph time; the form keeps the knob-set time');

  // Silence: the input mutes, the hold policy keeps the shape and says why.
  flag('silence', true);
  const silent = await until('a silent window', async () => {
    const lfos = (await state())['audio-lfos'];
    return lfos.every(l => l.status === 'silent') && lfos;
  }, 20000);
  await until('the panel to show silence', async () => {
    const shown = await cards(desktop.page);
    return shown.length === 2 && shown.every(c => c.status === 'silent');
  });
  await sleep(300);
  await panel.screenshot({path: join(output, 'audio-lfo-silent.png')});
  pass('silence holds the last shape', silent.map(l => ({id: l.id, status: l.status, target: l.to.slice(0, 4)})));
  flag('silence', false);
  assert.deepEqual(desktop.problems, []);
  await desktop.context.close();

  const phone = await launch('phone', {width: 390, height: 1400, scale: 2, touch: true});
  await phone.page.goto(origin + '/connections', {waitUntil: 'domcontentloaded'});
  const phonePanel = phone.page.locator('.audio-lfo-panel');
  await phonePanel.waitFor({timeout: 20000});
  await until('phone cards', async () => (await cards(phone.page)).length === 2);
  const overflow = await phone.page.evaluate(() => document.documentElement.scrollWidth - innerWidth);
  assert.ok(overflow <= 1, 'no horizontal page scroll at phone width: ' + overflow);
  await phonePanel.locator('.audio-lfo-card').first().scrollIntoViewIfNeeded();
  await sleep(300);
  await phonePanel.locator('.audio-lfo-card').first().screenshot({path: join(output, 'audio-lfo-phone.png')});
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
  flag('silence', false);
  report.finishedAt = new Date().toISOString();
  writeFileSync(join(output, 'report.json'), JSON.stringify(report, null, 2));
}
