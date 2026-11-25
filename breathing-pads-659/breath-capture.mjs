// Capture running cue pads breathing over the cue pad preview animation (#659).
// Run against a disposable preview-output show: node breath-capture.mjs <origin> <out-dir>
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import {createRequire} from 'node:module';

const require = createRequire(process.env.PLAYWRIGHT_FROM + '/package.json');
const {chromium} = require('playwright-core');

const origin = process.argv[2];
const out = process.argv[3];
fs.mkdirSync(path.join(out, 'frames'), {recursive: true});
const sleep = ms => new Promise(r => setTimeout(r, ms));
const read = async (p, options = {}) => {
  const response = await fetch(origin + p, {...options, headers: {accept: 'application/json', 'content-type': 'application/json'}});
  const body = await response.json();
  assert.ok(response.ok && body.status !== 'rejected', JSON.stringify(body).slice(0, 400));
  return body;
};
const show = await read('/api/state');
assert.equal(show.status.output.type, 'preview');
const session = 'breath-qa-' + Date.now();
const command = (op, body) => read('/api/command', {method: 'POST', body: JSON.stringify({
  op, epoch: show.epoch, 'show-id': show['show-id'], 'expected-epoch': show.epoch, 'runtime-id': show['runtime-id'],
  'command-id': crypto.randomUUID(), source: {adapter: 'web', 'session-id': session}, ...body})});
const launch = (cue, extra = {}) => command('cue/launch', {'cue-id': cue, launch: {mode: 'persistent'}, ...extra});
const endAll = async () => {
  for (const i of (await read('/api/state')).instances) await command('instance/end', {'instance-id': i.id}).catch(() => {});
};

const report = {origin, startedAt: new Date().toISOString()};
const browser = await chromium.launch({executablePath: '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  headless: true, args: ['--hide-scrollbars']});
try {
  const context = await browser.newContext({viewport: {width: 1440, height: 1000}, deviceScaleFactor: 2});
  const page = await context.newPage();
  const errors = [];
  page.on('console', m => m.type() === 'error' && errors.push(m.text()));
  page.on('pageerror', e => errors.push(String(e)));
  await page.goto(origin + '/perform');
  await page.locator('#cue-grid .cue-cell:not(.empty)').first().waitFor();
  await endAll();
  await sleep(2500);

  // Six running, one releasing, one queued for the next phrase.
  for (const cue of ['amber', 'ember', 'ice', 'indigo', 'orchid', 'sage']) await launch(cue);
  const moon = await launch('moon');
  await launch('spot', {grid: {phrases: 1}, 'command-id': crypto.randomUUID()});
  await page.locator('#cue-grid .cue-cell.running').nth(5).waitFor();
  await page.locator('#cue-grid .cue-cell.queued').first().waitFor();
  await sleep(1200);
  const panel = page.locator('.cue-panel');

  const states = await page.$$eval('#cue-grid .cue-cell:not(.empty)', cells => cells.map(c => ({
    name: c.querySelector('.cue-name')?.textContent, classes: c.className, label: c.getAttribute('aria-label')})));
  report.padStates = states;

  // Phase agreement with the server transport, and the breath on one pad.
  const samplePhase = async () => { const samples = [];
  for (let i = 0; i < 24; i++) {
    const s = await page.evaluate(async () => {
      const grid = document.getElementById('cue-grid');
      const pad = grid.querySelector('.cue-cell.running');
      const t0 = performance.now();
      const state = await (await fetch('/api/state')).json();
      const t1 = performance.now();
      const style = grid.style;
      return {rtt: t1 - t0, serverBeat: state.transport.beat, serverPhase: state.transport.beat % 1,
        bpm: state.transport.bpm, clientPhase: Number(style.getPropertyValue('--beat-phase')),
        breath: Number(style.getPropertyValue('--breath')),
        overlay: Number(getComputedStyle(pad, '::before').opacity),
        cueColor: pad.style.getPropertyValue('--cue-color')};
    });
    samples.push(s);
    await sleep(83);
  }
  return samples; };
  const samples = await samplePhase();
  const wrap = d => ((d % 1) + 1.5) % 1 - 0.5;
  const errs = samples.map(s => Math.abs(wrap(s.clientPhase - s.serverPhase)));
  report.phase = {samples: samples.length, meanAbsErrorBeats: errs.reduce((a, b) => a + b, 0) / errs.length,
    maxAbsErrorBeats: Math.max(...errs), meanRttMs: samples.reduce((a, s) => a + s.rtt, 0) / samples.length,
    overlayRange: [Math.min(...samples.map(s => s.overlay)), Math.max(...samples.map(s => s.overlay))],
    distinctPreviewColors: new Set(samples.map(s => s.cueColor)).size, rows: samples};

  // Recording: ~4 s of the cue panel. End Moon at the start so it releases on camera.
  await command('instance/end', {'instance-id': moon['instance-id']});
  await page.locator('#cue-grid .cue-cell.releasing').first().waitFor();
  await panel.screenshot({path: path.join(out, 'still-running-releasing-queued.png')});
  const frames = [];
  const t0 = Date.now();
  while (Date.now() - t0 < 4500) {
    const file = path.join(out, 'frames', `f${String(frames.length).padStart(4, '0')}.png`);
    await panel.screenshot({path: file});
    frames.push({file, t: Date.now() - t0});
  }
  report.recording = {frames: frames.length, seconds: frames.at(-1).t / 1000};
  fs.writeFileSync(path.join(out, 'frames.txt'), frames.map((f, i) =>
    `file '${f.file}'\nduration ${(((frames[i + 1]?.t ?? f.t + 100) - f.t) / 1000).toFixed(3)}`).join('\n') + `\nfile '${frames.at(-1).file}'\n`);

  // Tempo change: the clock follows the projected bpm.
  await command('transport/set', {transport: {bpm: 96}});
  await sleep(1500);
  const tempo = await samplePhase();
  const terrs = tempo.map(s => Math.abs(wrap(s.clientPhase - s.serverPhase)));
  report.tempoChange = {bpm: tempo[0].bpm, meanAbsErrorBeats: terrs.reduce((a, b) => a + b, 0) / terrs.length,
    maxAbsErrorBeats: Math.max(...terrs)};
  await command('transport/set', {transport: {bpm: 120}});
  await sleep(800);

  // Stills on the beat (bright) and off the beat (dim).
  for (const [name, lo, hi] of [['still-on-beat', 0.0, 0.08], ['still-off-beat', 0.45, 0.55]]) {
    await page.waitForFunction(([lo, hi]) => {
      const p = Number(document.getElementById('cue-grid').style.getPropertyValue('--beat-phase'));
      return p >= lo && p <= hi;
    }, [lo, hi], {polling: 'raf'});
    await panel.screenshot({path: path.join(out, name + '.png')});
  }

  // Frame cost: 64 real running pads on a full page made from the show's cues.
  const cueIds = (await read('/api/state')).cues.map(c => c.id);
  const cells = Array.from({length: 64}, (_, i) => ({x: i % 8, y: Math.floor(i / 8), 'cue-id': cueIds[i % cueIds.length]}));
  await command('page/put', {page: {id: 'breath-64', name: 'Breath 64', cells}});
  await endAll();
  await sleep(2500);
  await page.getByRole('button', {name: 'Breath 64'}).click();
  await page.locator('#cue-grid [data-x="7"][data-y="7"]:not(.empty)').waitFor();
  const cdp = await context.newCDPSession(page);
  await cdp.send('Performance.enable');
  const measure = async label => {
    const m0 = Object.fromEntries((await cdp.send('Performance.getMetrics')).metrics.map(m => [m.name, m.value]));
    const raf = await page.evaluate(() => new Promise(resolve => {
      const ts = []; const step = t => { ts.push(t); if (ts.length < 301) requestAnimationFrame(step); else resolve(ts); };
      requestAnimationFrame(step);
    }));
    const m1 = Object.fromEntries((await cdp.send('Performance.getMetrics')).metrics.map(m => [m.name, m.value]));
    const gaps = raf.slice(1).map((t, i) => t - raf[i]).sort((a, b) => a - b);
    const wall = (raf.at(-1) - raf[0]) / 1000;
    const per = k => 1000 * (m1[k] - m0[k]) / (raf.length - 1);
    return {label, running: await page.locator('#cue-grid .cue-cell.running').count(), frames: raf.length - 1,
      wallSeconds: wall, rafMedianMs: gaps[gaps.length >> 1], rafP95Ms: gaps[Math.floor(gaps.length * .95)],
      perFrameMs: {recalcStyle: per('RecalcStyleDuration'), layout: per('LayoutDuration'),
        script: per('ScriptDuration'), task: per('TaskDuration')},
      styleRecalcs: m1.RecalcStyleCount - m0.RecalcStyleCount, layouts: m1.LayoutCount - m0.LayoutCount};
  };
  report.frameCost = [await measure('idle page, clock stopped')];
  for (const cue of cueIds) await launch(cue);
  await page.waitForFunction(() => document.querySelectorAll('#cue-grid .cue-cell.running').length === 64);
  await sleep(1500);
  report.frameCost.push(await measure('64 running pads breathing'));
  await panel.screenshot({path: path.join(out, 'still-64-running.png')});

  // Reduced motion: static running indicator, clock loop not scheduled.
  const reduced = await browser.newContext({viewport: {width: 1440, height: 1000}, deviceScaleFactor: 2, reducedMotion: 'reduce'});
  const rpage = await reduced.newPage();
  await rpage.goto(origin + '/perform');
  await rpage.getByRole('button', {name: 'Breath 64'}).click();
  await rpage.waitForFunction(() => document.querySelectorAll('#cue-grid .cue-cell.running').length === 64);
  const rsamples = [];
  for (let i = 0; i < 10; i++) {
    rsamples.push(await rpage.evaluate(() => {
      const grid = document.getElementById('cue-grid');
      return {phase: grid.style.getPropertyValue('--beat-phase'),
        overlay: getComputedStyle(grid.querySelector('.cue-cell.running'), '::before').opacity};
    }));
    await sleep(120);
  }
  report.reducedMotion = {distinctOverlay: [...new Set(rsamples.map(s => s.overlay))],
    distinctPhaseWrites: new Set(rsamples.map(s => s.phase)).size, samples: rsamples};
  await rpage.locator('.cue-panel').screenshot({path: path.join(out, 'still-reduced-motion.png')});
  await reduced.close();

  await endAll();
  report.consoleErrors = errors;
} finally {
  await browser.close();
}
fs.writeFileSync(path.join(out, 'report.json'), JSON.stringify(report, null, 2));
console.log(JSON.stringify({...report, phase: {...report.phase, rows: undefined}, padStates: report.padStates?.length}, null, 2));
