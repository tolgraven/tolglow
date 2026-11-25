// Verify CSS-animation breathing phase, frame cost with 64 pads and reduced motion (#659 review).
import fs from 'node:fs';
import {createRequire} from 'node:module';
const require = createRequire(process.env.PLAYWRIGHT_FROM + '/package.json');
const {chromium} = require('playwright-core');
const [origin, outFile] = process.argv.slice(2);
const sleep = ms => new Promise(r => setTimeout(r, ms));
const read = async (p, o = {}) => (await fetch(origin + p, {...o, headers: {accept: 'application/json', 'content-type': 'application/json'}})).json();
const show = await read('/api/state');
if (show.status.output.type !== 'preview') throw new Error('not preview output');
const command = (op, body) => read('/api/command', {method: 'POST', body: JSON.stringify({op, epoch: show.epoch,
  'show-id': show['show-id'], 'runtime-id': show['runtime-id'], 'command-id': crypto.randomUUID(),
  source: {adapter: 'web', 'session-id': 'breath-verify'}, ...body})});
const endAll = async () => { for (const i of (await read('/api/state')).instances) await command('instance/end', {'instance-id': i.id}); };
const wrap = d => ((d % 1) + 1.5) % 1 - 0.5;
const report = {};
const browser = await chromium.launch({executablePath: '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome', headless: true});
try {
  const context = await browser.newContext({viewport: {width: 1440, height: 1000}});
  const page = await context.newPage();
  const errors = [];
  page.on('console', m => m.type() === 'error' && errors.push(m.text()));
  page.on('pageerror', e => errors.push(String(e)));
  await page.goto(origin + '/perform');
  await page.locator('#cue-grid .cue-cell:not(.empty)').first().waitFor();
  await endAll(); await sleep(2000);
  const cueIds = show.cues.map(c => c.id);
  for (const cue of ['amber', 'ember', 'ice']) await command('cue/launch', {'cue-id': cue, launch: {mode: 'persistent'}});
  await command('cue/launch', {'cue-id': 'spot', launch: {mode: 'persistent'}, grid: {phrases: 1}});
  await page.waitForFunction(() => document.getElementById('cue-grid').dataset.breath === 'live');
  await sleep(1000);
  const samplePhase = async () => {
    const rows = [];
    for (let i = 0; i < 24; i++) {
      rows.push(await page.evaluate(async () => {
        const grid = document.getElementById('cue-grid');
        const anims = grid.getAnimations({subtree: true});
        const breath = anims.filter(a => a.animationName === 'pad-breath');
        const flash = anims.find(a => a.animationName === 'pad-queued-dot');
        const t0 = performance.now();
        const state = await (await fetch('/api/state')).json();
        const dur = breath[0].effect.getComputedTiming().duration;
        return {rtt: performance.now() - t0, serverPhase: state.transport.beat % 1, bpm: state.transport.bpm,
          phases: breath.map(a => (a.currentTime % dur) / dur), duration: dur,
          flashPhase: flash ? (flash.currentTime % (dur / 2)) / (dur / 2) : null,
          overlay: Number(getComputedStyle(grid.querySelector('.cue-cell.running'), '::before').opacity),
          mode: grid.dataset.breath, beatDuration: grid.style.getPropertyValue('--beat-duration')};
      }));
      await sleep(83);
    }
    const errs = rows.flatMap(r => r.phases.map(p => Math.abs(wrap(p - r.serverPhase))));
    const flashErrs = rows.filter(r => r.flashPhase != null).map(r => Math.abs(wrap(r.flashPhase - (2 * r.serverPhase) % 1)));
    return {bpm: rows[0].bpm, duration: rows[0].duration, beatDuration: rows[0].beatDuration, pads: rows[0].phases.length,
      meanAbsErrorBeats: errs.reduce((a, b) => a + b, 0) / errs.length, maxAbsErrorBeats: Math.max(...errs),
      queuedFlashMaxErrorHalfBeats: flashErrs.length ? Math.max(...flashErrs) : null,
      overlayRange: [Math.min(...rows.map(r => r.overlay)), Math.max(...rows.map(r => r.overlay))],
      meanRttMs: rows.reduce((a, r) => a + r.rtt, 0) / rows.length};
  };
  report.phase120 = await samplePhase();
  await command('transport/set', {transport: {bpm: 96}});
  await sleep(1500);
  report.phase96 = await samplePhase();
  await command('transport/set', {transport: {'playing?': false}});
  await sleep(800);
  report.paused = await page.evaluate(() => {
    const grid = document.getElementById('cue-grid');
    return {mode: grid.dataset.breath, animations: grid.getAnimations({subtree: true}).length,
      overlay: getComputedStyle(grid.querySelector('.cue-cell.running'), '::before').opacity,
      queuedDot: grid.querySelector('.cue-cell.queued .cue-dot') && getComputedStyle(grid.querySelector('.cue-cell.queued .cue-dot')).opacity};
  });
  await command('transport/set', {transport: {'playing?': true, bpm: 120}});

  // 64 running pads.
  const cells = Array.from({length: 64}, (_, i) => ({x: i % 8, y: Math.floor(i / 8), 'cue-id': cueIds[i % cueIds.length]}));
  await command('page/put', {page: {id: 'breath-64', name: 'Breath 64', cells}});
  await endAll(); await sleep(2000);
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
    const per = k => 1000 * (m1[k] - m0[k]) / (raf.length - 1);
    return {label, running: await page.locator('#cue-grid .cue-cell.running').count(),
      rafMedianMs: gaps[gaps.length >> 1], rafP95Ms: gaps[Math.floor(gaps.length * .95)],
      perFrameMs: {recalcStyle: per('RecalcStyleDuration'), layout: per('LayoutDuration'), script: per('ScriptDuration'), task: per('TaskDuration')},
      styleRecalcs: m1.RecalcStyleCount - m0.RecalcStyleCount};
  };
  report.frameCost = [await measure('idle page, nothing running')];
  for (const cue of cueIds) await command('cue/launch', {'cue-id': cue, launch: {mode: 'persistent'}});
  await page.waitForFunction(() => document.querySelectorAll('#cue-grid .cue-cell.running').length === 64);
  await sleep(1500);
  report.frameCost.push(await measure('64 running pads breathing'));
  report.phase64 = await samplePhase();

  // Reduced motion, switched at runtime.
  await page.emulateMedia({reducedMotion: 'reduce'});
  await sleep(300);
  report.reducedMotion = await page.evaluate(async () => {
    const grid = document.getElementById('cue-grid');
    const out = [];
    for (let i = 0; i < 6; i++) {
      out.push(getComputedStyle(grid.querySelector('.cue-cell.running'), '::before').opacity);
      await new Promise(r => setTimeout(r, 120));
    }
    return {mode: grid.dataset.breath, animations: grid.getAnimations({subtree: true}).length, overlays: [...new Set(out)]};
  });
  await page.emulateMedia({reducedMotion: 'no-preference'});
  await sleep(300);
  report.afterReducedMotion = await page.evaluate(() => document.getElementById('cue-grid').dataset.breath);
  await endAll();
  report.consoleErrors = errors;
} finally { await browser.close(); }
fs.writeFileSync(outFile, JSON.stringify(report, null, 2));
console.log(JSON.stringify(report, null, 1));
