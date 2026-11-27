// Main-thread cost per received show frame while the Room renders (#783).
//
//   node dev/studio/room-frame-cost.mjs <studio-origin> [label] [seconds] [cpuprofile-out]
//
// With ROOM_HEADS=<n>, first replace the host's rig with n moving heads through
// the ordinary rig API and launch the sweep, ember and breathe cues (the dense
// scene used in #783). Run only against a capture-only host you own; this
// changes its show. Headless Chrome with the real GPU; set CHROMIUM_PATH.
// Reports CDP TaskDuration/ScriptDuration per received state message, the
// synchronous socket handler time and the engine.update time. Not foreground,
// Safari or iPad evidence.
import {randomUUID} from 'node:crypto';
import {chromium} from 'playwright-core';
import {mkdir} from 'node:fs/promises';
const [origin, label = 'run', seconds = '10', profileOut] = process.argv.slice(2);
const dir = 'target/room-frame-cost/chrome-' + label;
await mkdir(dir, {recursive: true});
if (process.env.ROOM_HEADS) {
  const s = await (await fetch(origin + '/api/state')).json();
  const rig = structuredClone(s.rig), base = rig.fixtures[0], n = Number(process.env.ROOM_HEADS);
  rig.room = {width: 24, height: 8, depth: 20};
  rig.trusses = [0, 1, 2, 3].map(i => ({id: 'truss-' + i, position: [0, 6, -6 + i * 4], length: 20}));
  rig.fixtures = Array.from({length: n}, (_, i) => ({...structuredClone(base), id: 'head-' + (i + 1), name: 'Beam ' + (i + 1),
    position: [-9.5 + (i % 24) * 0.82, 5.5, -6 + Math.floor(i / 24) * 4],
    universe: 1 + Math.floor(i / 39), address: 1 + (i % 39) * 13}));
  const saved = await fetch(origin + '/api/rig', {method: 'POST', headers: {'Content-Type': 'application/json'}, body: JSON.stringify(rig)});
  if (!saved.ok) throw new Error('The host refused the rig: ' + saved.status);
  for (const cue of ['sweep', 'ember', 'breathe']) {
    const now = await (await fetch(origin + '/api/state')).json();
    const reply = await (await fetch(origin + '/api/command', {method: 'POST', headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({op: 'cue/launch', 'cue-id': cue, launch: {mode: 'persistent'}, 'runtime-id': now['runtime-id'],
        'show-id': now['show-id'], epoch: now.epoch, 'expected-epoch': now.epoch, 'command-id': randomUUID(),
        source: {type: 'repl', 'session-id': 'room-frame-cost'}})})).json();
    if ((reply.ack || reply).status !== 'accepted') throw new Error('The host rejected ' + cue);
  }
}

const context = await chromium.launchPersistentContext(dir, {executablePath: process.env.CHROMIUM_PATH,
  headless: true, args: ['--headless=new', '--use-angle=metal', '--enable-gpu'], viewport: {width: 1440, height: 900}});
try {
  const page = context.pages()[0] || await context.newPage();
  await page.addInitScript(() => {
    const stats = window.__bench = {messages: 0, onmessageMs: 0, bytes: 0};
    const desc = Object.getOwnPropertyDescriptor(WebSocket.prototype, 'onmessage');
    Object.defineProperty(WebSocket.prototype, 'onmessage', {configurable: true, get: desc.get,
      set(fn) { desc.set.call(this, fn && function(ev) { const t = performance.now(); try { return fn.call(this, ev); }
        finally { if (String(this.url).includes('/api/state/stream')) { stats.messages++; stats.bytes += ev.data.length; stats.onmessageMs += performance.now() - t; } } }); }});
  });
  await page.goto(origin, {waitUntil: 'domcontentloaded'});
  await page.waitForFunction(() => document.querySelector('#connection-label')?.textContent === 'Studio connected', null, {timeout: 60000});
  // Saved UI preferences may reopen another view; the Room lives on Perform.
  await page.locator('a[data-view=perform]').click();
  await page.locator('#preview').scrollIntoViewIfNeeded();
  await page.waitForFunction(() => document.querySelector('#preview')?.tolglowPreview?.ready, null, {timeout: 90000});
  await page.waitForTimeout(4000);
  await page.evaluate(() => {
    const e = document.querySelector('#preview').tolglowPreview, s = window.__bench;
    s.updates = 0; s.updateMs = 0;
    const original = e.update;
    e.update = function(frame) { const t = performance.now(); try { return original.call(this, frame); } finally { s.updates++; s.updateMs += performance.now() - t; } };
    s.mutations = 0;
    new MutationObserver(list => { s.mutations += list.length; }).observe(document.querySelector('#main'), {subtree: true, childList: true, characterData: true, attributes: true});
  });
  const cdp = await context.newCDPSession(page);
  await cdp.send('Performance.enable');
  const metrics = async () => Object.fromEntries((await cdp.send('Performance.getMetrics')).metrics.map(m => [m.name, m.value]));
  if (profileOut) { await cdp.send('Profiler.enable'); await cdp.send('Profiler.setSamplingInterval', {interval: 200}); }
  const before = await metrics();
  const s0 = await page.evaluate(() => ({...window.__bench}));
  if (profileOut) await cdp.send('Profiler.start');
  await page.waitForTimeout(Number(seconds) * 1000);
  let profile;
  if (profileOut) profile = (await cdp.send('Profiler.stop')).profile;
  const after = await metrics();
  const s1 = await page.evaluate(() => ({...window.__bench}));
  const frames = s1.messages - s0.messages;
  const per = k => +(((after[k] - before[k]) * 1000) / frames).toFixed(3);
  const telemetry = await page.evaluate(() => { const t = document.querySelector('#preview').tolglowPreview.getTelemetry(); return {thread: t.thread, renderer: t.deviceRenderer, activeLights: t.activeLights, fixtures: document.querySelector('#fixture-count')?.textContent}; });
  const result = {label, seconds: Number(seconds), frames, framesPerSecond: +(frames / Number(seconds)).toFixed(1),
    avgMessageBytes: Math.round((s1.bytes - s0.bytes) / frames),
    perFrameMs: {task: per('TaskDuration'), script: per('ScriptDuration'), layout: per('LayoutDuration'), style: per('RecalcStyleDuration'),
      onmessage: +((s1.onmessageMs - s0.onmessageMs) / frames).toFixed(3), engineUpdate: +((s1.updateMs - (s0.updateMs || 0)) / Math.max(1, s1.updates)).toFixed(3)},
    engineUpdatesPerFrame: +(s1.updates / frames).toFixed(2), domMutationsPerFrame: +(s1.mutations / frames).toFixed(1),
    mainThreadBusyPct: +(((after.TaskDuration - before.TaskDuration) / Number(seconds)) * 100).toFixed(1), telemetry};
  console.log(JSON.stringify(result, null, 1));
  if (profileOut) { const {writeFile} = await import('node:fs/promises'); await writeFile(profileOut, JSON.stringify(profile)); }
} finally { await context.close(); }
