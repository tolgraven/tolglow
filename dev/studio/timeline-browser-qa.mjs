// Browser check for the musical timeline (#646) against tolglow.qa.timeline.
//
//   lein run -m tolglow.qa.timeline 4191 target/timeline-qa/control
//   CHROMIUM_PATH=... node dev/studio/timeline-browser-qa.mjs http://127.0.0.1:4191 target/timeline-qa/out target/timeline-qa/control
//
// Uses the production Studio build, headless Chrome and the real GPU. It
// asserts the layout and behaviour, writes stills, a recording (timeline.mp4,
// through the system ffmpeg; FFMPEG_PATH overrides it) and report.json.
// Headless desktop Chrome is not evidence for a foreground display or iPad Safari.
import assert from 'node:assert/strict';
import {spawnSync} from 'node:child_process';
import {existsSync, mkdirSync, rmSync, writeFileSync} from 'node:fs';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const [origin = 'http://127.0.0.1:4191', output = 'target/timeline-qa/out', control = 'target/timeline-qa/control'] = process.argv.slice(2);
const only = process.env.TIMELINE_QA_ONLY;
const executablePath = process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const ffmpeg = process.env.FFMPEG_PATH || '/opt/homebrew/bin/ffmpeg';
mkdirSync(output, {recursive: true});
mkdirSync(control, {recursive: true});
const report = {origin, startedAt: new Date().toISOString(), checks: {}, measurements: {}};
const sleep = ms => new Promise(done => setTimeout(done, ms));
const flag = (name, on) => {
  const file = join(control, name);
  if (on) writeFileSync(file, '');
  else if (existsSync(file)) rmSync(file);
};
const pass = (name, detail = true) => { report.checks[name] = detail; console.log('ok', name); };

async function state() {
  const response = await fetch(origin + '/api/state', {headers: {Accept: 'application/json'}});
  assert.equal(response.status, 200);
  return response.json();
}

async function launch(name, {width, height, touch = false, scale = 1}) {
  const profile = resolve(output, '..', 'chrome-profile-' + name + '-' + Date.now());
  const context = await chromium.launchPersistentContext(profile, {
    executablePath, headless: true,
    args: ['--headless=new', '--use-angle=metal', '--enable-gpu', '--ignore-gpu-blocklist', '--hide-scrollbars',
           '--autoplay-policy=no-user-gesture-required'],
    viewport: {width, height}, deviceScaleFactor: scale, hasTouch: touch, isMobile: false,
  });
  const page = context.pages()[0] || await context.newPage();
  const problems = [];
  page.on('pageerror', error => problems.push(error.message));
  page.on('console', message => { if (message.type() === 'error') problems.push('console: ' + message.text()); });
  return {context, page, problems, profile};
}

// Geometry of everything the timeline draws, in viewport pixels, from one frame.
const snapshot = page => page.evaluate(() => {
  const box = el => { const r = el.getBoundingClientRect(); return {x: r.x, y: r.y, w: r.width, h: r.height, cx: r.x + r.width / 2}; };
  const all = selector => [...document.querySelectorAll('#timeline-panel ' + selector)];
  const viewport = document.querySelector('#timeline-panel .timeline-viewport');
  const now = document.querySelector('#timeline-panel .timeline-now');
  const canvas = document.querySelector('#timeline-panel canvas');
  let audio = null;
  if (canvas && canvas.width) {
    const data = canvas.getContext('2d').getImageData(0, 0, canvas.width, canvas.height).data;
    const half = Math.floor(canvas.width / 2) + 2;
    let left = 0, right = 0, firstX = canvas.width, lastX = 0;
    for (let y = 0; y < canvas.height; y += 2) for (let x = 0; x < canvas.width; x++) {
      if (data[(y * canvas.width + x) * 4 + 3] > 0) { if (x < half) left++; else right++; firstX = Math.min(firstX, x); lastX = Math.max(lastX, x); }
    }
    audio = {left, right, firstX: firstX / canvas.width, lastX: lastX / canvas.width, width: canvas.width, height: canvas.height};
  }
  return {
    width: innerWidth, overflow: document.documentElement.scrollWidth - innerWidth,
    panel: box(document.querySelector('#timeline-panel')),
    viewport: viewport && box(viewport), now: now && box(now),
    readout: document.querySelector('#timeline-panel .timeline-readout')?.textContent,
    zoom: document.querySelector('#timeline-panel .timeline-zoom')?.textContent,
    events: all('.timeline-event').map(el => ({id: el.dataset.eventId, category: el.dataset.category, state: el.dataset.state,
      label: el.getAttribute('aria-label'), colour: getComputedStyle(el).color, ...box(el),
      chip: box(el.querySelector('.timeline-chip'))})),
    ramps: all('.timeline-ramp').map(el => ({id: el.dataset.eventId, state: el.dataset.state, ...box(el),
      span: box(el.querySelector('.timeline-ramp-span')), points: el.querySelector('polyline')?.getAttribute('points')})),
    fills: all('.timeline-past .timeline-ramp-fill').map(el => ({state: el.dataset.state, ...box(el)})),
    sections: all('.timeline-section').map(el => ({source: el.dataset.source, state: el.dataset.state, text: el.textContent, ...box(el),
      opacity: getComputedStyle(el).color})),
    match: document.querySelector('#timeline-panel .timeline-match')?.textContent || null,
    ticks: all('.timeline-grid .timeline-tick').map(el => ({kind: el.dataset.kind, label: el.querySelector('span')?.textContent || null, x: el.getBoundingClientRect().x})),
    meters: all('[role=meter]').map(el => ({id: el.dataset.meter, now: Number(el.getAttribute('aria-valuenow')), text: el.getAttribute('aria-valuetext'),
      unavailable: el.classList.contains('unavailable'), targets: el.querySelectorAll('.timeline-meter-mark.target').length})),
    feel: document.querySelector('#timeline-panel .timeline-feel')?.getAttribute('aria-label'),
    lanes: all('.timeline-lane').map(el => ({lane: el.dataset.lane, collapsed: el.classList.contains('collapsed'), h: el.getBoundingClientRect().height})),
    audio,
    targets: all('.timeline-event,.timeline-ramp,.timeline-cancel,.timeline-tools button,.timeline-detail-actions button')
      .map(el => ({tag: el.className, ...box(el)})),
  };
});

// Positions of the scrolling layer on consecutive display frames.
const scroll = (page, ms) => page.evaluate(ms => new Promise(done => {
  const layer = document.querySelector('#timeline-panel .timeline-grid .timeline-layer');
  const frames = [];
  const read = time => {
    frames.push([time, new DOMMatrixReadOnly(getComputedStyle(layer).transform).m41]);
    if (time - frames[0][0] < ms) requestAnimationFrame(read); else done(frames);
  };
  requestAnimationFrame(read);
}), ms);

function motion(frames) {
  const steps = frames.slice(1).map(([time, x], index) => ({dt: time - frames[index][0], dx: x - frames[index][1]}));
  const moved = steps.filter(step => step.dx !== 0);
  const sorted = moved.map(step => -step.dx).sort((a, b) => a - b);
  // Speed per step, so a late display frame is not mistaken for a jump.
  const speeds = steps.filter(step => step.dt > 0).map(step => -step.dx / step.dt * 1000).sort((a, b) => a - b);
  const seconds = (frames.at(-1)[0] - frames[0][0]) / 1000;
  const intervals = steps.map(step => step.dt).sort((a, b) => a - b);
  return {frames: frames.length, seconds, movedFraction: moved.length / steps.length,
    forward: steps.every(step => step.dx <= 0.01), pixelsPerSecond: -(frames.at(-1)[1] - frames[0][1]) / seconds,
    medianStep: sorted[Math.floor(sorted.length / 2)], maxStep: sorted.at(-1),
    speed: {median: speeds[Math.floor(speeds.length / 2)], p95: speeds[Math.floor(speeds.length * 0.95)], max: speeds.at(-1)},
    frameMs: {median: intervals[Math.floor(intervals.length / 2)], p95: intervals[Math.floor(intervals.length * 0.95)]}};
}

// Timeline items move every frame, so Playwright never sees them as "stable".
// Send a real pointer press at the item's current position instead, after
// checking that the point really hits it.
async function press(page, selector, touch) {
  const point = await page.evaluate(selector => {
    const el = document.querySelector(selector);
    if (!el) return null;
    const r = el.getBoundingClientRect();
    const x = r.x + Math.min(22, r.width / 2), y = r.y + r.height / 2;
    const hit = document.elementFromPoint(x, y);
    return {x, y, hit: !!hit && (hit === el || el.contains(hit))};
  }, selector);
  assert.ok(point, 'found ' + selector);
  assert.ok(point.hit, 'nothing covers ' + selector);
  if (touch) await page.touchscreen.tap(point.x, point.y); else await page.mouse.click(point.x, point.y);
}

// Studio posts commands as Transit JSON: ["^ ", "~:op", "schedule/cancel", ...].
function plain(value) {
  if (typeof value === 'string') return value.startsWith('~:') ? value.slice(2) : value;
  if (!Array.isArray(value)) return value;
  if (value[0] !== '^ ') return value.map(plain);
  const out = {};
  for (let i = 1; i < value.length; i += 2) out[plain(value[i])] = plain(value[i + 1]);
  return out;
}

const nearest = (xs, x) => xs.reduce((best, value) => Math.abs(value - x) < Math.abs(best - x) ? value : best, Infinity);

// The scenario has short stretches with no ramp running. Take the snapshot
// while one is under the centre line with room on both sides.
// The error says which half never appeared: a host whose synthetic recording
// has ended has ramps but no forecast (#801).
async function snapshotWithRunningRamp(page) {
  const seen = {rampUnderNow: 0, predicted: 0, attempts: 0};
  for (let attempt = 0; attempt < 120; attempt++) {
    const shot = await snapshot(page);
    const under = r => r.state === 'running' && shot.now.x - r.span.x > 8 && r.span.x + r.span.w - shot.now.x > 30;
    const ramp = shot.ramps.some(under), predicted = shot.sections.some(s => s.state === 'predicted');
    seen.attempts++; if (ramp) seen.rampUnderNow++; if (predicted) seen.predicted++;
    if (ramp && predicted) return shot;
    await sleep(250);
  }
  throw new Error('no running ramp reached the centre line while a section was predicted: ' + JSON.stringify(seen));
}

async function ready(page) {
  await page.goto(origin + '/perform', {waitUntil: 'domcontentloaded'});
  await page.locator('#timeline-panel .timeline-event').first().waitFor({timeout: 30000});
  await page.locator('#timeline-panel .timeline-section[data-source=causal]').first().waitFor({timeout: 90000});
  await page.locator('#timeline-panel .timeline-ramp[data-state=running]').first().waitFor({timeout: 90000});
}

// A failed run can leave the host at another tempo. Every run starts from 120
// BPM so the tempo step really changes the tempo.
async function setTempo(page, bpm) {
  await page.locator('#bpm').fill(String(bpm));
  await page.locator('#bpm').press('Enter');
  await page.waitForFunction(bpm => new RegExp(bpm.toFixed(1).replace('.', '\\.') + ' BPM')
    .test(document.querySelector('#timeline-panel .timeline-readout')?.textContent || ''), bpm, {timeout: 10000});
  await page.locator('#bpm').blur();
  for (let attempt = 0; attempt < 40 && (await state()).transport.bpm !== bpm; attempt++) await sleep(100);
  assert.equal((await state()).transport.bpm, bpm, 'the owner transport is at ' + bpm + ' BPM');
}

async function cancelFromTimeline(page, touch) {
  // A scheduled event at least two beats ahead, so it cannot fire mid-test.
  const before = await snapshot(page);
  const beat = (before.viewport.w / 2) / 32; // only to pick a margin in pixels: the default window is 32 beats a side
  const candidate = before.events.filter(e => e.state === 'pending' && /-(spot|slow|period|accent|wash)$/.test(e.id) && e.chip.cx - before.now.x > 2.5 * beat)
    .sort((a, b) => a.chip.cx - b.chip.cx)[0];
  assert.ok(candidate, 'a scheduled, cancellable event is upcoming');
  const commands = [];
  const listen = request => { if (request.url().endsWith('/api/command') && request.method() === 'POST') commands.push(plain(JSON.parse(request.postData()))); };
  page.on('request', listen);
  await press(page, `#timeline-panel .timeline-event[data-event-id="${candidate.id}"]`, touch);
  const detail = page.locator('#timeline-panel .timeline-detail');
  await detail.waitFor();
  assert.equal(await detail.getAttribute('data-event-id'), candidate.id);
  const detailText = await detail.textContent();
  const cancel = detail.getByRole('button', {name: /^Cancel event /});
  const cancelBox = await cancel.boundingBox();
  assert.ok(cancelBox.width >= 44 && cancelBox.height >= 44, 'cancel is a 44 px target');
  await (touch ? cancel.tap() : cancel.click());
  await page.locator(`#timeline-panel .timeline-event[data-event-id="${candidate.id}"][data-state=cancelled]`).waitFor({timeout: 5000});
  page.off('request', listen);
  const command = commands.find(c => c.op === 'schedule/cancel');
  assert.ok(command, 'the timeline posted schedule/cancel: ' + JSON.stringify(commands));
  assert.deepEqual(command['event-ids'], [candidate.id], 'the timeline sent schedule/cancel for that event ID');
  const {schedule} = await state();
  assert.ok(!schedule.upcoming.some(e => e['event-id'] === candidate.id), 'gone from the owner queue');
  const outcome = schedule.recent.find(e => e['event-id'] === candidate.id);
  assert.equal(outcome.status, 'cancelled');
  assert.equal(outcome.reason, 'operator-cancel');
  const after = await snapshot(page);
  const marker = after.events.find(e => e.id === candidate.id);
  assert.ok(marker.chip.cx <= after.now.x + 2,
    `the cancelled marker is in the past half: ${(marker.chip.cx - after.now.x).toFixed(2)} px right of now, at ${outcome['at-beat']}`);
  assert.match(await detail.textContent(), /Cancelled/);
  assert.equal(await detail.getByRole('button', {name: /^Cancel event /}).count(), 0, 'nothing left to cancel');
  return {eventId: candidate.id, command: {op: command.op, 'event-ids': command['event-ids'], source: command.source},
    outcome: {status: outcome.status, reason: outcome.reason, 'at-beat': outcome['at-beat']}, detailBefore: detailText};
}

function layoutChecks(name, shot) {
  assert.ok(shot.overflow <= 0, name + ': no horizontal overflow');
  assert.ok(Math.abs(shot.now.x - shot.viewport.cx) <= 1.5, name + ': now is the centre of the lanes');
  const small = shot.targets.filter(t => t.w < 43.5 || t.h < 43.5);
  assert.deepEqual(small, [], name + ': every event action is at least 44 px');
  const tolerance = shot.viewport.w / 64 * 0.35; // a third of a beat at the default zoom
  for (const event of shot.events) {
    if (event.state === 'pending') assert.ok(event.chip.cx >= shot.now.x - tolerance, `${name}: pending ${event.id} is right of now`);
    else assert.ok(event.chip.cx <= shot.now.x + tolerance, `${name}: ${event.state} ${event.id} is left of now`);
  }
  assert.ok(shot.audio && shot.audio.left > 200, name + ': the waveform is drawn in the past half');
  assert.equal(shot.audio.right, 0, name + ': no live audio is drawn right of now');
  return {viewport: shot.viewport, now: shot.now.x, events: shot.events.length, ramps: shot.ramps.length, smallestTarget:
    shot.targets.reduce((least, t) => Math.min(least, t.w, t.h), Infinity)};
}

async function desktop() {
  const {context, page, problems} = await launch('desktop', {width: 1440, height: 900, scale: 2});
  try {
    await ready(page);
    await setTempo(page, 120);
    report.measurements.gpu = await page.evaluate(() => {
      const gl = document.createElement('canvas').getContext('webgl2');
      const info = gl && gl.getExtension('WEBGL_debug_renderer_info');
      return {userAgent: navigator.userAgent, renderer: info && gl.getParameter(info.UNMASKED_RENDERER_WEBGL)};
    });
    await sleep(1500);
    const shot = await snapshotWithRunningRamp(page);
    report.measurements.desktop = layoutChecks('desktop', shot);
    pass('past and future split at the centre line', {nowX: shot.now.x, lanesCentreX: shot.viewport.cx});

    const categories = [...new Set(shot.events.map(e => e.category))];
    // A rejected event is drawn in the danger colour whatever its category.
    const accepted = shot.events.filter(e => e.state !== 'rejected');
    const colours = new Set(accepted.map(e => e.category + ' ' + e.colour));
    assert.ok(categories.length >= 3, 'at least three action categories on screen: ' + categories);
    assert.equal(colours.size, new Set(accepted.map(e => e.category)).size, 'one colour per category');
    assert.equal(new Set(accepted.map(e => e.colour)).size, colours.size, 'categories do not share a colour');
    assert.ok(new Set(shot.events.map(e => Math.round(e.y))).size >= 2, 'overlapping events are stacked on separate lines');
    assert.ok(shot.events.some(e => e.state === 'fired') && shot.events.some(e => e.state === 'pending'));
    pass('events are icons coloured by category in stacked lines',
      {categories, states: [...new Set(shot.events.map(e => e.state))], lines: new Set(shot.events.map(e => Math.round(e.y))).size});

    const running = shot.ramps.find(r => r.state === 'running' && r.span.x < shot.now.x && r.span.x + r.span.w > shot.now.x);
    assert.ok(running, 'now is inside the running ramp bar');
    assert.equal(running.points.split(' ').length, 17, 'the easing curve has the 17 projected samples');
    const fill = shot.fills.find(f => Math.abs(f.x - running.span.x) < 2);
    assert.ok(fill, 'the running ramp has its filled copy in the past half');
    const progress = (shot.now.x - running.span.x) / running.span.w;
    const owner = (await state()).schedule.ramps.find(r => r['event-id'] === running.id);
    pass('a ramp is a bar from start to end with its easing curve and a progress fill',
      {eventId: running.id, drawnProgress: progress, ownerSpanBeats: [owner.start, owner.end], curve: owner.curve});

    const causal = shot.sections.filter(s => s.source === 'causal');
    const predicted = shot.sections.filter(s => s.state === 'predicted');
    const phraseTicks = shot.ticks.filter(t => t.kind === 'phrase').map(t => t.x);
    assert.ok(causal.length >= 1 && causal.every(s => s.x <= shot.now.x), 'confirmed sections are in the past');
    assert.ok(predicted.length >= 1 && predicted.every(s => s.x + s.w >= shot.now.x), 'predicted sections are ahead');
    // The synthetic music changes its bass pattern every 16 beats, which is one
    // phrase. A section that began left of the lane has no visible boundary.
    const boundaries = (sections, lane) => sections.filter(s => s.x >= lane.x + 1).map(s => s.x);
    const visible = boundaries(causal, shot.viewport);
    assert.ok(visible.length >= 1, 'a confirmed boundary is on screen');
    const sectionError = Math.max(...visible.map(x => Math.abs(nearest(phraseTicks, x) - x)));
    assert.ok(sectionError <= 2, 'confirmed section boundaries sit on phrase lines, error ' + sectionError);
    const predictedError = Math.max(...boundaries(predicted, shot.viewport).filter(x => x > shot.now.x)
      .map(x => Math.abs(nearest(phraseTicks, x) - x)));
    assert.ok(predictedError <= 2, 'predicted boundaries sit on phrase lines, error ' + predictedError);
    pass('sections: confirmed in the past, predicted ahead, boundaries on the grid',
      {causal: causal.map(s => s.text), predicted: predicted.map(s => s.text), match: shot.match,
       confirmedBoundaryErrorPx: sectionError, predictedBoundaryErrorPx: predictedError});

    const bars = shot.ticks.filter(t => t.kind !== 'beat');
    assert.ok(shot.ticks.some(t => t.kind === 'beat') && bars.length >= 16 && phraseTicks.length >= 3, 'beats, bars and phrases are ruled');
    assert.ok(shot.meters.length === 5 && shot.meters.slice(0, 3).every(m => !m.unavailable && m.now > 0), 'audio meters read a level');
    await page.screenshot({path: join(output, 'desktop-1440.png')});

    // Display-rate motion between committed samples.
    const before = motion(await scroll(page, 3000));
    assert.ok(before.forward, 'the timeline only moves forward while playing');
    assert.ok(before.movedFraction > 0.9, 'the playhead moves on display frames, not only on committed samples: ' + before.movedFraction);
    assert.ok(before.speed.p95 < before.speed.median * 2.5 && before.speed.max < before.speed.median * 6,
      'no jumps: speed in px/s ' + JSON.stringify(before.speed));
    const pixelsPerBeat = shot.viewport.w / 64;
    const tempoBefore = (await state()).transport.bpm;
    assert.equal(tempoBefore, 120, 'the run starts at 120 BPM');
    const measuredTempo = before.pixelsPerSecond / pixelsPerBeat * 60;
    assert.ok(Math.abs(measuredTempo - tempoBefore) < tempoBefore * 0.03,
      `scroll speed is the transport tempo: measured ${measuredTempo} BPM at ${pixelsPerBeat} px a beat, transport ${tempoBefore}`);
    pass('the playhead is interpolated at display rate', {...before, measuredTempo, transportTempo: tempoBefore, pixelsPerBeat});

    const cancelled = await cancelFromTimeline(page, false);
    pass('cancel from the timeline sends schedule/cancel and the projection updates', cancelled);
    await page.screenshot({path: join(output, 'desktop-cancelled.png')});
    await page.locator('#timeline-panel .timeline-detail').getByRole('button', {name: 'Close'}).click();

    // Tempo change through Studio's own tempo field.
    const gridBefore = await snapshot(page);
    const spacing = shot => { const xs = shot.ticks.filter(t => t.kind !== 'beat').map(t => t.x); return xs[1] - xs[0]; };
    await setTempo(page, 96);
    await sleep(1200);
    const after = motion(await scroll(page, 3000));
    const gridAfter = await snapshot(page);
    // Positions are written to a hundredth of a pixel.
    assert.ok(Math.abs(spacing(gridAfter) - spacing(gridBefore)) < 0.05,
      `bars keep their width, the axis is beats: ${spacing(gridBefore)} then ${spacing(gridAfter)}`);
    assert.ok(Math.abs(after.pixelsPerSecond / before.pixelsPerSecond - 96 / tempoBefore) < 0.03, 'the scroll slows with the tempo');
    assert.ok(after.movedFraction > 0.9 && after.forward);
    const causalAfter = gridAfter.sections.filter(s => s.source === 'causal');
    const phraseAfter = gridAfter.ticks.filter(t => t.kind === 'phrase').map(t => t.x);
    const errorAfter = Math.max(...boundaries(causalAfter, gridAfter.viewport).map(x => Math.abs(nearest(phraseAfter, x) - x)));
    assert.ok(errorAfter <= 2, 'sections still sit on phrase lines after the tempo change: ' + errorAfter);
    const pendingAfter = gridAfter.events.filter(e => e.state === 'pending' && /-(wash|period|spot|slow)$/.test(e.id));
    const barXs = gridAfter.ticks.filter(t => t.kind !== 'beat').map(t => t.x);
    const eventError = Math.max(...pendingAfter.map(e => Math.abs(nearest(barXs, e.chip.cx) - e.chip.cx)));
    assert.ok(pendingAfter.length && eventError <= 1.5, 'events posted at bars sit on bar lines: ' + eventError);
    layoutChecks('desktop after tempo change', gridAfter);
    const measuredAfter = after.pixelsPerSecond / pixelsPerBeat * 60;
    assert.ok(Math.abs(measuredAfter - 96) < 96 * 0.03, 'scroll speed follows the new tempo: ' + measuredAfter);
    pass('grid, sections and events stay aligned to the transport across a tempo change',
      {bpm: [tempoBefore, (await state()).transport.bpm], measuredTempo: [measuredTempo, measuredAfter], barSpacingPx: [spacing(gridBefore), spacing(gridAfter)],
       pixelsPerSecond: [before.pixelsPerSecond, after.pixelsPerSecond], sectionErrorPx: errorAfter, eventErrorPx: eventError});
    await sleep(2500);
    await page.screenshot({path: join(output, 'desktop-after-tempo-change.png')});

    // Zoom and lane collapse.
    await page.getByRole('button', {name: 'Zoom out: show more bars'}).click();
    await page.waitForFunction(() => document.querySelector('#timeline-panel .timeline-zoom')?.textContent === '±16 bars');
    await page.waitForFunction(previous => {
      const xs = [...document.querySelectorAll('#timeline-panel .timeline-grid .timeline-tick:not([data-kind=beat])')].map(el => el.getBoundingClientRect().x);
      return xs.length > 1 && Math.abs((xs[1] - xs[0]) - previous / 2) < 0.5;
    }, spacing(gridAfter));
    const wide = await snapshot(page);
    layoutChecks('desktop at ±16 bars', wide);
    await page.screenshot({path: join(output, 'desktop-16-bars.png')});
    await page.getByRole('button', {name: 'Zoom in: show fewer bars'}).click();
    await page.getByRole('button', {name: 'Zoom in: show fewer bars'}).click();
    await page.waitForFunction(() => document.querySelector('#timeline-panel .timeline-zoom')?.textContent === '±4 bars');
    await sleep(600);
    layoutChecks('desktop at ±4 bars', await snapshot(page));
    await page.screenshot({path: join(output, 'desktop-4-bars.png')});
    await page.getByRole('button', {name: 'Zoom out: show more bars'}).click();
    await page.getByRole('button', {name: 'Hide audio lane'}).click();
    await page.locator('#timeline-panel canvas').waitFor({state: 'detached', timeout: 3000});
    await page.getByRole('button', {name: 'Show audio lane'}).click();
    await page.locator('#timeline-panel canvas').waitFor();
    pass('zoom and collapsible lanes', {windows: ['±8 bars', '±16 bars', '±4 bars'], barSpacingPx: [spacing(gridAfter), spacing(wide)]});

    // Simulated match loss and the data-driven feel slot.
    flag('match-off', true);
    await page.waitForFunction(() => !document.querySelector('#timeline-panel .timeline-section[data-state=predicted]'), null, {timeout: 6000});
    const lost = await snapshot(page);
    assert.match(lost.match, /^No match/);
    await page.screenshot({path: join(output, 'desktop-match-lost.png')});
    flag('match-off', false);
    await page.locator('#timeline-panel .timeline-section[data-state=predicted]').first().waitFor({timeout: 6000});
    pass('predicted sections disappear when the simulated match breaks and return with it', {lost: lost.match});
    assert.equal((await snapshot(page)).feel, 'Track feel: none');
    flag('feel', true);
    await page.waitForFunction(() => /Squares/.test(document.querySelector('#timeline-panel .timeline-feel')?.getAttribute('aria-label') || ''), null, {timeout: 6000});
    const feelSlot = await page.evaluate(() => {
      const slot = document.querySelector('#timeline-panel .timeline-feel');
      const img = slot.querySelector('img');
      return {label: slot.getAttribute('aria-label'), icon: !!img && img.complete && img.naturalWidth > 0,
        swatches: [...slot.querySelectorAll('.timeline-feel-palette i')].map(i => getComputedStyle(i).backgroundColor)};
    });
    assert.ok(feelSlot.icon, 'the personality icon SVG is drawn');
    assert.equal(feelSlot.swatches.length, 3, 'the three palette entries (a colour, a split, a colour) each give a swatch');
    await page.screenshot({path: join(output, 'desktop-feel-slot.png')});
    flag('feel', false);
    await page.waitForFunction(() => document.querySelector('#timeline-panel .timeline-feel')?.getAttribute('aria-label') === 'Track feel: none', null, {timeout: 6000});
    pass('the feel slot is empty until a personality is selected, then draws the active one', feelSlot);

    // Meters move with the music.
    const levels = [];
    for (let i = 0; i < 12; i++) { levels.push((await snapshot(page)).meters.map(m => m.now)); await sleep(90); }
    assert.ok(new Set(levels.map(l => l[0])).size > 3 && new Set(levels.map(l => l[2])).size > 3, 'RMS and energy meters update');
    pass('meters update', {samples: levels.length, meters: (await snapshot(page)).meters});

    assert.deepEqual(problems, [], 'no page errors');
  } finally {
    await setTempo(page, 120).catch(error => console.error('could not restore 120 BPM', error));
    await context.close();
  }
}

async function tablet(name, width, height) {
  const {context, page, problems} = await launch(name, {width, height, touch: true, scale: 2});
  try {
    await ready(page);
    await sleep(1500);
    const shot = await snapshot(page);
    report.measurements[name] = layoutChecks(name, shot);
    assert.equal(shot.meters.length, 5);
    await page.screenshot({path: join(output, name + '.png')});
    const cancelled = await cancelFromTimeline(page, true);
    await page.screenshot({path: join(output, name + '-cancelled.png')});
    assert.deepEqual(problems, [], 'no page errors');
    pass(name + ' layout and touch cancel', {...report.measurements[name], cancelled: cancelled.eventId});
  } finally { await context.close(); }
}

async function phone() {
  const {context, page, problems} = await launch('phone-390', {width: 390, height: 844, touch: true, scale: 3});
  try {
    await ready(page);
    await sleep(1500);
    const shot = await snapshot(page);
    report.measurements.phone = layoutChecks('phone', shot);
    await page.screenshot({path: join(output, 'phone-390.png')});
    assert.deepEqual(problems, [], 'no page errors');
    pass('phone width layout', report.measurements.phone);
  } finally { await context.close(); }
}

// Main-thread cost of the timeline with the Room rendering beside it: time in
// every animation-frame callback (the Room, Reagent renders and the timeline
// loop) with the panel open and hidden.
async function frameBudget() {
  // The Room renders only while its canvas is in the viewport. At 1440 x 900
  // the open timeline pushes it below the fold, so the viewport is tall enough
  // for both, and each measurement first checks that the Room is rendering.
  const {context, page, problems} = await launch('budget', {width: 1440, height: 1440});
  try {
    await page.addInitScript(() => {
      window.__roomActive = null;
      document.addEventListener('tolglow-preview-activity', event => { window.__roomActive = event.detail; }, true);
    });
    await page.addInitScript(() => {
      const native = window.requestAnimationFrame.bind(window);
      const stats = window.__frameCost = {frames: [], current: 0, scheduled: false};
      window.requestAnimationFrame = callback => native(time => {
        if (!stats.scheduled) { stats.scheduled = true; queueMicrotask(() => { stats.frames.push(stats.current); stats.current = 0; stats.scheduled = false; }); }
        const started = performance.now();
        try { callback(time); } finally { stats.current += performance.now() - started; }
      });
    });
    await ready(page);
    await page.bringToFront();
    assert.ok(await page.locator('#preview-panel canvas').count() >= 1, 'the Room is mounted beside the timeline');
    const measure = async ms => {
      await page.waitForFunction(() => window.__roomActive === true, null, {timeout: 10000});
      await page.evaluate(() => { window.__frameCost.frames.length = 0; });
      const frames = motion(await page.evaluate(ms => new Promise(done => {
        const out = []; const read = t => { out.push([t, -out.length]); if (t - out[0][0] < ms) requestAnimationFrame(read); else done(out); };
        requestAnimationFrame(read);
      }), ms));
      const costs = (await page.evaluate(() => window.__frameCost.frames.slice())).sort((a, b) => a - b);
      const at = q => costs[Math.min(costs.length - 1, Math.floor(costs.length * q))];
      const roomRendering = await page.evaluate(() => window.__roomActive === true);
      assert.ok(roomRendering, 'the Room kept rendering through the measurement');
      return {frames: costs.length, roomRendering, callbackMs: {median: at(0.5), p95: at(0.95), max: costs.at(-1)}, frameIntervalMs: frames.frameMs};
    };
    await sleep(2000);
    const open = await measure(8000);
    await page.locator('#timeline-panel').getByRole('button', {name: 'Hide', exact: true}).click();
    await sleep(1000);
    const hidden = await measure(8000);
    await page.locator('#timeline-panel').getByRole('button', {name: 'Show', exact: true}).click();
    report.measurements.frameBudget = {viewport: '1440x1440 @1x', roomRendering: true, timelineOpen: open, timelineHidden: hidden,
      addedMedianMs: open.callbackMs.median - hidden.callbackMs.median, addedP95Ms: open.callbackMs.p95 - hidden.callbackMs.p95};
    assert.deepEqual(problems, [], 'no page errors');
    pass('frame cost measured with the Room open', report.measurements.frameBudget);
  } finally { await context.close(); }
}

async function recording() {
  // Frames come from Chrome's own screencast (DevTools protocol) and are
  // encoded with the system ffmpeg, so no Playwright video download is needed.
  const {context, page} = await launch('recording', {width: 1280, height: 720});
  const frames = join(output, 'recording-frames');
  rmSync(frames, {recursive: true, force: true});
  mkdirSync(frames, {recursive: true});
  const timings = [];
  let cdp = null;
  try {
    await ready(page);
    await page.evaluate(() => document.querySelector('#timeline-panel').scrollIntoView({block: 'start'}));
    await sleep(1000);
    cdp = await context.newCDPSession(page);
    cdp.on('Page.screencastFrame', ({data, metadata, sessionId}) => {
      const file = 'frame-' + String(timings.length).padStart(5, '0') + '.jpg';
      writeFileSync(join(frames, file), Buffer.from(data, 'base64'));
      timings.push([file, metadata.timestamp]);
      cdp.send('Page.screencastFrameAck', {sessionId}).catch(() => {});
    });
    await cdp.send('Page.startScreencast', {format: 'jpeg', quality: 90, maxWidth: 1280, maxHeight: 720, everyNthFrame: 1});
    await sleep(3000);
    const first = await snapshot(page);
    const target = first.events.filter(e => e.state === 'pending' && /-(spot|slow|period|accent|wash)$/.test(e.id) && e.chip.cx - first.now.x > 80)
      .sort((a, b) => a.chip.cx - b.chip.cx)[0];
    if (target) {
      await press(page, `#timeline-panel .timeline-event[data-event-id="${target.id}"]`, false);
      await sleep(1500);
      await page.locator('#timeline-panel .timeline-detail').getByRole('button', {name: /^Cancel event /}).click();
      await sleep(2500);
      await page.locator('#timeline-panel .timeline-detail').getByRole('button', {name: 'Close'}).click();
    }
    await setTempo(page, 96);
    await sleep(6000);
    await setTempo(page, 120);
    await sleep(3000);
  } finally {
    if (cdp) await cdp.send('Page.stopScreencast').catch(() => {});
    await setTempo(page, 120).catch(() => {});
    await context.close();
  }
  // Each frame is shown until the next one arrived, as the screencast timed it.
  assert.ok(timings.length > 30, 'the screencast delivered frames: ' + timings.length);
  const list = timings.map(([file, at], index) =>
    `file '${file}'\nduration ${Math.max(0.001, ((timings[index + 1]?.[1] ?? at + 1 / 30) - at)).toFixed(4)}`);
  writeFileSync(join(frames, 'frames.txt'), list.join('\n') + `\nfile '${timings.at(-1)[0]}'\n`);
  const seconds = timings.at(-1)[1] - timings[0][1];
  const encoded = spawnSync(ffmpeg, ['-y', '-loglevel', 'error', '-f', 'concat', '-safe', '0', '-i', join(frames, 'frames.txt'),
    '-vf', 'fps=30,scale=1280:-2', '-c:v', 'libx264', '-pix_fmt', 'yuv420p', '-movflags', '+faststart', join(output, 'timeline.mp4')],
    {encoding: 'utf8'});
  assert.equal(encoded.status, 0, 'ffmpeg encoded the recording: ' + encoded.stderr);
  report.measurements.recording = {frames: timings.length, seconds, framesPerSecond: timings.length / seconds};
  pass('recording written', {file: 'timeline.mp4', ...report.measurements.recording});
}

const steps = {desktop, ipad: () => tablet('ipad-landscape-1024', 1024, 768), portrait: () => tablet('ipad-portrait-820', 820, 1180),
  phone, budget: frameBudget, recording};
try {
  for (const [name, step] of Object.entries(steps)) if (!only || only.split(',').includes(name)) await step();
  report.result = 'passed';
} catch (error) {
  report.result = 'failed';
  report.error = String(error && error.stack || error);
  process.exitCode = 1;
  console.error(error);
} finally {
  flag('match-off', false);
  flag('feel', false);
  report.finishedAt = new Date().toISOString();
  writeFileSync(join(output, 'report.json'), JSON.stringify(report, null, 2));
}
