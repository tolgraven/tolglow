// Browser check for the musical timeline (#646) against tolglow.qa.timeline.
//
//   lein run -m tolglow.qa.timeline 4190 target/timeline-qa/control
//   CHROMIUM_PATH=... node dev/studio/timeline-browser-qa.mjs http://127.0.0.1:4190 target/timeline-qa/out target/timeline-qa/control
//
// Uses the production Studio build, headless Chrome and the real GPU. It
// asserts the layout and behaviour, writes stills, a recording and report.json.
// Headless desktop Chrome is not evidence for a foreground display or iPad Safari.
import assert from 'node:assert/strict';
import {existsSync, mkdirSync, readdirSync, renameSync, rmSync, writeFileSync} from 'node:fs';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const [origin = 'http://127.0.0.1:4190', output = 'target/timeline-qa/out', control = 'target/timeline-qa/control'] = process.argv.slice(2);
const only = process.env.TIMELINE_QA_ONLY;
const executablePath = process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
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

async function launch(name, {width, height, touch = false, scale = 1, video = false}) {
  const profile = resolve(output, '..', 'chrome-profile-' + name + '-' + Date.now());
  const context = await chromium.launchPersistentContext(profile, {
    executablePath, headless: true,
    args: ['--headless=new', '--use-angle=metal', '--enable-gpu', '--ignore-gpu-blocklist', '--hide-scrollbars',
           '--autoplay-policy=no-user-gesture-required'],
    viewport: {width, height}, deviceScaleFactor: scale, hasTouch: touch, isMobile: false,
    recordVideo: video ? {dir: join(output, 'video-' + name), size: {width, height}} : undefined,
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
  const seconds = (frames.at(-1)[0] - frames[0][0]) / 1000;
  const intervals = steps.map(step => step.dt).sort((a, b) => a - b);
  return {frames: frames.length, seconds, movedFraction: moved.length / steps.length,
    forward: steps.every(step => step.dx <= 0.01), pixelsPerSecond: -(frames.at(-1)[1] - frames[0][1]) / seconds,
    medianStep: sorted[Math.floor(sorted.length / 2)], maxStep: sorted.at(-1),
    frameMs: {median: intervals[Math.floor(intervals.length / 2)], p95: intervals[Math.floor(intervals.length * 0.95)]}};
}

const nearest = (xs, x) => xs.reduce((best, value) => Math.abs(value - x) < Math.abs(best - x) ? value : best, Infinity);

async function ready(page) {
  await page.goto(origin + '/perform', {waitUntil: 'domcontentloaded'});
  await page.locator('#timeline-panel .timeline-event').first().waitFor({timeout: 30000});
  await page.locator('#timeline-panel .timeline-section[data-source=causal]').first().waitFor({timeout: 90000});
  await page.locator('#timeline-panel .timeline-ramp[data-state=running]').first().waitFor({timeout: 90000});
}

async function cancelFromTimeline(page, touch) {
  // A scheduled event at least two beats ahead, so it cannot fire mid-test.
  const before = await snapshot(page);
  const beat = (before.viewport.w / 2) / 32; // only to pick a margin in pixels: the default window is 32 beats a side
  const candidate = before.events.filter(e => e.state === 'pending' && /-(spot|slow|period|accent|wash)$/.test(e.id) && e.chip.cx - before.now.x > 2.5 * beat)
    .sort((a, b) => a.chip.cx - b.chip.cx)[0];
  assert.ok(candidate, 'a scheduled, cancellable event is upcoming');
  const commands = [];
  const listen = request => { if (request.url().endsWith('/api/command') && request.method() === 'POST') commands.push(request.postDataJSON()); };
  page.on('request', listen);
  const button = page.locator(`#timeline-panel .timeline-event[data-event-id="${candidate.id}"]`);
  await (touch ? button.tap() : button.click());
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
  assert.deepEqual(command['event-ids'], [candidate.id], 'the timeline sent schedule/cancel for that event ID');
  const {schedule} = await state();
  assert.ok(!schedule.upcoming.some(e => e['event-id'] === candidate.id), 'gone from the owner queue');
  const outcome = schedule.recent.find(e => e['event-id'] === candidate.id);
  assert.equal(outcome.status, 'cancelled');
  assert.equal(outcome.reason, 'operator-cancel');
  const after = await snapshot(page);
  const marker = after.events.find(e => e.id === candidate.id);
  assert.ok(marker.chip.cx <= after.now.x + 2, 'the cancelled marker is in the past half');
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
  const {context, page, problems} = await launch('desktop', {width: 1440, height: 900, scale: 2, video: false});
  try {
    await ready(page);
    report.measurements.gpu = await page.evaluate(() => {
      const gl = document.createElement('canvas').getContext('webgl2');
      const info = gl && gl.getExtension('WEBGL_debug_renderer_info');
      return {userAgent: navigator.userAgent, renderer: info && gl.getParameter(info.UNMASKED_RENDERER_WEBGL)};
    });
    await sleep(1500);
    const shot = await snapshot(page);
    report.measurements.desktop = layoutChecks('desktop', shot);
    pass('past and future split at the centre line', {nowX: shot.now.x, lanesCentreX: shot.viewport.cx});

    const categories = [...new Set(shot.events.map(e => e.category))];
    const colours = new Set(shot.events.map(e => e.category + ' ' + e.colour));
    assert.ok(categories.length >= 3, 'at least three action categories on screen: ' + categories);
    assert.equal(colours.size, categories.length, 'one colour per category');
    assert.ok(new Set(shot.events.map(e => Math.round(e.y))).size >= 2, 'overlapping events are stacked on separate lines');
    assert.ok(shot.events.some(e => e.state === 'fired') && shot.events.some(e => e.state === 'pending'));
    pass('events are icons coloured by category in stacked lines',
      {categories, states: [...new Set(shot.events.map(e => e.state))], lines: new Set(shot.events.map(e => Math.round(e.y))).size});

    const running = shot.ramps.find(r => r.state === 'running');
    assert.ok(running.span.x < shot.now.x && running.span.x + running.span.w > shot.now.x, 'now is inside the running ramp bar');
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
    const sectionError = Math.max(...causal.map(s => Math.abs(nearest(phraseTicks, s.x) - s.x)));
    assert.ok(sectionError <= 2, 'confirmed section boundaries sit on phrase lines, error ' + sectionError);
    pass('sections: confirmed in the past, predicted ahead, boundaries on the grid',
      {causal: causal.map(s => s.text), predicted: predicted.map(s => s.text), match: shot.match, boundaryErrorPx: sectionError});

    const bars = shot.ticks.filter(t => t.kind !== 'beat');
    assert.ok(shot.ticks.some(t => t.kind === 'beat') && bars.length >= 16 && phraseTicks.length >= 3, 'beats, bars and phrases are ruled');
    assert.ok(shot.meters.length === 5 && shot.meters.slice(0, 3).every(m => !m.unavailable && m.now > 0), 'audio meters read a level');
    await page.screenshot({path: join(output, 'desktop-1440.png')});

    // Display-rate motion between committed samples.
    const before = motion(await scroll(page, 3000));
    assert.ok(before.forward, 'the timeline only moves forward while playing');
    assert.ok(before.movedFraction > 0.9, 'the playhead moves on display frames, not only on committed samples: ' + before.movedFraction);
    assert.ok(before.maxStep < before.medianStep * 3.5, 'no jumps: max step ' + before.maxStep + ' vs median ' + before.medianStep);
    const pixelsPerBeat = shot.viewport.w / 64;
    const tempoBefore = (await state()).transport.bpm;
    assert.ok(Math.abs(before.pixelsPerSecond / pixelsPerBeat * 60 - tempoBefore) < tempoBefore * 0.03, 'scroll speed is the transport tempo');
    pass('the playhead is interpolated at display rate', before);

    const cancelled = await cancelFromTimeline(page, false);
    pass('cancel from the timeline sends schedule/cancel and the projection updates', cancelled);
    await page.screenshot({path: join(output, 'desktop-cancelled.png')});
    await page.locator('#timeline-panel .timeline-detail').getByRole('button', {name: 'Close'}).click();

    // Tempo change through Studio's own tempo field.
    const gridBefore = await snapshot(page);
    const spacing = shot => { const xs = shot.ticks.filter(t => t.kind !== 'beat').map(t => t.x); return xs[1] - xs[0]; };
    await page.locator('#bpm').fill('96');
    await page.locator('#bpm').press('Enter');
    await page.waitForFunction(() => /96\.0 BPM/.test(document.querySelector('#timeline-panel .timeline-readout')?.textContent || ''));
    await sleep(1200);
    const after = motion(await scroll(page, 3000));
    const gridAfter = await snapshot(page);
    assert.ok(Math.abs(spacing(gridAfter) - spacing(gridBefore)) < 0.01, 'bars keep their width: the axis is beats');
    assert.ok(Math.abs(after.pixelsPerSecond / before.pixelsPerSecond - 96 / tempoBefore) < 0.03, 'the scroll slows with the tempo');
    assert.ok(after.movedFraction > 0.9 && after.forward);
    const causalAfter = gridAfter.sections.filter(s => s.source === 'causal');
    const phraseAfter = gridAfter.ticks.filter(t => t.kind === 'phrase').map(t => t.x);
    const errorAfter = Math.max(...causalAfter.map(s => Math.abs(nearest(phraseAfter, s.x) - s.x)));
    assert.ok(errorAfter <= 2, 'sections still sit on phrase lines after the tempo change: ' + errorAfter);
    const pendingAfter = gridAfter.events.filter(e => e.state === 'pending' && /-(wash|period|spot|slow)$/.test(e.id));
    const barXs = gridAfter.ticks.filter(t => t.kind !== 'beat').map(t => t.x);
    const eventError = Math.max(...pendingAfter.map(e => Math.abs(nearest(barXs, e.chip.cx) - e.chip.cx)));
    assert.ok(pendingAfter.length && eventError <= 1.5, 'events posted at bars sit on bar lines: ' + eventError);
    layoutChecks('desktop after tempo change', gridAfter);
    pass('grid, sections and events stay aligned to the transport across a tempo change',
      {bpm: [tempoBefore, 96], barSpacingPx: [spacing(gridBefore), spacing(gridAfter)],
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
    assert.equal(await page.locator('#timeline-panel canvas').count(), 0, 'a collapsed lane is not rendered');
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
    await page.screenshot({path: join(output, 'desktop-feel-slot.png')});
    flag('feel', false);
    await page.waitForFunction(() => document.querySelector('#timeline-panel .timeline-feel')?.getAttribute('aria-label') === 'Track feel: none', null, {timeout: 6000});
    pass('the feel slot is empty until data fills it');

    // Meters move with the music.
    const levels = [];
    for (let i = 0; i < 12; i++) { levels.push((await snapshot(page)).meters.map(m => m.now)); await sleep(90); }
    assert.ok(new Set(levels.map(l => l[0])).size > 3 && new Set(levels.map(l => l[2])).size > 3, 'RMS and energy meters update');
    pass('meters update', {samples: levels.length, meters: (await snapshot(page)).meters});

    await page.locator('#bpm').fill(String(tempoBefore));
    await page.locator('#bpm').press('Enter');
    assert.deepEqual(problems, [], 'no page errors');
  } finally { await context.close(); }
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
  const {context, page, problems} = await launch('budget', {width: 1440, height: 900});
  try {
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
    assert.ok(await page.locator('#preview-panel canvas').count() >= 1, 'the Room is mounted beside the timeline');
    const measure = async ms => {
      await page.evaluate(() => { window.__frameCost.frames.length = 0; });
      const frames = motion(await page.evaluate(ms => new Promise(done => {
        const out = []; const read = t => { out.push([t, -out.length]); if (t - out[0][0] < ms) requestAnimationFrame(read); else done(out); };
        requestAnimationFrame(read);
      }), ms));
      const costs = (await page.evaluate(() => window.__frameCost.frames.slice())).sort((a, b) => a - b);
      const at = q => costs[Math.min(costs.length - 1, Math.floor(costs.length * q))];
      return {frames: costs.length, callbackMs: {median: at(0.5), p95: at(0.95), max: costs.at(-1)}, frameIntervalMs: frames.frameMs};
    };
    await sleep(2000);
    const open = await measure(8000);
    await page.locator('#timeline-panel').getByRole('button', {name: 'Hide'}).click();
    await sleep(1000);
    const hidden = await measure(8000);
    await page.locator('#timeline-panel').getByRole('button', {name: 'Show'}).click();
    report.measurements.frameBudget = {viewport: '1440x900 @1x', roomOpen: true, timelineOpen: open, timelineHidden: hidden,
      addedMedianMs: open.callbackMs.median - hidden.callbackMs.median, addedP95Ms: open.callbackMs.p95 - hidden.callbackMs.p95};
    assert.deepEqual(problems, [], 'no page errors');
    pass('frame cost measured with the Room open', report.measurements.frameBudget);
  } finally { await context.close(); }
}

async function recording() {
  const {context, page} = await launch('recording', {width: 1280, height: 720, video: true});
  try {
    await ready(page);
    await page.evaluate(() => document.querySelector('#timeline-panel').scrollIntoView({block: 'start'}));
    await sleep(4000);
    const first = await snapshot(page);
    const target = first.events.filter(e => e.state === 'pending' && /-(spot|slow|period|accent|wash)$/.test(e.id) && e.chip.cx - first.now.x > 80)
      .sort((a, b) => a.chip.cx - b.chip.cx)[0];
    if (target) {
      await page.locator(`#timeline-panel .timeline-event[data-event-id="${target.id}"]`).click();
      await sleep(1500);
      await page.locator('#timeline-panel .timeline-detail').getByRole('button', {name: /^Cancel event /}).click();
      await sleep(2500);
      await page.locator('#timeline-panel .timeline-detail').getByRole('button', {name: 'Close'}).click();
    }
    await page.locator('#bpm').fill('96');
    await page.locator('#bpm').press('Enter');
    await sleep(6000);
    await page.locator('#bpm').fill('120');
    await page.locator('#bpm').press('Enter');
    await sleep(3000);
  } finally {
    await context.close();
    const directory = join(output, 'video-recording');
    const file = readdirSync(directory).find(name => name.endsWith('.webm'));
    if (file) renameSync(join(directory, file), join(output, 'timeline.webm'));
  }
  pass('recording written', 'timeline.webm');
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
