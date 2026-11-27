// Run through tolglow.qa.compound-widgets (#655). Phase "studio" drags the XY
// pad (mouse and touch), the Z fader and a vertical fader, opens and closes
// the ring knob, steps a sub-knob, switches to the Ember knob skin and turns a
// knob with the wheel, reading every committed value back from /api/state.
// While that page stays open the harness submits REPL param/set commands and
// the phase checks they reached the open view.
// Phase "push" renders the recorded Push views through the capture worker.
// Headless Chromium; this is not iPad Safari or physical Push evidence.
import assert from 'node:assert/strict';
import {mkdir, readFile, writeFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const require = createRequire(import.meta.url);
const {createWorker} = require('../../resources/devices/push-display/worker.cjs');
const [origin, id, directory, phase] = process.argv.slice(2);
assert.ok(origin && id && directory && phase, 'Pass the show URL, the instance ID, a capture directory and a phase');
const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
await mkdir(directory, {recursive: true});

async function state(page) {
  const response = await page.request.get(origin + '/api/state', {headers: {accept: 'application/json'}});
  assert.ok(response.ok());
  return response.json();
}

const params = show => (show.instances.find(instance => String(instance.id) === id) || {}).params || {};

async function until(page, predicate, what) {
  let last;
  for (let attempt = 0; attempt < 80; attempt++) {
    last = params(await state(page));
    if (predicate(last)) return last;
    await new Promise(resolve => setTimeout(resolve, 100));
  }
  throw new Error('Timed out waiting for ' + what + '; last params ' + JSON.stringify(last));
}

const near = (a, b, tolerance = 0.03) => Math.abs(a - b) <= tolerance;

async function drag(page, locator, from, to, steps = 8) {
  const box = await locator.boundingBox();
  const at = ([fx, fy]) => [box.x + fx * box.width, box.y + fy * box.height];
  const [x0, y0] = at(from), [x1, y1] = at(to);
  await page.mouse.move(x0, y0);
  await page.mouse.down();
  await page.mouse.move(x1, y1, {steps});
  await page.mouse.up();
}

// Touch through the DevTools protocol so Chromium raises touch pointer events.
async function touchDrag(page, locator, from, to) {
  const box = await locator.boundingBox();
  const at = ([fx, fy]) => ({x: box.x + fx * box.width, y: box.y + fy * box.height});
  const cdp = await page.context().newCDPSession(page);
  await cdp.send('Input.dispatchTouchEvent', {type: 'touchStart', touchPoints: [at(from)]});
  for (let i = 1; i <= 8; i++) {
    const f = [from[0] + (to[0] - from[0]) * i / 8, from[1] + (to[1] - from[1]) * i / 8];
    await cdp.send('Input.dispatchTouchEvent', {type: 'touchMove', touchPoints: [at(f)]});
  }
  await cdp.send('Input.dispatchTouchEvent', {type: 'touchEnd', touchPoints: []});
  await cdp.detach();
}

async function studioPhase(browser) {
  const context = await browser.newContext({viewport: {width: 1440, height: 1100}, deviceScaleFactor: 2});
  const page = await context.newPage();
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(origin + '/perform');
  const card = page.locator('article.live-effect-card[data-live-instance=' + JSON.stringify(id) + ']');
  await card.waitFor();
  const shot = async (locator, name) => {
    await locator.scrollIntoViewIfNeeded();
    await locator.screenshot({path: join(directory, 'studio-' + name + '.png')});
  };
  const report = {};
  await page.waitForTimeout(400);
  await shot(card, 'strip');

  await card.getByRole('button', {name: /^Expand /}).click();
  const row = param => card.locator('[data-param=' + param + ']');
  await row('offset').locator('.parameter-xy').waitFor();
  await shot(card, 'detail-before');

  // XY pad: press at one point and drag to another; both axes commit at once.
  const pad = card.getByRole('slider', {name: 'Offset pad', exact: true});
  assert.equal(await pad.getAttribute('aria-roledescription'), 'XY pad');
  await drag(page, pad, [0.5, 0.5], [0.75, 0.25]);
  let p = await until(page, q => near(q.offset[0], 0.5) && near(q.offset[1], 0.5), 'the XY drag');
  report.xyMouse = p.offset;
  await page.waitForFunction(() => document.querySelector('[data-param=offset] .xy-pointer')?.style.left.startsWith('7'));
  await shot(row('offset'), 'xy-dragged');
  // Dragging past the edge clamps to the bounds.
  await drag(page, pad, [0.75, 0.25], [1.4, -0.4]);
  p = await until(page, q => q.offset[0] === 1 && q.offset[1] === 1, 'the clamped XY drag');
  report.xyClamped = p.offset;
  // Keys move one axis at a time.
  await pad.focus();
  await page.keyboard.press('ArrowLeft');
  await page.keyboard.press('Shift+ArrowDown');
  p = await until(page, q => near(q.offset[0], 0.98, 1e-6) && near(q.offset[1], 0.8, 1e-6), 'XY keys');
  report.xyKeys = p.offset;

  // XYZ: the pad edits pan and tilt; the Z fader beside it edits depth.
  const aimPad = card.getByRole('slider', {name: 'Aim pad', exact: true});
  await drag(page, aimPad, [0.5, 0.5], [0.25, 0.75]);
  p = await until(page, q => near(q.aim[0], -90, 4) && near(q.aim[1], -45, 3) && q.aim[2] === 5, 'the aim pad drag');
  report.aimPad = p.aim;
  const depth = card.getByRole('slider', {name: 'Aim Depth', exact: true});
  assert.equal(await depth.getAttribute('aria-orientation'), 'vertical');
  const depthTrack = depth.locator('.fader-track');
  await drag(page, depthTrack, [0.5, 0.9], [0.5, 0.25]);
  p = await until(page, q => near(q.aim[2], 15, 0.5) && near(q.aim[0], -90, 4), 'the Z fader drag');
  report.aimDepth = p.aim;
  await shot(row('aim'), 'xyz-dragged');

  // Vertical fader on a declared integer fader: ticks, drag and keys.
  const steps = card.getByRole('slider', {name: 'Steps', exact: true});
  assert.equal(await steps.locator('.fader-tick').count(), 9);
  assert.equal(await steps.locator('.fader-tick.major').count(), 3);
  await drag(page, steps.locator('.fader-track'), [0.5, 0.8], [0.5, -0.2]);
  await until(page, q => q.steps === 8, 'the fader drag to the top');
  await steps.focus();
  await page.keyboard.press('ArrowDown');
  p = await until(page, q => q.steps === 7, 'the fader key');
  report.steps = p.steps;
  await shot(row('steps'), 'fader');

  // Ring knob: pressing opens one knob per sub-parameter; Escape closes it.
  const rings = card.getByRole('button', {name: 'Edit Tone components', exact: true});
  assert.equal(await rings.getAttribute('aria-expanded'), 'false');
  assert.equal(await row('tone').locator('.ring-track').count(), 4);
  await shot(row('tone'), 'rings-closed');
  await rings.click();
  await card.locator('button[aria-label="Edit Tone components"][aria-expanded=true]').waitFor();
  const components = card.getByRole('group', {name: 'Tone components', exact: true});
  assert.equal(await components.getByRole('slider').count(), 4);
  await shot(row('tone'), 'rings-open');
  const mid = components.getByRole('slider', {name: 'Tone Mid rotary', exact: true});
  await mid.focus();
  await page.keyboard.press('PageUp');
  p = await until(page, q => near(q.tone[1], 0.5, 1e-6) && q.tone[0] === 0.2, 'the Mid sub-knob');
  report.ringStep = p.tone;
  await page.waitForFunction(() => document.querySelector('[data-param=tone] .rings-legend')?.textContent.includes('0.5'));
  await page.keyboard.press('Escape');
  await components.waitFor({state: 'detached'});
  assert.equal(await rings.getAttribute('aria-expanded'), 'false');
  assert.equal(await page.evaluate(() => document.activeElement?.getAttribute('aria-label')), 'Edit Tone components');

  // Ember skin: a portable UI preference; the knob turns with the wheel.
  await page.getByLabel('Knob style', {exact: true}).selectOption('ember');
  await card.locator('.parameter-dial[data-skin=ember]').first().waitFor();
  const level = card.getByRole('slider', {name: 'Level rotary', exact: true});
  const before = params(await state(page)).level;
  const box = await level.boundingBox();
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  for (let i = 0; i < 5; i++) await page.mouse.wheel(0, -60);
  p = await until(page, q => q.level > before + 0.01, 'the ember wheel turn');
  // Let the queued wheel edits settle, then a press in the bottom dead zone holds the value.
  await page.waitForTimeout(600);
  const held = params(await state(page)).level;
  report.wheel = [before, held];
  await page.mouse.click(box.x + box.width / 2, box.y + box.height * 0.92);
  await page.waitForTimeout(400);
  assert.equal(params(await state(page)).level, held, 'the bottom gap is a dead zone');
  const settings = await (await page.request.get(origin + '/api/settings', {headers: {accept: 'application/json'}})).json();
  assert.equal(settings.ui['knob-skin'], 'ember');
  await shot(card, 'detail-ember');
  await shot(row('level'), 'ember-knob');

  report.repl = await replRoundTrip(page, card);

  // Strip layout with the Ember skin.
  await card.getByRole('button', {name: /^Collapse /}).click();
  await card.locator('.control-strip .parameter-xy').first().waitFor();
  await shot(card, 'strip-ember');
  assert.deepEqual(errors, []);
  await context.close();
  return report;
}

async function touchPhase(browser) {
  // iPad-sized viewport with touch input. Chromium touch emulation only.
  const context = await browser.newContext({viewport: {width: 834, height: 1112}, deviceScaleFactor: 2, hasTouch: true, isMobile: false});
  const page = await context.newPage();
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(origin + '/perform');
  const card = page.locator('article.live-effect-card[data-live-instance=' + JSON.stringify(id) + ']');
  await card.waitFor();
  await card.getByRole('button', {name: /^Expand /}).click();
  const pad = card.getByRole('slider', {name: 'Offset pad', exact: true});
  await pad.scrollIntoViewIfNeeded();
  await touchDrag(page, pad, [0.5, 0.5], [0.1, 0.9]);
  const p = await until(page, q => near(q.offset[0], -0.8) && near(q.offset[1], -0.8), 'the touch drag on the XY pad');
  const depth = card.getByRole('slider', {name: 'Aim Depth', exact: true}).locator('.fader-track');
  await touchDrag(page, depth, [0.5, 0.5], [0.5, 0.95]);
  const q = await until(page, r => near(r.aim[2], 1, 0.6), 'the touch drag on the Z fader');
  await card.screenshot({path: join(directory, 'studio-touch-ipad-width.png')});
  assert.deepEqual(errors, []);
  await context.close();
  return {xyTouch: p.offset, depthTouch: q.aim[2]};
}

async function waitForFile(path, ms) {
  for (let waited = 0; waited < ms; waited += 100) {
    try { return await readFile(path, 'utf8'); } catch { await new Promise(resolve => setTimeout(resolve, 100)); }
  }
  throw new Error('Timed out waiting for ' + path);
}

// With this page still open, the harness submits REPL param/set commands:
// offset [-0.5 0.5], aim [45 30 10] and tone [0.9 0.1 0.9 0.1].
async function replRoundTrip(page, card) {
  await writeFile(join(directory, 'studio-open'), 'open\n');
  await waitForFile(join(directory, 'repl-done'), 30000);
  const pad = card.getByRole('slider', {name: 'Offset pad', exact: true});
  await page.waitForFunction(() => document.querySelector('[aria-label="Offset pad"]')?.getAttribute('aria-valuetext') === 'X -0.5, Y 0.5');
  const pointer = await card.locator('[aria-label="Offset pad"] .xy-pointer').getAttribute('style');
  assert.match(pointer, /left: ?25%/);
  assert.match(pointer, /bottom: ?75%/);
  await page.waitForFunction(() => document.querySelector('[aria-label="Aim Depth"]')?.getAttribute('aria-valuenow') === '10');
  assert.equal(await card.getByRole('slider', {name: 'Aim pad', exact: true}).getAttribute('aria-valuetext'), 'Pan 45, Tilt 30');
  await page.waitForFunction(() => /Low 0.9, Mid 0.1, High 0.9, Air 0.1/.test(document.querySelector('[data-param=tone] svg.parameter-rings-glyph')?.getAttribute('aria-label')));
  await card.screenshot({path: join(directory, 'studio-after-repl.png')});
  return {pad: await pad.getAttribute('aria-valuetext'), pointer};
}

async function pushPhase() {
  const recording = await readFile(join(directory, 'push-views.ndjson'), 'utf8');
  const frames = recording.trim().split('\n').map(line => JSON.parse(line));
  let browser;
  const worker = await createWorker({url: origin + '/push-display', executablePath,
    startupTimeoutMs: 30000, captureTimeoutMs: 8000}, async options => (browser = await chromium.launch(options)));
  const page = browser.contexts()[0].pages()[0];
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  const report = {};
  try {
    for (const {id: frame, view} of frames) {
      const label = frame.split(':')[1];
      const result = await worker.capture(view);
      await writeFile(join(directory, 'push-' + label + '.png'), Buffer.from(result.png, 'base64'));
      report[label] = {rings: await page.locator('.typed-rings').count(),
                       subKnobs: await page.locator('.ring-components .ring-component').count()};
    }
    assert.ok(report.resting.rings > 0, 'the untouched tone encoder shows the rings');
    assert.equal(report.resting.subKnobs, 0);
    assert.equal(report['touch-tone'].subKnobs, 4, 'touching the tone opens one knob per sub-parameter');
    assert.deepEqual(errors, []);
  } finally { await worker.close(); }
  return report;
}

if (phase === 'push') {
  console.log(JSON.stringify({phase, report: await pushPhase()}));
} else {
  const browser = await chromium.launch({executablePath, headless: true,
    args: ['--headless=new', '--use-angle=metal']});
  try {
    const report = {...await studioPhase(browser), ...await touchPhase(browser)};
    console.log(JSON.stringify({phase, report}));
  } finally { await browser.close(); }
}
