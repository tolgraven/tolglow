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

  // Ember skin: a portable UI preference.
  await page.getByLabel('Widget style', {exact: true}).selectOption('ember');
  await card.locator('.parameter-dial[data-skin=ember]').first().waitFor();
  const level = card.getByRole('slider', {name: 'Level rotary', exact: true});
  let box = await level.boundingBox();
  const at = (fx, fy) => [box.x + fx * box.width, box.y + fy * box.height];
  const refresh = async () => { await level.scrollIntoViewIfNeeded(); box = await level.boundingBox(); };
  const settle = async () => { await page.waitForTimeout(500); return params(await state(page)).level; };
  // The wheel turns only a focused knob, in proportion to vertical distance:
  // three 100 px events are 300 px of a 1000 px sweep.
  await page.evaluate(() => document.activeElement?.blur());
  await page.mouse.move(...at(0.5, 0.3));
  const unfocused = await settle();
  await page.mouse.wheel(0, -100);
  assert.equal(await settle(), unfocused, 'an unfocused knob lets the page scroll');
  await refresh();
  await page.mouse.move(...at(0.5, 0.3));
  await level.focus();
  // Chromium scales synthetic wheel deltas by the device pixel ratio, so the
  // check reads the CSS-pixel distance the page actually received.
  await page.evaluate(() => { window.__wheelPixels = 0;
    window.addEventListener('wheel', e => { window.__wheelPixels -= e.deltaY * (e.deltaMode === 1 ? 20 : e.deltaMode === 2 ? 400 : 1); }, {capture: true, passive: true}); });
  await page.mouse.wheel(0, -100); await page.mouse.wheel(0, -100); await page.mouse.wheel(0, -100);
  await page.waitForTimeout(200);
  const scrolled = await page.evaluate(() => window.__wheelPixels);
  assert.ok(scrolled > 0, 'the wheel events reached the page');
  p = await until(page, q => near(q.level, Math.min(1, unfocused + scrolled / 1000), 0.005), 'the ember wheel turn');
  const turned = await settle();
  await page.mouse.wheel(120, 0);
  assert.equal(await settle(), turned, 'a horizontal wheel does not turn the knob');
  report.wheel = {from: unfocused, to: turned, pixels: scrolled};
  await refresh();
  // A fresh press in the bottom gap holds the value...
  await page.mouse.click(...at(0.5, 0.92));
  assert.equal(await settle(), turned, 'a press in the bottom gap is a dead zone');
  // ...but a drag through the gap lands exactly on the nearer bound.
  const dragPath = async points => {
    await page.mouse.move(...at(...points[0])); await page.mouse.down();
    for (const point of points.slice(1)) await page.mouse.move(...at(...point), {steps: 6});
    await page.mouse.up();
  };
  await dragPath([[0.5, 0.12], [0.95, 0.47], [0.6, 0.95]]);
  await until(page, q => q.level === 1, 'a drag through the high end');
  await dragPath([[0.5, 0.12], [0.05, 0.47], [0.4, 0.95]]);
  await until(page, q => q.level === 0, 'a drag through the low end');
  report.dragThroughGap = [1, 0];
  const settings = await (await page.request.get(origin + '/api/settings', {headers: {accept: 'application/json'}})).json();
  assert.equal(settings.ui['knob-skin'], 'ember');
  await page.keyboard.press('PageUp'); await page.keyboard.press('PageUp');
  await until(page, q => near(q.level, 0.2, 1e-9), 'Level keys');
  await shot(row('level'), 'ember-knob');

  report.repl = await replRoundTrip(page, card);
  report.comparison = await skinComparison(page, card, row, shot);

  assert.deepEqual(errors, []);
  await context.close();
  return report;
}

// Standard and Ember with identical parameter values and no focus or hover,
// then each knob skin at 0, 0.5 and 1 (set with Home and PageUp, which send
// param/set like any other key).
async function skinComparison(page, card, row, shot) {
  const skin = async name => {
    await page.getByLabel('Widget style', {exact: true}).selectOption(name);
    await card.locator(name === 'ember' ? '.parameter-dial[data-skin=ember]' : '.parameter-dial:not([data-skin])').first().waitFor();
  };
  const rest = async () => {
    await page.evaluate(() => document.activeElement?.blur());
    await page.mouse.move(2, 2);
    await page.waitForTimeout(400);
  };
  const values = params(await state(page));
  const rings = card.getByRole('button', {name: 'Edit Tone components', exact: true});
  for (const name of ['standard', 'ember']) {
    await skin(name); await rest();
    await shot(card, 'ab-' + name);
    for (const param of ['level', 'offset', 'aim', 'steps', 'tone']) await shot(row(param), 'ab-' + param + '-' + name);
    await rings.click();
    await card.getByRole('group', {name: 'Tone components', exact: true}).waitFor();
    await rest();
    await shot(row('tone'), 'ab-tone-open-' + name);
    await rings.click();
    await card.getByRole('group', {name: 'Tone components', exact: true}).waitFor({state: 'detached'});
  }
  assert.deepEqual(params(await state(page)), values, 'the A/B pairs show identical values');
  const level = card.getByRole('slider', {name: 'Level rotary', exact: true});
  for (const [label, keys, value] of [['0', ['Home'], 0], ['0.5', ['Home', 'PageUp', 'PageUp', 'PageUp', 'PageUp', 'PageUp'], 0.5], ['1', ['End'], 1]]) {
    await level.focus();
    for (const key of keys) await page.keyboard.press(key);
    await until(page, q => near(q.level, value, 1e-9), 'Level at ' + label);
    for (const name of ['standard', 'ember']) {
      await skin(name); await rest();
      await shot(level, 'knob-' + name + '-' + label);
    }
  }
  // The compact strip, Standard then Ember, with the same values.
  await card.getByRole('button', {name: /^Collapse /}).click();
  await card.locator('.control-strip .parameter-xy').first().waitFor();
  const strip = params(await state(page));
  for (const name of ['standard', 'ember']) {
    await skin(name); await rest();
    await shot(card, 'ab-strip-' + name);
  }
  assert.deepEqual(params(await state(page)), strip);
  return values;
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
  // A second finger on the same pad is ignored while the first holds it.
  const box = await pad.boundingBox();
  const point = (fx, fy, id) => ({x: box.x + fx * box.width, y: box.y + fy * box.height, id});
  const cdp = await page.context().newCDPSession(page);
  await cdp.send('Input.dispatchTouchEvent', {type: 'touchStart', touchPoints: [point(0.75, 0.25, 0)]});
  await cdp.send('Input.dispatchTouchEvent', {type: 'touchStart', touchPoints: [point(0.75, 0.25, 0), point(0.2, 0.8, 1)]});
  for (let i = 1; i <= 6; i++)
    await cdp.send('Input.dispatchTouchEvent', {type: 'touchMove', touchPoints: [point(0.75, 0.25, 0), point(0.2 - i * 0.02, 0.8 + i * 0.02, 1)]});
  await cdp.send('Input.dispatchTouchEvent', {type: 'touchEnd', touchPoints: []});
  await cdp.detach();
  await page.waitForTimeout(500);
  const twoFingers = await until(page, q => near(q.offset[0], 0.5) && near(q.offset[1], 0.5), 'the first finger keeping the pad');
  const depth = card.getByRole('slider', {name: 'Aim Depth', exact: true}).locator('.fader-track');
  await touchDrag(page, depth, [0.5, 0.5], [0.5, 0.95]);
  const q = await until(page, r => near(r.aim[2], 1, 0.6), 'the touch drag on the Z fader');
  await card.screenshot({path: join(directory, 'studio-touch-ipad-width.png')});
  assert.deepEqual(errors, []);
  await context.close();
  return {xyTouch: p.offset, twoFingers: twoFingers.offset, depthTouch: q.aim[2]};
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
