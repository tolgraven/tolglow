// Run through tolglow.qa.knob-looks-browser. Phase "studio" saves two knob
// looks from the Studio strip and recalls the first; phase "repl" checks a REPL
// recall reached the open strip; phase "push" renders the virtual Push 2 views
// through the shipped capture worker. Knob looks: docs/linked-knobs.md (#547).
import assert from 'node:assert/strict';
import {mkdir, readFile, writeFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const require = createRequire(import.meta.url);
const {createWorker} = require('../../resources/devices/push-display/worker.cjs');
const [origin, directory, phase] = process.argv.slice(2);
const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
await mkdir(directory, {recursive: true});

async function state(page) {
  const response = await page.request.get(origin + '/api/state', {headers: {accept: 'application/json'}});
  assert.ok(response.ok());
  return response.json();
}

async function until(page, predicate, what) {
  for (let attempt = 0; attempt < 60; attempt++) {
    const show = await state(page);
    if (predicate(show)) return show;
    await new Promise(resolve => setTimeout(resolve, 100));
  }
  throw new Error('Timed out waiting for ' + what);
}

const knobValues = show => Object.fromEntries(show.knobs.knobs.map(knob => [knob.id, knob.value]));
const slotStates = show => show['knob-presets'].slots.map(slot => slot.state);
const look = (strip, n) => strip.locator('li.knob-look').nth(n - 1);

async function setKnob(page, name, value) {
  const slider = page.getByLabel(`${name} knob`, {exact: true});
  await slider.fill(String(value));
}

async function studioPhase(page) {
  await page.goto(origin + '/perform');
  const strip = page.getByRole('region', {name: 'Knob looks'});
  await strip.waitFor();
  assert.equal(await strip.locator('li.knob-look[data-state="empty"]').count(), 8);
  await strip.getByRole('button', {name: 'Save Look 1', exact: true}).click();
  await until(page, show => slotStates(show)[0] === 'selected', 'look 1 saved');
  await look(strip, 1).and(page.locator('[data-state="selected"]')).waitFor();

  await setKnob(page, 'Look', 0.8);
  await setKnob(page, 'Glow', 0.9);
  await until(page, show => knobValues(show).look === 0.8 && knobValues(show).glow === 0.9, 'knobs turned');
  await page.locator('li.knob-look[data-moved="true"]').first().waitFor();
  const moved = await look(strip, 1).locator('.knob-look-state').textContent();
  assert.equal(moved, 'selected · moved');
  await strip.getByRole('button', {name: 'Save Look 2', exact: true}).click();
  await until(page, show => slotStates(show)[1] === 'selected', 'look 2 saved');

  await strip.getByRole('button', {name: 'Recall Look 1', exact: true}).click();
  const show = await until(page, show => knobValues(show).look === 0.2 && knobValues(show).glow === 0.3, 'look 1 recalled');
  assert.deepEqual(slotStates(show).slice(0, 3), ['selected', 'stored', 'empty']);
  const instance = cue => show.instances.find(item => item['cue-id'] === cue);
  assert.ok(Math.abs(instance('sweep').params.width - 0.13) < 1e-9, 'width resolves 0.2 through 0.05–0.45');
  assert.equal(instance('breathe').params.period, 1.5, '0.2 selects 3/2 beats from 2 → 1/2');
  // Glow uses ease-in; 0.3 on the knob is not 0.3 of the range.
  assert.ok(Math.abs(instance('ember').params.level - (0.2 + 0.8 * 0.3)) > 0.01, 'the ease-in curve is kept');
  await page.locator('li.knob-look[data-state="selected"]:not([data-moved])').first().waitFor();
  await strip.screenshot({path: join(directory, 'knob-looks-studio.png')});

  // Touch layout: the strip's buttons meet the 44 px target on a tablet.
  await strip.getByRole('button', {name: 'Save Look 3', exact: true}).click();
  await until(page, show => slotStates(show)[2] === 'selected', 'look 3 saved');
  const tablet = await page.context().browser().newContext({viewport: {width: 1024, height: 1366}, hasTouch: true, isMobile: true});
  try {
    const touch = await tablet.newPage();
    await touch.goto(origin + '/perform');
    const touchStrip = touch.getByRole('region', {name: 'Knob looks'});
    await touchStrip.locator('li.knob-look').first().waitFor();
    const small = await touchStrip.evaluate(root => [...root.querySelectorAll('button')]
      .filter(element => element.offsetParent)
      .map(element => [element.getAttribute('aria-label'), Math.round(element.getBoundingClientRect().height)])
      .filter(([, height]) => height < 44));
    assert.deepEqual(small, [], 'touch targets under 44 px');
  } finally { await tablet.close(); }
  return {states: slotStates(await state(page)), moved};
}

async function replPhase(page) {
  await page.goto(origin + '/perform');
  const strip = page.getByRole('region', {name: 'Knob looks'});
  await look(strip, 2).and(page.locator('[data-state="selected"]')).waitFor();
  const summary = await look(strip, 2).locator('.knob-look-summary').textContent();
  assert.equal(summary, 'Look 0.800 · Glow 0.900');
  assert.equal(await page.getByLabel('Look value', {exact: true}).textContent(), '0.800');
  await strip.screenshot({path: join(directory, 'knob-looks-after-repl.png')});
  return {summary};
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
    for (const {id, view} of frames) {
      const label = id.split(':')[1];
      const result = await worker.capture(view);
      await writeFile(join(directory, 'push-' + label + '.png'), Buffer.from(result.png, 'base64'));
      report[label] = await page.locator('.push-knob-looks article').evaluateAll(columns =>
        columns.map(column => column.className));
    }
    assert.deepEqual(errors, []);
    assert.equal(report['knob-looks-after'].length, 8);
    assert.equal(report['knob-looks-after'][0], 'selected');
    return report;
  } finally { await worker.close(); }
}

if (phase === 'push') {
  console.log(JSON.stringify({phase, report: await pushPhase()}));
} else {
  const browser = await chromium.launch({
    executablePath,
    args: ['--headless=new', '--disable-dev-shm-usage', '--use-angle=metal'],
  });
  try {
    const page = await browser.newPage({viewport: {width: 1440, height: 1400}});
    page.setDefaultTimeout(8000);
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    const report = await (phase === 'repl' ? replPhase(page) : studioPhase(page)).catch(async error => {
      await page.screenshot({path: join(directory, `knob-looks-${phase}-failure.png`), fullPage: true});
      throw error;
    });
    assert.deepEqual(errors, []);
    console.log(JSON.stringify({phase, report}));
  } finally {
    await browser.close();
  }
}
