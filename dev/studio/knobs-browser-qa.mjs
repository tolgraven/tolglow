// Run through tolglow.qa.knobs-browser. Phase "browser" builds a knob in the
// Studio panel and turns it; phase "repl" checks that a REPL knob/set reached
// the open page. Linked knobs: docs/linked-knobs.md (#666).
import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, directory, phase] = process.argv.slice(2);
await mkdir(directory, {recursive: true});

async function state(page) {
  const response = await page.request.get(origin + '/api/state', {headers: {accept: 'application/json'}});
  assert.ok(response.ok());
  return response.json();
}

const resolved = (page, label) => page.getByLabel(`${label} resolved value`, {exact: true});

async function addLink(page, card, destination, fields) {
  const details = card.locator('details.knob-add-link');
  // The live board above re-renders each frame; open the disclosure directly.
  await details.evaluate(element => { element.open = true; });
  await details.getByLabel('Knob destination').selectOption({label: destination});
  for (const [label, value] of Object.entries(fields)) {
    const control = details.getByLabel(label, {exact: true});
    if (await control.evaluate(element => element.tagName === 'SELECT')) await control.selectOption({label: value});
    else await control.fill(value);
  }
  const before = await card.locator('li.knob-link').count();
  await details.getByRole('button', {name: 'Link', exact: true}).click();
  await assert.doesNotReject(card.locator('li.knob-link').nth(before).waitFor());
}

async function browserPhase(page, browser) {
  await page.goto(origin + '/perform');
  const panel = page.getByRole('region', {name: 'Linked knobs'});
  await panel.waitFor();
  await panel.getByLabel('New knob name').fill('Look');
  await panel.getByRole('button', {name: 'Create knob'}).click();
  const card = panel.locator('article.knob-card').first();
  await card.waitFor();

  await addLink(page, card, 'sweep · width', {});
  await addLink(page, card, 'breathe · period', {From: '2', To: '1/2', Steps: 'Preferred beat ratios'});
  await addLink(page, card, 'ember · level LFO gain (high)', {From: '0.5', To: '1', 'Knob start': '0.5'});
  await addLink(page, card, 'ember · level LFO duty', {From: '0.125', To: '0.5', Direction: 'Down', 'Knob start': '0.5'});
  assert.equal(await card.locator('li.knob-link').count(), 4);

  // Just above the 3/7 boundary between 4/3 and 1 beat.
  await card.getByLabel('Look knob').fill(String(3 / 7));
  await page.waitForFunction(() =>
    document.querySelector('[aria-label="breathe · period resolved value"]')?.textContent === '1 beats');
  const texts = {};
  for (const label of ['sweep · width', 'breathe · period', 'ember · level LFO to', 'ember · level LFO duty']) {
    texts[label] = await resolved(page, label).textContent();
  }
  assert.deepEqual(texts, {'sweep · width': '0.193', 'breathe · period': '1 beats',
                           'ember · level LFO to': '0.500', 'ember · level LFO duty': '0.500'});

  // A link edit changes the resolution but writes nothing until the knob moves.
  await card.getByLabel('sweep · width direction').selectOption('down');
  await page.waitForFunction(() =>
    document.querySelector('[aria-label="sweep · width resolved value"]')?.textContent === '0.257');
  const pending = await card.locator('li.knob-link[data-link-id="link-1"] .knob-link-status').textContent();
  assert.match(pending, /differs from the knob/);
  await card.getByLabel('Look knob').fill('0.5');
  await page.waitForFunction(() =>
    document.querySelector('[aria-label="sweep · width resolved value"]')?.textContent === '0.225');

  // A fast drag: every intermediate position is an input event. Turns stay in
  // order, so the owner ends on the last position and the slider never jumps back.
  const slider = card.getByLabel('Look knob');
  const seen = [];
  const positions = Array.from({length: 64}, (_, i) => (i + 10) / 100);
  for (const position of positions) {
    await slider.evaluate((element, value) => {
      const setter = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value').set;
      setter.call(element, String(value));
      element.dispatchEvent(new Event('input', {bubbles: true}));
      element.dispatchEvent(new Event('change', {bubbles: true}));
    }, position);
    seen.push(Number(await slider.inputValue()));
  }
  await assert.doesNotReject(async () => {
    for (let attempt = 0; attempt < 40; attempt++) {
      if ((await state(page)).knobs.knobs[0].value === 0.73) return;
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    throw new Error('the owner did not end on the last dragged position');
  });
  assert.deepEqual(seen, positions, 'the slider followed every turn without jumping back');
  await page.waitForFunction(() => document.querySelector('[aria-label="Look value"]')?.textContent === '0.730');
  await slider.fill('0.5');
  await page.waitForFunction(() =>
    document.querySelector('[aria-label="sweep · width resolved value"]')?.textContent === '0.225');

  // The REPL reads the same committed projection and destination values.
  const show = await state(page);
  const knob = show.knobs.knobs[0];
  assert.equal(knob.name, 'Look');
  assert.equal(knob.value, 0.5);
  assert.deepEqual(knob.links.map(link => link.resolved.text), ['0.225', '1 beats', '0.500', '0.500']);
  assert.ok(knob.links.every(link => !link.diverged && link.status === 'ok'), JSON.stringify(knob.links));
  const instance = cue => show.instances.find(item => item['cue-id'] === cue);
  assert.ok(Math.abs(instance('sweep').params.width - 0.225) < 1e-9);
  assert.equal(instance('breathe').params.period, 1);

  // A refusal is shown and leaves the links as they were.
  await addLink(page, card, 'sweep · width', {From: 'fast'}).catch(() => {});
  await panel.getByRole('alert').waitFor();
  const alert = await panel.getByRole('alert').textContent();
  assert.match(alert, /Enter numbers/);
  assert.equal(await card.locator('li.knob-link').count(), 4);

  await panel.screenshot({path: join(directory, 'knobs-panel-studio.png')});
  // Target sizes at this desktop width, reported rather than asserted.
  const targets = await panel.evaluate(root => [...root.querySelectorAll('button, input, select, summary')]
    .filter(element => element.offsetParent)
    .map(element => {
      const box = element.getBoundingClientRect();
      return [element.getAttribute('aria-label') || element.textContent.trim(), Math.round(box.width), Math.round(box.height)];
    }));
  // On a touch screen every panel control is at least 44 px tall.
  const touch = await browser.newContext({viewport: {width: 1024, height: 1366}, hasTouch: true, isMobile: true});
  try {
    const tablet = await touch.newPage();
    await tablet.goto(origin + '/perform');
    const touchPanel = tablet.getByRole('region', {name: 'Linked knobs'});
    await touchPanel.locator('article.knob-card').first().waitFor();
    assert.ok(await tablet.evaluate(() => matchMedia('(pointer: coarse)').matches));
    const small = await touchPanel.evaluate(root => [...root.querySelectorAll('button, input, select, summary')]
      .filter(element => element.offsetParent)
      .map(element => [element.getAttribute('aria-label') || element.textContent.trim(),
                       Math.round(element.getBoundingClientRect().height)])
      .filter(([, height]) => height < 44));
    assert.deepEqual(small, [], 'touch targets under 44 px');
  } finally {
    await touch.close();
  }
  return {texts, pending, alert, targets, links: knob.links.map(link => [link.label, link.resolved.text, link.status])};
}

async function replPhase(page) {
  await page.goto(origin + '/perform');
  const panel = page.getByRole('region', {name: 'Linked knobs'});
  await panel.waitFor();
  await page.waitForFunction(() =>
    document.querySelector('[aria-label="breathe · period resolved value"]')?.textContent === '1/2 beats');
  const card = panel.locator('article.knob-card').first();
  const value = await card.getByLabel('Look value').textContent();
  assert.equal(value, '1.000');
  assert.equal(await card.getByLabel('Look knob').inputValue(), '1');
  const texts = {};
  for (const label of ['sweep · width', 'breathe · period', 'ember · level LFO to', 'ember · level LFO duty']) {
    texts[label] = await resolved(page, label).textContent();
  }
  assert.deepEqual(texts, {'sweep · width': '0.000', 'breathe · period': '1/2 beats',
                           'ember · level LFO to': '1.000', 'ember · level LFO duty': '0.125'});
  await panel.screenshot({path: join(directory, 'knobs-panel-after-repl.png')});
  return {value, texts};
}

const browser = await chromium.launch({
  executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args: ['--headless=new', '--disable-dev-shm-usage', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'],
});
try {
  const page = await browser.newPage({viewport: {width: 1440, height: 1400}});
  page.setDefaultTimeout(8000);
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  const report = await (phase === 'repl' ? replPhase(page) : browserPhase(page, browser)).catch(async error => {
    await page.screenshot({path: join(directory, `knobs-${phase}-failure.png`), fullPage: true});
    throw error;
  });
  assert.deepEqual(errors, []);
  await writeFile(join(directory, `knobs-${phase}.json`), JSON.stringify(report, null, 2));
  console.log(`knobs ${phase} phase passed`, JSON.stringify(report));
} finally {
  await browser.close();
}
