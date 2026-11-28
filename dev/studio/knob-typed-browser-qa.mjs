// Run through tolglow.qa.knob-typed-browser. Phase "browser" generates a knob
// candidate restricted to choices, switches and numbers, adds a choice to it
// from the typed picker and sweeps it in the Room, then links a switch to a
// kept knob and turns it. Phase "repl" checks that a REPL knob with a vector
// link reached the open page. docs/generated-knobs.md (#667).
import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, directory, phase] = process.argv.slice(2);
await mkdir(directory, {recursive: true});

async function state(page) {
  const response = await page.request.get(origin + '/api/state', {headers: {accept: 'application/json'}});
  assert.ok(response.ok());
  return response.json();
}

async function eventually(check, what) {
  for (let attempt = 0; attempt < 80; attempt++) {
    const value = await check();
    if (value) return value;
    await new Promise(done => setTimeout(done, 100));
  }
  throw new Error(`timed out waiting for ${what}`);
}

async function setSlider(slider, value) {
  await slider.evaluate((element, v) => {
    const setter = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value').set;
    setter.call(element, String(v));
    element.dispatchEvent(new Event('input', {bubbles: true}));
    element.dispatchEvent(new Event('change', {bubbles: true}));
  }, value);
}

const typedParams = show => show.instances.find(i => i['cue-id'] === 'typed').params;

async function browserPhase(page) {
  await page.goto(origin + '/perform');
  const panel = page.getByRole('region', {name: 'Generate a knob'});
  await panel.waitFor();

  // Restrict the candidate to choices, switches and numbers.
  const types = panel.getByRole('group', {name: 'Destination types the candidate may use'});
  for (const name of ['Choices', 'Switches', 'Numbers']) await types.getByRole('button', {name, exact: true}).click();
  await panel.getByLabel('Candidate seed').fill('3');
  await panel.getByRole('button', {name: 'Generate', exact: true}).click();
  const card = panel.locator('article.candidate-card');
  await card.waitFor();
  await eventually(async () => (await card.textContent()).includes('Seed 3'), 'seed 3');
  const generated = (await state(page))['knob-candidates'];
  assert.deepEqual(generated.candidate.recipe.types, ['boolean', 'enum', 'number']);
  const kinds = generated.candidate.links.map(link => link.schema.type);
  assert.ok(kinds.every(type => ['number', 'enum', 'boolean'].includes(type)), JSON.stringify(kinds));

  // Add a choice from the picker: it joins the candidate with hysteresis.
  const picker = card.locator('details.knob-add-link');
  await picker.locator('summary').click();
  const destination = picker.getByLabel('Knob destination');
  const options = await destination.locator('option').allTextContents();
  for (const label of ['typed · shape (choice)', 'typed · strobe (switch)', 'typed · centre (vector)', 'typed · tint (colour)'])
    assert.ok(options.includes(label), `${label} offered: ${options.join(', ')}`);
  const existing = generated.candidate.links.find(link => link.destination.param === 'shape');
  if (existing) await card.getByRole('button', {name: `Drop ${existing.label} from the candidate`}).click();
  if (existing) await eventually(async () => !(await state(page))['knob-candidates'].candidate.links
    .some(link => link.destination.param === 'shape'), 'generated choice dropped');
  await destination.selectOption({label: 'typed · shape (choice)'});
  // A choice's endpoints become lists of its declared values once the picker re-renders.
  await picker.locator('select[aria-label="From"]').selectOption({label: 'dot'});
  await picker.locator('select[aria-label="To"]').selectOption({label: 'wave'});
  assert.equal(await picker.getByLabel('Hysteresis').inputValue(), '0.05');
  await picker.getByRole('button', {name: 'Link'}).click();
  const shapeRow = card.locator('li.candidate-link', {hasText: 'typed · shape'});
  await shapeRow.waitFor();
  assert.match(await shapeRow.textContent(), /dot → wave/);
  assert.match(await shapeRow.textContent(), /holds ±0\.05/);

  // Sweep it in the Room: zones of 0.2 hold their step 0.05 past a boundary.
  const before = typedParams(await state(page));
  const sweep = card.getByLabel('Sweep the candidate in the Room');
  const seen = [];
  for (const value of [0.25, 0.17, 0.14]) {
    await setSlider(sweep, value);
    await eventually(async () => (await state(page))['knob-candidates']?.audition?.value === value, `audition at ${value}`);
    const text = await eventually(async () => {
      const link = (await state(page))['knob-candidates'].candidate.links.find(l => l.destination.param === 'shape');
      return link?.resolved?.text;
    }, 'resolved shape');
    await eventually(async () => (await shapeRow.locator('output').textContent()) === text, 'row shows the shared value');
    seen.push(text);
  }
  assert.deepEqual(seen, ['bar', 'bar', 'dot'], 'the sweep holds bar at 0.17 and moves on at 0.14');
  assert.deepEqual(typedParams(await state(page)), before, 'live parameters unchanged by the audition');
  await panel.screenshot({path: join(directory, 'typed-candidate.png')});
  await card.getByRole('button', {name: 'Discard'}).click();

  // A kept knob links a switch from the same picker and turns it live.
  const knobs = page.getByRole('region', {name: 'Linked knobs'});
  await knobs.getByLabel('New knob name').fill('Flash');
  await knobs.getByRole('button', {name: 'Create knob'}).click();
  const knob = knobs.locator('article.knob-card', {hasText: 'Flash'});
  await knob.waitFor();
  const add = knob.locator('details.knob-add-link');
  await add.locator('summary').click();
  await add.getByLabel('Knob destination').selectOption({label: 'typed · strobe (switch)'});
  assert.equal(await add.locator('select[aria-label="From"]').inputValue(), 'false');
  assert.equal(await add.locator('select[aria-label="To"]').inputValue(), 'true');
  assert.equal(await add.getByLabel('Curve').count(), 0, 'a switch has no curve');
  await add.getByRole('button', {name: 'Link'}).click();
  const row = knob.locator('li.knob-link', {hasText: 'typed · strobe'});
  await row.waitFor();
  assert.match(await row.textContent(), /off → on/);
  const slider = knob.getByLabel('Flash knob');
  for (const [value, strobe] of [[0.6, true], [0.48, true], [0.44, false]]) {
    await setSlider(slider, value);
    await eventually(async () => typedParams(await state(page)).strobe === strobe, `strobe ${strobe} at ${value}`);
  }
  await eventually(async () => (await row.locator('output').textContent()) === 'off', 'row shows off');
  await knobs.screenshot({path: join(directory, 'typed-knob.png')});

  // Phone width: both panels fit without horizontal scrolling.
  await page.setViewportSize({width: 390, height: 1400});
  await page.getByRole('group', {name: 'Performance sections'}).getByRole('button', {name: /^Live/}).click();
  for (const region of [knobs, panel]) {
    await region.waitFor();
    const overflow = await region.evaluate(root => root.scrollWidth - root.clientWidth);
    assert.ok(overflow <= 1, `panel overflows by ${overflow}px at phone width`);
  }
  await knobs.screenshot({path: join(directory, 'typed-knob-phone.png')});
  return {kinds, seen, strobe: typedParams(await state(page)).strobe};
}

async function replPhase(page) {
  await page.goto(origin + '/perform');
  const knobs = page.getByRole('region', {name: 'Linked knobs'});
  const knob = knobs.locator('article.knob-card', {hasText: 'Aim'});
  await knob.waitFor();
  const row = knob.locator('li.knob-link', {hasText: 'typed · centre'});
  await eventually(async () => (await row.locator('output').textContent()) === '[0.000 1.000 2.000] m', 'REPL vector value');
  assert.match(await row.textContent(), /-2 0 1 → 2 2 3/);
  await knobs.screenshot({path: join(directory, 'typed-knob-after-repl.png')});
  return {value: await row.locator('output').textContent()};
}

const context = await chromium.launchPersistentContext(resolve('target/knob-typed-chrome'), {
  executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args: ['--headless=new', '--use-angle=metal', '--disable-dev-shm-usage'],
  viewport: {width: 1440, height: 1400},
});
try {
  const page = await context.newPage();
  page.setDefaultTimeout(10000);
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  const report = await (phase === 'repl' ? replPhase(page) : browserPhase(page)).catch(async error => {
    await page.screenshot({path: join(directory, `typed-${phase}-failure.png`), fullPage: true});
    throw error;
  });
  assert.deepEqual(errors, []);
  await writeFile(join(directory, `typed-${phase}.json`), JSON.stringify(report, null, 2));
  console.log(`typed knobs ${phase} phase passed`, JSON.stringify(report));
} finally {
  await context.close();
}
