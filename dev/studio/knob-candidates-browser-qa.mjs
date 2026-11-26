// Run through tolglow.qa.knob-candidates-browser. Phase "browser" generates a
// knob candidate in Studio, sweeps it in the Room, rerolls twice, keeps one and
// asks for a selection with too few parameters. Phase "repl" checks that a REPL
// generation and audition reached the open page. docs/generated-knobs.md (#667).
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

const candidateOf = async page => (await state(page))['knob-candidates'];

async function linkLabels(panel) {
  return panel.locator('li.candidate-link .candidate-link-label').allTextContents();
}

async function browserPhase(page) {
  await page.goto(origin + '/perform');
  const panel = page.getByRole('region', {name: 'Generate a knob'});
  await panel.waitFor();
  await panel.getByLabel('Candidate seed').fill('11');
  await panel.getByRole('button', {name: 'Generate', exact: true}).click();
  const card = panel.locator('article.candidate-card');
  await card.waitFor();
  await eventually(async () => (await card.textContent()).includes('Seed 11'), 'seed 11');
  const first = await linkLabels(panel);
  assert.ok(first.length >= 2, JSON.stringify(first));

  // Sweep to the top: the Room shows the candidate, live output and knobs are unchanged.
  const before = await state(page);
  await setSlider(card.getByLabel('Sweep the candidate in the Room'), 1);
  const banner = page.getByRole('status', {name: 'Room draft audition'});
  await banner.waitFor();
  await eventually(async () => (await banner.textContent()).includes('Knob candidate'), 'candidate banner');
  await eventually(async () => (await candidateOf(page))?.audition?.value === 1, 'audition at 1');
  const auditioned = await state(page);
  assert.deepEqual(auditioned.knobs.knobs, before.knobs.knobs, 'saved knobs unchanged');
  const params = show => Object.fromEntries(show.instances.map(i => [i.id, i.params]));
  assert.deepEqual(params(auditioned), params(before), 'live parameters unchanged');
  assert.equal(await banner.getByRole('button', {name: 'Apply'}).count(), 0, 'a candidate cannot be applied as a scene');
  await page.locator('#preview-panel').screenshot({path: join(directory, 'room-candidate-audition.png')});
  await panel.screenshot({path: join(directory, 'candidate-panel-seed-11.png')});

  // Two rerolls give two more distinct mappings, and the audition follows each.
  const seen = [JSON.stringify(first)];
  for (const seed of [12, 13]) {
    await panel.getByRole('button', {name: 'Reroll'}).click();
    await eventually(async () => (await card.textContent()).includes(`Seed ${seed}`), `seed ${seed}`);
    await eventually(async () => (await candidateOf(page))?.audition?.['draft-revision'] >= seed - 11, 'audition follows');
    seen.push(JSON.stringify(await linkLabels(panel)));
  }
  assert.equal(new Set(seen).size, 3, `three distinct candidates: ${seen.join(' | ')}`);
  await panel.screenshot({path: join(directory, 'candidate-panel-seed-13.png')});

  // Keep the third: it becomes an ordinary knob with its recipe; the audition ends.
  await panel.getByLabel('Candidate knob name').fill('Lift');
  await panel.getByRole('button', {name: 'Keep as knob'}).click();
  const knobs = page.getByRole('region', {name: 'Linked knobs'});
  await knobs.locator('article.knob-card', {hasText: 'Lift'}).waitFor();
  await eventually(async () => !(await candidateOf(page))?.candidate, 'candidate cleared');
  await eventually(async () => (await banner.count()) === 0, 'audition ended');
  const kept = (await state(page)).knobs.knobs.find(knob => knob.name === 'Lift');
  assert.equal(kept.recipe.seed, 13);
  assert.equal(kept.value, 1);
  await panel.getByRole('button', {name: /Regenerate Lift/}).waitFor();
  await knobs.screenshot({path: join(directory, 'kept-knob.png')});

  // Only the two cues without parameters: the panel explains why nothing is generated.
  await panel.getByRole('button', {name: 'gather-path', exact: true}).click();
  await panel.getByRole('button', {name: 'colour-drift', exact: true}).click();
  await panel.getByLabel('Candidate seed').fill('');
  await panel.getByRole('button', {name: 'Generate', exact: true}).click();
  const why = panel.locator('.candidate-insufficient');
  await why.waitFor();
  const explanation = await why.textContent();
  assert.match(explanation, /No parameter in the selection can be generated/);
  assert.match(explanation, /gather-path — The cue has no parameters/);
  await panel.screenshot({path: join(directory, 'candidate-too-few.png')});

  // Phone width: the panel fits without horizontal scrolling.
  await page.setViewportSize({width: 390, height: 1400});
  // Phone width shows one Perform section at a time; the panel is under Live.
  await page.getByRole('group', {name: 'Performance sections'}).getByRole('button', {name: /^Live/}).click();
  await panel.waitFor();
  await panel.getByRole('button', {name: 'gather-path', exact: true}).click();
  await panel.getByRole('button', {name: 'colour-drift', exact: true}).click();
  await panel.getByLabel('Candidate seed').fill('11');
  await panel.getByRole('button', {name: 'Generate', exact: true}).click();
  await eventually(async () => (await panel.textContent()).includes('Seed 11'), 'phone candidate');
  const overflow = await panel.evaluate(root => root.scrollWidth - root.clientWidth);
  assert.ok(overflow <= 1, `panel overflows by ${overflow}px at phone width`);
  await panel.screenshot({path: join(directory, 'candidate-panel-phone.png')});
  return {first, seen, kept: {name: kept.name, recipe: kept.recipe}, explanation};
}

async function replPhase(page) {
  await page.goto(origin + '/perform');
  const panel = page.getByRole('region', {name: 'Generate a knob'});
  await panel.waitFor();
  await eventually(async () => (await panel.textContent()).includes('Seed 30 · Colour'), 'REPL candidate');
  const slider = panel.getByLabel('Sweep the candidate in the Room');
  await eventually(async () => (await slider.inputValue()) === '0.2', 'REPL audition value');
  const banner = page.getByRole('status', {name: 'Room draft audition'});
  await eventually(async () => (await banner.textContent()).includes('at 0.20'), 'REPL audition banner');
  await panel.screenshot({path: join(directory, 'candidate-panel-after-repl.png')});
  return {labels: await linkLabels(panel), slider: await slider.inputValue()};
}

const context = await chromium.launchPersistentContext(resolve('target/knob-candidates-chrome'), {
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
    await page.screenshot({path: join(directory, `candidates-${phase}-failure.png`), fullPage: true});
    throw error;
  });
  assert.deepEqual(errors, []);
  await writeFile(join(directory, `candidates-${phase}.json`), JSON.stringify(report, null, 2));
  console.log(`knob candidates ${phase} phase passed`, JSON.stringify(report));
} finally {
  await context.close();
}
