// Run through tolglow.qa.post-effects against the demo show (#479).
import assert from 'node:assert/strict';
import {mkdir} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, directory] = process.argv.slice(2);
await mkdir(directory, {recursive: true});
const context = await chromium.launchPersistentContext(join(directory, 'profile'), {
  executablePath: process.env.CHROMIUM_PATH
    || (process.platform === 'darwin' ? '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome' : '/usr/bin/chromium'),
  headless: true,
  args: ['--headless=new', '--disable-dev-shm-usage', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'],
  viewport: {width: 1280, height: 900},
});
const page = context.pages()[0] || await context.newPage();
const errors = [];
page.on('pageerror', error => errors.push(error.message));
const commands = [];
page.on('request', request => {
  if (request.url() === origin + '/api/command') commands.push(request.postData() || '');
});
const replies = [];
page.on('response', async response => {
  if (response.url() === origin + '/api/command') replies.push(response.status() + ' ' + (await response.text()).slice(0, 300));
});
page.on('console', message => { if (message.type() === 'error') errors.push('console: ' + message.text()); });
const capture = name => page.screenshot({path: join(directory, name + '.png')});

async function state() {
  const response = await page.request.get(origin + '/api/state', {headers: {accept: 'application/json'}});
  assert.ok(response.ok());
  return response.json();
}

const value = (map, id) => map[id] ?? map[':' + id];

function emitted(snapshot, id) {
  return value(snapshot.output.values, id)['emitted-color'];
}

function saturation([r, g, b]) {
  const high = Math.max(r, g, b);
  return high > 0 ? (high - Math.min(r, g, b)) / high : 0;
}

async function until(predicate, message) {
  for (let tries = 0; tries < 80; tries++) {
    if (predicate(await state())) return;
    await page.waitForTimeout(50);
  }
  throw new Error('Timed out: ' + message + ' ' + JSON.stringify({commands, replies, errors}));
}

const running = snapshot => snapshot.instances.some(i => i['cue-id'] === 'left-out' && i.phase === 'running');

async function heldIds() {
  const {instances} = await state();
  return instances.filter(i => i['cue-id'] === 'left-out' && i.phase !== 'releasing').map(i => [i.id, i['launch-mode']]);
}

try {
  await page.goto(origin);
  const pad = page.locator('#cue-grid [data-cue-id="left-out"]');
  await pad.waitFor();
  assert.equal(await page.locator('#launch-modes button.selected').getAttribute('data-mode'), 'toggle',
               'Studio stays in toggle mode; the cue itself asks to be held');

  // The guard lifts Moon, a muddy warm white, toward its saturation floor on the right half only.
  let before = await state();
  for (let tries = 0; tries < 40 && !value(before.output.values, 'head-8'); tries++) {
    await page.waitForTimeout(100);
    before = await state();
  }
  const left = emitted(before, 'head-2');
  const right = emitted(before, 'head-7');
  assert.ok(saturation(left) < 0.3, 'Unguarded Moon is a low-saturation mix: ' + left);
  // The guard's floor curve: s + (f - s) * smoothstep(s / f), here f = 0.75.
  const t = saturation(left) / 0.75;
  const lifted = saturation(left) + (0.75 - saturation(left)) * t * t * (3 - 2 * t);
  assert.ok(Math.abs(saturation(right) - lifted) < 0.01, 'The guard lifts the right half: ' + right);
  assert.ok(saturation(right) > saturation(left) + 0.08);
  assert.ok(Math.abs(Math.max(...right) - Math.max(...left)) < 0.005, 'The guard keeps the value');
  await pad.scrollIntoViewIfNeeded();
  await capture('01-guarded-right-half');

  // Hold the blackout pad with the mouse; release it.
  const box = await pad.boundingBox();
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  const hit = await page.evaluate(({x, y}) => {
    const element = document.elementFromPoint(x, y);
    return element ? (element.closest('[data-cue-id]')?.dataset.cueId ?? element.tagName + '.' + element.className) : null;
  }, {x: box.x + box.width / 2, y: box.y + box.height / 2});
  assert.equal(hit, 'left-out', 'The pad is under the pointer');
  await page.mouse.down();
  await until(running, 'the press launches the blackout');
  const held = await heldIds();
  assert.equal(held.length, 1);
  assert.equal(held[0][1], 'held');
  await page.waitForTimeout(150);
  const dark = await state();
  for (const id of ['head-1', 'head-2', 'head-3', 'head-4']) {
    assert.deepEqual(emitted(dark, id).map(Math.round), [0, 0, 0], id + ' is dark while held');
  }
  assert.ok(Math.max(...emitted(dark, 'head-6')) > 0.2, 'The right half keeps its light');
  await capture('02-left-half-held-dark');
  await page.mouse.up();
  await until(snapshot => !running(snapshot), 'the release ends the blackout');
  await page.waitForTimeout(150);
  const restored = await state();
  assert.ok(Math.max(...emitted(restored, 'head-2')) > 0.2, 'Release restores the left half');
  const launches = commands.filter(c => c.includes('cue/launch') && c.includes('left-out'));
  assert.equal(launches.length, 1, 'One press, one launch');
  assert.ok(launches[0].includes('held'), 'The press asks for a held launch');
  assert.equal(commands.filter(c => c.includes('instance/end')).length, 1, 'The release ends it');
  await capture('03-released');

  // The Compose layer editor offers both colour filters with editable settings.
  await page.goto(origin + '/compose');
  await page.locator('#slot-cue option[value=moon]').waitFor({state: 'attached'});
  await page.locator('#slot-cue').selectOption('moon');
  await page.locator('#add-slot').click();
  const slot = page.locator('.composition-slot').first();
  await slot.waitFor();
  await slot.locator('.layer-transforms summary').click();
  const add = slot.getByLabel('Add layer transform to slot 1');
  await add.selectOption('guard');
  await add.selectOption('grade');
  const transforms = slot.locator('.layer-transform');
  assert.equal(await transforms.count(), 2);
  assert.ok(await transforms.nth(0).getByText('Guard previous color to a palette').isVisible());
  assert.ok(await transforms.nth(1).getByText('Shade previous color').isVisible());
  assert.ok(await transforms.nth(0).getByLabel('Saturation floor').isVisible());
  await transforms.nth(0).getByLabel('Hue restriction').fill('0.7');
  await slot.scrollIntoViewIfNeeded();
  await capture('04-compose-colour-filters');

  assert.deepEqual(errors, []);
  console.log(JSON.stringify({guarded: {left, right}, held, commands: commands.length}));
} finally {
  await context.close();
}
