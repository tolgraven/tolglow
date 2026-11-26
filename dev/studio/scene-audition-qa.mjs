// Audition a Room draft in the optimized Studio against a disposable preview show.
// Arguments: the Ring origin, a directory for screenshots and the live cue's instance ID.
// Set CHROMIUM_ARGS to choose the GPU backend; the default is software WebGL.
import assert from 'node:assert/strict';
import {mkdir, rm, writeFile} from 'node:fs/promises';
import {setTimeout as delay} from 'node:timers/promises';
import {chromium} from 'playwright-core';

const [origin, directory, liveInstance] = process.argv.slice(2);
assert.ok(origin && directory && liveInstance, 'Specify the preview origin, a capture directory and the live instance');
await mkdir(directory, {recursive: true});

async function request(path, body) {
  const response = await fetch(origin + path, {method: body ? 'POST' : 'GET',
    headers: {accept: 'application/json', 'content-type': 'application/json'},
    body: body && JSON.stringify(body)});
  return {ok: response.ok, data: await response.json()};
}
const state = async () => (await request('/api/state')).data;
const command = async fields => {
  const {ok, data} = await request('/api/command', fields);
  assert.ok(ok && data.status !== 'rejected', JSON.stringify(data));
  return data;
};
const stage = operations => command({op: 'scene-draft/stage', 'draft-id': 'chorus', operations});
const red = values => values['head-1']['emitted-color'][0];
const blue = values => values['head-1']['emitted-color'][2];

const initial = await state();
assert.equal(initial.status.output.type, 'preview');
assert.ok(red(initial.output.values) > blue(initial.output.values), 'the live show starts red');

const profile = directory + '/chrome-profile';
const context = await chromium.launchPersistentContext(profile, {
  executablePath: process.env.CHROMIUM_PATH || chromium.executablePath(), headless: false,
  viewport: {width: 1280, height: 900}, deviceScaleFactor: 1,
  args: ['--headless=new', ...(process.env.CHROMIUM_ARGS || '--use-angle=swiftshader --enable-unsafe-swiftshader').split(' ')]});
const report = {origin, renderer: null, steps: []};
try {
  const page = context.pages()[0] || await context.newPage();
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(origin, {waitUntil: 'domcontentloaded'});
  await page.waitForFunction(() => document.querySelector('#connection-label')?.textContent === 'Studio connected');
  const panel = page.locator('#preview-panel');
  const banner = page.locator('.scene-draft-banner');
  await panel.scrollIntoViewIfNeeded();
  await page.waitForFunction(() => /Canvas · [0-9.]+ fps/.test(document.querySelector('#preview-panel')?.innerText), null, {timeout: 90000});
  report.renderer = await page.evaluate(() => document.querySelector('#preview').tolglowPreview.getTelemetry().deviceRenderer);

  // Mean colour of the Room canvas as the browser composites it. The work light
  // adds a neutral floor, so compare the red and blue means by their difference.
  const room = () => page.evaluate(() => {
    const source = document.querySelector('#preview');
    const canvas = document.createElement('canvas');
    canvas.width = 160; canvas.height = 90;
    const context = canvas.getContext('2d', {willReadFrequently: true});
    context.drawImage(source, 0, 0, canvas.width, canvas.height);
    const pixels = context.getImageData(0, 0, canvas.width, canvas.height).data;
    let red = 0, blue = 0;
    for (let i = 0; i < pixels.length; i += 4) { red += pixels[i]; blue += pixels[i + 2]; }
    return {red: red / (pixels.length / 4), blue: blue / (pixels.length / 4)};
  });
  const settle = async (label, expected) => {
    let colour;
    for (let attempt = 0; attempt < 40; ++attempt) {
      colour = await room();
      if (expected(colour)) break;
      await delay(250);
    }
    report.steps.push({label, room: colour});
    assert.ok(expected(colour), label + ': ' + JSON.stringify(colour));
    await panel.screenshot({path: directory + '/' + label + '.png'});
  };

  const redRoom = c => c.red - c.blue > 3, blueRoom = c => c.blue - c.red > 3;
  await settle('1-live', redRoom);
  assert.equal(await banner.count(), 0);

  // Stage from the web command path, as any adapter does.
  const staged = await stage([{op: 'instance/end', 'instance-id': liveInstance}, {op: 'cue/launch', 'cue-id': 'indigo'}]);
  assert.equal(staged['draft-revision'], 0);
  await banner.waitFor();
  await page.waitForFunction(() => /chorus · revision 0 · 2 staged/.test(document.querySelector('.scene-draft-banner')?.innerText));
  await settle('2-draft-audition', blueRoom);
  const during = await state();
  assert.ok(red(during.output.values) > blue(during.output.values), 'live output values stay red during the audition');
  assert.ok(blue(during['draft-output'].values) > red(during['draft-output'].values), 'the draft frame is blue');
  assert.deepEqual(during['draft-output'].draft, {id: 'chorus', revision: 0});
  assert.equal(during.instances.filter(instance => instance['cue-id'] === 'indigo').length, 0);
  report.steps.push({label: 'audition', frames: during['scene-draft'].frames});

  // Apply on the next bar from the banner.
  await banner.getByRole('button', {name: 'Apply on next bar'}).click();
  await page.waitForFunction(() => /applies at beat [0-9.]+/.test(document.querySelector('.scene-draft-banner')?.innerText));
  assert.ok(await banner.getByRole('button', {name: 'Apply', exact: true}).isDisabled());
  await panel.screenshot({path: directory + '/3-scheduled.png'});
  await banner.waitFor({state: 'detached', timeout: 15000});
  const applied = await state();
  const outcome = applied['scene-draft']['last-outcome'];
  assert.equal(outcome.status, 'applied');
  assert.equal(outcome.beat, outcome['due-beat']);
  assert.equal(applied.instances.filter(instance => instance['cue-id'] === 'indigo').length, 1);
  assert.equal(applied['draft-output'], null);
  await settle('4-applied-live', blueRoom);
  report.steps.push({label: 'applied', outcome});

  // A live cue edit makes the next draft stale; the banner says why and only Discard remains.
  await stage([{op: 'cue/launch', 'cue-id': 'ember'}]);
  await banner.waitFor();
  await command({op: 'cue/put', cue: {id: 'ember-again', 'effect-id': 'ember'}});
  await page.waitForFunction(() => /stale: a cue or effect changed\. Stage it again/.test(document.querySelector('.scene-draft-banner')?.innerText));
  assert.ok(await banner.getByRole('button', {name: 'Apply', exact: true}).isDisabled());
  assert.ok(await banner.getByRole('button', {name: 'Apply on next bar'}).isDisabled());
  await panel.screenshot({path: directory + '/5-stale.png'});
  const refused = await request('/api/command', {op: 'scene-draft/apply', 'draft-id': 'chorus', 'expected-draft-revision': 0});
  assert.equal(refused.data.code, 'stale-registry');
  await banner.getByRole('button', {name: 'Discard'}).click();
  await banner.waitFor({state: 'detached'});
  await page.waitForFunction(() => document.querySelector('#toast')?.textContent === 'Room draft discarded.');
  assert.equal((await state())['scene-draft'], null);
  assert.deepEqual(errors, []);
  await writeFile(directory + '/report.json', JSON.stringify(report, null, 2));
  console.log(JSON.stringify(report));
} finally {
  await context.close();
  await rm(profile, {recursive: true, force: true});
}
