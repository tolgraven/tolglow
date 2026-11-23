// This scenario saves a macro, so the launcher supplies a disposable preview show.
import assert from 'node:assert/strict';
import { mkdir, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import { chromium } from 'playwright-core';

const [origin, directory] = process.argv.slice(2);
const devices = [
  ['phone', { width: 390, height: 844 }, true],
  ['ipad', { width: 1024, height: 768 }, true],
  ['desktop', { width: 1440, height: 900 }, false]
];

function rectangles(locator) {
  return locator.evaluateAll(nodes => nodes.map(node => {
    const { top, width, height } = node.getBoundingClientRect();
    return [top, width, height].map(Math.round);
  }));
}

async function screenshot(page, path) {
  await page.evaluate(() => window.scrollTo(0, 0));
  await page.screenshot({ path });
}

async function editSteps(page, slots, label, capture) {
  await page.locator('#composition-name').fill('Evening drift · ' + label);
  await page.locator('#composition-id').fill('layout-' + label);
  for (const [index, cue] of ['ember', 'sweep', 'breathe', 'orbital'].entries()) {
    await page.locator('#slot-cue').selectOption(cue);
    await page.locator('#add-slot').click();
    await slots.nth(index).waitFor();
    await capture();
  }
  assert.equal(await slots.count(), 4);
  await page.getByLabel('Step 1 duration value').fill('2');
  await page.getByLabel('Step 1 duration unit').selectOption('bars');
  await screenshot(page, join(directory, label + '-compose.png'));
  await capture();
  const params = slots.first().getByText('Parameters', { exact: true });
  await params.click();
  await slots.first().locator('.slot-parameter-editor[open]').waitFor();
  await capture();
  await params.click();
}

async function saveAndReopenAsMacro(page, slots, label, capture) {
  const behavior = page.locator('#composition-mode');
  await behavior.selectOption('macro');
  await slots.locator('.duration-control').first().waitFor({ state: 'detached' });
  await slots.nth(1).getByRole('button', { name: 'Move earlier: step 2' }).click();
  await slots.first().locator('.slot-title').filter({ hasText: 'Slow sweep' }).waitFor();
  await capture();
  const reply = page.waitForResponse(response =>
    new URL(response.url()).pathname === '/api/composition' && response.request().method() === 'POST');
  await page.locator('#save-composition').click();
  assert.ok((await reply).ok(), 'The host accepts the composition');

  await page.locator('#new-composition').click();
  await slots.first().waitFor({ state: 'detached' });
  const saved = page.locator('[data-composition-id="layout-' + label + '"]');
  await saved.getByRole('button', { name: 'Open' }).click();
  await slots.nth(3).waitFor();
  assert.equal(await behavior.inputValue(), 'macro');
  await behavior.selectOption('sequence');
  assert.equal(await page.getByLabel('Step 2 duration value').inputValue(), '2');
  assert.equal(await page.getByLabel('Step 2 duration unit').inputValue(), 'bars');
}

async function measureRig(page, narrow) {
  await page.locator('a[data-view=rig]').click();
  const rig = page.locator('.rig-editor');
  const tools = rig.getByRole('group', { name: '3D editor tool' });
  const explore = tools.getByRole('button', { name: 'Explore', exact: true });
  await explore.click();
  await rig.locator('.rig-path-form').waitFor({ state: 'detached' });
  const canvas = rig.locator('canvas');
  await canvas.waitFor({ state: 'attached' });
  const full = await rectangles(canvas);

  await tools.getByRole('button', { name: 'Draw path', exact: true }).click();
  await rig.getByLabel('Duration', { exact: true }).waitFor();
  const drawing = await rectangles(canvas);
  if (narrow) assert.equal(full[0][1], drawing[0][1], 'Phone places the inspector below the room');
  else assert.ok(full[0][1] > drawing[0][1], 'Explore returns the inspector width to the room');
  await explore.click();
  await rig.locator('.rig-path-form').waitFor({ state: 'detached' });
  return { explore: full, draw: drawing };
}

async function exercisePage(page, label, viewport) {
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  const frames = join(directory, 'frames');
  let frame = 0;
  async function capture() {
    if (label !== 'ipad') return;
    await mkdir(frames, { recursive: true });
    await screenshot(page, join(frames, String(++frame).padStart(3, '0') + '.png'));
  }

  await page.goto(origin + '/compose');
  await page.locator('#connection-dot.connected').waitFor({ state: 'attached' });
  await page.locator('#slot-cue option[value=orbital]').waitFor({ state: 'attached' });
  await page.locator('#new-composition').click();
  await page.waitForFunction(() => document.querySelector('#composition-name').value === 'New composition');
  const slots = page.locator('.composition-slot');
  await editSteps(page, slots, label, capture);
  const steps = await rectangles(slots);
  const narrow = label === 'phone';
  assert.ok(steps.every(([, width, height]) => width < viewport.width && height <= (narrow ? 110 : 60)),
            'Collapsed steps fit the editor and keep their controls in one tablet row or two phone rows');
  await saveAndReopenAsMacro(page, slots, label, capture);
  const rig = await measureRig(page, narrow);
  assert.deepEqual(errors, []);
  return { steps, ...rig };
}

const response = await fetch(origin + '/api/state', { headers: { Accept: 'application/json' } });
const state = await response.json();
assert.equal(state.status.output.type, 'preview', 'Use a disposable preview-only show');
await mkdir(directory, { recursive: true });
const browser = await chromium.launch({
  executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args: ['--disable-dev-shm-usage', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']
});
try {
  const results = {};
  for (const [label, viewport, hasTouch] of devices) {
    const page = await browser.newPage({ viewport, hasTouch });
    try {
      results[label] = await exercisePage(page, label, viewport);
    } finally {
      await page.close();
    }
  }
  await writeFile(join(directory, 'metrics.json'), JSON.stringify(results, null, 2) + '\n');
  console.log(JSON.stringify(results));
} finally {
  await browser.close();
}
