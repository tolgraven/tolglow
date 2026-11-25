// Run through tolglow.qa.performance-layout with the cue-actions scenario.
import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, directory] = process.argv.slice(2);
await mkdir(directory, {recursive: true});

async function readState(page) {
  const response = await page.request.get(origin + '/api/state', {headers: {accept: 'application/json'}});
  assert.ok(response.ok());
  return response.json();
}

async function touchTarget(control, name) {
  const box = await control.boundingBox();
  assert.ok(box && box.width >= 44 && box.height >= 44, `${name} target needs 44 × 44 CSS px: ${JSON.stringify(box)}`);
  return {width: box.width, height: box.height};
}

async function longPress(page, pad) {
  await pad.scrollIntoViewIfNeeded();
  const {x, y, width, height} = await pad.boundingBox();
  const cdp = await page.context().newCDPSession(page);
  try {
    await cdp.send('Input.dispatchTouchEvent', {
      type: 'touchStart', touchPoints: [{id: 1, x: x + width / 2, y: y + height / 2}],
    });
    await page.getByRole('dialog', {name: 'Ember', exact: true}).waitFor();
  } finally {
    await cdp.send('Input.dispatchTouchEvent', {type: 'touchEnd', touchPoints: []});
    await cdp.detach();
  }
}

const profiles = [
  {name: 'desktop', viewport: {width: 1440, height: 900}, hasTouch: false},
  {name: 'tablet', viewport: {width: 1024, height: 768}, hasTouch: true},
  {name: 'phone', viewport: {width: 390, height: 844}, hasTouch: true},
];

async function exercise(page, {name, viewport, hasTouch}) {
  const errors = [];
  const writes = [];
  const appendBodies = [];
  page.on('pageerror', error => errors.push(error.message));
  page.on('request', request => {
    if (request.method() === 'POST' && request.url().startsWith(origin + '/api/')) {
      const path = new URL(request.url()).pathname;
      writes.push(path);
      if (path === '/api/composition/append') appendBodies.push(request.postData());
    }
  });
  const click = control => hasTouch ? control.tap() : control.click();
  const macro = (state, id) => state.compositions.find(item => item.id === id);
  const activeIds = state => state.instances.map(instance => instance.id).sort();

  await page.goto(origin + '/perform');
  const cueSection = page.getByRole('group', {name: 'Performance sections'}).getByRole('button', {name: 'Cues', exact: true});
  if (await cueSection.isVisible()) await click(cueSection);
  const pad = page.locator('#cue-grid [data-cue-id="ember"]:enabled');
  await pad.waitFor();
  const before = await readState(page);
  assert.equal(before.status.output.type, 'preview');
  const target = macro(before, 'qa/night-set');
  assert.equal(target.name, 'Night set');

  if (hasTouch) await longPress(page, pad);
  else {
    await pad.focus();
    await page.keyboard.press('Shift+F10');
  }
  const actions = page.getByRole('dialog', {name: 'Ember', exact: true});
  const action = actions.getByRole('button', {name: 'Add to macro', exact: true});
  await action.waitFor();
  const open = await touchTarget(action, 'Add to macro');
  await click(action);
  const picker = page.getByRole('dialog', {name: 'Add Ember to macro'});
  const choice = picker.getByRole('combobox', {name: 'Saved macro', exact: true});
  const cancel = picker.getByRole('button', {name: 'Cancel', exact: true});
  const add = picker.getByRole('button', {name: 'Add cue', exact: true});
  await choice.waitFor();
  await page.waitForFunction(() => document.activeElement?.id === 'cue-saved-macro');
  const touchTargets = {
    open,
    select: await touchTarget(choice, 'Saved macro'),
    cancel: await touchTarget(cancel, 'Cancel'),
    add: await touchTarget(add, 'Add cue'),
    close: await touchTarget(picker.getByRole('button', {name: 'Close actions'}), 'Close actions'),
  };
  assert.deepEqual(await choice.locator('option').allTextContents(), ['Choose a macro', 'Night set', 'Early set']);
  assert.ok(await add.isDisabled());
  await choice.selectOption({label: 'Early set'});
  await click(cancel);
  await actions.waitFor();
  await page.waitForFunction(() => document.activeElement?.id === 'add-to-saved-macro');
  assert.ok(await actions.getByRole('button', {name: 'Add to composition'}).isEnabled());
  assert.deepEqual((await readState(page)).compositions, before.compositions);
  assert.ok(!writes.includes('/api/composition/append'));

  await click(action);
  await choice.selectOption({label: 'Night set'});
  const bounds = await picker.boundingBox();
  assert.ok(bounds && bounds.x >= 0 && bounds.y >= 0 && bounds.x + bounds.width <= viewport.width && bounds.y + bounds.height <= viewport.height);
  await page.screenshot({path: join(directory, `${name}-macro-picker.png`)});
  if (hasTouch) await click(add);
  else {
    const endpoint = '**/api/composition/append';
    await page.route(endpoint, async route => {
      const receipt = await route.fetch();
      assert.ok(receipt.ok(), 'Ring committed the first request before its reply was lost');
      await route.abort('failed');
    }, {times: 1});
    await click(add);
    await click(picker.getByRole('button', {name: 'Retry append', exact: true}));
  }
  await page.getByText('Added to Night set.', {exact: true}).waitFor();
  await picker.waitFor({state: 'hidden'});
  if (!hasTouch) assert.equal(appendBodies[0], appendBodies[1], 'The retry used the original command ID and request');

  const after = await readState(page);
  const saved = macro(after, 'qa/night-set');
  assert.deepEqual(saved.slots.slice(0, -1), target.slots);
  assert.equal(saved.slots.at(-1)['cue-id'], 'ember');
  assert.equal(saved['running?'], false);
  assert.deepEqual(macro(after, 'qa/early-set'), macro(before, 'qa/early-set'));
  assert.deepEqual(macro(after, 'qa/steps'), macro(before, 'qa/steps'));
  assert.deepEqual(activeIds(after), activeIds(before));
  assert.deepEqual(writes.filter(path => path === '/api/command' || path.startsWith('/api/composition')),
                   Array(hasTouch ? 1 : 2).fill('/api/composition/append'));
  assert.deepEqual(errors, []);
  return {viewport, bounds, touchTargets, macro: saved.id, slot: saved.slots.at(-1),
          appendAttempts: appendBodies.length, activeInstances: activeIds(after)};
}

const browser = await chromium.launch({
  executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args: ['--disable-dev-shm-usage', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'],
});
try {
  const report = {};
  for (const profile of profiles) {
    const page = await browser.newPage({viewport: profile.viewport, hasTouch: profile.hasTouch});
    page.setDefaultTimeout(7000);
    try {
      report[profile.name] = await exercise(page, profile);
    } catch (error) {
      const menu = page.locator('#cue-actions');
      if (await menu.isVisible()) console.error(await menu.ariaSnapshot());
      await page.screenshot({path: join(directory, `${profile.name}-failure.png`)});
      throw error;
    } finally {
      await page.close();
    }
  }
  const output = JSON.stringify(report, null, 2) + '\n';
  await writeFile(join(directory, 'macro-picker.json'), output);
  console.log(output);
} finally {
  await browser.close();
}
