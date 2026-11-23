// Run through tolglow.qa.performance-layout against a disposable show.
import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, directory] = process.argv.slice(2);
await mkdir(directory, {recursive: true});
const browser = await chromium.launch({
  executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args: ['--disable-dev-shm-usage', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'],
});
const page = await browser.newPage({viewport: {width: 1024, height: 768}, hasTouch: true});
const errors = [];
page.on('pageerror', error => errors.push(error.message));
const cdp = await page.context().newCDPSession(page);
const touch = (type, touchPoints) => cdp.send('Input.dispatchTouchEvent', {type, touchPoints});
const cell = cue => page.locator('#cue-grid [data-cue-id="' + cue + '"]');
const commands = [];
page.on('request', request => {
  if (request.url() === origin + '/api/command') commands.push(request.postData());
});
const capture = name => page.screenshot({path: join(directory, name + '.png')});

async function center(cue, id) {
  const pad = cell(cue);
  await pad.scrollIntoViewIfNeeded();
  const bounds = await pad.boundingBox();
  return {x: bounds.x + bounds.width / 2, y: bounds.y + bounds.height / 2, id};
}

let initialIds;
async function waitForHeldCount(count) {
  // Only held cues are launched by this isolated operator. Listen to the cards
  // mounted by the normal state socket, including release-phase transitions.
  await page.waitForFunction(({count, initial}) => {
    const running = [...document.querySelectorAll('[data-live-instance]:not(.releasing)')];
    return running.filter(card => !initial.includes(card.dataset.liveInstance)).length === count;
  }, {count, initial: initialIds}, {timeout: 3500});
  const response = await page.request.get(origin + '/api/state', {headers: {accept: 'application/json'}});
  assert.ok(response.ok());
  const {instances} = await response.json();
  const ids = instances.filter(i => i['launch-mode'] === 'held' && i.phase !== 'releasing').map(i => i.id);
  assert.equal(ids.length, count);
  return ids;
}

async function fingerMenu() {
  const finger = await center('ember', 1);
  await touch('touchStart', [finger]);
  const menu = page.getByRole('dialog', {name: /Ember/i});
  try {
    await menu.waitFor();
  } finally {
    await touch('touchEnd', []);
  }
  assert.ok(await menu.getByRole('button', {name: /Copy cue/}).isEnabled());
  await capture('01-finger-menu');
  await menu.getByRole('button', {name: 'Close actions'}).tap();
  assert.equal(commands.length, 0, 'A long press must not change the running cues');
}

async function cancelledDuet() {
  const first = await center('merlot', 1);
  const second = await center('orchid', 2);
  await touch('touchStart', [first, second]);
  const holding = await waitForHeldCount(2);
  await capture('02-two-fingers-held');
  await touch('touchCancel', []);
  await waitForHeldCount(0);
  assert.ok(holding.every(id => commands.some(body => body.includes('instance/end') && body.includes(id))));
  return holding;
}

async function rapidRetrigger() {
  const target = await center('indigo', 1);
  let awaitingFirst = true;
  const releaseReply = Promise.withResolvers();
  await page.route('**/api/command', async route => {
    if (!awaitingFirst || !route.request().postData().includes('cue/launch')) return route.continue();
    awaitingFirst = false;
    const reply = await route.fetch();
    await releaseReply.promise;
    await route.fulfill({response: reply});
  });
  await page.mouse.move(target.x, target.y);
  await page.mouse.down();
  try {
    const [original] = await waitForHeldCount(1);
    await page.mouse.up();
    await page.mouse.down();
    const second = (await waitForHeldCount(2)).find(id => id !== original);
    assert.ok(second);
    releaseReply.resolve();
    assert.deepEqual(await waitForHeldCount(1), [second]);
    await capture('03-second-press-playing');
    return [original, second];
  } finally {
    releaseReply.resolve();
    await page.mouse.up();
    await page.unrouteAll({behavior: 'wait'});
  }
}

try {
  await page.goto(origin + '/perform');
  await cell('ember').waitFor();
  await page.waitForFunction(() => !document.querySelector('#cue-grid [data-cue-id="ember"]').disabled);
  const grid = page.getByRole('group', {name: 'Performance sections'}).getByRole('button', {name: 'Cues', exact: true});
  if (await grid.isVisible()) await grid.tap();
  await page.waitForFunction(() => document.querySelectorAll('[data-live-instance]').length === 4);
  initialIds = await page.locator('[data-live-instance]').evaluateAll(cards => cards.map(card => card.dataset.liveInstance));
  await fingerMenu();
  await page.getByRole('button', {name: 'Hold', exact: true}).tap();
  const cancelled = await cancelledDuet();
  const retriggered = await rapidRetrigger();
  await waitForHeldCount(0);
  await capture('04-released');
  assert.deepEqual(errors, []);
  await writeFile(join(directory, 'gestures.json'), JSON.stringify({cancelled, retriggered, errors}, null, 2) + '\n');
} finally {
  await browser.close();
}
