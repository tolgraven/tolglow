// Run through tolglow.qa.template-groups-browser. Phase "studio" launches a
// template group from the Template groups row, sizes one of its cards in
// Arrange and saves a running macro as a template group. Phase "repl" checks
// that a REPL placement and resize reached the open board, then stops both
// placements from Studio. Template groups: docs/template-groups.md (#812).
import assert from 'node:assert/strict';
import {mkdir} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, directory, phase] = process.argv.slice(2);
const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
await mkdir(directory, {recursive: true});

async function state(page) {
  const response = await page.request.get(origin + '/api/state', {headers: {accept: 'application/json'}});
  assert.ok(response.ok());
  return response.json();
}

async function until(page, predicate, what) {
  for (let attempt = 0; attempt < 80; attempt++) {
    const show = await state(page);
    if (predicate(show)) return show;
    await new Promise(resolve => setTimeout(resolve, 100));
  }
  throw new Error('Timed out waiting for ' + what);
}

const template = (show, id) => show['template-groups'].find(group => group.id === id);
const placements = (show, id) => (template(show, id)?.placements || []).map(p => p.placement);
const section = (page, placement) =>
  page.locator(`section.live-macro[data-macro="template-group/verse-look/${placement}"]`);
const sizes = async locator =>
  locator.locator('[data-card-size]').evaluateAll(cards => cards.map(card => card.dataset.cardSize));

// A screenshot of the Template groups row down to the last placement section.
async function boardShot(page, file) {
  const boxes = [];
  for (const locator of [page.getByRole('region', {name: 'Template groups'}),
                         page.locator('section.live-macro')]) {
    for (const element of await locator.all()) boxes.push(await element.boundingBox());
  }
  const x = Math.min(...boxes.map(b => b.x)), y = Math.min(...boxes.map(b => b.y));
  const right = Math.max(...boxes.map(b => b.x + b.width)), bottom = Math.max(...boxes.map(b => b.y + b.height));
  await page.screenshot({path: join(directory, file), fullPage: true,
                         clip: {x: x - 8, y: y - 8, width: right - x + 16, height: bottom - y + 16}});
}

async function studioPhase(page) {
  await page.goto(origin + '/perform');
  const row = page.getByRole('region', {name: 'Template groups'});
  await row.waitFor();
  assert.equal(await row.locator('li[data-template-group="verse-look"]').count(), 1);
  await row.getByRole('button', {name: 'Launch Verse look', exact: true}).click();
  const launched = await until(page, show => placements(show, 'verse-look').length === 1, 'the main placement');
  const placement = template(launched, 'verse-look').placements[0];
  assert.deepEqual(placement['member-slots'], ['fill', 'pulse', 'spot']);
  const main = section(page, 'main');
  await main.waitFor();
  assert.equal(await main.locator('.live-macro-heading span').first().textContent(), 'Template');
  assert.deepEqual(await sizes(main), ['small', 'small', 'small']);

  await main.getByRole('button', {name: 'Arrange Verse look · main', exact: true}).click();
  await main.locator('button[aria-label^="Large card for"]').nth(1).click();
  const sized = await until(page, show => (template(show, 'verse-look').cards || []).some(c => c.size === 'large'),
                           'a large card on the template');
  assert.equal(template(sized, 'verse-look').version, 1, 'a card edit keeps the version');
  await page.waitForFunction(() => [...document.querySelectorAll(
    'section.live-macro[data-macro="template-group/verse-look/main"] [data-card-size]')]
    .some(card => card.dataset.cardSize === 'large'));
  await main.getByRole('button', {name: 'Finish arranging Verse look · main', exact: true}).click();
  await page.waitForTimeout(3500); // let the launch toast clear
  await boardShot(page, 'studio-placement.png');

  const opening = page.locator('section.live-macro[data-macro="opening"]');
  await opening.waitFor();
  await opening.getByRole('button', {name: 'Save Opening as a template group', exact: true}).click();
  const saved = await until(page, show => template(show, 'opening'), 'the macro saved as a template group');
  assert.deepEqual(template(saved, 'opening').members.map(m => m.id), ['a', 'b']);
  await row.locator('li[data-template-group="opening"]').waitFor();
  console.log('studio phase: launched main, sized', JSON.stringify(template(saved, 'verse-look').cards),
              'saved opening as a template group');
}

async function replPhase(page) {
  await page.goto(origin + '/perform');
  const left = section(page, 'left');
  await left.waitFor();
  assert.equal(await page.locator('section.live-macro[data-template-group="verse-look"]').count(), 2);
  await page.waitForFunction(() => [...document.querySelectorAll(
    'section.live-macro[data-macro="template-group/verse-look/left"] [data-card-size]')]
    .some(card => card.dataset.cardSize === 'medium'));
  // The arrangement belongs to the template, so both placements show the REPL resize.
  const shown = await sizes(left);
  assert.deepEqual(await sizes(section(page, 'main')), shown);
  await boardShot(page, 'repl-placements.png');
  const row = page.getByRole('region', {name: 'Template groups'});
  await row.getByRole('button', {name: 'Stop Verse look', exact: true}).click();
  await until(page, show => placements(show, 'verse-look').length === 0, 'both placements stopped');
  await page.locator('section.live-macro[data-template-group="verse-look"]').first().waitFor({state: 'detached'});
  console.log('repl phase: both placements shown with sizes', JSON.stringify(shown),
              'then stopped from Studio');
}

async function changedPhase(page) {
  await page.goto(origin + '/perform');
  const main = section(page, 'main');
  await main.waitFor();
  await main.getByRole('button', {name: 'Relaunch Verse look · main', exact: true}).waitFor();
  assert.equal(await main.getByRole('button', {name: 'Arrange Verse look · main', exact: true}).count(), 0,
               'a Changed placement offers Relaunch instead of Arrange');
  assert.equal(await main.locator('.live-template-chip').textContent(), 'Changed');
  await main.getByRole('button', {name: 'Relaunch Verse look · main', exact: true}).click();
  const relaunched = await until(page, show => {
    const p = template(show, 'verse-look').placements.find(p => p.placement === 'main');
    return p && !p['stale?'];
  }, 'the main placement relaunched');
  const [left, main2] = template(relaunched, 'verse-look').placements;
  assert.deepEqual([left.placement, left['stale?'], main2['member-slots']], ['left', true, ['fill', 'pulse']]);
  await main.getByRole('button', {name: 'Arrange Verse look · main', exact: true}).waitFor();
  await section(page, 'left').getByRole('button', {name: 'Relaunch Verse look · left', exact: true}).waitFor();
  await boardShot(page, 'changed-relaunch.png');
  console.log('changed phase: Studio relaunched main with', JSON.stringify(main2['member-slots']), '; left still Changed');
}

const browser = await chromium.launch({executablePath, headless: true,
                                       args: ['--headless=new', '--no-first-run', '--no-default-browser-check']});
try {
  const page = await browser.newPage({viewport: {width: 1440, height: 1000}, deviceScaleFactor: 2});
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await ({studio: studioPhase, repl: replPhase, changed: changedPhase}[phase])(page);
  assert.deepEqual(errors, [], 'no page errors');
} finally {
  await browser.close();
}
