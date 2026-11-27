// Place, mount, calibrate and save a library fixture through the optimized
// Studio against a dedicated captured-output ./bin/studio. It publishes a QA
// fixture type and mutates that show's rig; never point it at a real show.
// usage: node dev/preview/placement-browser-qa.mjs http://127.0.0.1:19300 target/placement-qa
import {chromium} from 'playwright-core';
import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
const origin = process.argv[2] || 'http://127.0.0.1:8515';
const directory = process.argv[3] || 'target/placement-qa';
const json = {headers: {Accept: 'application/json'}};
const state = async () => (await fetch(`${origin}/api/state`, json)).json();
const post = async (path, body) => {
  const response = await fetch(`${origin}${path}`, {method: 'POST', headers: {'Content-Type': 'application/json', Accept: 'application/json'}, body: JSON.stringify(body)});
  return {ok: response.ok, data: await response.json()};
};
assert.equal((await state()).status.output.type, 'preview', 'Use an isolated captured-output Studio');
await mkdir(directory, {recursive: true});
const typeId = `qa-spot-${Date.now()}`;
const published = await post('/api/fixtures/publish', {source: `{:id "${typeId}" :version 1 :name "QA moving spot" :footprint 7
  :channels {:dimmer 1 :red 2 :green 3 :blue 4 :pan [5 6] :tilt 7}
  :calibration {:pan {:min-deg -270 :max-deg 270} :tilt {:min-deg -135 :max-deg 135}}}`});
assert.ok(published.ok, JSON.stringify(published.data));

const context = await chromium.launchPersistentContext(`${directory}/chrome-profile`, {
  executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium', headless: true,
  args: ['--headless=new', '--use-angle=metal', '--enable-gpu'], viewport: {width: 1440, height: 1200}, deviceScaleFactor: 1});
const report = {origin, typeId, steps: []};
const step = (name, data = {}) => {report.steps.push({name, ...data}); console.log(`placement: ${name}`);};
try {
  const page = await context.newPage(), errors = []; page.setDefaultTimeout(30000);
  page.on('pageerror', error => errors.push(error.message));
  page.on('console', message => {if (message.type() === 'error') errors.push(message.text());});
  await page.goto(origin); await page.locator('[data-view="rig"]').first().click();
  await page.waitForFunction(() => document.querySelector('.rig-editor canvas')?.tolglowPreview);
  await page.waitForFunction(() => document.querySelector('.rig-editor canvas').tolglowPreview.ready, null, {timeout: 120000});
  const panel = page.locator('.rig-editor');
  const settle = () => page.evaluate(() => new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r))));
  const snapshot = async () => {await settle(); return page.evaluate(() => document.querySelector('.rig-editor canvas').tolglowPreview.snapshot);};
  const project = async point => {
    await page.locator('.rig-editor canvas').scrollIntoViewIfNeeded();
    return page.evaluate(point => {
      const e = document.querySelector('.rig-editor canvas').tolglowPreview, b = e.canvas.getBoundingClientRect();
      e.camera.updateMatrixWorld(); const p = e.camera.position.clone().fromArray(point).project(e.camera);
      return {x: b.left + (p.x + 1) * b.width / 2, y: b.top + (1 - p.y) * b.height / 2};
    }, point);
  };
  // The worker presents frames asynchronously; give it time to draw the edit.
  const shot = async name => {await settle(); await page.waitForTimeout(1500); await panel.screenshot({path: `${directory}/${name}.png`}); step(`screenshot ${name}.png`);};
  const before = await state();

  await panel.getByRole('button', {name: 'Place fixture', exact: true}).click();
  await panel.getByLabel('Fixture type to place').selectOption(typeId);
  const floor = await project([1, 0, 2]);
  await page.mouse.click(floor.x, floor.y);
  let rig = (await snapshot()).rig, placed = rig.fixtures.at(-1);
  assert.equal(rig.fixtures.length, before.rig.fixtures.length + 1);
  assert.equal(placed['fixture-type-id'], typeId);
  assert.ok(Math.abs(placed.position[0] - 1) < .05 && Math.abs(placed.position[1] - .25) < 1e-6 && Math.abs(placed.position[2] - 2) < .05, JSON.stringify(placed.position));
  assert.deepEqual(placed['rotation-deg'], [0, 0, 0]);
  assert.deepEqual((await state()).rig.fixtures.length, before.rig.fixtures.length, 'Placing is a draft');
  step('placed on the floor', {id: placed.id, position: placed.position, universe: placed.universe, address: placed.address});
  await shot('01-placed-on-floor');

  await panel.getByRole('button', {name: 'Undo', exact: true}).click();
  assert.equal((await snapshot()).rig.fixtures.length, before.rig.fixtures.length);
  const again = await project([1, 0, 2]);
  await page.mouse.click(again.x, again.y);
  rig = (await snapshot()).rig; placed = rig.fixtures.at(-1);
  assert.equal(rig.fixtures.length, before.rig.fixtures.length + 1, JSON.stringify((await snapshot()).editor));
  step('undo removed the placement; placed again', {id: placed.id});

  await panel.getByRole('button', {name: 'Hanging', exact: true}).click();
  placed = (await snapshot()).rig.fixtures.at(-1);
  assert.deepEqual(placed['rotation-deg'], [0, 0, 180]);
  await panel.getByLabel('Y · m', {exact: true}).fill('4.6'); await panel.getByLabel('Y · m', {exact: true}).press('Tab');
  await panel.getByRole('button', {name: 'Aim home', exact: true}).click();
  const head = await project((await snapshot()).rig.fixtures.at(-1).position);
  await page.mouse.move(head.x, head.y); await page.mouse.down(); await page.mouse.move(head.x + 160, head.y + 10, {steps: 6}); await page.mouse.up();
  placed = (await snapshot()).rig.fixtures.at(-1);
  assert.notDeepEqual(placed['rotation-deg'], [0, 0, 180], 'Aim home turned the fixture');
  step('hung, raised to 4.6 m and turned with the home-beam handle', {rotation: placed['rotation-deg']});
  await shot('02-hanging-aim-home');

  const saved = page.waitForResponse(r => new URL(r.url()).pathname === '/api/rig' && r.request().method() === 'POST');
  await page.getByRole('button', {name: 'Save rig', exact: true}).click();
  const response = await saved; assert.ok(response.ok(), await response.text());
  const committed = (await state()).rig.fixtures.find(f => String(f.id) === String(placed.id));
  assert.ok(committed, 'The saved rig contains the placed fixture');
  assert.equal(committed['fixture-type-id'], typeId); assert.equal(committed.footprint, 7);
  assert.deepEqual(committed['rotation-deg'].map(n => +n.toFixed(4)), placed['rotation-deg'].map(n => +n.toFixed(4)));
  step('saved through /api/rig', {committed: {id: committed.id, universe: committed.universe, address: committed.address,
    position: committed.position, rotation: committed['rotation-deg'], channels: committed.channels}});

  await panel.getByRole('button', {name: 'Calibrate aim', exact: true}).click();
  const target = await project([0, 0, 0]);
  await page.mouse.click(target.x, target.y);
  await settle();
  const check = page.waitForResponse(r => new URL(r.url()).pathname === '/api/fixtures/aim-check');
  await panel.getByRole('button', {name: 'Check aim', exact: true}).click();
  assert.ok((await check).ok());
  const result = await panel.getByLabel('Aim check result').textContent();
  assert.match(result, /Pan .*DMX .*tilt .*DMX/);
  step('aim check solved the tapped floor target', {result});
  await shot('03-calibrate-aim');
  for (const point of [[2, 0, 1], [-2, 0, -1]]) {
    for (const [i, label] of ['X', 'Y', 'Z'].entries()) {
      const field = panel.getByLabel(`Target ${label} · m`, {exact: true}); await field.fill(String(point[i])); await field.press('Tab');
    }
    await panel.getByRole('button', {name: 'Record current pan/tilt', exact: true}).click();
  }
  await panel.getByRole('button', {name: 'Record current pan/tilt', exact: true}).click();
  assert.equal((await snapshot()).editor['calibration-points'].length, 3);
  const fit = page.waitForResponse(r => new URL(r.url()).pathname === '/api/fixtures/aim-fit');
  await panel.getByRole('button', {name: 'Fit mounting', exact: true}).click();
  assert.ok((await fit).ok());
  step('fit request answered for three recorded observations', {result: await panel.getByLabel('Mounting fit result').textContent()});
  assert.deepEqual(errors, [], 'No page errors');
  await writeFile(`${directory}/report.json`, JSON.stringify(report, null, 2));
  console.log('placement: all checks passed');
} finally {
  await context.close();
}
