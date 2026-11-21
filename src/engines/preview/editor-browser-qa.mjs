// Actual optimized re-frame UI + captured-output Ring backend. Run against a
// dedicated ./bin/studio without physical output flags; this mutates its show.
import {chromium} from 'playwright-core';
import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
const origin = process.argv[2] || 'http://127.0.0.1:8515';
const directory = process.argv[3] || '/tmp/studio-qa/rig-editor';
const state = async () => (await fetch(`${origin}/api/state`)).json();
assert.equal((await state()).status.output.type, 'preview', 'Use an isolated captured-output Studio');
await mkdir(directory, {recursive: true});
const browser = await chromium.launch({executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium', headless: true,
  args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
const reports = [];
async function open({width = 1440, height = 1100, touch = false, fallback = false} = {}) {
  const context = await browser.newContext({viewport: {width, height}, hasTouch: touch, deviceScaleFactor: 1});
  if (fallback) await context.addInitScript(() => {window.Worker = undefined;});
  const page = await context.newPage(), errors = []; page.setDefaultTimeout(30000);
  page.on('pageerror', error => errors.push(error.message));
  page.on('console', message => {if (message.type() === 'error') errors.push(message.text());});
  await page.goto(origin); await page.locator('[data-view="rig"]').click();
  await page.waitForFunction(() => document.querySelector('.rig-editor canvas')?.tolglowPreview);
  await page.evaluate(() => document.querySelector('.rig-editor canvas').tolglowPreview.setQuality('low'));
  await page.waitForFunction(() => document.querySelector('.rig-editor canvas').tolglowPreview.ready, null, {timeout: 120000});
  return {page, context, errors, panel: page.locator('.rig-editor')};
}
async function project(page, point) {
  await page.locator('.rig-editor canvas').scrollIntoViewIfNeeded();
  return page.evaluate(point => {
    const e = document.querySelector('.rig-editor canvas').tolglowPreview, b = e.canvas.getBoundingClientRect();
    e.camera.updateMatrixWorld(); const p = e.camera.position.clone().fromArray(point).project(e.camera);
    return {x: b.left + (p.x + 1) * b.width / 2, y: b.top + (1 - p.y) * b.height / 2};
  }, point);
}
async function drag(page, point, dx, dy, cancel = false) {
  const p = await project(page, point); await page.mouse.move(p.x, p.y); await page.mouse.down();
  await page.mouse.move(p.x + dx, p.y + dy, {steps: 5});
  if (cancel) await page.keyboard.press('Escape');
  await page.mouse.up();
}
const draft = page => page.evaluate(async () => {await new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r))); return document.querySelector('.rig-editor canvas').tolglowPreview.snapshot.rig;});
const editor = page => page.evaluate(async () => {await new Promise(r => requestAnimationFrame(() => requestAnimationFrame(r))); return document.querySelector('.rig-editor canvas').tolglowPreview.snapshot.editor;});
async function number(page, label, value) {
  const field = page.locator('.rig-editor').getByLabel(label, {exact: true}); await field.fill(String(value)); await field.press('Tab');
}
async function posted(page, path, action) {
  const response = page.waitForResponse(r => new URL(r.url()).pathname === path && r.request().method() === 'POST');
  await action(); const result = await response; const body = await result.json();
  assert.ok(result.ok(), JSON.stringify(body)); assert.notEqual(body.status, 'rejected', JSON.stringify(body)); return body;
}
try {
  const {page, context, errors, panel} = await open();
  const before = await state(), index = 3, fixture = before.rig.fixtures[index];
  console.log('desktop: actual worker ready');
  await panel.getByRole('button', {name: 'Move', exact: true}).click();
  await drag(page, fixture.position, 45, 20);
  await page.waitForFunction(() => document.querySelector('[aria-label="Selected rig object"]').value === 'fixtures:3');
  const moved = (await draft(page)).fixtures[index]; assert.notDeepEqual(moved.position, fixture.position);
  assert.deepEqual({...moved, position: fixture.position}, fixture, 'Only position changes while dragging');
  assert.deepEqual((await state()).rig.fixtures[index], fixture, 'The committed rig does not follow a draft');
  await panel.getByRole('button', {name: 'Undo', exact: true}).click();
  assert.deepEqual((await draft(page)).fixtures[index], fixture);
  await drag(page, fixture.position, 30, 12, true);
  assert.deepEqual((await draft(page)).fixtures[index], fixture);
  assert.equal((await editor(page)).tool, 'navigate');
  assert.equal(await page.evaluate(() => document.querySelector('.rig-editor canvas').tolglowPreview.getNavigationState().interacting), false);
  await number(page, 'X · m', -0.4321);
  await panel.getByRole('button', {name: 'Rotate', exact: true}).click();
  await drag(page, (await draft(page)).fixtures[index].position, 40, 0);
  assert.equal((await draft(page)).fixtures[index]['rotation-deg'][1], (fixture['rotation-deg']?.[1] || 0) + 20);
  await posted(page, '/api/rig', () => page.getByRole('button', {name: 'Save rig', exact: true}).click());
  const saved = (await state()).rig;
  assert.equal(saved.fixtures[index].position[0], -0.4321);
  assert.deepEqual({...saved.fixtures[index], position: fixture.position, 'rotation-deg': fixture['rotation-deg']}, {...fixture, 'rotation-deg': fixture['rotation-deg']});
  await panel.getByLabel('Selected rig object').selectOption('trusses:0');
  await number(page, 'Y · m', 4.25); await number(page, 'Rotate Z · °', 7.5);
  await posted(page, '/api/rig', () => page.getByRole('button', {name: 'Save rig', exact: true}).click());
  assert.equal((await state()).rig.trusses[0].position[1], 4.25);
  console.log('desktop: draft, undo, cancel, numeric precision, rotation and rig saves passed');

  await panel.getByRole('button', {name: 'Draw path', exact: true}).click();
  for (const point of [[-2, 0, 0], [2, 0, 0], [0, 0, -2]]) {const p = await project(page, point); await page.mouse.click(p.x, p.y);}
  assert.equal((await editor(page)).points.length, 3);
  const id = `qa-aim-${Date.now()}`;
  await panel.getByLabel('Path ID', {exact: true}).fill(id); await panel.getByLabel('Path name').fill('QA sweeping trio');
  await panel.getByRole('checkbox', {name: fixture.name, exact: true}).click();
  await page.waitForFunction(name => [...document.querySelectorAll('.rig-targets label')].find(label => label.textContent === name)?.querySelector('input').checked, fixture.name);
  const created = await posted(page, '/api/aim-path', () => panel.getByRole('button', {name: 'Create cue', exact: true}).click());
  const published = await state(), effect = published.effects.find(e => e.id === id);
  assert.equal(created['effect-id'], id); assert.equal(effect.editor.points.length, 3);
  assert.deepEqual(effect.targets, [fixture.id]); assert.deepEqual(effect.editor.duration, {bars: 4});
  assert.ok(!published.instances.some(i => i['cue-id'] === created['cue-id']), 'Create cue does not launch it');
  await number(page, 'Point 1 X', -3.125);
  await panel.getByLabel('Saved aim paths').selectOption(id);
  assert.equal((await editor(page)).points[0][0], effect.editor.points[0][0]);
  await panel.getByLabel('Playback').selectOption('once'); await panel.getByLabel('Musical unit').selectOption('beats');
  await number(page, 'Duration', 8);
  const commandResponse = page.waitForResponse(r => new URL(r.url()).pathname === '/api/command' && r.request().method() === 'POST');
  await posted(page, '/api/aim-path', () => panel.getByRole('button', {name: 'Create + run', exact: true}).click());
  const command = await commandResponse, payload = command.request().postDataJSON(), launch = await command.json();
  assert.equal(payload.launch.mode, 'one-shot'); assert.equal(payload['cue-id'], id); assert.ok(launch['instance-id']);
  assert.ok((await state()).instances.some(i => i.id === launch['instance-id']));
  console.log('desktop: path creation, saved editing and explicit one-shot run passed');

  const downloadPromise = page.waitForEvent('download'); await page.locator('#export-show').click();
  const download = await downloadPromise; await download.saveAs(`${directory}/show.edn`);
  const exported = await (await fetch(`${origin}/api/export`)).text(), exportState = await state();
  assert.ok(exported.includes(id));
  await panel.getByLabel('Selected rig object').selectOption('fixtures:3'); await number(page, 'X · m', 7);
  await posted(page, '/api/rig', () => page.getByRole('button', {name: 'Save rig', exact: true}).click());
  await posted(page, '/api/import', () => page.locator('#import-show').setInputFiles({name: 'rig-roundtrip.edn', mimeType: 'text/plain', buffer: Buffer.from(exported)}));
  await page.waitForFunction(epoch => document.querySelector('.rig-editor canvas')?.tolglowPreview.snapshot.epoch > epoch, exportState.epoch);
  const restored = await state(); assert.deepEqual(restored.rig, exportState.rig);
  assert.deepEqual(restored.effects.find(e => e.id === id).editor, exportState.effects.find(e => e.id === id).editor);
  assert.equal((await editor(page)).tool, 'navigate'); assert.equal((await editor(page)).points.length, 0);
  await panel.getByLabel('Saved aim paths').selectOption(id);
  await page.evaluate(async () => {const e = document.querySelector('.rig-editor canvas').tolglowPreview; e.pause(); await e.draw();});
  await panel.screenshot({path: `${directory}/desktop-editor.png`});
  const telemetry = await page.evaluate(() => document.querySelector('.rig-editor canvas').tolglowPreview.getTelemetry());
  await page.evaluate(() => {window.oldRigPreview = document.querySelector('.rig-editor canvas').tolglowPreview;});
  await page.locator('[data-view="compose"]').click();
  await page.waitForFunction(() => oldRigPreview.disposed && !oldRigPreview.canvas.tolglowPreview);
  assert.deepEqual(errors, []); reports.push({name: 'desktop', passed: ['draft placement', 'undo', 'Escape cancel', 'rotation', 'precise numeric editing', 'fixture/truss save', 'draw/edit path', 'create without launch', 'one-shot run', 'actual file export/import', 'show identity rebase', 'teardown'], telemetry, errors});
  await context.close();

  for (const [name, width, height, fallback] of [['ipad', 820, 1180, false], ['phone-fallback', 390, 844, true]]) {
    const {page, context, errors, panel} = await open({width, height, touch: true, fallback});
    console.log(`${name}: actual renderer ready`);
    await panel.getByRole('button', {name: 'Draw path', exact: true}).click();
    const cdp = await context.newCDPSession(page), point = await project(page, [0, 0, 0]);
    await cdp.send('Input.dispatchTouchEvent', {type: 'touchStart', touchPoints: [{x: point.x, y: point.y, id: 1}]});
    await cdp.send('Input.dispatchTouchEvent', {type: 'touchMove', touchPoints: [{x: point.x + 30, y: point.y - 10, id: 1}]});
    await cdp.send('Input.dispatchTouchEvent', {type: 'touchCancel', touchPoints: []});
    assert.equal((await editor(page)).points.length, 0, 'Touch cancellation restores path');
    const next = await project(page, [0, 0, 0]); await page.touchscreen.tap(next.x, next.y);
    assert.equal((await editor(page)).points.length, 1);
    await panel.getByRole('button', {name: 'Discard draft', exact: true}).click();
    assert.equal((await editor(page)).points.length, 0);
    assert.equal(await page.evaluate(() => document.querySelector('.rig-editor canvas').tolglowPreview.getNavigationState().interacting), false);
    const sizes = await panel.locator('button,select,input:not([type="checkbox"])').evaluateAll(nodes => nodes.map(n => ({height: n.getBoundingClientRect().height, name: n.getAttribute('aria-label') || n.textContent})).filter(n => n.height > 0));
    assert.ok(sizes.every(n => n.height >= 44), JSON.stringify(sizes.filter(n => n.height < 44)));
    assert.ok(await page.evaluate(() => document.documentElement.scrollWidth <= window.innerWidth), 'No horizontal page overflow');
    await panel.screenshot({path: `${directory}/${name}-editor.png`});
    const telemetry = await page.evaluate(() => document.querySelector('.rig-editor canvas').tolglowPreview.getTelemetry());
    assert.equal(telemetry.thread || 'main', fallback ? 'main' : 'worker');
    await page.locator('[data-view="compose"]').click(); assert.deepEqual(errors, []);
    reports.push({name, passed: ['actual touch add', 'touchcancel restores', 'discard restores navigation', '44px controls', 'no horizontal overflow', 'renderer lifecycle'], telemetry, errors});
    await context.close();
  }
  await writeFile(`${directory}/evidence.json`, JSON.stringify({scope: 'Actual optimized re-frame/Ring workflows with captured output; Chromium viewport/touch emulation, not physical Safari or fixture validation', reports}, null, 2));
  console.log(JSON.stringify({passed: reports.map(r => r.name), directory}));
} catch (error) {
  for (const context of browser.contexts()) for (const page of context.pages()) {
    await page.screenshot({path: `${directory}/failure.png`, fullPage: true}).catch(() => {});
    console.error(await page.locator('.rig-editor').innerText().catch(() => 'Editor unavailable'));
  }
  throw error;
} finally {await browser.close();}
