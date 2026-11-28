// Exercise Studio video editing (#672, #673, #674) in the optimized build
// against the preview-only show from tolglow.qa-video-editing. Usage:
//   node dev/studio/video-editing-browser-qa.mjs http://127.0.0.1:19780 target/video-editing-qa
// Every edit is checked in the committed settings or registry read back
// from the server, never only in the page.
import assert from 'node:assert/strict';
import {mkdirSync, writeFileSync} from 'node:fs';
import {chromium} from 'playwright-core';

const origin = process.argv[2] || 'http://127.0.0.1:19780';
const out = process.argv[3] || 'target/video-editing-qa';
mkdirSync(out, {recursive: true});

const json = async path => {
  const response = await fetch(origin + path, {headers: {Accept: 'application/json'}});
  assert.equal(response.status, 200, path);
  return response.json();
};
const strips = async () => (await json('/api/settings')).video.mappings['video/grid'].strips;
const strip = async id => (await strips()).find(s => s.id === id);
const surfaceOf = async id => (await json('/api/settings')).video.surfaces[id];
const plasma = async () => (await json('/api/state')).effects.find(e => e.id === 'shader-plasma');
async function until(read, ok, label, timeout = 6000) {
  const deadline = Date.now() + timeout;
  let value;
  while (Date.now() < deadline) {
    value = await read();
    if (ok(value)) return value;
    await new Promise(r => setTimeout(r, 60));
  }
  throw new Error(`${label}: last value ${JSON.stringify(value)}`);
}
const near = (a, b, e = 1e-3) => Math.abs(a - b) <= e;

const browser = await chromium.launchPersistentContext(`${out}/profile`, {
  executablePath: process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  headless: true, args: ['--headless=new', '--use-angle=metal'],
  viewport: {width: 1440, height: 1200}, deviceScaleFactor: 2,
});
const report = {};
try {
  const page = await browser.newPage();
  const problems = [];
  page.on('pageerror', error => problems.push(error.message));
  page.on('console', m => { if (m.type() === 'error' && !/Failed to load resource/.test(m.text())) problems.push(m.text()); });
  await page.goto(origin + '/rig');
  await page.addStyleTag({content: '.topbar{position:static !important}'});

  // --- #672: numeric placement, rectangle gesture, undo/redo, fixture group ---
  const panel = page.getByRole('region', {name: 'Video strip mapping'});
  await panel.waitFor();
  await panel.scrollIntoViewIfNeeded();
  await page.waitForFunction(() => document.querySelectorAll('#video-strips .video-strip polygon').length === 32);
  const line = await panel.locator('.video-strip').first().locator('.video-strip-line').boundingBox();
  await page.mouse.click(line.x + line.width / 2, line.y + line.height / 2);
  const placement = panel.getByRole('group', {name: 'Strip placement'});
  await placement.waitFor();
  report.placementCards = await placement.locator('.param-row').evaluateAll(rows => rows.map(r => ({
    control: r.dataset.control, slot: r.dataset.slot, area: r.style.gridArea || `${r.style.gridColumn} / ${r.style.gridRow}`})));
  assert.equal(await placement.getAttribute('data-skin'), 'ember', 'Ember is the default widget style');
  assert.ok(await placement.locator('.parameter-rotary-control[data-skin=ember]').count() >= 5, 'Ember knobs');
  await placement.getByLabel('Centre down exact value').fill('0.5');
  await placement.getByLabel('Centre down exact value').press('Tab');
  report.centred = await until(() => strip('tube-1'), s => near((s.points[0][1] + s.points[1][1]) / 2, 0.5), 'centre down committed');
  await placement.getByLabel('Angle exact value').fill('30');
  await placement.getByLabel('Angle exact value').press('Tab');
  report.turned = await until(() => strip('tube-1'), s => {
    const [[u0, v0], [u1, v1]] = s.points; return near(Math.atan2(v1 - v0, (u1 - u0) * 16 / 9) * 180 / Math.PI, 30, 0.01);
  }, 'angle committed');
  await page.waitForTimeout(300);
  await panel.screenshot({path: `${out}/strip-placement.png`});

  // Draw a rectangle along the bottom of the frame: it places the selected strip.
  // Buttons scroll the page, so measure the frame again before each gesture.
  let canvas = await panel.locator('.video-canvas').boundingBox();
  const at = (u, v) => [canvas.x + u * canvas.width, canvas.y + v * canvas.height];
  const [x0, y0] = at(0.6, 0.87), [x1, y1] = at(0.15, 0.97);
  await page.mouse.move(x0, y0); await page.mouse.down();
  await page.mouse.move(x1, y1, {steps: 10});
  await page.screenshot({path: `${out}/rectangle-drawing.png`, clip: {x: canvas.x, y: canvas.y, width: canvas.width, height: canvas.height}});
  await page.mouse.up();
  report.rectangle = await until(() => strip('tube-1'), s => near(s.points[0][0], 0.6, 0.01) && near(s.points[1][0], 0.15, 0.01)
    && near(s.points[0][1], 0.92, 0.01) && near(s.height, 0.1, 0.01), 'rectangle placed the strip right to left');

  // Undo returns the 30° placement; Redo puts the rectangle back.
  const undo = panel.getByRole('button', {name: /^Undo strip tube-1/});
  await undo.click();
  await until(() => strip('tube-1'), s => JSON.stringify(s.points) === JSON.stringify(report.turned.points), 'undo restored the strip');
  await panel.getByRole('button', {name: /^Redo strip tube-1/}).click();
  await until(() => strip('tube-1'), s => JSON.stringify(s.points) === JSON.stringify(report.rectangle.points), 'redo replaced it');
  report.history = (await json('/api/video')).history;

  // A fixture group: click the frame to clear the selection, draw its rectangle, tick three heads.
  await panel.locator('.video-canvas').scrollIntoViewIfNeeded();
  canvas = await panel.locator('.video-canvas').boundingBox();
  await page.mouse.click(...at(0.5, 0.15));
  const [gx0, gy0] = at(0.1, 0.45), [gx1, gy1] = at(0.9, 0.55);
  await page.mouse.move(gx0, gy0); await page.mouse.down(); await page.mouse.move(gx1, gy1, {steps: 8}); await page.mouse.up();
  await panel.locator('.video-pending-rect').waitFor();
  await panel.getByText('Map a fixture group').click();
  for (const head of ['head-3', 'head-1', 'head-2']) await panel.getByLabel(`Include ${head}`, {exact: true}).check();
  await panel.getByRole('button', {name: 'Place group on the rectangle'}).click();
  report.group = await until(() => strip('group-1'), s => s && s.targets.join() === 'head-3,head-1,head-2', 'group placed in tick order');
  assert.ok(near(report.group.points[0][1], 0.5, 0.01) && near(report.group.height, 0.1, 0.01), 'on the rectangle');
  const targets = panel.getByRole('group', {name: 'Fixture group targets'});
  await targets.waitFor();
  await targets.getByRole('button', {name: 'Order left to right in the room'}).click();
  await until(() => strip('group-1'), s => s.targets.join() === 'head-1,head-2,head-3', 'ordered left to right');
  await targets.getByRole('button', {name: 'Move head-1 later'}).click();
  report.reordered = await until(() => strip('group-1'), s => s.targets.join() === 'head-2,head-1,head-3', 'moved later');
  const sampled = await until(async () => (await json('/api/video')).mappings['video/grid'].strips.find(s => s.id === 'group-1'),
    s => s && s.colors.length === 3, 'group sampled');
  report.groupColors = sampled.colors;
  await page.waitForTimeout(400);
  await panel.screenshot({path: `${out}/fixture-group.png`});

  // --- #673: uniforms, size and rate ---
  const shaders = page.getByRole('region', {name: 'Shader cues'});
  await shaders.scrollIntoViewIfNeeded();
  await shaders.getByRole('button', {name: 'Uniforms, size and rate'}).click();
  const uniforms = shaders.getByRole('group', {name: 'Uniforms of shader-plasma'});
  await uniforms.waitFor();
  const live = uniforms.getByRole('group', {name: /^Live uniforms of /});
  await live.waitFor();
  report.liveControls = await live.locator('.param-row').count();
  await live.getByLabel('level exact value').fill('0.4');
  await live.getByLabel('level exact value').press('Tab');
  report.liveLevel = await until(async () => (await json('/api/state')).instances.find(i => i['effect-id'] === 'shader-plasma').params.level,
    v => near(v, 0.4), 'live uniform through param/set');
  const size = uniforms.getByRole('group', {name: 'Render size and rate'});
  await size.getByLabel('Width exact value').fill('320');
  await size.getByLabel('Height exact value').fill('180');
  await size.getByLabel('Rate exact value').fill('25');
  await uniforms.getByLabel('New uniform name').fill('u_glow');
  await uniforms.getByRole('button', {name: 'Add uniform'}).click();
  await uniforms.locator('.shader-uniform').filter({hasText: 'u_glow'}).getByRole('button', {name: 'Make it a control'}).click();
  await page.waitForTimeout(200);
  await shaders.screenshot({path: `${out}/shader-uniforms-draft.png`});
  await uniforms.getByRole('button', {name: 'Save version'}).click();
  report.saved = await until(plasma, e => e.version === 2 && e.shader.fps === 25, 'version 2 saved');
  assert.deepEqual(report.saved.shader.size, [320, 180]);
  assert.equal(report.saved.shader.uniforms.u_glow.value.key, 'glow');
  assert.ok(report.saved.params.glow, 'the new control is a parameter');
  // An invalid edit is refused with its path and keeps the draft.
  await uniforms.locator('.shader-uniform').filter({hasText: 'u_level'}).getByLabel('u_level type').selectOption('vec3');
  await uniforms.getByRole('button', {name: 'Save version'}).click();
  report.refusal = await uniforms.getByRole('alert').textContent();
  assert.match(report.refusal, /vec3/);
  assert.equal((await plasma()).version, 2);
  await uniforms.getByRole('button', {name: 'Discard changes'}).click();
  await uniforms.getByRole('button', {name: 'Undo version'}).click();
  report.undone = await until(plasma, e => e.version === 3 && e.shader.fps === 30 && !e.params.glow, 'undo republished version 1');
  await uniforms.getByRole('button', {name: 'Redo version'}).click();
  await until(plasma, e => e.version === 4 && e.shader.fps === 25, 'redo republished version 2');
  await page.waitForTimeout(300);
  await shaders.screenshot({path: `${out}/shader-uniforms.png`});

  // --- #674: Room drag handles ---
  const surfaces = page.getByRole('region', {name: 'Video surfaces'});
  await surfaces.scrollIntoViewIfNeeded();
  await surfaces.getByRole('button', {name: 'Move in the Room'}).click();
  const rig = page.locator('.rig-editor');
  await rig.waitFor();
  // A Room out of view pauses, so bring it into view before waiting for it.
  await rig.locator('canvas').scrollIntoViewIfNeeded();
  await page.waitForFunction(() => document.querySelector('.rig-editor canvas')?.tolglowPreview?.ready, null, {timeout: 120000});
  await page.waitForFunction(() => (document.querySelector('.rig-editor canvas').tolglowPreview.snapshot?.editor?.['surface-handles'] || []).length === 3);
  const project = point => page.evaluate(point => {
    const e = document.querySelector('.rig-editor canvas').tolglowPreview, b = e.canvas.getBoundingClientRect();
    e.camera.updateMatrixWorld(); const p = e.camera.position.clone().fromArray(point).project(e.camera);
    return {x: b.left + (p.x + 1) * b.width / 2, y: b.top + (1 - p.y) * b.height / 2};
  }, point);
  const handle = kind => page.evaluate(kind => document.querySelector('.rig-editor canvas').tolglowPreview.snapshot.editor['surface-handles'].find(h => h.kind === kind).position, kind);
  const before = await surfaceOf('sheet');
  await page.waitForTimeout(800);
  await rig.locator('.rig-editor-scene').screenshot({path: `${out}/room-handles.png`});
  // Centre: move 1 m along x on the floor plane.
  const centre = await handle('move'), from = await project(centre), to = await project([centre[0] + 1, centre[1], centre[2]]);
  await page.mouse.move(from.x, from.y); await page.mouse.down(); await page.mouse.move(to.x, to.y, {steps: 8});
  report.draftDuringDrag = await page.evaluate(() => document.querySelector('.rig-editor canvas').tolglowPreview.snapshot.editor['surface-handles'][0].position);
  assert.deepEqual((await surfaceOf('sheet')).position, before.position, 'nothing commits during the drag');
  await page.mouse.up();
  report.moved = await until(() => surfaceOf('sheet'), s => near(s.position[0], before.position[0] + 1, 0.15), 'centre drag committed');
  // Raise: up 0.5 m.
  const top = await handle('raise'), r0 = await project(top), r1 = await project([top[0], top[1] + 0.5, top[2]]);
  await page.mouse.move(r0.x, r0.y); await page.mouse.down(); await page.mouse.move(r1.x, r1.y, {steps: 6}); await page.mouse.up();
  report.raised = await until(() => surfaceOf('sheet'), s => near(s.position[1], before.position[1] + 0.5, 0.1), 'raise committed');
  // Turn: swing the turn handle a quarter towards the audience.
  const side = await handle('turn'), c = (await surfaceOf('sheet')).position;
  const reach = Math.hypot(side[0] - c[0], side[2] - c[2]);
  const t0 = await project(side), t1 = await project([c[0] + reach * Math.cos(Math.PI / 6), c[1], c[2] - reach * Math.sin(Math.PI / 6)]);
  await page.mouse.move(t0.x, t0.y); await page.mouse.down(); await page.mouse.move(t1.x, t1.y, {steps: 8}); await page.mouse.up();
  report.turnedSurface = await until(() => surfaceOf('sheet'), s => near(s['rotation-deg']?.[1] ?? 0, 30, 3), 'turn committed');
  await page.waitForTimeout(800);
  await rig.locator('.rig-editor-scene').screenshot({path: `${out}/room-handles-moved.png`});
  // Undo the turn from the tool's panel; the REPL-shared history names it.
  await rig.getByRole('button', {name: /^Undo surface sheet/}).click();
  report.afterUndo = await until(() => surfaceOf('sheet'), s => near(s['rotation-deg']?.[1] ?? 0, 0, 0.01), 'undo restored the yaw');
  report.problems = problems;
  assert.deepEqual(problems, []);
  writeFileSync(`${out}/report.json`, JSON.stringify(report, null, 1));
  console.log(JSON.stringify({passed: true, ...report}, null, 1));
} finally {
  await browser.close();
}
