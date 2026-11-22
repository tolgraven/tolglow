// Exercise the optimized Studio against a dedicated capture-only show.
import {strict as assert} from 'node:assert';
import {chromium} from 'playwright-core';

const origin = process.argv[2] || 'http://127.0.0.1:8687';
const read = async () => (await fetch(origin + '/api/state', {headers: {Accept: 'application/json'}})).json();
const before = await read();
assert.equal(before.status.output.type, 'preview');
const heads = before.rig.fixtures.filter(f => f.channels.pan && f.channels.tilt).slice(0, 2);
assert.equal(heads.length, 2);
const browser = await chromium.launch({executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
try {
  const context = await browser.newContext({viewport: {width: 1024, height: 900}, hasTouch: true});
  const page = await context.newPage();
  page.setDefaultTimeout(30000);
  const errors = []; page.on('pageerror', e => { errors.push(e.message); console.error(e.message); });
  await page.goto(origin);
  await page.locator('[data-view="rig"]').click();
  const panel = page.locator('.rig-editor'), canvas = panel.locator('canvas');
  await page.waitForFunction(() => document.querySelector('.rig-editor canvas')?.tolglowPreview);
  await page.evaluate(() => document.querySelector('.rig-editor canvas').tolglowPreview.setQuality('low'));
  await page.waitForFunction(() => document.querySelector('.rig-editor canvas').tolglowPreview.ready, null, {timeout: 120000});
  await panel.getByRole('button', {name: 'Aim at surface', exact: true}).tap();
  for (const head of heads) {
    const name = head.name || head.id;
    await panel.getByRole('checkbox', {name, exact: true}).tap();
    await page.waitForFunction(name => [...document.querySelectorAll('.rig-targets label')]
      .find(label => label.textContent === name)?.querySelector('input').checked, name);
  }
  async function command(action) {
    const reply = page.waitForResponse(r => new URL(r.url()).pathname === '/api/surface-aim' && r.request().method() === 'POST');
    await action();
    assert.ok((await reply).ok());
    await panel.getByRole('button', {name: 'Apply aim', exact: true}).waitFor();
    await page.waitForFunction(() => ![...document.querySelectorAll('.rig-editor button')].find(b => b.textContent === 'Apply aim').disabled);
    return read();
  }
  const point = [1, 0, 1];
  await canvas.scrollIntoViewIfNeeded();
  const xy = await canvas.evaluate((node, p) => {
    const {camera} = node.tolglowPreview;
    camera.updateMatrixWorld();
    const q = camera.position.clone().fromArray(p).project(camera), b = node.getBoundingClientRect();
    return [b.left + (q.x + 1) * b.width / 2, b.top + (1 - q.y) * b.height / 2];
  }, point);
  let show = await command(() => page.touchscreen.tap(...xy));
  for (const head of heads) {
    assert.ok(Math.hypot(...show['surface-aim'][head.id].map((x, i) => x - point[i])) < .14);
    assert.deepEqual(show.output.values[head.id].aim, show['surface-aim'][head.id]);
  }
  await panel.getByLabel('Aiming mode').selectOption('follow');
  show = await command(() => panel.getByRole('button', {name: 'Apply aim', exact: true}).tap());
  assert.equal(Object.keys(show['surface-aim']).length, 0);
  const link = show.rig['edge-tracking'][0];
  assert.deepEqual([link.leader, link.follower, link['surface-id']], [heads[0].id, heads[1].id, 'floor']);
  show = await command(() => panel.getByRole('button', {name: 'Release selected'}).tap());
  assert.deepEqual(show.rig['edge-tracking'], []);
  assert.deepEqual(errors, []);
  console.log('Touch picked the floor, both output heads aimed there, the follow action preserved the leader, and release removed the link.');
} finally {
  await browser.close();
}
