// Run against a disposable, captured-output Studio with the production bundle.
const assert = require('node:assert/strict');
const {chromium} = require('playwright-core');

async function field(panel, label, value) {
  const input = panel.getByRole('spinbutton', {name: label, exact: true});
  await input.fill(String(value));
  await input.press('Enter');
}

async function waitField(page, locator, value, changed = false) {
  const label = await locator.getAttribute('aria-label');
  await page.waitForFunction(([label, previous, changed]) => {
    const input = document.querySelector('[aria-label="' + CSS.escape(label) + '"]');
    return input && (input.value !== previous) === changed;
  }, [label, value, changed]);
}

async function draw(canvas, page, fractions) {
  await canvas.scrollIntoViewIfNeeded();
  const rect = await canvas.boundingBox();
  for (const [x, y] of fractions) {
    await page.touchscreen.tap(rect.x + rect.width * x, rect.y + rect.height * y);
  }
}

async function drag(canvas, session, from, to, cancelled) {
  await canvas.scrollIntoViewIfNeeded();
  const rect = await canvas.boundingBox();
  const point = ([x, y]) => [{x: rect.x + rect.width * x, y: rect.y + rect.height * y}];
  await session.send('Input.dispatchTouchEvent', {type: 'touchStart', touchPoints: point(from)});
  await session.send('Input.dispatchTouchEvent', {type: 'touchMove', touchPoints: point(to)});
  await session.send('Input.dispatchTouchEvent', {type: cancelled ? 'touchCancel' : 'touchEnd', touchPoints: []});
}

async function inspectShape(browser, baseURL, viewport, shape, countLabel) {
  const page = await browser.newPage({baseURL, viewport, hasTouch: viewport.width < 600});
  try {
    await page.goto('/rig');
    const click = locator => viewport.width < 600 ? locator.tap() : locator.click();
    await click(page.getByRole('button', {name: 'Pixel layouts', exact: true}));
    const panel = page.getByRole('region', {name: 'Pixel layout editor'});
    await click(panel.getByRole('button', {name: shape}));
    await panel.getByText(countLabel).waitFor();
    const bounds = await panel.boundingBox();
    assert.ok(bounds.x >= -1 && bounds.x + bounds.width <= viewport.width + 1);
    await click(page.getByRole('button', {name: 'Discard draft', exact: true}));
  } finally {await page.close();}
}

(async () => {
  const browser = await chromium.launch({executablePath: process.env.STUDIO_CHROMIUM || '/usr/bin/chromium',
    args: ['--no-sandbox', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
  try {
    const baseURL = process.env.STUDIO_URL || 'http://127.0.0.1:8500';
    const page = await browser.newPage({baseURL,
      viewport: {width: 834, height: 1112}, hasTouch: true});
    const errors = [], writes = [];
    page.on('pageerror', e => errors.push(e.message));
    page.on('request', r => {if (r.method() === 'POST' && new URL(r.url()).pathname === '/api/rig') writes.push(r);});
    await page.goto('/rig');
    const tools = page.getByRole('group', {name: '3D editor tool'});
    await tools.getByRole('button', {name: 'Pixel layouts'}).tap();
    const panel = page.getByRole('region', {name: 'Pixel layout editor'});
    const canvas = page.locator('.rig-editor canvas');
    await panel.getByRole('button', {name: 'New path'}).tap();
    const picker = panel.getByLabel('Pixel layout', {exact: true});
    await waitField(page, picker, '', true);
    const id = await picker.inputValue();
    await page.waitForFunction(() => document.querySelector('.rig-editor canvas')?.style.cursor === 'crosshair');
    const first = [.29, .66], last = [.72, .65];
    await draw(canvas, page, [first, last]);
    await panel.getByText(/32 pixels ·/).waitFor();
    const options = panel.getByLabel('Path anchor', {exact: true}).locator('option');
    assert.equal(await options.count(), 2);
    const session = await page.context().newCDPSession(page);
    const x = panel.getByRole('spinbutton', {name: 'Anchor X · m', exact: true});
    const original = await x.inputValue();
    await drag(canvas, session, last, [.61, .77], true);
    await waitField(page, x, original);
    await drag(canvas, session, last, [.61, .77], false);
    await waitField(page, x, original, true);
    await page.getByRole('button', {name: 'Undo', exact: true}).tap();
    await waitField(page, x, original);
    await field(panel, 'Pixels', 4);
    await field(panel, 'Start address', 509);
    await panel.getByLabel('Channel order').selectOption('rgbw');
    await panel.getByText(/4 pixels · U\d+ \/ 509 → U\d+ \/ 12/).waitFor();
    await panel.getByRole('button', {name: 'Individual pixels'}).tap();
    await field(panel, 'Pixel number', 3);
    await panel.getByRole('group', {name: 'Coordinates for pixel 3'}).waitFor();
    await field(panel, 'Pixel Y · m', 2.75);
    assert.equal(writes.length, 0);
    const response = page.waitForResponse(r => r.url().endsWith('/api/rig') && r.request().method() === 'POST');
    await panel.getByRole('button', {name: 'Save rig layout'}).tap();
    assert.equal((await response).status(), 200);
    await page.reload();
    await tools.getByRole('button', {name: 'Pixel layouts'}).tap();
    await panel.getByLabel('Pixel layout', {exact: true}).selectOption(id);
    await panel.getByText(/4 pixels · U\d+ \/ 509 → U\d+ \/ 12/).waitFor();
    await panel.getByRole('button', {name: 'Individual pixels'}).tap();
    await field(panel, 'Pixel number', 3);
    await panel.getByRole('group', {name: 'Coordinates for pixel 3'}).waitFor();
    assert.equal(await panel.getByLabel('Pixel Y · m').inputValue(), '2.75');
    await panel.getByRole('button', {name: 'Reset pixel to shape'}).tap();
    await waitField(page, panel.getByLabel('Pixel Y · m'), '0');
    await page.getByRole('button', {name: 'Discard draft', exact: true}).tap();
    await tools.getByRole('button', {name: 'Pixel layouts'}).tap();
    await panel.getByLabel('Pixel layout', {exact: true}).selectOption(id);
    await panel.getByRole('button', {name: 'Individual pixels'}).tap();
    await field(panel, 'Pixel number', 3);
    await panel.getByRole('group', {name: 'Coordinates for pixel 3'}).waitFor();
    assert.equal(await panel.getByLabel('Pixel Y · m').inputValue(), '2.75');
    const {rig} = await (await page.request.get('/api/state', {headers: {Accept: 'application/json'}})).json();
    const layout = rig['pixel-layouts'].find(layout => layout.id === id);
    assert.equal(layout['position-overrides'][2][1], 2.75);
    const heads = rig.fixtures.filter(f => f['pixel-layout-id'] === id);
    assert.deepEqual(heads.map(f => [f.universe, f.address]), [
      [layout.universe, 509], [layout.universe + 1, 1], [layout.universe + 1, 5], [layout.universe + 1, 9]]);
    assert.equal(heads[2].position[1], 2.75);
    assert.equal(writes.length, 1);
    assert.deepEqual(errors, []);
    await page.close();
    await inspectShape(browser, baseURL, {width: 1440, height: 920}, 'New grid', /32 pixels ·/);
    await inspectShape(browser, baseURL, {width: 390, height: 844}, 'New helix', /64 pixels ·/);
    console.log('Desktop grid and phone helix controls fit their layouts. iPad-sized Chromium touch drawing, drag/cancel, undo, explicit save, multi-universe restore and individual overrides passed.');
  } finally {await browser.close();}
})().catch(e => {console.error(e); process.exitCode = 1;});
