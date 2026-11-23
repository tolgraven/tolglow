// Point this at a disposable, captured-output Ring host serving the production bundle.
const assert = require('node:assert/strict');
const {chromium} = require('playwright-core');

// Observe the production camera only to locate the drawn handles in CSS pixels.
// All authoring changes below use the same toolbar, touches and API as an operator.
async function observeRigCamera(page) {
  await page.route('**/studio/engine-loader.js', async route => {
    const response = await route.fetch();
    const observer = `
      const load = window.tolglowLoadEngine;
      window.tolglowLoadEngine = async name => {
        const module = await load(name);
        if (name !== 'preview') return module;
        return {...module, RoomPreview: class extends module.RoomPreview {
          constructor(canvas, status, options) {
            super(canvas, status, options);
            if (!canvas.closest('.rig-editor')) return;
            window.hasLiveSurface = () => !!this.snapshot?.['glass-volumes']?.length;
            window.rigHandle = axis => {
              const guide = this.snapshot?.['sculpt-guide'];
              if (!guide) return null;
              const world = [...guide.center];
              if (axis !== null) world[axis] += Math.max(0.32, (guide.max[axis] - guide.min[axis]) / 2);
              const p = this.camera.position.clone().fromArray(world).project(this.camera);
              const r = canvas.getBoundingClientRect();
              return {x: r.x + (p.x + 1) * r.width / 2, y: r.y + (1 - p.y) * r.height / 2};
            };
          }
        }};
      };`;
    await route.fulfill({response, body: (await response.text()) + observer});
  });
}

async function drag(page, session, axis, dx, dy, cancel) {
  const canvas = page.locator('.rig-editor canvas');
  await canvas.scrollIntoViewIfNeeded();
  await page.waitForFunction(axis => window.rigHandle?.(axis), axis);
  const from = await page.evaluate(axis => window.rigHandle(axis), axis);
  await session.send('Input.dispatchTouchEvent', {type:'touchStart', touchPoints:[from]});
  await session.send('Input.dispatchTouchEvent', {type:'touchMove', touchPoints:[{x:from.x + dx, y:from.y + dy}]});
  await session.send('Input.dispatchTouchEvent', {type:cancel ? 'touchCancel' : 'touchEnd', touchPoints:[]});
}

(async () => {
  const browser = await chromium.launch({executablePath: process.env.STUDIO_CHROMIUM || '/usr/bin/chromium',
    args: ['--no-sandbox', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
  try {
    const baseURL = process.env.STUDIO_URL || 'http://127.0.0.1:8500';
    const page = await browser.newPage({baseURL, viewport: {width:834,height:1112}, hasTouch:true});
    await observeRigCamera(page);
    const errors = [], writes = [];
    page.on('pageerror', error => errors.push(error.message));
    page.on('request', request => {if (request.method() === 'POST' && new URL(request.url()).pathname === '/api/spatial/cues') writes.push(request.url());});
    await page.goto('/rig');
    await page.getByRole('button', {name:'Sculpt volume', exact:true}).tap();
    const panel = page.getByRole('region', {name:'Sculpt volume'});
    await panel.getByLabel('Primitive', {exact:true}).selectOption({label:'Rounded box'});
    await panel.getByRole('button', {name:'Replace shape'}).tap();
    await panel.getByLabel('Corner rounding', {exact:true}).fill('0.65');
    await panel.getByLabel('Corner rounding', {exact:true}).press('Enter');
    const session = await page.context().newCDPSession(page);
    await drag(page, session, 0, 48, 0, false);
    const scale = panel.getByLabel('Stretch X', {exact:true});
    await page.waitForFunction(el => Number(el.value) > 1, await scale.elementHandle());
    await panel.getByRole('button', {name:'Undo sculpt'}).tap();
    await page.waitForFunction(el => Number(el.value) === 1, await scale.elementHandle());
    await drag(page, session, null, 40, 0, true);
    const translation = panel.getByLabel('Translation · m X', {exact:true});
    await page.waitForFunction(el => Number(el.value) === 0, await translation.elementHandle());
    await panel.getByLabel('Primitive', {exact:true}).selectOption({label:'Sphere'});
    await panel.getByRole('button', {name:'Intersect',exact:true}).tap();
    const cueId = 'sculpt-touch-' + Date.now();
    await panel.getByLabel('Cue ID', {exact:true}).fill(cueId);
    await panel.getByLabel('Field color').fill('#40a0e0');
    const beams = panel.getByLabel('Beam intersects');
    await beams.tap();
    await page.waitForFunction(input => input.checked, await beams.elementHandle());
    await panel.getByLabel('Outside falloff · m', {exact:true}).fill('0.4');
    await panel.getByLabel('Outside falloff · m', {exact:true}).press('Enter');
    assert.equal(writes.length,0);
    const response = page.waitForResponse(r => r.url().endsWith('/api/spatial/cues') && r.request().method() === 'POST');
    await panel.getByRole('button', {name:'Register + launch cue'}).tap();
    assert.equal((await response).status(),200);
    await panel.getByRole('status').filter({hasText:cueId}).waitFor();
    const {cues,instances} = await (await page.request.get('/api/state', {headers:{Accept:'application/json'}})).json();
    assert.ok(cues.some(c => c.id === cueId));
    assert.ok(instances.some(i => i['cue-id'] === cueId));
    const canvas = page.locator('.rig-editor canvas');
    await canvas.scrollIntoViewIfNeeded();
    await page.waitForFunction(() => window.hasLiveSurface());
    await page.evaluate(() => window.dispatchEvent(new Event('blur')));
    await page.waitForFunction(() => !window.hasLiveSurface());
    await page.evaluate(() => window.dispatchEvent(new Event('focus')));
    await page.waitForFunction(() => window.hasLiveSurface());
    assert.equal(writes.length,1);
    assert.deepEqual(errors,[]);
    console.log('iPad-sized Chromium sculpted and stretched a volume, cancelled a drag, built an intersection, painted and launched a live grid cue.');
  } finally {await browser.close();}
})().catch(e => {console.error(e);process.exitCode = 1;});
