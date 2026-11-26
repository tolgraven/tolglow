// Arrange the Perform panels in layout edit mode at desktop, tablet and phone
// sizes: drag to reorder, drag a corner to resize, use the keyboard, cancel
// with Escape, save, reload, detach and redock the Room, recover a damaged
// saved layout and reset to the built-in layout. At each step it checks that a
// running cue's controls and the Room's preview were not remounted, and that
// Studio keeps one socket and one preview worker.
//
//   node dev/studio/panel-layout-qa.mjs http://127.0.0.1:4530 target/qa/panel-layout
//
// Run against a production build served by `lein run -- --port <port>` (the
// default preview output is a capture sink). Headless Chrome with the real
// GPU; tablet and phone use emulated touch through CDP touch events, which is
// not evidence of iPad Safari or a physical touch screen.
import assert from 'node:assert/strict';
import {mkdir, rm, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin = 'http://127.0.0.1:4530', directory = 'target/qa/panel-layout'] = process.argv.slice(2);
await mkdir(directory, {recursive: true});
const executablePath = process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const only = process.env.ONLY;

const viewports = [
  {name: 'desktop', width: 1440, height: 900, touch: false, cls: 'desktop', columns: 12},
  {name: 'tablet', width: 1024, height: 768, touch: true, cls: 'tablet', columns: 6},
  {name: 'phone', width: 390, height: 844, touch: true, cls: 'phone', columns: 2},
].filter(v => !only || v.name === only);

// Count preview workers and sockets from outside the application.
function instrument() {
  const wrap = win => {
    if (win.__qa) return;
    const qa = win.__qa = {workers: [], sockets: 0};
    const NativeWorker = win.Worker;
    win.Worker = function(url, options) {
      const worker = new NativeWorker(url, options);
      const record = {href: String(url), terminated: false};
      qa.workers.push(record);
      const terminate = worker.terminate.bind(worker);
      worker.terminate = () => { record.terminated = true; terminate(); };
      return worker;
    };
    const NativeSocket = win.WebSocket;
    win.WebSocket = function(...args) { qa.sockets++; return new NativeSocket(...args); };
    win.WebSocket.prototype = NativeSocket.prototype;
    Object.assign(win.WebSocket, {CONNECTING: 0, OPEN: 1, CLOSING: 2, CLOSED: 3});
  };
  wrap(window);
}

const summary = {origin, viewports: {}};

for (const viewport of viewports) {
  const profile = join(directory, 'profile-' + viewport.name);
  await rm(profile, {recursive: true, force: true});
  const context = await chromium.launchPersistentContext(profile, {
    executablePath, headless: true,
    args: ['--headless=new', '--use-angle=metal', '--enable-gpu', '--ignore-gpu-blocklist'],
    viewport: {width: viewport.width, height: viewport.height},
    hasTouch: viewport.touch, isMobile: viewport.touch && viewport.name === 'phone',
    deviceScaleFactor: viewport.touch ? 2 : 1,
  });
  const errors = [];
  const record = summary.viewports[viewport.name] = {steps: []};
  let roomPage = null;
  try {
    for (const blank of context.pages()) await blank.close();
    const page = await context.newPage();
    await page.addInitScript(instrument);
    page.on('pageerror', e => errors.push(e.message));
    page.on('console', m => { if (m.type() === 'error' && !/favicon|ERR_CONNECTION/.test(m.text())) errors.push('console: ' + m.text()); });
    page.on('popup', p => { roomPage = p; });
    const cdp = await context.newCDPSession(page);
    const shot = name => page.screenshot({path: join(directory, `${viewport.name}-${name}.png`)});

    const census = async label => {
      const data = await page.evaluate(() => ({
        sockets: window.__qa.sockets,
        workers: window.__qa.workers.filter(w => !w.terminated && /preview/.test(w.href)).length,
        workersCreated: window.__qa.workers.filter(w => /preview/.test(w.href)).length,
        order: [...document.querySelectorAll('#view-perform .performance-layout > .layout-region')].map(e => e.dataset.panel),
        custom: !!document.querySelector('#view-perform .custom-layout'),
        editing: !!document.querySelector('#view-perform .editing-layout'),
        cardTagged: document.querySelector('[data-live-instance]')?.__qaTag === 'card',
        canvasTagged: document.querySelector('#preview')?.__qaTag === 'canvas',
        liveInstance: document.querySelector('[data-live-instance]')?.dataset.liveInstance ?? null,
      }));
      record.steps.push({label, ...data});
      console.log(viewport.name, label, JSON.stringify(data));
      return data;
    };
    const spans = id => page.evaluate(id => {
      const el = document.querySelector(`#view-perform [data-panel="${id}"]`);
      return {column: el.style.gridColumn, row: el.style.gridRow, box: el.getBoundingClientRect().toJSON()};
    }, id);
    const center = async locator => {
      const box = await locator.boundingBox();
      assert.ok(box, 'element has a box');
      return {x: box.x + box.width / 2, y: box.y + box.height / 2};
    };
    // One continuous pointer drag, by mouse or by a touch contact.
    const drag = async (from, to, {midway, steps = 8} = {}) => {
      if (viewport.touch) {
        const point = p => [{x: p.x, y: p.y, id: 1, radiusX: 4, radiusY: 4, force: 1}];
        await cdp.send('Input.dispatchTouchEvent', {type: 'touchStart', touchPoints: point(from)});
        for (let i = 1; i <= steps; i++) {
          const p = {x: from.x + (to.x - from.x) * i / steps, y: from.y + (to.y - from.y) * i / steps};
          await cdp.send('Input.dispatchTouchEvent', {type: 'touchMove', touchPoints: point(p)});
          await page.waitForTimeout(16);
        }
        if (midway) await midway();
        await cdp.send('Input.dispatchTouchEvent', {type: 'touchEnd', touchPoints: []});
      } else {
        await page.mouse.move(from.x, from.y);
        await page.mouse.down();
        await page.mouse.move(to.x, to.y, {steps});
        if (midway) await midway();
        await page.mouse.up();
      }
      await page.waitForTimeout(150);
    };
    // Centre an element: the phone's sticky Perform heading covers the top.
    const reveal = locator => locator.evaluate(e => e.scrollIntoView({block: 'center', behavior: 'instant'}));
    const tap = async locator => viewport.touch ? locator.tap() : locator.click();
    const showSection = async id => {
      if (viewport.name !== 'phone') return;
      const tab = page.locator('#view-perform .perform-section-tabs button', {hasText: new RegExp('^' + id)});
      if (await tab.isVisible()) await tab.click();
    };

    await page.goto(origin + '/', {waitUntil: 'domcontentloaded'});
    await page.waitForFunction(() => document.querySelector('#connection-label')?.textContent === 'Studio connected', null, {timeout: 60000});
    await showSection('Room');
    await page.locator('#preview-panel').scrollIntoViewIfNeeded();
    await page.waitForFunction(() => document.querySelector('#preview')?.tolglowPreview?.ready === true, null, {timeout: 90000});
    await showSection('Cues');

    // A running cue whose control card must survive every move.
    // The server's show outlives this page: release what an earlier run left.
    await page.locator('#end-all').evaluate(button => button.click());
    await page.waitForFunction(() => !document.querySelector('[data-live-instance]'), null, {timeout: 20000});
    const cue = page.locator('#cue-grid .cue-cell:not(.empty)').first();
    await tap(cue);
    await page.waitForSelector('[data-live-instance]', {state: 'attached', timeout: 20000});
    await page.waitForTimeout(300);
    assert.equal(await page.locator('[data-live-instance]').count(), 1, 'one running cue');
    await page.evaluate(() => {
      document.querySelector('[data-live-instance]').__qaTag = 'card';
      document.querySelector('#preview').__qaTag = 'canvas';
    });
    await page.waitForTimeout(500);
    const before = await census('built-in');
    assert.equal(before.custom, false);
    assert.deepEqual(before.order, ['cues', 'live', 'room', 'tools']);
    assert.equal(before.sockets, 1);
    assert.equal(before.workers, 1, 'one preview worker');
    await shot('1-built-in');

    // Enter edit mode: every panel is shielded and handled.
    await tap(page.locator('#edit-layout'));
    await page.waitForSelector('#view-perform .editing-layout');
    assert.equal(await page.locator('#view-perform .layout-chrome').count(), 4);
    assert.equal(await page.locator('#view-perform .layout-content[inert]').count(), 4);
    const seeded = await census('edit-seeded');
    assert.deepEqual(seeded.order, ['cues', 'live', 'room', 'tools']);
    await page.locator('#view-perform [data-panel="cues"]').scrollIntoViewIfNeeded();
    await shot('2-edit');

    // Drag the Room's title onto the Live panel: a drop target shows mid-drag.
    const roomHandle = page.locator('#view-perform [data-panel="room"] .layout-handle');
    await reveal(roomHandle);
    const liveBox = await page.locator('#view-perform [data-panel="live"]').boundingBox();
    const from = await center(roomHandle);
    // Aim at a visible part of Live, just above the Room on stacked layouts.
    const to = {x: liveBox.x + liveBox.width / 2,
      y: Math.min(liveBox.y + liveBox.height - 40, Math.max(liveBox.y + 60, from.y - 120))};
    let sawTarget = null;
    await drag(from, to, {midway: async () => {
      sawTarget = await page.evaluate(() => document.querySelector('#view-perform [data-drop-target]')?.dataset.panel ?? null);
      await shot('3-dragging');
    }});
    const dropped = await census('after-drag');
    assert.equal(sawTarget, 'live', 'Live is marked as the drop target');
    assert.deepEqual(dropped.order, ['cues', 'room', 'live', 'tools'], 'Room moved before Live');
    assert.ok(dropped.cardTagged && dropped.canvasTagged, 'no remount after a drag');

    // Drag the Room's corner wider and taller.
    const resize = page.locator('#view-perform [data-panel="room"] .layout-resize');
    await reveal(resize);
    const n = s => Number(s.replace('span ', ''));
    const roomBefore = await spans('room');
    const corner = await center(resize);
    const hit = await page.evaluate(({x, y}) => document.elementFromPoint(x, y)?.className, corner);
    assert.equal(hit, 'layout-resize', 'the corner handle is on top at its centre');
    const full = n(roomBefore.column) === viewport.columns;
    const dx = viewport.columns > 2 ? Math.round(roomBefore.box.width / 3) * (full ? -1 : 1) : 0;
    await drag(corner, {x: Math.min(viewport.width - 5, corner.x + dx), y: corner.y + 120});
    const roomAfter = await spans('room');
    assert.ok(n(roomAfter.row) >= n(roomBefore.row) + 5, `Room taller: ${roomBefore.row} -> ${roomAfter.row}`);
    if (viewport.columns > 2) assert.ok(full ? n(roomAfter.column) < n(roomBefore.column) : n(roomAfter.column) > n(roomBefore.column),
      `Room ${full ? 'narrower' : 'wider'}: ${roomBefore.column} -> ${roomAfter.column}`);
    else assert.equal(roomAfter.column, 'span 2');
    await census('after-resize');
    await shot('4-resized');

    // Escape cancels a drag in progress and leaves the layout as it was.
    const toolsHandle = page.locator('#view-perform [data-panel="tools"] .layout-handle');
    await reveal(toolsHandle);
    const orderBeforeEscape = (await census('before-escape')).order;
    const t = await center(toolsHandle);
    await drag(t, {x: t.x, y: Math.max(10, t.y - 300)}, {midway: () => page.keyboard.press('Escape')});
    const escaped = await census('after-escape');
    assert.deepEqual(escaped.order, orderBeforeEscape, 'Escape cancelled the drag');
    assert.equal(escaped.editing, true, 'Escape during a drag keeps edit mode');

    // Keyboard: arrows on a title move the panel and keep focus on it.
    await toolsHandle.focus();
    await page.keyboard.press('ArrowUp');
    await page.waitForTimeout(100);
    const keyed = await census('keyboard-move');
    assert.deepEqual(keyed.order, ['cues', 'room', 'tools', 'live']);
    assert.equal(await page.evaluate(() => document.activeElement?.closest('[data-panel]')?.dataset.panel), 'tools', 'focus stays on the moved panel');
    assert.match(await page.evaluate(() => document.activeElement?.getAttribute('aria-label')), /Move Tools panel, position 3 of 4/);
    // The visible step buttons work from the keyboard and keep focus too.
    await page.locator('#view-perform [data-panel="tools"] [data-step="later"]').focus();
    await page.keyboard.press('Enter');
    await page.waitForTimeout(100);
    assert.deepEqual((await census('step-later')).order, ['cues', 'room', 'live', 'tools']);
    assert.equal(await page.evaluate(() => document.activeElement?.getAttribute('aria-label')), 'Move Tools later');
    await page.locator('#view-perform [data-panel="tools"] [data-step="earlier"]').focus();
    await page.keyboard.press('Enter');
    await page.waitForTimeout(100);
    assert.deepEqual((await census('step-earlier')).order, ['cues', 'room', 'tools', 'live']);
    assert.equal(await page.evaluate(() => document.activeElement?.getAttribute('aria-label')), 'Move Tools earlier');
    const toolsBefore = await spans('tools');
    await page.locator('#view-perform [data-panel="tools"] .layout-resize').focus();
    await page.keyboard.press('Shift+ArrowDown');
    await page.waitForTimeout(100);
    assert.equal(n((await spans('tools')).row), n(toolsBefore.row) + 4, 'Shift+ArrowDown adds four rows');
    assert.match(await page.locator('#view-perform .layout-toolbar .sr-only').textContent(), /Tools · position 3 of 4/);

    // Done saves for this size class only.
    await tap(page.locator('#layout-done'));
    await page.waitForSelector('#view-perform .editing-layout', {state: 'detached'});
    const stored = await page.evaluate(() => JSON.parse(localStorage.getItem('tolglow.studio.panel-layouts')));
    assert.deepEqual(Object.keys(stored.layouts), [viewport.cls]);
    assert.deepEqual(stored.layouts[viewport.cls].map(e => e.id), ['cues', 'room', 'tools', 'live']);
    const saved = await census('saved');
    assert.equal(saved.custom, true);
    assert.ok(saved.cardTagged && saved.canvasTagged, 'running cue card and Room canvas are the same nodes');
    assert.equal(saved.sockets, 1, 'one socket');
    assert.equal(saved.workers, 1, 'one preview worker');
    assert.equal(saved.workersCreated, before.workersCreated, 'no preview worker was recreated');
    assert.equal(saved.liveInstance, before.liveInstance, 'the running cue was not relaunched');
    // The Room's 3D view fills its resized tile.
    const stage = await page.evaluate(() => {
      const region = document.querySelector('#view-perform [data-panel="room"]').getBoundingClientRect();
      const stage = document.querySelector('#preview-panel .preview-stage').getBoundingClientRect();
      return {region: region.height, stage: stage.height};
    });
    assert.ok(stage.stage >= 160 && stage.stage < stage.region, `preview stage fills the tile ${JSON.stringify(stage)}`);
    await page.evaluate(() => window.scrollTo(0, 0));
    await shot('5-saved');

    // Reload: the saved layout returns for this screen.
    await page.reload({waitUntil: 'domcontentloaded'});
    await page.waitForFunction(() => document.querySelector('#connection-label')?.textContent === 'Studio connected', null, {timeout: 60000});
    await page.waitForSelector('#view-perform .custom-layout');
    await reveal(page.locator('#preview-panel'));
    const reloaded = await census('reloaded');
    assert.deepEqual(reloaded.order, ['cues', 'room', 'tools', 'live']);
    assert.equal((await spans('room')).row, roomAfter.row);
    assert.equal(await page.locator('#view-perform .perform-section-tabs').isVisible(), false, 'a custom layout replaces the phone section tabs');
    await page.waitForFunction(() => document.querySelector('#preview')?.tolglowPreview?.ready === true, null, {timeout: 90000});

    // Detach the Room from edit mode and redock it (one engine throughout).
    if (viewport.name !== 'phone') {
      await tap(page.locator('#edit-layout'));
      await page.waitForSelector('#view-perform .editing-layout');
      await page.locator('#view-perform [data-panel="room"] .layout-detach').click();
      await page.waitForSelector('#room-window-placeholder', {timeout: 20000});
      assert.ok(roomPage, 'the Room window opened');
      await roomPage.waitForFunction(() => document.querySelector('#preview')?.tolglowPreview?.ready === true, null, {timeout: 90000});
      const detached = await census('detached');
      assert.equal(await page.locator('#preview').count(), 0, 'Studio holds no Room canvas while the Room is detached');
      assert.equal(await roomPage.locator('#preview').count(), 1, 'the Room window holds the only Room canvas');
      assert.equal(await page.locator('#view-perform [data-panel="room"] .layout-detach').textContent(), 'Redock');
      await shot('6-detached');
      await page.locator('#view-perform [data-panel="room"] .layout-detach').click();
      await page.waitForSelector('#room-window-placeholder', {state: 'detached', timeout: 20000});
      await page.waitForFunction(() => document.querySelector('#preview')?.tolglowPreview?.ready === true, null, {timeout: 90000});
      const redocked = await census('redocked');
      assert.equal(redocked.workers, 1, 'one preview worker after redock');
      assert.equal(redocked.sockets, 1, 'still one socket');
      record.detach = {detachedWorkersInStudio: detached.workers, redockedWorkers: redocked.workers};
      await tap(page.locator('#layout-cancel'));
    }

    // A damaged saved layout is repaired: unknown panels go, missing ones return.
    await page.evaluate(cls => localStorage.setItem('tolglow.studio.panel-layouts', JSON.stringify(
      {version: 1, layouts: {[cls]: [{id: 'tools', w: 99, h: 1}, {id: 'mixer', w: 3, h: 9}, {id: 'tools', w: 1, h: 30}]}})), viewport.cls);
    await page.reload({waitUntil: 'domcontentloaded'});
    await page.waitForSelector('#view-perform .custom-layout', {timeout: 60000});
    const repaired = await census('repaired');
    assert.deepEqual(repaired.order, ['tools', 'cues', 'live', 'room']);
    assert.equal((await spans('tools')).column, `span ${viewport.columns}`);

    // Reset returns to the built-in responsive layout and clears the storage.
    await tap(page.locator('#edit-layout'));
    await page.waitForSelector('#view-perform .editing-layout');
    await tap(page.locator('#layout-reset'));
    await page.waitForSelector('#view-perform .custom-layout', {state: 'detached'});
    const reset = await census('reset');
    assert.deepEqual(reset.order, ['cues', 'live', 'room', 'tools']);
    assert.equal(await page.evaluate(() => localStorage.getItem('tolglow.studio.panel-layouts')), null);
    // Escape outside a gesture cancels editing.
    await tap(page.locator('#edit-layout'));
    await page.waitForSelector('#view-perform .editing-layout');
    await page.keyboard.press('Escape');
    await page.waitForSelector('#view-perform .editing-layout', {state: 'detached'});
    assert.equal(await page.evaluate(() => localStorage.getItem('tolglow.studio.panel-layouts')), null);

    await page.locator('#end-all').evaluate(button => button.click());
    assert.deepEqual(errors, [], 'no page errors');
    record.passed = true;
    console.log(viewport.name, 'passed');
  } catch (error) {
    record.error = String(error.stack || error);
    record.pageErrors = errors;
    const page = context.pages()[0];
    if (page) await page.screenshot({path: join(directory, `${viewport.name}-failed.png`)}).catch(() => {});
    throw error;
  } finally {
    await writeFile(join(directory, 'results.json'), JSON.stringify(summary, null, 2) + '\n');
    await context.close();
  }
}
console.log('all viewports passed');
