// Arrange the Perform panels in layout edit mode at desktop, tablet and phone
// sizes: drag to reorder, drag a corner to resize, use the keyboard, cancel
// with Escape, save, reload, detach and redock the Room and other panels
// (window-sized content, a tile's own Source editor, removing a detached tile,
// cue pads in a Cues window), recover a damaged saved layout and reset to the
// built-in layout, and the Mac app's window and layout bindings, simulated. At each step it checks that a
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
    const queries = [];
    page.on('websocket', socket => {
      if (new URL(socket.url()).pathname === '/api/state/stream')
        socket.on('framesent', ({payload}) => { if (String(payload).includes('~:queries')) queries.push(String(payload)); });
    });
    const cdp = await context.newCDPSession(page);
    const shot = async name => { await page.waitForTimeout(400); await page.screenshot({path: join(directory, `${viewport.name}-${name}.png`)}); };

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
    assert.equal(await page.locator('#layout-announce').textContent(), '', 'the live region starts empty');
    await tap(page.locator('#edit-layout'));
    await page.waitForSelector('#view-perform .editing-layout');
    assert.match(await page.locator('#layout-announce').textContent(), /Editing/, 'entering edit mode is announced');
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
    assert.equal(await page.evaluate(() => document.activeElement?.textContent), 'Later Tools', 'focus stays on Later; its name starts with the visible word');
    await page.locator('#view-perform [data-panel="tools"] [data-step="earlier"]').focus();
    await page.keyboard.press('Enter');
    await page.waitForTimeout(100);
    assert.deepEqual((await census('step-earlier')).order, ['cues', 'room', 'tools', 'live']);
    assert.equal(await page.evaluate(() => document.activeElement?.textContent), 'Earlier Tools');
    const toolsBefore = await spans('tools');
    await page.locator('#view-perform [data-panel="tools"] .layout-resize').focus();
    await page.keyboard.press('Shift+ArrowDown');
    await page.waitForTimeout(100);
    assert.equal(n((await spans('tools')).row), n(toolsBefore.row) + 4, 'Shift+ArrowDown adds four rows');
    assert.match(await page.locator('#layout-announce').textContent(), /Tools · position 3 of 4/);

    // Done saves for this size class only.
    await tap(page.locator('#layout-done'));
    await page.waitForSelector('#view-perform .editing-layout', {state: 'detached'});
    const stored = await page.evaluate(() => JSON.parse(localStorage.getItem('tolglow.studio.panel-layouts')));
    assert.equal(stored.version, 2);
    assert.deepEqual(Object.keys(stored.classes), [viewport.cls]);
    assert.equal(stored.classes[viewport.cls].active, 'Layout 1');
    assert.deepEqual(stored.classes[viewport.cls].layouts[0].panels.map(e => e.id), ['cues', 'room', 'tools', 'live']);
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
      assert.equal(await page.locator('#view-perform [data-panel="room"] .layout-detach').textContent(), 'Redock Room', 'Redock names its panel');
      await shot('6-detached');
      // The Room follows its window's size too (#743): a short window drops
      // the camera and preview settings, as a compact tile does.
      const roomTier = () => roomPage.evaluate(() => document.querySelector('.panel-window-content[data-panel="room"]')?.dataset.tier);
      assert.equal(await roomTier(), 'full', 'a 960 × 640 Room window shows full content');
      assert.ok(await roomPage.locator('.camera-navigation:visible').count() > 0, 'with camera navigation');
      const roomSize = roomPage.viewportSize();
      await roomPage.setViewportSize({width: 960, height: 240});
      await roomPage.waitForFunction(() => document.querySelector('.panel-window-content[data-panel="room"]')?.dataset.tier === 'compact', null, {timeout: 5000});
      assert.equal(await roomPage.locator('.camera-navigation:visible').count(), 0, 'a short Room window hides camera navigation');
      await roomPage.screenshot({path: join(directory, `${viewport.name}-6b-room-window-compact.png`)});
      await roomPage.setViewportSize(roomSize);
      await roomPage.waitForFunction(() => document.querySelector('.panel-window-content[data-panel="room"]')?.dataset.tier === 'full', null, {timeout: 5000});
      await page.locator('#view-perform [data-panel="room"] .layout-detach').click();
      await page.waitForSelector('#room-window-placeholder', {state: 'detached', timeout: 20000});
      await page.waitForFunction(() => document.querySelector('#preview')?.tolglowPreview?.ready === true, null, {timeout: 90000});
      const redocked = await census('redocked');
      assert.equal(redocked.workers, 1, 'one preview worker after redock');
      assert.equal(redocked.sockets, 1, 'still one socket');
      record.detach = {detachedWorkersInStudio: detached.workers, redockedWorkers: redocked.workers};
      await tap(page.locator('#layout-cancel'));
      await page.waitForSelector('#view-perform .editing-layout', {state: 'detached'});
    }

    // A damaged version 1 layout is migrated and repaired: unknown panels go, missing ones return.
    await page.evaluate(cls => localStorage.setItem('tolglow.studio.panel-layouts', JSON.stringify(
      {version: 1, layouts: {[cls]: [{id: 'tools', w: 99, h: 1}, {id: 'mixer', w: 3, h: 9}, {id: 'tools', w: 1, h: 30}]}})), viewport.cls);
    await page.reload({waitUntil: 'domcontentloaded'});
    await page.waitForSelector('#view-perform .custom-layout', {timeout: 60000});
    const repaired = await census('repaired');
    assert.deepEqual(repaired.order, ['tools', 'cues', 'live', 'room']);
    assert.equal((await spans('tools')).column, `span ${viewport.columns}`);

    // Reset returns to the built-in responsive layout and keeps the saved one.
    await tap(page.locator('#edit-layout'));
    await page.waitForSelector('#view-perform .editing-layout');
    await tap(page.locator('#layout-reset'));
    await page.waitForSelector('#view-perform .custom-layout', {state: 'detached'});
    const reset = await census('reset');
    assert.deepEqual(reset.order, ['cues', 'live', 'room', 'tools']);
    const storedAfterReset = await page.evaluate(() => JSON.parse(localStorage.getItem('tolglow.studio.panel-layouts')));
    assert.equal(storedAfterReset.classes[viewport.cls].active, null);
    assert.deepEqual(storedAfterReset.classes[viewport.cls].layouts.map(l => l.name), ['Layout 1']);
    // Escape outside a gesture cancels editing.
    await tap(page.locator('#edit-layout'));
    await page.waitForSelector('#view-perform .editing-layout');
    await page.keyboard.press('Escape');
    await page.waitForSelector('#view-perform .editing-layout', {state: 'detached'});
    assert.deepEqual(await page.evaluate(() => JSON.parse(localStorage.getItem('tolglow.studio.panel-layouts')).classes),
                     storedAfterReset.classes, 'Escape saved nothing');

    // ---- Named layouts, parameter tiles and the visualization panel.
    await page.locator('#end-all').evaluate(button => button.click());
    await page.waitForFunction(() => !document.querySelector('[data-live-instance]'), null, {timeout: 20000});
    await showSection('Cues');
    await tap(page.locator('#cue-grid .cue-cell:not(.empty)').first());
    await page.waitForSelector('[data-live-instance]', {state: 'attached', timeout: 20000});
    const instance = await page.locator('[data-live-instance]').getAttribute('data-live-instance');
    await page.evaluate(() => {
      document.querySelector('[data-live-instance]').__qaTag = 'card';
      document.querySelector('#preview').__qaTag = 'canvas';
    });
    await tap(page.locator('#edit-layout'));
    await page.waitForSelector('#view-perform .editing-layout');
    assert.equal(await page.locator('#layout-name').inputValue(), 'Layout 2', 'a new layout gets a free name');
    await page.locator('#layout-name').fill('Tiles');
    await tap(page.locator('#layout-add-param'));
    const tile = page.locator('#view-perform [data-panel="param-1"]');
    await tile.waitFor();
    assert.equal(await page.evaluate(() => document.activeElement?.closest('[data-panel]')?.dataset.panel), 'param-1', 'focus moves to the new tile');
    // Pick the running cue's first parameter from the tile's menu.
    const menu = tile.locator('.layout-chrome .tile-picker');
    const option = await menu.locator('option').evaluateAll(os => os.find(o => / · running$/.test(o.textContent))?.value);
    assert.ok(option, 'the menu offers the running cue');
    await menu.selectOption(option);
    const optionLabel = (await menu.locator(`option[value="${option}"]`).textContent()).replace(/ · running$/, '');
    // Shrink it to a 1×1 tile from the keyboard.
    await tile.locator('.layout-resize').focus();
    for (let i = 0; i < 4; i++) await page.keyboard.press('Shift+ArrowLeft');
    for (let i = 0; i < 4; i++) await page.keyboard.press('Shift+ArrowUp');
    await page.waitForTimeout(100);
    assert.equal(await tile.getAttribute('data-tier'), 'single');
    await tap(page.locator('#layout-add-viz'));
    await page.locator('#view-perform [data-panel="viz"]').waitFor();
    assert.equal(await page.locator('#layout-add-viz').isDisabled(), true, 'one visualization panel per layout');
    await tap(page.locator('#layout-done'));
    await page.waitForSelector('#view-perform .editing-layout', {state: 'detached'});
    let named = await page.evaluate(() => JSON.parse(localStorage.getItem('tolglow.studio.panel-layouts')).classes);
    assert.equal(named[viewport.cls].active, 'Tiles');
    assert.deepEqual(named[viewport.cls].layouts.map(l => l.name), ['Layout 1', 'Tiles']);
    const tilesEntry = named[viewport.cls].layouts[1].panels.find(p => p.id === 'param-1');
    assert.ok(tilesEntry.binding?.cue && tilesEntry.binding?.param, 'the tile stores its cue and parameter');
    await reveal(tile);
    await tile.locator('.tile-single .tile-readout').waitFor();
    const reading = await tile.locator('.tile-readout').textContent();
    assert.ok(reading && reading !== '—', `the 1×1 tile reads the live value: ${reading}`);
    assert.equal(await tile.locator('.param-row').count(), 0, 'a 1×1 tile shows no control');
    assert.match(await tile.locator('.tile-title').textContent(), new RegExp(optionLabel.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')));
    await shot('7-tile-single');
    const viz = page.locator('#view-perform [data-panel="viz"]');
    await reveal(viz);
    assert.ok(await viz.locator('.viz-item').count() >= 1, 'the visualization panel lists the running parameters');
    await shot('8-visualization');

    // Expanding the tile restores the full control and keeps its parameter.
    await tap(page.locator('#edit-layout'));
    await page.waitForSelector('#view-perform .editing-layout');
    await tile.locator('.layout-resize').focus();
    for (let i = 0; i < 3; i++) await page.keyboard.press('Shift+ArrowRight');
    for (let i = 0; i < 5; i++) await page.keyboard.press('Shift+ArrowDown');
    await page.waitForTimeout(100);
    assert.equal(await tile.getAttribute('data-tier'), 'full');
    await tap(page.locator('#layout-done'));
    await page.waitForSelector('#view-perform .editing-layout', {state: 'detached'});
    await reveal(tile);
    await tile.locator('.tile-full .param-row').waitFor();
    assert.equal(await tile.locator('.tile-head .tile-picker').inputValue(), option, 'the expanded tile keeps its parameter');
    await shot('9-tile-full');
    const kept = await census('tiles-saved');
    assert.ok(kept.cardTagged && kept.canvasTagged, 'adding tiles did not remount the running cue or the Room');
    assert.equal(kept.liveInstance, instance);

    // Switch between named layouts and the built-in one without remounting.
    const select = page.locator('#layout-select');
    await select.selectOption('Layout 1');
    await page.waitForFunction(() => !document.querySelector('#view-perform [data-panel="param-1"]'));
    assert.deepEqual((await census('switch-layout-1')).order, ['tools', 'cues', 'live', 'room']);
    await select.selectOption('');
    await page.waitForSelector('#view-perform .custom-layout', {state: 'detached'});
    await select.selectOption('Tiles');
    await tile.waitFor();
    const switched = await census('switch-tiles');
    assert.ok(switched.cardTagged && switched.canvasTagged, 'switching layouts did not remount the running cue or the Room');
    assert.equal(switched.sockets, 1);
    assert.equal(switched.workers, 1);
    assert.equal(switched.liveInstance, instance, 'switching layouts did not relaunch the cue');
    await shot('10-switcher');

    // Detach panels other than the Room into their own windows.
    if (viewport.name !== 'phone') {
      await tap(page.locator('#edit-layout'));
      await page.waitForSelector('#view-perform .editing-layout');
      roomPage = null;
      await page.locator('#view-perform [data-panel="live"] .layout-detach').click();
      await page.waitForSelector('#view-perform [data-detached="live"]', {timeout: 20000});
      assert.ok(roomPage, 'the Live window opened');
      const liveWindow = roomPage;
      await liveWindow.waitForSelector('[data-live-instance]', {timeout: 20000});
      assert.equal(await liveWindow.title(), 'Tolglow · Live', 'the panel window is titled');
      assert.equal(await liveWindow.locator('[data-live-instance]').getAttribute('data-live-instance'), instance,
                   'the detached Live panel shows the same running cue');
      assert.equal(await page.locator('[data-live-instance]').count(), 0, 'the card exists in one window only');
      const detachedLive = await census('live-detached');
      assert.equal(detachedLive.sockets, 1, 'no second socket for a detached panel');
      assert.equal(detachedLive.workers, 1, 'the Room keeps its one preview worker');
      // Act in the detached window: the card's Detail toggle works there.
      await liveWindow.locator('.live-effect-expand').first().click();
      await liveWindow.waitForSelector('.live-effect-card[data-density="detail"]', {timeout: 10000});
      await liveWindow.screenshot({path: join(directory, `${viewport.name}-11-live-window.png`)});
      // Content in a window follows the window's size, not its tile's (#743);
      // the Room window above shows the tier rules reaching a window.
      const liveTier = () => liveWindow.locator('.panel-window-content[data-panel="live"]').getAttribute('data-tier');
      assert.equal(await liveTier(), 'full', 'a tall Live window shows full content');
      const liveSize = liveWindow.viewportSize();
      await liveWindow.setViewportSize({width: 720, height: 240});
      await liveWindow.waitForFunction(() => document.querySelector('.panel-window-content[data-panel="live"]')?.dataset.tier === 'compact', null, {timeout: 5000});
      await liveWindow.setViewportSize(liveSize);
      await liveWindow.waitForFunction(() => document.querySelector('.panel-window-content[data-panel="live"]')?.dataset.tier === 'full', null, {timeout: 5000});
      // A parameter tile detaches too.
      roomPage = null;
      await tile.locator('.layout-detach').click();
      await page.waitForSelector('#view-perform [data-detached="param-1"]', {timeout: 20000});
      const tileWindow = roomPage;
      await tileWindow.waitForSelector('.parameter-tile .tile-full', {timeout: 20000});
      // The tile's Source trigger is its own: a distinct ID, and it opens the
      // editor in the tile window, not in the Live card's window (#743).
      const tileTrigger = tileWindow.locator('.parameter-tile .param-source-trigger').first();
      const tileTriggerId = await tileTrigger.getAttribute('id');
      assert.match(tileTriggerId, /^source-tile-param-1-/, 'the tile trigger has a tile-scoped ID');
      const cardIds = await liveWindow.evaluate(() => [...document.querySelectorAll('.param-source-trigger')].map(e => e.id));
      assert.ok(!cardIds.includes(tileTriggerId), 'no Live card trigger shares the tile trigger ID');
      await tileTrigger.click();
      await tileWindow.waitForSelector('.parameter-tile .live-source-editor', {timeout: 5000});
      await tileWindow.screenshot({path: join(directory, `${viewport.name}-11b-tile-source.png`)});
      assert.equal(await liveWindow.locator('.live-source-editor').count(), 0, 'the Live window opened no editor');
      assert.equal(await tileTrigger.getAttribute('aria-expanded'), 'true');
      assert.equal(await tileTrigger.getAttribute('aria-controls'),
                   await tileWindow.locator('.parameter-tile .live-source-editor').getAttribute('id'),
                   'the trigger controls the editor beside it');
      // Keyboard focus moves to the editor in the tile's own window, and back.
      await tileWindow.waitForFunction(() => /^source-editor-heading-tile-param-1-/.test(document.activeElement?.id || ''), null, {timeout: 5000});
      await tileWindow.locator('.parameter-tile .live-source-editor button', {hasText: 'Close'}).click();
      await tileWindow.waitForSelector('.parameter-tile .live-source-editor', {state: 'detached', timeout: 5000});
      await tileWindow.waitForFunction(id => document.activeElement?.id === id, tileTriggerId, {timeout: 5000});
      record.tileSource = {tileTriggerId};
      await tap(page.locator('#layout-cancel'));
      await page.waitForSelector('#view-perform .editing-layout', {state: 'detached'});
      await page.evaluate(() => window.scrollTo(0, 0));
      await shot('12-detached-placeholders');
      // Closing a window redocks its panel; the placeholder's Redock does too.
      await liveWindow.close();
      await page.waitForSelector('#view-perform [data-detached="live"]', {state: 'detached', timeout: 10000});
      await page.waitForSelector('#view-perform [data-panel="live"] [data-live-instance]', {state: 'attached', timeout: 10000});
      assert.equal(await page.locator('[data-live-instance]').getAttribute('data-live-instance'), instance, 'the redocked card is the same running cue');
      await page.locator('#view-perform [data-detached="param-1"] button', {hasText: 'Redock'}).click();
      await page.waitForSelector('#view-perform [data-detached="param-1"]', {state: 'detached', timeout: 10000});
      await tile.locator('.tile-full').waitFor();
      assert.equal(tileWindow.isClosed(), true, 'Redock closed the tile window');
      const redockedPanels = await census('panels-redocked');
      assert.equal(redockedPanels.sockets, 1);
      assert.equal(redockedPanels.workers, 1);
      assert.equal(redockedPanels.liveInstance, instance, 'detaching never relaunched the cue');
      record.panelWindows = {liveInstance: instance, sockets: redockedPanels.sockets};

      // Removing a detached tile in edit mode closes its window at once (#743).
      await tap(page.locator('#edit-layout'));
      await page.waitForSelector('#view-perform .editing-layout');
      roomPage = null;
      await tile.locator('.layout-detach').click();
      await page.waitForSelector('#view-perform [data-detached="param-1"]', {timeout: 20000});
      const removedWindow = roomPage;
      await removedWindow.waitForSelector('.parameter-tile', {timeout: 20000});
      await tile.locator('.layout-remove').click();
      await page.waitForFunction(() => !document.querySelector('#view-perform [data-panel="param-1"]'), null, {timeout: 5000});
      for (let i = 0; i < 20 && !removedWindow.isClosed(); i++) await page.waitForTimeout(100);
      assert.equal(removedWindow.isClosed(), true, 'the removed tile window closed before Done or Cancel');
      assert.equal((await census('tile-removed')).editing, true, 'still editing');
      await tap(page.locator('#layout-cancel'));
      await tile.locator('.tile-full').waitFor();
      assert.equal(await page.locator('#view-perform [data-detached="param-1"]').count(), 0, 'Cancel brings the tile back docked');

      // Cue pad previews in a detached Cues window follow that window's focus,
      // not Studio's (#743). Studio's focus is faked: headless Chrome reports
      // focus for every page.
      if (viewport.name === 'desktop') {
        await tap(page.locator('#edit-layout'));
        await page.waitForSelector('#view-perform .editing-layout');
        roomPage = null;
        await page.locator('#view-perform [data-panel="cues"] .layout-detach').click();
        await page.waitForSelector('#view-perform [data-detached="cues"]', {timeout: 20000});
        const cuesWindow = roomPage;
        await cuesWindow.waitForSelector('#cue-grid', {timeout: 20000});
        await tap(page.locator('#layout-cancel'));
        const lastQuery = () => queries[queries.length - 1] || '';
        const settle = async () => { await page.waitForTimeout(600); return lastQuery().includes('visualization/cue-pads'); };
        await page.evaluate(() => { document.hasFocus = () => false; window.dispatchEvent(new Event('blur')); });
        await cuesWindow.evaluate(() => window.dispatchEvent(new Event('focus')));
        assert.equal(await settle(), true, 'the Cues window samples its pads while Studio lacks focus');
        await cuesWindow.evaluate(() => { document.hasFocus = () => false; window.dispatchEvent(new Event('blur')); });
        assert.equal(await settle(), false, 'and pauses when its own window loses focus');
        await cuesWindow.evaluate(() => { delete document.hasFocus; window.dispatchEvent(new Event('focus')); });
        assert.equal(await settle(), true, 'and resumes when it has focus again');
        await page.evaluate(() => { delete document.hasFocus; window.dispatchEvent(new Event('focus')); });
        await cuesWindow.close();
        await page.waitForSelector('#view-perform [data-detached="cues"]', {state: 'detached', timeout: 10000});
        record.cuesWindow = {samplesWhileStudioUnfocused: true, pausesOnOwnBlur: true};
      }
    }

    // Dragging to the bottom edge scrolls the page, and a panel that was below
    // the fold becomes the drop target; Escape then cancels the drag.
    {
      await tap(page.locator('#edit-layout'));
      await page.waitForSelector('#view-perform .editing-layout');
      await page.evaluate(() => window.scrollTo(0, 0));
      await page.waitForTimeout(200);
      const order = (await census('before-autoscroll')).order;
      const handle = page.locator(`#view-perform [data-panel="${order[0]}"] .layout-handle`);
      const start = await center(handle);
      const bottom = {x: viewport.width / 2, y: viewport.height - 6};
      const limit = await page.evaluate(() => document.documentElement.scrollHeight - innerHeight);
      const hidden = await page.evaluate(vh => [...document.querySelectorAll('#view-perform .layout-region')]
        .filter(e => e.getBoundingClientRect().top > vh).map(e => e.dataset.panel), viewport.height);
      assert.ok(hidden.length > 0, 'some panel starts below the fold');
      if (viewport.touch) {
        const point = p => [{x: p.x, y: p.y, id: 2, radiusX: 4, radiusY: 4, force: 1}];
        await cdp.send('Input.dispatchTouchEvent', {type: 'touchStart', touchPoints: point(start)});
        for (let i = 1; i <= 8; i++) {
          await cdp.send('Input.dispatchTouchEvent', {type: 'touchMove', touchPoints: point({x: start.x + (bottom.x - start.x) * i / 8, y: start.y + (bottom.y - start.y) * i / 8})});
          await page.waitForTimeout(16);
        }
      } else {
        await page.mouse.move(start.x, start.y);
        await page.mouse.down();
        await page.mouse.move(bottom.x, bottom.y, {steps: 8});
      }
      if (viewport.touch) {
        // A second finger on another handle neither takes over nor, when it
        // lifts, stops the drag's auto-scroll (#743).
        await page.waitForTimeout(120);
        const other = await page.evaluate(() => {
          const dragged = document.querySelector('#view-perform .layout-region[data-gesture] .layout-handle');
          for (const e of document.querySelectorAll('#view-perform .layout-handle, #view-perform .layout-resize')) {
            if (e === dragged) continue;
            const r = e.getBoundingClientRect();
            const x = r.left + r.width / 2, y = r.top + r.height / 2;
            const hit = document.elementFromPoint(x, y);
            if (y > 20 && y < innerHeight - 30 && x > 10 && x < innerWidth - 10 && hit && e.contains(hit)) return {x, y};
          }
          return null;
        });
        if (viewport.name === 'tablet') assert.ok(other, 'another handle is in reach of a second finger: ' + JSON.stringify(await page.evaluate(() =>
          [...document.querySelectorAll('#view-perform .layout-handle, #view-perform .layout-resize')].map(e => {
            const r = e.getBoundingClientRect(); const hit = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);
            return [e.className, Math.round(r.top), Math.round(r.left), hit && hit.className];
          }))));
        if (other) {
          const both = [{x: bottom.x, y: bottom.y, id: 2, radiusX: 4, radiusY: 4, force: 1},
                        {x: other.x, y: other.y, id: 3, radiusX: 4, radiusY: 4, force: 1}];
          await cdp.send('Input.dispatchTouchEvent', {type: 'touchStart', touchPoints: both});
          await page.waitForTimeout(80);
          // The touch points listed are those still down: this lifts the second finger only.
          await cdp.send('Input.dispatchTouchEvent', {type: 'touchMove', touchPoints: [both[0]]});
          const atLift = await page.evaluate(() => window.scrollY);
          await page.waitForTimeout(400);
          const after = await page.evaluate(() => window.scrollY);
          assert.ok(after > atLift || after >= limit - 1, `auto-scroll continued after the second finger lifted (${atLift} → ${after} of ${limit})`);
          record.secondPointer = {atLift, after, limit};
        }
      }
      await page.waitForTimeout(1500);
      const scrolled = await page.evaluate(() => window.scrollY);
      const target = await page.evaluate(() => document.querySelector('#view-perform [data-drop-target]')?.dataset.panel ?? null);
      assert.ok(scrolled > 100, `holding at the edge scrolled the page (${scrolled} px)`);
      assert.ok(hidden.includes(target), `a panel from below the fold is the drop target (${target})`);
      // Holding on stops at the end of the page: the dragged panel's own
      // translation must not lengthen the page and keep it scrolling.
      await page.waitForTimeout(2500);
      const end = await page.evaluate(() => window.scrollY);
      assert.ok(end >= scrolled && end <= limit, `auto-scroll stopped at the end of the page (${end} of ${limit} px)`);
      await page.keyboard.press('Escape');
      if (viewport.touch) await cdp.send('Input.dispatchTouchEvent', {type: 'touchEnd', touchPoints: []});
      else await page.mouse.up();
      await page.waitForTimeout(200);
      assert.deepEqual((await census('after-autoscroll')).order, order, 'Escape cancelled the scrolled drag');
      const still = await page.evaluate(() => window.scrollY);
      await page.waitForTimeout(300);
      assert.equal(await page.evaluate(() => window.scrollY), still, 'scrolling stopped with the drag');
      record.autoscroll = {scrolled, target, end, limit};
      await tap(page.locator('#layout-cancel'));
      await page.waitForSelector('#view-perform .editing-layout', {state: 'detached'});
    }

    // Delete the Tiles layout: two steps, then the built-in layout.
    await tap(page.locator('#edit-layout'));
    await page.waitForSelector('#view-perform .editing-layout');
    await tap(page.locator('#layout-delete'));
    await tap(page.locator('#layout-delete-confirm'));
    await page.waitForSelector('#view-perform .custom-layout', {state: 'detached'});
    named = await page.evaluate(() => JSON.parse(localStorage.getItem('tolglow.studio.panel-layouts')).classes);
    assert.deepEqual(named[viewport.cls].layouts.map(l => l.name), ['Layout 1']);
    assert.equal(named[viewport.cls].active, null);

    // The Mac app's window binding (#669): a host copy of the layouts is used
    // when it is newer than the page's, and saving also posts a host copy.
    if (viewport.name === 'desktop') {
      const hostCopy = JSON.stringify({version: 2, savedAt: Date.now() + 60000, classes: {desktop: {active: 'From the app',
        layouts: [{name: 'From the app', panels: [{id: 'room', w: 12, h: 20}, {id: 'cues', w: 6, h: 20}]}]}}});
      const host = await context.newPage();
      host.on('pageerror', e => errors.push('host page: ' + e.message));
      await host.addInitScript(copy => {
        window.__posted = [];
        window.__keep = true;
        window.tolglowStudioWindow = Object.freeze({runtimeId: '', show: 'qa', layouts: copy});
        // Like StudioPage.swift: keep the copy (or refuse it) and answer.
        window.webkit = {messageHandlers: {tolglowStudioWindow: {postMessage: m => {
          window.__posted.push(JSON.parse(JSON.stringify(m)));
          if (m.type === 'layouts' && window.__keep !== 'silent') setTimeout(() => window.dispatchEvent(new CustomEvent('tolglow-studio-window',
            {detail: {type: 'layouts', request: m.request, kept: window.__keep}})), 20);
        }}}};
      }, hostCopy);
      await host.goto(origin + '/', {waitUntil: 'domcontentloaded'});
      await host.waitForSelector('#view-perform .custom-layout', {timeout: 60000});
      const hostOrder = await host.evaluate(() => [...document.querySelectorAll('#view-perform .performance-layout > .layout-region')].map(e => e.dataset.panel));
      assert.deepEqual(hostOrder, ['room', 'cues', 'live', 'tools'], 'the newer app copy is used');
      await host.locator('#edit-layout').click();
      await host.waitForSelector('#view-perform .editing-layout');
      await host.locator('#layout-done').click();
      await host.waitForSelector('#view-perform .editing-layout', {state: 'detached'});
      const posted = await host.evaluate(() => window.__posted.filter(m => m.type === 'layouts'));
      assert.equal(posted.length, 1, 'saving posts one host copy');
      assert.equal(JSON.parse(posted[0].layouts).classes.desktop.active, 'From the app');
      assert.equal(typeof posted[0].request, 'number', 'each copy carries a request the app answers');
      await host.waitForFunction(() => document.body.textContent.includes('Saved “From the app”'), null, {timeout: 5000});
      // The app refuses a copy over its limit, or does not answer: the save is
      // not reported as kept, and the operator is warned.
      for (const answer of [false, 'silent']) {
        await host.waitForSelector('#toast', {state: 'detached', timeout: 10000});
        await host.evaluate(value => { window.__keep = value; }, answer);
        await host.locator('#edit-layout').click();
        await host.waitForSelector('#view-perform .editing-layout');
        const saved = Date.now();
        await host.locator('#layout-done').click();
        await host.waitForSelector('#toast.error', {timeout: 6000});
        const warning = await host.locator('#toast').textContent();
        assert.match(warning, /did not keep a copy of this layout/, `a warning when the app answers ${answer}`);
        if (answer === 'silent') assert.ok(Date.now() - saved >= 2900, 'with no answer the warning waits for the reply time');
      }
      await host.waitForSelector('#toast', {state: 'detached', timeout: 10000});
      await host.evaluate(() => { window.__keep = true; });
      // The app hosts a window for every panel (RoomWindows.swift): Detach opens
      // it, identifies it as that panel's, and the app's close report redocks it.
      await host.evaluate(() => {
        window.__identified = [];
        window.webkit.messageHandlers.tolglowRoomWindow = {postMessage: m => window.__identified.push(JSON.parse(JSON.stringify(m)))};
      });
      const hostPopups = [];
      host.on('popup', p => hostPopups.push(p));
      await host.locator('#edit-layout').click();
      await host.waitForSelector('#view-perform .editing-layout');
      await host.locator('#view-perform [data-panel="live"] .layout-detach').click();
      await host.waitForSelector('#view-perform [data-detached="live"]', {timeout: 5000});
      await host.waitForFunction(() => window.__identified.length > 0, null, {timeout: 5000});
      assert.equal(hostPopups.length, 1, 'Detach opens a window for Live in the Mac app');
      assert.deepEqual(await host.evaluate(() => window.__identified), [{action: 'identify', role: 'panel', id: 'live'}]);
      await hostPopups[0].waitForFunction(() => document.querySelector('.panel-window-content[data-panel=live]'), null, {timeout: 5000});
      // A Room close report or another panel's does not redock Live.
      await host.evaluate(() => {
        for (const detail of [{status: 'closed', role: 'room'}, {status: 'closed', role: 'panel', id: 'cues'}, {status: 'closed'}])
          window.dispatchEvent(new CustomEvent('tolglow-room-window', {detail}));
      });
      await host.waitForTimeout(300);
      assert.equal(await host.locator('#view-perform [data-detached="live"]').count(), 1, 'other windows closing leave Live detached');
      await host.evaluate(() => window.dispatchEvent(new CustomEvent('tolglow-room-window', {detail: {status: 'closed', role: 'panel', id: 'live'}})));
      await host.waitForSelector('#view-perform [data-detached="live"]', {state: 'detached', timeout: 5000});
      await host.locator('#layout-cancel').click();
      record.hostStore = {usedHostCopy: true, posted: posted.length, refusedWarns: true, panelWindowHosted: true};
      await host.close();
    }

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
