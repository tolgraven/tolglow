// Pop the Room out of Studio, play a cue in the main window, move the Room
// window between emulated displays and redock it, counting preview workers and
// WebGL contexts at every step. Later steps lose the Room's own display while
// two remain, keep the Room following the show while Studio is hidden
// (emulated), and share video from the popped-out Room to a recorded receiver.
//
//   node dev/studio/room-pop-out-qa.mjs http://127.0.0.1:4290 target/qa/room-pop-out
//
// Run against a production build served by `lein run -- --port <port>` (the
// default preview output is a capture sink). Uses headless Chrome with the
// real GPU. Displays come from CDP Emulation.addScreen/removeScreen and the
// window-management permission from Browser.setPermission, so this is not
// evidence of physical displays, hot-plugging, Safari or the packaged app.
// Headless Chrome cannot hide a page, so a hidden Studio is emulated: its
// document reports hidden and its animation frames are held. Sharing runs the
// real encoder on the Room's canvas, but the receiver endpoints under
// /api/preview/airplay/ are answered by this script, so no LAN listener is
// bound and no AirPlay receiver is involved.
import assert from 'node:assert/strict';
import {mkdir, rm, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin = 'http://127.0.0.1:4290', directory = 'target/qa/room-pop-out'] = process.argv.slice(2);
const profile = join(directory, 'profile');
await rm(profile, {recursive: true, force: true});
await mkdir(directory, {recursive: true});

// Count graphics owners from outside the application: every Worker and every
// WebGL context, in Studio and in any window it opens. Module workers start
// from a blob that counts WebGL contexts before importing the real worker.
function instrument() {
  const wrap = win => {
    if (win.__room) return;
    const room = win.__room = {workers: [], canvases: []};
    const NativeWorker = win.Worker;
    const counter = 'self.__webgl=0;const g=OffscreenCanvas.prototype.getContext;' +
      'OffscreenCanvas.prototype.getContext=function(t,...a){const c=g.call(this,t,...a);if(c&&/webgl/.test(t))self.__webgl++;return c};';
    win.Worker = function(url, options) {
      const href = new URL(url, win.location.href).href;
      const source = options?.type === 'module'
        ? `import ${JSON.stringify('data:text/javascript,' + encodeURIComponent(counter))};import ${JSON.stringify(href)};`
        : counter + `importScripts(${JSON.stringify(href)});`;
      const blob = win.URL.createObjectURL(new win.Blob([source], {type: 'text/javascript'}));
      const worker = new NativeWorker(blob, options);
      const record = {href, name: options?.name, terminated: false};
      room.workers.push(record);
      const terminate = worker.terminate.bind(worker);
      worker.terminate = () => {record.terminated = true; terminate();};
      return worker;
    };
    const getContext = win.HTMLCanvasElement.prototype.getContext;
    win.HTMLCanvasElement.prototype.getContext = function(type, ...rest) {
      const context = getContext.call(this, type, ...rest);
      if (context && /webgl/.test(type) && !room.canvases.includes(this)) room.canvases.push(this);
      return context;
    };
    const open = win.open;
    win.open = function(...args) {
      if (win.__blockPopups) return null;
      const popup = open.apply(this, args);
      try { if (popup) wrap(popup); } catch { /* cross-origin */ }
      return popup;
    };
  };
  wrap(window);
  // Emulated hidden Studio: hold its animation frames (Reagent schedules on
  // them) and report the document hidden, as a background window would.
  const frame = window.requestAnimationFrame.bind(window);
  const held = [];
  window.requestAnimationFrame = callback => {
    if (!window.__holdFrames) return frame(callback);
    held.push(callback);
    return 0;
  };
  window.__releaseFrames = () => { window.__holdFrames = false; held.splice(0).forEach(frame); };
  const hidden = Object.getOwnPropertyDescriptor(Document.prototype, 'hidden');
  Object.defineProperty(document, 'hidden', {configurable: true,
    get() { return window.__emulateHidden === true || hidden.get.call(this); }});
}

const context = await chromium.launchPersistentContext(profile, {
  executablePath: process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  headless: true,
  args: ['--headless=new', '--use-angle=metal', '--enable-gpu', '--ignore-gpu-blocklist'],
  viewport: {width: 1280, height: 900},
});
const results = {origin, steps: []};
const errors = [];
let page;
let roomPage = null;
try {
  for (const blank of context.pages()) await blank.close();
  page = await context.newPage();
  page.on('popup', p => { roomPage = p; p.on('close', () => { if (roomPage === p) roomPage = null; }); });
  const cdp = await context.newCDPSession(page);
  await page.addInitScript(instrument);
  const sockets = [];
  context.on('page', p => p.on('websocket', s => sockets.push({url: s.url(), page: p === page ? 'studio' : 'room'})));
  page.on('websocket', s => sockets.push({url: s.url(), page: 'studio'}));
  page.on('pageerror', e => errors.push('studio: ' + e.message));
  const permission = setting => cdp.send('Browser.setPermission', {permission: {name: 'window-management'}, setting, origin});
  await permission('prompt');

  // Live graphics owners: CDP-attached workers, plus the in-page records.
  async function census(label) {
    const pages = [page, roomPage].filter(p => p && !p.isClosed());
    const workers = [];
    for (const p of pages) {
      for (const w of p.workers()) {
        if (!/blob:/.test(w.url()) && !/preview-worker/.test(w.url())) continue;
        let webgl = null;
        try { webgl = await w.evaluate(() => self.__webgl ?? null); } catch { /* closing */ }
        workers.push({page: p === page ? 'studio' : 'room', url: w.url(), webgl});
      }
    }
    const targets = (await cdp.send('Target.getTargets')).targetInfos
      .filter(t => t.type === 'worker' || t.type === 'page').map(t => ({type: t.type, url: t.url}));
    const records = await page.evaluate(() => {
      const docs = [window, ...[...(window.__room?.popups || [])]];
      const room = window.__room;
      const own = room.workers.filter(w => !w.terminated).map(w => w.href);
      const canvases = room.canvases.filter(c => c.isConnected).length;
      return {created: room.workers.length, terminatedByStudio: room.workers.filter(w => w.terminated).length,
        studioWorkers: own, studioWebglCanvases: canvases,
        studioCanvas: !!document.querySelector('#preview'),
        placeholder: !!document.querySelector('#room-window-placeholder'), docs: docs.length};
    });
    const room = roomPage && !roomPage.isClosed() ? roomPage : null;
    const roomRecords = room ? await room.evaluate(() => ({
      created: window.__room?.workers.length ?? null,
      terminated: window.__room?.workers.filter(w => w.terminated).length ?? null,
      webglCanvases: window.__room?.canvases.filter(c => c.isConnected).length ?? null,
      canvas: !!document.querySelector('#preview'),
      thread: document.querySelector('#preview')?.tolglowPreview?.getTelemetry().thread ?? null,
      screenX, screenY, outerWidth, outerHeight,
    })) : null;
    const previewWorkers = workers.filter(w => /preview-worker/.test(w.url) || w.webgl !== null);
    const webglContexts = previewWorkers.reduce((n, w) => n + (w.webgl || 0), 0)
      + records.studioWebglCanvases + (roomRecords?.webglCanvases || 0);
    const entry = {label, previewWorkers: previewWorkers.length, webglContexts, workers, records, room: roomRecords,
      workerTargets: targets.filter(t => t.type === 'worker').length};
    results.steps.push(entry);
    console.log(label, JSON.stringify({previewWorkers: entry.previewWorkers, webglContexts, room: !!roomRecords,
      studioCanvas: records.studioCanvas, placeholder: records.placeholder, workerTargets: entry.workerTargets}));
    return entry;
  }

  const ready = async target => {
    await target.waitForFunction(() => document.querySelector('#preview')?.tolglowPreview?.ready === true, null, {timeout: 90000});
  };
  const notice = target => target.locator('.room-window-notice').first().textContent({timeout: 5000});
  const roomToolbar = () => page.locator('#preview-panel .preview-size-controls');

  await page.goto(origin + '/', {waitUntil: 'domcontentloaded'});
  await page.waitForFunction(() => document.querySelector('#connection-label')?.textContent === 'Studio connected', null, {timeout: 60000});
  await page.locator('#preview-panel').scrollIntoViewIfNeeded();
  await ready(page);
  await page.waitForTimeout(1000);
  const docked = await census('docked');
  assert.equal(docked.previewWorkers, 1, 'one preview worker before pop-out');
  assert.equal(docked.webglContexts, 1, 'one WebGL context before pop-out');
  await roomToolbar().screenshot({path: join(directory, '01-room-toolbar.png')});
  await page.locator('#preview-panel').screenshot({path: join(directory, '02-docked-room.png')});

  // A PiP capture prepared in Studio ends when the Room leaves: its video and
  // canvas belong to the docked panel.
  let dockedCapture = null;
  const prepare = page.getByRole('button', {name: 'Prepare live preview for Picture in Picture'});
  if (await prepare.count()) {
    await prepare.click();
    await page.getByRole('button', {name: 'Open Picture in Picture'}).waitFor({timeout: 10000});
    dockedCapture = await page.evaluateHandle(() => document.querySelector('#preview-panel video').srcObject);
    results.dockedPip = {before: await dockedCapture.evaluate(s => s.getTracks().map(t => t.readyState))};
  }

  // Pop out.
  const popup = page.waitForEvent('popup');
  await page.getByRole('button', {name: 'Open the Room in its own window'}).click();
  const room = await popup;
  room.on('pageerror', e => errors.push('room: ' + e.message));
  await room.setViewportSize({width: 960, height: 640});
  await ready(room);
  await page.waitForSelector('#room-window-placeholder');
  await room.waitForTimeout(1000);
  const popped = await census('popped');
  assert.equal(popped.previewWorkers, 1, 'one preview worker while popped out');
  assert.equal(popped.webglContexts, 1, 'one WebGL context while popped out');
  assert.equal(popped.records.studioCanvas, false, 'Studio holds no Room canvas while popped out');
  assert.equal(popped.room.thread, 'worker');
  if (dockedCapture) {
    results.dockedPip.afterPopOut = await dockedCapture.evaluate(s => s.getTracks().map(t => t.readyState));
    assert.deepEqual(results.dockedPip.afterPopOut, ['ended'], 'the docked PiP capture stops when the Room pops out');
    await dockedCapture.dispose();
  }
  await page.locator('#room-window-placeholder').screenshot({path: join(directory, '03-studio-placeholder.png')});
  await room.screenshot({path: join(directory, '04-room-window.png')});

  // Change a cue in Studio and watch the Room window receive it.
  const roomState = () => room.evaluate(() => {
    const s = document.querySelector('#preview').tolglowPreview.snapshot;
    return {revision: s?.revision, frame: s?.['frame-number'], output: JSON.stringify(s?.output?.values ?? null)};
  });
  const before = await roomState();
  const instancesBefore = await page.locator('[data-live-instance]:not(.releasing)').count();
  const cue = page.locator('#cue-grid [data-cue-id]').first();
  const cueId = await cue.getAttribute('data-cue-id');
  await cue.click();
  // The cue toggles: it starts, or stops when an earlier run left it running.
  await page.waitForFunction(n => document.querySelectorAll('[data-live-instance]:not(.releasing)').length !== n, instancesBefore, {timeout: 10000});
  await room.waitForFunction(previous => {
    const s = document.querySelector('#preview').tolglowPreview.snapshot;
    return JSON.stringify(s?.output?.values ?? null) !== previous;
  }, before.output, {timeout: 10000});
  const after = await roomState();
  results.cue = {cueId, before: {revision: before.revision, frame: before.frame}, after: {revision: after.revision, frame: after.frame},
    outputChanged: before.output !== after.output};
  console.log('cue', JSON.stringify(results.cue));
  await room.waitForTimeout(1500);
  await room.screenshot({path: join(directory, '05-room-after-cue.png')});
  await census('popped-after-cue');

  // One display: no permission prompt, explain manual placement.
  const move = room.getByRole('button', {name: 'Send the Room window to another display'});
  await move.click();
  results.single = await notice(room);
  assert.match(results.single, /Only one display/);

  // Two displays, permission denied.
  const second = (await cdp.send('Emulation.addScreen', {left: 1920, top: 0, width: 1920, height: 1080, label: 'Emulated projector'})).screenInfo;
  await room.waitForFunction(() => /Another display is connected/.test(document.querySelector('.room-window-notice')?.textContent || ''), null, {timeout: 5000})
    .then(() => { results.connected = true; }, () => { results.connected = false; });
  await permission('denied');
  await move.click();
  await room.waitForFunction(() => /denied/.test(document.querySelector('.room-window-notice')?.textContent || ''), null, {timeout: 5000});
  results.denied = await notice(room);

  // Granted: the window moves to the other display in one action.
  await permission('granted');
  await move.click();
  await room.waitForFunction(() => /The Room is on/.test(document.querySelector('.room-window-notice')?.textContent || ''), null, {timeout: 5000});
  results.placed = {notice: await notice(room), ...(await room.evaluate(() => ({screenX, screenY, outerWidth, outerHeight})))};
  assert.ok(results.placed.screenX >= second.left, 'the Room window is on the second display');
  await census('placed');
  await room.screenshot({path: join(directory, '06-room-on-second-display.png')});

  // Unplug and reconnect the emulated display.
  await cdp.send('Emulation.removeScreen', {screenId: second.id});
  await room.waitForFunction(() => /disconnected/.test(document.querySelector('.room-window-notice')?.textContent || ''), null, {timeout: 5000});
  results.unplugged = {notice: await notice(room), ...(await room.evaluate(() => ({screenX, screenY})))};
  const again = (await cdp.send('Emulation.addScreen', {left: 1920, top: 0, width: 1920, height: 1080, label: 'Emulated projector'})).screenInfo;
  await room.waitForFunction(() => /Another display is connected/.test(document.querySelector('.room-window-notice')?.textContent || ''), null, {timeout: 5000});
  results.reconnected = await notice(room);
  await move.click();
  await room.waitForFunction(() => /The Room is on/.test(document.querySelector('.room-window-notice')?.textContent || ''), null, {timeout: 5000});
  results.replaced = await room.evaluate(() => ({screenX, screenY}));
  await page.screenshot({path: join(directory, '07-studio-while-popped.png')});

  // Redock from the Room window.
  const closed = room.waitForEvent('close');
  await room.getByRole('button', {name: 'Return the Room to Studio'}).click();
  await closed;
  await ready(page);
  await page.waitForTimeout(1000);
  const redocked = await census('redocked');
  assert.equal(redocked.previewWorkers, 1, 'one preview worker after redock');
  assert.equal(redocked.webglContexts, 1, 'one WebGL context after redock');
  assert.equal(redocked.room, null);
  await page.locator('#preview-panel').screenshot({path: join(directory, '08-redocked-room.png')});

  // Closing the Room window redocks too.
  const popup2 = page.waitForEvent('popup');
  await page.getByRole('button', {name: 'Open the Room in its own window'}).click();
  const room2 = await popup2;
  await ready(room2);
  await census('popped-again');
  await room2.close();
  await page.waitForSelector('#preview-panel #preview');
  await ready(page);
  await page.waitForTimeout(1000);
  const afterClose = await census('closed-window');
  assert.equal(afterClose.previewWorkers, 1, 'one preview worker after the window was closed');
  assert.equal(afterClose.webglContexts, 1, 'one WebGL context after the window was closed');

  // Reloading the Room window ends it: Studio redocks and closes the window
  // instead of leaving an empty page behind.
  const popupReload = page.waitForEvent('popup');
  await page.getByRole('button', {name: 'Open the Room in its own window'}).click();
  const roomReload = await popupReload;
  await ready(roomReload);
  const reloadClosed = roomReload.waitForEvent('close', {timeout: 5000}).then(() => true, () => false);
  await roomReload.reload().catch(() => {});
  await page.waitForSelector('#preview-panel #preview');
  results.reloadedWindowClosed = await reloadClosed;
  assert.ok(results.reloadedWindowClosed, 'a reloaded Room window closes instead of staying empty');
  await ready(page);
  await page.waitForTimeout(1000);
  const afterReload = await census('reloaded-window');
  assert.equal(afterReload.previewWorkers, 1, 'one preview worker after the Room window reloaded');
  assert.equal(afterReload.webglContexts, 1, 'one WebGL context after the Room window reloaded');

  // Move to display from Studio pops out and places in one action.
  const popup3 = page.waitForEvent('popup');
  await page.getByRole('button', {name: 'Send the Room window to another display'}).click();
  const room3 = await popup3;
  await room3.waitForFunction(() => /The Room is on/.test(document.querySelector('.room-window-notice')?.textContent || ''), null, {timeout: 15000});
  results.oneAction = await room3.evaluate(() => ({screenX, screenY, outerWidth, outerHeight}));
  assert.ok(results.oneAction.screenX >= again.left);
  await ready(room3);
  await census('one-action');

  // PiP capture prepared in the Room window ends when the Room moves back.
  const pip = room3.getByRole('button', {name: 'Prepare live preview for Picture in Picture'});
  if (await pip.count()) {
    await pip.click();
    await room3.getByRole('button', {name: 'Open Picture in Picture'}).waitFor({timeout: 10000});
    results.pipPrepared = await room3.evaluate(() => document.querySelector('video')?.srcObject?.getTracks().map(t => t.readyState));
    const tracks = await room3.evaluateHandle(() => document.querySelector('video').srcObject);
    await room3.getByRole('button', {name: 'Open Picture in Picture'}).click();
    results.pipOpened = await room3.waitForFunction(() => document.pictureInPictureElement?.tagName ?? false, null, {timeout: 3000})
      .then(h => h.jsonValue(), error => 'not opened: ' + error.message.split('\n')[0]);
    const closed3 = room3.waitForEvent('close');
    await room3.getByRole('button', {name: 'Return the Room to Studio'}).click();
    await closed3;
    results.pipAfterRedock = 'room window closed with its capture';
    await tracks.dispose().catch(() => {});
  } else {
    results.pipPrepared = 'PiP unavailable in this browser';
    await room3.getByRole('button', {name: 'Return the Room to Studio'}).click();
  }
  await ready(page);
  await page.waitForTimeout(1000);
  const final = await census('final');
  assert.equal(final.previewWorkers, 1);
  assert.equal(final.webglContexts, 1);

  // Reloading Studio while popped out closes the Room window with it.
  const popupStudioReload = page.waitForEvent('popup');
  await page.getByRole('button', {name: 'Open the Room in its own window'}).click();
  const roomStudioReload = await popupStudioReload;
  await ready(roomStudioReload);
  const studioReloadClosed = roomStudioReload.waitForEvent('close', {timeout: 10000}).then(() => true, () => false);
  await page.reload({waitUntil: 'domcontentloaded'});
  await page.waitForFunction(() => document.querySelector('#connection-label')?.textContent === 'Studio connected', null, {timeout: 60000});
  await page.locator('#preview-panel').scrollIntoViewIfNeeded();
  await ready(page);
  results.studioReloadClosedWindow = await studioReloadClosed;
  assert.ok(results.studioReloadClosedWindow, 'the Room window closes when Studio reloads');
  await page.waitForTimeout(1000);
  const afterStudioReload = await census('studio-reloaded');
  assert.equal(afterStudioReload.previewWorkers, 1, 'one preview worker after Studio reloaded');
  assert.equal(afterStudioReload.webglContexts, 1, 'one WebGL context after Studio reloaded');

  // Three displays: losing the display the Room is on is noticed even though
  // two displays remain, and Move to display then uses the other free one.
  const third = (await cdp.send('Emulation.addScreen', {left: 3840, top: 0, width: 1920, height: 1080, label: 'Emulated projector B'})).screenInfo;
  const popup5 = page.waitForEvent('popup');
  await page.getByRole('button', {name: 'Send the Room window to another display'}).click();
  const room5 = await popup5;
  room5.on('pageerror', e => errors.push('room: ' + e.message));
  await room5.waitForFunction(() => /The Room is on/.test(document.querySelector('.room-window-notice')?.textContent || ''), null, {timeout: 15000});
  await ready(room5);
  const firstPlace = await room5.evaluate(() => ({screenX, screenY}));
  const placedOn = firstPlace.screenX >= third.left ? third : again;
  const other = placedOn === third ? again : third;
  await cdp.send('Emulation.removeScreen', {screenId: placedOn.id});
  await room5.waitForFunction(() => /disconnected/.test(document.querySelector('.room-window-notice')?.textContent || ''), null, {timeout: 5000});
  results.ownDisplayLost = {placedAt: firstPlace, removed: placedOn.label, notice: await notice(room5)};
  const move5 = room5.getByRole('button', {name: 'Send the Room window to another display'});
  await move5.click();
  await room5.waitForFunction(() => /The Room is on/.test(document.querySelector('.room-window-notice')?.textContent || ''), null, {timeout: 5000});
  results.ownDisplayLost.replaced = await room5.evaluate(() => ({screenX, screenY}));
  assert.ok(results.ownDisplayLost.replaced.screenX >= other.left && results.ownDisplayLost.replaced.screenX < other.left + other.width,
    'Move to display uses the remaining free display');
  await census('own-display-lost');

  // Hidden Studio (emulated): Reagent's frames stop, so the Room window
  // flushes renders from its own frames and keeps following the show: its
  // committed frame advances and a cue played from Studio reaches it. Show
  // frames reach the engine on the :room frame lane without a Reagent render
  // (#783), so only the cue's structural revision depends on that flush.
  const roomShow = () => room5.evaluate(() => {
    const s = document.querySelector('#preview').tolglowPreview.snapshot;
    return {revision: s?.revision, frame: s?.['frame-number']};
  });
  const cueButton = page.locator(`#cue-grid [data-cue-id="${cueId}"]`);
  await page.evaluate(() => { window.__holdFrames = true; window.__emulateHidden = false; });
  await room5.waitForTimeout(300);
  const heldStart = await roomShow();
  await room5.waitForTimeout(1500);
  const heldEnd = await roomShow();
  // Control: the cue's revision waits for a render while Studio is visible
  // with its renders held, then the hidden Studio's Room window flushes it.
  await cueButton.click({force: true});
  await room5.waitForTimeout(1500);
  const heldCue = await roomShow();
  await room5.waitForTimeout(1000);
  const heldCueLater = await roomShow();
  await page.evaluate(() => { window.__emulateHidden = true; });
  const hiddenStart = heldCue;
  const cueReached = await room5.waitForFunction(previous => document.querySelector('#preview').tolglowPreview.snapshot?.revision !== previous,
    hiddenStart.revision, {timeout: 10000}).then(() => true, () => false);
  await room5.waitForTimeout(500);
  const hiddenEnd = await roomShow();
  await page.evaluate(() => { window.__emulateHidden = false; window.__releaseFrames(); });
  results.hiddenStudio = {heldVisible: {start: heldStart, end: heldEnd, afterCue: heldCue, afterCueLater: heldCueLater},
    hidden: {start: hiddenStart, end: hiddenEnd}, cueReached};
  console.log('hidden-studio', JSON.stringify(results.hiddenStudio));
  assert.ok(heldEnd.frame > heldStart.frame, 'show frames reach the Room on its frame lane while Studio renders are held');
  assert.equal(heldCue.revision, heldStart.revision, 'control: with Studio renders held and Studio visible, a cue revision does not reach the Room');
  assert.equal(heldCueLater.frame, heldCue.frame, 'control: frames of the new revision wait at the fence until a render');
  assert.ok(hiddenEnd.frame > hiddenStart.frame, 'the Room keeps advancing while Studio is hidden');
  assert.ok(cueReached, 'a cue played in Studio reaches the Room while Studio is hidden');

  // Share video from the popped-out Room. The receiver endpoints are recorded
  // here: the real encoder runs on the Room canvas and uploads HLS segments.
  const receiver = {starts: [], stops: [], uploads: []};
  const transit = 'application/transit+json';
  await page.route(/\/api\/preview\/airplay\//, async route => {
    const request = route.request();
    const path = new URL(request.url()).pathname.replace('/api/preview/airplay/', '');
    const reply = body => route.fulfill({status: 200, contentType: transit, body: JSON.stringify(body)});
    if (path === 'interfaces') return reply([['^ ', '~:address', '192.0.2.10', '~:interface', 'qa0']]);
    if (path === 'start') {
      const writer = 'writer-' + (receiver.starts.length + 1);
      receiver.starts.push({writer, at: Date.now()});
      return reply(['^ ', '~:writer', writer, '~:url', 'http://192.0.2.10:9/cap/master.m3u8',
        '~:playback-path', '/api/preview/airplay/watch/cap/master.m3u8']);
    }
    if (path === 'stop') {
      receiver.stops.push({body: request.postData(), at: Date.now()});
      return reply(['^ ', '~:status', '~:closed']);
    }
    const [writer, file] = path.split('/');
    if (request.method() === 'PUT' && file) {
      receiver.uploads.push({writer, file, bytes: (request.postDataBuffer() || []).length, at: Date.now()});
      return route.fulfill({status: 204, body: ''});
    }
    return route.fulfill({status: 404, body: ''});
  });
  const uploadsBy = writer => receiver.uploads.filter(u => u.writer === writer && /segment/.test(u.file)).length;
  const waitUploads = async (writer, count, timeout = 20000) => {
    const deadline = Date.now() + timeout;
    while (uploadsBy(writer) < count) {
      if (Date.now() > deadline) throw new Error(`expected ${count} segments from ${writer}, saw ${uploadsBy(writer)}`);
      await room5.waitForTimeout(250);
    }
  };
  const share = async () => {
    await room5.getByRole('button', {name: 'Share video'}).click();
    await room5.getByRole('button', {name: 'Start sharing'}).click();
    await room5.getByRole('button', {name: 'Stop sharing'}).waitFor({timeout: 30000});
  };
  await share();
  await waitUploads('writer-1', 2);
  results.sharing = {size: await room5.locator('#preview-panel a.text-button').first().textContent()};
  await room5.getByRole('button', {name: 'Stop sharing'}).click();
  await room5.getByRole('button', {name: 'Share video'}).waitFor({timeout: 5000});
  const stoppedAt = Date.now();
  await room5.waitForTimeout(2500);
  results.sharing.stop = {stops: receiver.stops.length, stopBody: receiver.stops[0]?.body,
    segments: uploadsBy('writer-1'), afterStop: receiver.uploads.filter(u => u.writer === 'writer-1' && u.at > stoppedAt + 500).length};
  assert.equal(receiver.stops.length, 1, 'Stop sharing ends the receiver session');
  assert.match(receiver.stops[0].body, /writer-1/);
  assert.equal(results.sharing.stop.afterStop, 0, 'no segments arrive after Stop sharing');

  // Changing destination while sharing: moving the Room window to another
  // display keeps one encoder and one session.
  await share();
  await waitUploads('writer-2', 2);
  const readd = (await cdp.send('Emulation.addScreen', {left: placedOn.left, top: 0, width: 1920, height: 1080, label: placedOn.label})).screenInfo;
  await room5.waitForTimeout(500);
  const beforeMove = uploadsBy('writer-2');
  await move5.click();
  // The notice already reads "The Room is on …", so wait for the window itself.
  await room5.waitForFunction(x => screenX !== x, results.ownDisplayLost.replaced.screenX, {timeout: 5000});
  await room5.waitForFunction(() => /The Room is on/.test(document.querySelector('.room-window-notice')?.textContent || ''), null, {timeout: 5000});
  await waitUploads('writer-2', beforeMove + 2);
  results.sharing.moved = {at: await room5.evaluate(() => ({screenX, screenY})), starts: receiver.starts.length,
    segmentsBefore: beforeMove, segmentsAfter: uploadsBy('writer-2')};
  assert.equal(receiver.starts.length, 2, 'moving the window starts no second session');
  await census('sharing-moved');

  // Redocking ends the share: the capture belonged to the Room window's canvas.
  const closed5 = room5.waitForEvent('close');
  await page.getByRole('button', {name: 'Return the Room to Studio'}).click();
  await closed5;
  const redockedAt = Date.now();
  await page.waitForFunction(() => document.querySelector('#preview-panel')?.textContent.includes('Share video'), null, {timeout: 10000});
  await ready(page);
  await page.waitForTimeout(3000);
  results.sharing.redock = {stops: receiver.stops.length, stopBody: receiver.stops[1]?.body,
    afterRedock: receiver.uploads.filter(u => u.at > redockedAt + 500).length, starts: receiver.starts.length};
  assert.equal(receiver.stops.length, 2, 'redocking ends the receiver session');
  assert.match(receiver.stops[1].body, /writer-2/);
  assert.equal(results.sharing.redock.afterRedock, 0, 'no encoder keeps uploading after redock');
  const afterShare = await census('shared-and-redocked');
  assert.equal(afterShare.previewWorkers, 1, 'one preview worker after sharing and redocking');
  assert.equal(afterShare.webglContexts, 1, 'one WebGL context after sharing and redocking');
  results.receiver = {starts: receiver.starts, stops: receiver.stops.length,
    uploads: receiver.uploads.length, files: [...new Set(receiver.uploads.map(u => u.file.replace(/-\d+\.ts$/, '-N.ts')))]};
  await page.unroute(/\/api\/preview\/airplay\//);
  await cdp.send('Emulation.removeScreen', {screenId: readd.id});
  await cdp.send('Emulation.removeScreen', {screenId: other.id});

  // Pop-up blocked: the browser returns no window.
  await page.evaluate(() => { window.__blockPopups = true; });
  await page.getByRole('button', {name: 'Open the Room in its own window'}).click();
  await page.waitForFunction(() => /blocked the Room window/.test(document.querySelector('#toast')?.textContent || ''), null, {timeout: 5000});
  results.blocked = await page.locator('#toast').textContent();
  assert.ok(await page.locator('#preview-panel #preview').count(), 'the Room stays docked when the window is blocked');
  await page.evaluate(() => { window.__blockPopups = false; });

  results.stateSockets = sockets.filter(s => s.url.includes('/state'));
  assert.equal(results.stateSockets.filter(s => s.page === 'room').length, 0, 'the Room window opens no show socket');
  results.errors = errors;
  assert.deepEqual(errors, []);
  results.pass = true;
} catch (error) {
  results.pass = false;
  results.failure = String(error.stack || error);
  results.errors = errors;
  if (page && !page.isClosed()) await page.screenshot({path: join(directory, 'failure.png')}).catch(() => {});
  process.exitCode = 1;
} finally {
  await writeFile(join(directory, 'result.json'), JSON.stringify(results, null, 2));
  console.log(JSON.stringify({pass: results.pass, failure: results.failure, cue: results.cue, single: results.single,
    denied: results.denied, placed: results.placed, unplugged: results.unplugged, reconnected: results.reconnected,
    oneAction: results.oneAction, pip: results.pipPrepared, pipOpened: results.pipOpened, dockedPip: results.dockedPip,
    ownDisplayLost: results.ownDisplayLost, hiddenStudio: results.hiddenStudio, sharing: results.sharing,
    receiver: results.receiver, blocked: results.blocked, errors}, null, 2));
  await context.close();
  await rm(profile, {recursive: true, force: true});
}
