// Pop the Room out of Studio, play a cue in the main window, move the Room
// window between emulated displays and redock it, counting preview workers and
// WebGL contexts at every step.
//
//   node dev/studio/room-pop-out-qa.mjs http://127.0.0.1:4290 target/qa/room-pop-out
//
// Run against a production build served by `lein run -- --port <port>` (the
// default preview output is a capture sink). Uses headless Chrome with the
// real GPU. Displays come from CDP Emulation.addScreen/removeScreen and the
// window-management permission from Browser.setPermission, so this is not
// evidence of physical displays, hot-plugging, Safari or the packaged app.
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
  await page.locator('#room-window-placeholder').screenshot({path: join(directory, '03-studio-placeholder.png')});
  await room.screenshot({path: join(directory, '04-room-window.png')});

  // Change a cue in Studio and watch the Room window receive it.
  const roomState = () => room.evaluate(() => {
    const s = document.querySelector('#preview').tolglowPreview.snapshot;
    return {revision: s?.revision, frame: s?.['frame-number'], output: JSON.stringify(s?.output?.values ?? null)};
  });
  const before = await roomState();
  const instancesBefore = await page.locator('[data-live-instance]').count();
  const cue = page.locator('#cue-grid [data-cue-id]').first();
  const cueId = await cue.getAttribute('data-cue-id');
  await cue.click();
  await page.waitForFunction(n => document.querySelectorAll('[data-live-instance]').length > n, instancesBefore, {timeout: 10000});
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
    oneAction: results.oneAction, pip: results.pipPrepared, blocked: results.blocked, errors}, null, 2));
  await context.close();
  await rm(profile, {recursive: true, force: true});
}
