// Views of one show in tabs, and windows bound to their show (#669).
//
//   node dev/studio/view-tabs-qa.mjs http://127.0.0.1:4661 http://127.0.0.1:4662 target/qa/view-tabs [http://127.0.0.1:4663]
//
// The optional fourth origin is a show started preview-only (as a second Mac
// window would be), whose device controls must be greyed and refused.
//
// Run against two shows served from a production build (two runtimes, each
// with a preview capture sink and recovery off), standing in for two Mac
// windows. Headless Chrome with the real GPU. The Mac app's menu items are
// simulated with the same `tolglow-view-tabs` events it sends, and its window
// binding with the same `window.tolglowStudioWindow` it injects; this is not
// evidence of the packaged app, Safari or real touch.
import assert from 'node:assert/strict';
import {mkdir, rm, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [originA = 'http://127.0.0.1:4661', originB = 'http://127.0.0.1:4662', directory = 'target/qa/view-tabs',
  originC = null] = process.argv.slice(2);
const profile = join(directory, 'profile');
await rm(profile, {recursive: true, force: true});
await mkdir(directory, {recursive: true});

// Count what a tab could start: workers, WebGL contexts and sockets.
function instrument() {
  const census = window.__census = {workers: 0, sockets: 0, webgl: 0, workerUrls: []};
  const NativeWorker = window.Worker;
  census.live = 0;
  window.Worker = function(url, ...rest) {
    census.workers++; census.live++; census.workerUrls.push(String(url).slice(0, 120));
    const worker = new NativeWorker(url, ...rest);
    const terminate = worker.terminate.bind(worker);
    worker.terminate = () => { census.live--; terminate(); };
    return worker;
  };
  window.Worker.prototype = NativeWorker.prototype;
  const NativeSocket = window.WebSocket;
  window.WebSocket = function(...args) { census.sockets++; return new NativeSocket(...args); };
  Object.assign(window.WebSocket, NativeSocket);
  window.WebSocket.prototype = NativeSocket.prototype;
  const getContext = HTMLCanvasElement.prototype.getContext;
  HTMLCanvasElement.prototype.getContext = function(type, ...rest) {
    const context = getContext.call(this, type, ...rest);
    if (context && /webgl/.test(type) && !this.__counted) { this.__counted = true; census.webgl++; }
    return context;
  };
}

const identity = async origin => (await fetch(origin + '/api/identity', {headers: {Accept: 'application/json'}})).json();
const state = async origin => (await fetch(origin + '/api/state', {headers: {Accept: 'application/json'}})).json();

const context = await chromium.launchPersistentContext(profile, {
  executablePath: process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  headless: true,
  args: ['--headless=new', '--use-angle=metal', '--enable-gpu', '--ignore-gpu-blocklist'],
  viewport: {width: 1440, height: 900},
});
const results = {originA, originB, steps: []};
const errors = [];
try {
  for (const blank of context.pages()) await blank.close();
  const [showA, showB] = [await identity(originA), await identity(originB)];
  assert.notEqual(showA['runtime-id'], showB['runtime-id']);
  results.runtimes = {a: showA['runtime-id'], b: showB['runtime-id']};

  const page = await context.newPage();
  await page.addInitScript(instrument);
  page.on('pageerror', e => errors.push(e.message));
  const posts = [];
  page.on('request', r => { if (r.method() === 'POST') posts.push(new URL(r.url()).pathname); });

  const connected = target => target.waitForFunction(
    () => document.querySelector('#connection-label')?.textContent === 'Studio connected', null, {timeout: 60000});
  const strip = async target => target.evaluate(() => ({
    tabs: [...document.querySelectorAll('#view-tabs .view-tab')].map(t => t.dataset.view),
    active: document.querySelector('#view-tabs .view-tab.selected')?.dataset.view,
    path: location.pathname,
    title: document.title,
    census: {...window.__census, workerUrls: [...window.__census.workerUrls]},
    previewCanvas: document.querySelector('#preview')?.__tabsMark ?? null,
  }));
  async function step(label, expected) {
    const live = baseline.census.live + (expected.active === 'rig' ? 1 : 0);
    await page.waitForFunction(([tabs, active, live]) => {
      const views = [...document.querySelectorAll('#view-tabs .view-tab')].map(t => t.dataset.view);
      return JSON.stringify(views) === JSON.stringify(tabs)
        && document.querySelector('#view-tabs .view-tab.selected')?.dataset.view === active
        && location.pathname === '/' + active && window.__census.live === live;
    }, [expected.tabs, expected.active, live], {timeout: 10000}).catch(() => {});
    const seen = await strip(page);
    results.steps.push({label, ...seen});
    console.log(label, JSON.stringify(seen));
    assert.deepEqual(seen.tabs, expected.tabs, label + ': tabs');
    assert.equal(seen.active, expected.active, label + ': active tab');
    assert.equal(seen.path, '/' + expected.active, label + ': the route follows the active tab');
    // Perform owns Studio's preview engine. The Rig view mounts its own
    // editor preview while it is shown (as it does without tabs) and ends it
    // when it is hidden; tabs add nothing of their own.
    const viewWorkers = expected.active === 'rig' ? 1 : 0;
    assert.equal(seen.census.live, baseline.census.live + viewWorkers, label + ': live workers are the view\'s own');
    assert.equal(seen.census.sockets, baseline.census.sockets, label + ': no new socket');
    assert.equal(seen.census.webgl, baseline.census.webgl, label + ': no new WebGL context');
    assert.equal(seen.previewCanvas, 'original', label + ': the same preview canvas');
    return seen;
  }

  await page.goto(originA + '/perform', {waitUntil: 'domcontentloaded'});
  await connected(page);
  await page.waitForFunction(() => document.querySelector('#preview')?.tolglowPreview?.ready === true, null, {timeout: 90000});
  await page.evaluate(() => { document.querySelector('#preview').__tabsMark = 'original'; });
  await page.waitForTimeout(500);
  const baseline = await strip(page);
  results.baseline = baseline;
  assert.deepEqual(baseline.tabs, ['perform']);
  assert.equal(baseline.census.sockets, 1, 'one state connection');
  assert.match(baseline.title, /^Perform · /);
  await page.screenshot({path: join(directory, 'tabs-one.png')});

  await page.click('#view-tab-new');
  await step('new tab', {tabs: ['perform', 'perform'], active: 'perform'});
  await page.click('.sidebar a[data-view="rig"]');
  const rig = await step('sidebar changes the active tab', {tabs: ['perform', 'rig'], active: 'rig'});
  assert.match(rig.title, /^Rig & space · /);
  await page.click('#view-tabs .view-tab[data-view="perform"]');
  await step('select first tab', {tabs: ['perform', 'rig'], active: 'perform'});
  await page.locator('body').focus();
  await page.keyboard.press('3');
  // Shortcut 3 is Rig; the first tab now shows Rig too. Put it back on Perform.
  await step('shortcut changes the active tab', {tabs: ['rig', 'rig'], active: 'rig'});
  await page.click('.sidebar a[data-view="perform"]');
  await step('back to perform', {tabs: ['perform', 'rig'], active: 'perform'});

  // The Mac app's menu items.
  const host = action => page.evaluate(a => window.dispatchEvent(new CustomEvent('tolglow-view-tabs', {detail: {action: a}})), action);
  await host('new');
  await step('menu: New Tab', {tabs: ['perform', 'perform', 'rig'], active: 'perform'});
  await page.click('.sidebar a[data-view="compose"]');
  await step('third tab on compose', {tabs: ['perform', 'compose', 'rig'], active: 'compose'});
  await host('next');
  await step('menu: Show Next Tab', {tabs: ['perform', 'compose', 'rig'], active: 'rig'});
  await host('next');
  await step('menu: next wraps', {tabs: ['perform', 'compose', 'rig'], active: 'perform'});
  await host('previous');
  await step('menu: Show Previous Tab', {tabs: ['perform', 'compose', 'rig'], active: 'rig'});
  await page.screenshot({path: join(directory, 'tabs-three.png')});
  const a11y = await page.evaluate(() => {
    const list = document.querySelector('#view-tabs [role=tablist]');
    const selected = list.querySelector('[role=tab][aria-selected=true]');
    const main = document.querySelector('main#main');
    return {
      children: [...list.children].map(c => c.getAttribute('role')),
      focusableInsideTabs: list.querySelectorAll('[role=tab] button, [role=tab] [tabindex], [role=tab] a').length,
      panel: [main.getAttribute('role'), main.getAttribute('aria-labelledby')],
      selectedId: selected.id,
      closeOutside: !!document.querySelector('#view-tab-close') && !list.contains(document.querySelector('#view-tab-close')),
      closeLabel: document.querySelector('#view-tab-close')?.getAttribute('aria-label'),
    };
  });
  results.a11y = a11y;
  assert.ok(a11y.children.every(r => r === 'tab'), 'the tablist holds only tabs');
  assert.equal(a11y.focusableInsideTabs, 0, 'no focusable control inside a tab');
  assert.deepEqual(a11y.panel, ['tabpanel', a11y.selectedId], 'main is the active tab panel');
  assert.ok(a11y.closeOutside, 'Close tab sits outside the tablist');
  assert.equal(a11y.closeLabel, 'Close Rig & space tab');

  // Close in different orders and reopen.
  await page.click('#view-tabs .view-tab[data-view="compose"] .view-tab-close');
  await step('close a background tab', {tabs: ['perform', 'rig'], active: 'rig'});
  await host('close');
  await step('menu: Close Tab closes the active tab', {tabs: ['perform'], active: 'perform'});
  assert.equal(await page.locator('.view-tab-close').count(), 0, 'the last tab has no close button');
  await host('close');
  await step('the last tab stays', {tabs: ['perform'], active: 'perform'});
  await page.click('#view-tab-reopen');
  await step('reopen the last closed view', {tabs: ['perform', 'rig'], active: 'rig'});
  await host('reopen');
  await step('menu: Reopen Closed Tab', {tabs: ['perform', 'rig', 'compose'], active: 'compose'});

  // Keyboard: arrows move along the tab list, Delete closes.
  await page.focus('#view-tabs .view-tab.selected');
  await page.keyboard.press('ArrowLeft');
  await step('ArrowLeft', {tabs: ['perform', 'rig', 'compose'], active: 'rig'});
  assert.equal(await page.evaluate(() => document.activeElement?.closest('.view-tab')?.dataset.view), 'rig', 'focus follows');
  await page.keyboard.press('Home');
  await step('Home', {tabs: ['perform', 'rig', 'compose'], active: 'perform'});
  await page.keyboard.press('End');
  await page.waitForFunction(() => document.activeElement?.closest('.view-tab')?.dataset.view === 'compose');
  await page.keyboard.press('Delete');
  await step('Delete closes the focused tab', {tabs: ['perform', 'rig'], active: 'rig'});

  // A second page of the same show is another window: it has its own socket.
  // Within this page no tab opened a device or sent a command.
  const allowed = new Set(['/api/settings']);
  assert.deepEqual([...new Set(posts)].filter(p => !allowed.has(p)), [], 'tabs sent no commands or device requests');
  results.posts = [...new Set(posts)];

  // Reload: the device restores its tabs and active view.
  await page.reload({waitUntil: 'domcontentloaded'});
  await connected(page);
  await page.waitForFunction(() => document.querySelector('#view-tabs .view-tab.selected')?.dataset.view === 'rig', null, {timeout: 10000});
  const restored = await strip(page);
  results.restored = restored;
  assert.deepEqual(restored.tabs, ['perform', 'rig']);
  assert.equal(restored.path, '/rig');
  await page.goto(originA + '/', {waitUntil: 'domcontentloaded'});
  await connected(page);
  await page.waitForFunction(() => location.pathname === '/rig', null, {timeout: 10000});
  results.homeRestoresActiveTab = await strip(page);

  // Damaged storage is repaired.
  await page.evaluate(() => localStorage.setItem('tolglow.studio.view-tabs',
    JSON.stringify({version: 1, active: 9, tabs: [{id: 3, view: 'debug'}, {id: 3, view: 'rig'}, {id: 4, view: 'gone'}]})));
  await page.goto(originA + '/', {waitUntil: 'domcontentloaded'});
  await connected(page);
  await page.waitForFunction(() => location.pathname === '/debug', null, {timeout: 10000});
  const repaired = await strip(page);
  results.repaired = repaired;
  assert.deepEqual(repaired.tabs, ['debug']);

  // Phone width.
  await page.evaluate(() => localStorage.setItem('tolglow.studio.view-tabs',
    JSON.stringify({version: 1, active: 2, tabs: [{id: 1, view: 'perform'}, {id: 2, view: 'rig'}, {id: 3, view: 'compose'}]})));
  await page.setViewportSize({width: 390, height: 844});
  await page.goto(originA + '/', {waitUntil: 'domcontentloaded'});
  await connected(page);
  await page.waitForFunction(() => location.pathname === '/rig', null, {timeout: 10000});
  const overflow = await page.evaluate(() => document.documentElement.scrollWidth - innerWidth);
  assert.ok(overflow <= 0, 'no horizontal page scroll at phone width');
  await page.screenshot({path: join(directory, 'tabs-phone.png')});
  await page.setViewportSize({width: 1440, height: 900});
  await page.close();

  // Mac windows: each window is bound to its own show's runtime.
  async function windowFor(origin, binding) {
    const win = await context.newPage();
    const sent = [];
    win.on('pageerror', e => errors.push(e.message));
    win.on('response', async r => {
      if (r.request().method() === 'POST' && new URL(r.url()).pathname === '/api/command') {
        let body = null; try { body = r.request().postData(); } catch {}
        sent.push({status: r.status(), body});
      }
    });
    await win.addInitScript(b => { window.tolglowStudioWindow = Object.freeze(b); }, binding);
    // The Mac app loads the show's root, like a fresh window.
    await win.goto(origin + '/', {waitUntil: 'domcontentloaded'});
    return {win, sent};
  }

  // Window B's page pointed at show A's address (a reused port): it refuses A.
  const before = (await state(originA))['blackout?'];
  const crossed = await windowFor(originA, {runtimeId: showB['runtime-id'], show: 'Rehearsal'});
  await crossed.win.waitForFunction(() => /belongs to Rehearsal/.test(document.querySelector('#connection-banner')?.textContent || ''), null, {timeout: 30000});
  results.refusal = await crossed.win.locator('#connection-banner').textContent();
  await crossed.win.screenshot({path: join(directory, 'window-refuses-other-show.png')});
  await crossed.win.click('#blackout');
  await crossed.win.waitForTimeout(1500);
  // A refused window has no show epoch, so Blackout sends nothing; any
  // command it did send would name show B and be refused by show A.
  results.crossedCommands = crossed.sent;
  for (const {status, body} of crossed.sent) {
    assert.match(body, new RegExp(showB['runtime-id']), 'the bound window names its own runtime');
    assert.equal(status, 422, 'show A refuses a command naming show B');
  }
  assert.equal((await state(originA))['blackout?'], before, 'show A is untouched');
  await crossed.win.close();

  // Two windows, each bound to its own show, both connected at once.
  const windowA = await windowFor(originA, {runtimeId: showA['runtime-id'], show: 'Club night'});
  const windowB = await windowFor(originB, {runtimeId: showB['runtime-id'], show: 'Rehearsal',
    tabs: JSON.stringify({version: 1, active: 2, tabs: [{id: 1, view: 'perform'}, {id: 2, view: 'rig'}]})});
  await connected(windowA.win);
  await connected(windowB.win);
  await windowB.win.waitForFunction(() => location.pathname === '/rig', null, {timeout: 10000});
  results.windows = {
    a: await windowA.win.evaluate(() => ({title: document.title, path: location.pathname})),
    b: await windowB.win.evaluate(() => ({title: document.title, path: location.pathname,
      tabs: [...document.querySelectorAll('#view-tabs .view-tab')].map(t => t.dataset.view)})),
  };
  assert.match(results.windows.a.title, /Club night/);
  assert.match(results.windows.b.title, /^Rig & space · Rehearsal/);
  assert.deepEqual(results.windows.b.tabs, ['perform', 'rig'], 'tabs come back from the app per show folder');
  const blackoutB = (await state(originB))['blackout?'];
  await windowA.win.click('#blackout');
  await windowA.win.waitForTimeout(1000);
  assert.equal(windowA.sent.at(-1)?.status, 200, 'window A controls show A');
  assert.equal((await state(originB))['blackout?'], blackoutB, 'show B did not change');
  await windowA.win.click('#blackout');
  await windowA.win.waitForTimeout(500);
  await windowB.win.screenshot({path: join(directory, 'window-b-rig-tab.png')});

  // A preview-only show (another window owns the devices): device controls are
  // greyed with the reason, and the owner refuses the request with a code.
  if (originC) {
    const c = await context.newPage();
    c.on('pageerror', e => errors.push(e.message));
    await c.goto(originC + '/connections', {waitUntil: 'domcontentloaded'});
    await connected(c);
    await c.waitForSelector('#sync-start[data-preview-only]', {timeout: 30000});
    results.previewOnly = await c.evaluate(() => ['#sync-start', '#audio-start', '#rdm-enable', '#stage-start', '#sensor-osc-start']
      .map(sel => { const el = document.querySelector(sel);
        return {sel, present: !!el, disabled: el?.disabled ?? null, title: el?.title ?? null}; }));
    for (const control of results.previewOnly.filter(x => x.present)) {
      assert.equal(control.disabled, true, control.sel + ' is greyed');
      assert.match(control.title, /owns this Mac's devices and output/, control.sel + ' says why');
    }
    assert.ok(results.previewOnly.filter(x => x.present).length >= 2, 'device controls found');
    const idC = await identity(originC);
    const refusal = await c.evaluate(async body => {
      const r = await fetch('/api/sync/start', {method: 'POST', headers: {'Content-Type': 'application/json', Accept: 'application/json'},
        body: JSON.stringify(body)});
      return {status: r.status, body: await r.json()};
    }, {'runtime-id': idC['runtime-id'], 'expected-epoch': idC.epoch});
    results.previewOnlyRefusal = refusal;
    assert.equal(refusal.status, 409);
    assert.equal(refusal.body.details.code, 'preview-only');
    await c.screenshot({path: join(directory, 'preview-only-connections.png')});
    await c.close();
  }
  results.errors = errors;
  assert.deepEqual(errors, [], 'no page errors');
  console.log('view tabs QA passed');
} finally {
  await writeFile(join(directory, 'results.json'), JSON.stringify(results, null, 2));
  await context.close();
}
