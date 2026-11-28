// Components rendered per real app-db change, by component (#790).
//
//   node dev/studio/panel-render-count.mjs <studio-origin> [view] [edits] [label]
//
// Opens Studio on view (perform or connections), launches the sweep cue if no
// cue runs, then measures two windows:
//   1. idle, for IDLE_SECONDS (default 6): show frames only;
//   2. edits: <edits> (default 12) changes of the running sweep's width
//      parameter, one every EDIT_MS (default 700). EDIT=api (default) sends
//      param/set through /api/command, as the REPL would; EDIT=ui presses
//      ArrowUp on the width fader of the Perform board, so the optimistic
//      draft, the command and its acknowledgement all run in the browser.
// React components that rendered are counted per commit through the React
// DevTools commit hook. A commit that follows an app-db change (an
// add-watch on re-frame's app-db that saw a different value) counts toward
// "db"; any other commit is a frame-lane render. Run against a development
// build (`shadow-cljs compile studio`) so component names are readable and
// re-frame's app-db is reachable; run only against a capture-only host you own,
// because this launches a cue and changes its parameter. Headless Chrome; set
// CHROMIUM_PATH. Not foreground, Safari or iPad evidence.
import {randomUUID} from 'node:crypto';
import {mkdir} from 'node:fs/promises';
import {chromium} from 'playwright-core';

const [origin, view = 'perform', editsArg = '12', label = 'run'] = process.argv.slice(2);
if (!origin) throw new Error('Pass the Studio origin.');
const edits = Number(editsArg), editMs = Number(process.env.EDIT_MS || 700), idleSeconds = Number(process.env.IDLE_SECONDS || 6);
const mode = process.env.EDIT || 'api';
const dir = 'target/panel-render-count/chrome-' + label;
await mkdir(dir, {recursive: true});

const state = async () => (await fetch(origin + '/api/state')).json();
const command = async (op, fields) => {
  const now = await state();
  const reply = await (await fetch(origin + '/api/command', {method: 'POST', headers: {'Content-Type': 'application/json'},
    body: JSON.stringify({op, ...fields, 'runtime-id': now['runtime-id'], 'show-id': now['show-id'], epoch: now.epoch,
      'expected-epoch': now.epoch, 'command-id': randomUUID(), source: {type: 'repl', 'session-id': 'panel-render-count'}})})).json();
  if ((reply.ack || reply).status !== 'accepted') throw new Error('The host rejected ' + op + ': ' + JSON.stringify(reply).slice(0, 300));
  return reply;
};
const sweepInstance = async () => (await state()).instances.find(i => i['cue-id'] === 'sweep');
if (!await sweepInstance()) await command('cue/launch', {'cue-id': 'sweep', launch: {mode: 'persistent'}});
const instance = await sweepInstance();
if (!instance) throw new Error('No running sweep instance.');

const context = await chromium.launchPersistentContext(dir, {executablePath: process.env.CHROMIUM_PATH,
  headless: true, args: ['--headless=new', '--use-angle=metal', '--enable-gpu'], viewport: {width: 1440, height: 900}});
try {
  const page = context.pages()[0] || await context.newPage();
  const errors = [];
  page.on('pageerror', e => errors.push(e.message));
  await page.addInitScript(() => {
    const m = window.__measure = {dbDirty: false, dbChanges: 0, frames: 0,
      db: {commits: 0, components: 0, byName: {}}, lane: {commits: 0, components: 0, byName: {}}};
    let seen = new WeakSet();
    const visit = (fiber, next, bucket) => {
      for (let f = fiber; f; f = f.sibling) {
        next.add(f);
        // Same rule as room-frame-cost.mjs: a component rendered in this commit
        // when its fiber is new to the committed tree and it mounted or worked.
        if ([0, 1, 11, 14, 15].includes(f.tag) && !seen.has(f) && (f.alternate === null || (f.flags & 1))) {
          bucket.components++;
          const name = f.type?.displayName || f.type?.name || 'anonymous';
          bucket.byName[name] = (bucket.byName[name] || 0) + 1;
        }
        if (f.child) visit(f.child, next, bucket);
      }
    };
    window.__REACT_DEVTOOLS_GLOBAL_HOOK__ = {supportsFiber: true, renderers: new Map(), inject() { return 1; },
      checkDCE() {}, onScheduleFiberRoot() {}, onCommitFiberUnmount() {}, onPostCommitFiberRoot() {},
      onCommitFiberRoot(_, root) {
        const bucket = m.dbDirty ? m.db : m.lane;
        m.dbDirty = false;
        bucket.commits++;
        const next = new WeakSet(); visit(root.current.child, next, bucket); seen = next;
      }};
    const desc = Object.getOwnPropertyDescriptor(WebSocket.prototype, 'onmessage');
    Object.defineProperty(WebSocket.prototype, 'onmessage', {configurable: true, get: desc.get,
      set(fn) { desc.set.call(this, fn && function(ev) { if (String(this.url).includes('/api/state/stream')) m.frames++; return fn.call(this, ev); }); }});
  });
  await page.goto(origin, {waitUntil: 'domcontentloaded'});
  await page.waitForFunction(() => document.querySelector('#connection-label')?.textContent === 'Studio connected', null, {timeout: 60000});
  const hooked = await page.evaluate(() => {
    const core = globalThis.cljs?.core, appDb = globalThis.re_frame?.db?.app_db;
    if (!core || !appDb) return false;
    core.add_watch(appDb, 'panel-render-count', (_, __, before, after) => {
      if (before !== after) { window.__measure.dbChanges++; window.__measure.dbDirty = true; }
    });
    return true;
  });
  if (!hooked) throw new Error('re-frame app-db is not reachable; use a development build.');
  await page.locator(`a[data-view=${view}]`).click();
  await page.waitForTimeout(4000);
  const snap = () => page.evaluate(() => structuredClone(window.__measure));
  const diff = (a, b) => {
    const bucket = k => {
      const byName = Object.fromEntries(Object.entries(b[k].byName).map(([n, v]) => [n, v - (a[k].byName[n] || 0)]).filter(([, v]) => v > 0));
      return {commits: b[k].commits - a[k].commits, components: b[k].components - a[k].components, byName};
    };
    return {frames: b.frames - a.frames, dbChanges: b.dbChanges - a.dbChanges, db: bucket('db'), lane: bucket('lane')};
  };
  const top = (byName, per, n = 15) => Object.entries(byName).sort((x, y) => y[1] - x[1]).slice(0, n).map(([k, v]) => `${k.replace(/^tolglow\.studio\./, '')} ${+(v / per).toFixed(2)}`);
  // Components that also render on ordinary frames (at least every other
  // lane frame) share a commit with an app-db change by coincidence.
  const frameReaders = d => new Set(Object.entries(d.lane.byName).filter(([, n]) => d.frames && n / d.frames >= 0.5).map(([k]) => k));
  const excluding = (d, names) => Object.entries(d.db.byName).filter(([k]) => !names.has(k)).reduce((a, [, n]) => a + n, 0);
  const summary = d => ({frames: d.frames, appDbChanges: d.dbChanges,
    componentsPerDbChangeExcludingFrameReaders: d.dbChanges ? +(excluding(d, frameReaders(d)) / d.dbChanges).toFixed(1) : 0,
    dbCommits: d.db.commits, componentsPerDbChange: d.dbChanges ? +(d.db.components / d.dbChanges).toFixed(1) : 0,
    componentsPerDbCommit: d.db.commits ? +(d.db.components / d.db.commits).toFixed(1) : 0,
    laneComponentsPerFrame: d.frames ? +(d.lane.components / d.frames).toFixed(2) : 0,
    topPerDbChange: d.dbChanges ? top(d.db.byName, d.dbChanges, Number(process.env.TOP || 60)) : [], topLanePerFrame: d.frames ? top(d.lane.byName, d.frames, 8) : []});

  const s0 = await snap();
  await page.waitForTimeout(idleSeconds * 1000);
  const s1 = await snap();
  let fader;
  if (mode === 'ui') {
    if (view !== 'perform') throw new Error('EDIT=ui needs the Perform view.');
    const sweep = page.locator('article', {has: page.getByRole('heading', {name: 'Slow sweep', exact: true})});
    fader = sweep.getByRole('slider', {name: 'width', exact: true});
    await fader.scrollIntoViewIfNeeded();
    await fader.focus();
  }
  const s2 = await snap();
  for (let i = 0; i < edits; i++) {
    if (mode === 'ui') await fader.press(i % 2 ? 'ArrowDown' : 'ArrowUp');
    else await command('param/set', {'instance-id': instance.id, param: 'width', value: i % 2 ? 0.3 : 0.35});
    await page.waitForTimeout(editMs);
  }
  const s3 = await snap();
  const idle = diff(s0, s1), edit = diff(s2, s3);
  console.log(JSON.stringify({label, view, mode, edits, idle: summary(idle), edit: summary(edit), pageErrors: errors.slice(0, 5)}, null, 1));
} finally { await context.close(); }
