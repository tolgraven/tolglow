import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {createHash} from 'node:crypto';
import {mkdtemp, readFile, writeFile} from 'node:fs/promises';
import {join, resolve} from 'node:path';
import {setTimeout as pause} from 'node:timers/promises';
import {chromium} from 'playwright-core';

const [app, resultFile] = process.argv.slice(2).map(path => resolve(path));
const resources = resolve('resources');
const runtime = join(app, 'Contents/Resources/runtime/bin/java');
const jar = join(app, 'Contents/Resources/studio.jar');
const show = await mkdtemp('/private/tmp/tolglow-smooth-room.');
const ready = join(show, 'port');
await writeFile(join(show, 'show.clj'), `(ns show)
(def config
  {:hz 40
   :show {:credential-path "credentials.edn" :workspace-options nil}
   :show-state {:actions [{:op :cue/launch :cue-id :ember}
                         {:op :cue/launch :cue-id :sweep}]}
   :init {:components [:show] :modules [:show-state :web-server]}})
`);
const server = spawn(runtime, ['-Djava.awt.headless=true', '-cp', resources + ':' + jar,
  'clojure.main', '-m', 'tolglow.desktop', show, ready], {cwd: show, stdio: ['pipe', 'inherit', 'inherit']});
const digest = bytes => createHash('sha256').update(bytes).digest('hex');
let browser, controller, targetId, address;
try {
  let port;
  for (let attempt = 0; attempt < 100; attempt++) {
    assert.equal(server.exitCode, null);
    port = (await readFile(ready, 'utf8').catch(() => '')).trim();
    if (/^\d+$/.test(port)) break;
    await pause(500);
  }
  assert.match(port, /^\d+$/);
  address = 'http://127.0.0.1:' + port;
  const served = await fetch(address + '/studio/js/main.js');
  assert.ok(served.ok);
  const mainSHA256 = digest(Buffer.from(await served.arrayBuffer()));
  assert.equal(mainSHA256, digest(await readFile(join(resources, 'studio/js/main.js'))));
  browser = await chromium.connectOverCDP('http://127.0.0.1:9223');
  const context = browser.contexts()[0];
  controller = await browser.newBrowserCDPSession();
  const open = context.waitForEvent('page');
  ({targetId} = await controller.send('Target.createTarget', {url: 'about:blank', background: false}));
  const page = await open;
  const cdp = await context.newCDPSession(page);
  assert.equal((await cdp.send('Target.getTargetInfo')).targetInfo.targetId, targetId);
  await page.goto(address, {waitUntil: 'domcontentloaded'});
  await page.evaluate(() => {document.title = 'Tolglow Room fixture interpolation test';});
  const panel = page.locator('#preview-panel');
  await panel.scrollIntoViewIfNeeded();
  const select = page.getByLabel('Preview fixture updates', {exact: true});
  const readMode = async mode => {
    const state = await page.waitForFunction(expected => {
      const engine = document.querySelector('#preview')?.tolglowPreview;
      const t = engine?.getTelemetry();
      if (!document.hidden && engine?.ready && t?.visible && t?.lightingMode === expected &&
          t.activeLights === 8 && t.thread === 'worker' && t.rendering && !t.softwareRenderer) {
        return {lightingMode: t.lightingMode, deviceRenderer: t.deviceRenderer,
          thread: t.thread, activeLights: t.activeLights, frameNumber: t.frameNumber};
      }
    }, mode, {timeout: 30000});
    return state.jsonValue();
  };
  const head = async () => {
    const state = await fetch(address + '/api/state');
    assert.ok(state.ok);
    return (await state.json()).output.values['head-1'].pan;
  };
  assert.equal(await select.inputValue(), 'interpolate');
  const initial = await readMode('interpolate');
  assert.match(initial.deviceRenderer, /ANGLE Metal Renderer: Apple M4/);
  const pans = [await head()];
  await select.selectOption('hold');
  const hold = await readMode('hold');
  pans.push(await head());
  await select.selectOption('interpolate');
  const restored = await readMode('interpolate');
  pans.push(await head());
  assert.ok(pans.every(Number.isFinite) && new Set(pans).size > 1, 'Committed moving-head samples must advance');
  assert.ok(restored.frameNumber > initial.frameNumber);
  await writeFile(resultFile, JSON.stringify({mainSHA256, browser:browser.version(),
    initial, hold, restored, pans}, null, 2) + '\n');
  console.log(JSON.stringify({initial, hold, restored, pans}));
} finally {
  if (targetId) await controller.send('Target.closeTarget', {targetId}).catch(() => {});
  await browser?.close();
  server.stdin.end();
  for (let i = 0; i < 100 && server.exitCode === null && server.signalCode === null; i++) await pause(100);
  if (server.exitCode === null && server.signalCode === null) server.kill('SIGTERM');
  if (address) assert.equal(await fetch(address, {signal: AbortSignal.timeout(1200)}).then(() => true, () => false), false);
}
