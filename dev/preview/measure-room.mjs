// Run from the built Tolglow checkout. This command owns its show and tab.
import {spawn, execFileSync} from 'node:child_process';
import {createHash, randomUUID} from 'node:crypto';
import {createWriteStream} from 'node:fs';
import {mkdir, mkdtemp, readFile, writeFile} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join, resolve} from 'node:path';
import {setTimeout as delay} from 'node:timers/promises';
import {chromium} from 'playwright-core';
import {measureRoom} from './room-cadence.mjs';
import {beginPassSample} from './room-pass-analysis.mjs';

const [cdp = 'http://127.0.0.1:9223', output = 'target/room-device'] = process.argv.slice(2);
const quality = process.env.ROOM_QUALITY || 'auto';
if (!['auto', 'mobile', 'desktop'].includes(quality)) throw new Error('ROOM_QUALITY must be auto, mobile or desktop.');
const passes = process.env.ROOM_PASSES?.split(',').map(s => s.trim());
const allowedPasses = ['full', 'cached-shadows', 'skip-fog-raymarch', 'present-cached-frame'];
if (passes && (quality === 'auto' || passes.some(p => !allowedPasses.includes(p)))) {
  throw new Error('ROOM_PASSES requires a fixed quality and one or more of '+allowedPasses.join(', '));
}
const scene = process.env.ROOM_SCENE || 'both';
if (!['both','dense','fixed'].includes(scene)) throw new Error('ROOM_SCENE must be both, dense or fixed.');
const endpoint = new URL(cdp);
if (endpoint.protocol !== 'http:' || !['127.0.0.1', '[::1]', 'localhost'].includes(endpoint.hostname)) {
  throw new Error('Pass the local Chrome CDP endpoint.');
}
const outputDir = resolve(output), source = process.cwd();
const temp = await mkdtemp(join(tmpdir(), 'tolglow-room-device-'));
await mkdir(outputDir, {recursive: true});
const gitHead = dir => execFileSync('git', ['-C', dir, 'rev-parse', 'HEAD'], {encoding: 'utf8'}).trim();
const fork = resolve(process.env.AFTERGLOW_ROOT || '../afterglow');
const ui = await readFile(join(source, 'resources/studio/js/main.js'));
const shader = await readFile(join(source, 'resources/studio/preview-worker.js'));
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
const metadata = {tolglow: gitHead(source), afterglow: gitHead(fork), mainJsSha256: hash(ui),
  workerJsSha256: hash(shader), platform: process.platform, architecture: process.arch,
  output: 'preview only', quality, warmup: {minimumSeconds: 8, minimumDrawnFrames: 4}, scenarios: []};
const config = join(temp, 'room.edn');
await writeFile(config, '{:init {:components [:show] :modules [:web-server]} :show {:workspace-options nil} :web-server {:host "127.0.0.1" :port 0}}');
const server = spawn(process.env.LEIN || 'lein', ['run', '-m', 'tolglow.application',
  '--config', config, '--output', 'preview', '--port', '0', '--host', '127.0.0.1'],
{cwd: source, env: {...process.env, AFTERGLOW_ROOT: fork}, detached: true, stdio: ['ignore', 'pipe', 'pipe']});
const log = createWriteStream(join(outputDir, 'host.log'));
server.stderr.pipe(log, {end: false}); server.stdout.pipe(log, {end: false});
let page, closing, probeWorker;
function stopServer() {
  if (server.pid && server.exitCode == null) {
    try {process.kill(-server.pid, 'SIGTERM');} catch (error) {if (error.code !== 'ESRCH') throw error;}
  }
}
const onExit = () => { stopServer(); };
process.once('exit', onExit);
async function shutdown() {
  if (!closing) closing = (async () => {
    try { await page?.close(); } finally {stopServer(); log.end();}
  })();
  return closing;
}
process.once('SIGINT', async () => {await shutdown(); process.exit(130);});
process.once('SIGTERM', async () => {await shutdown(); process.exit(143);});
try {
  const origin = await new Promise((ok, fail) => {
    let lines = '';
    const timer = setTimeout(() => fail(new Error('Private demo failed to start. Check host.log.')), 180000);
    const done = fn => value => {clearTimeout(timer); fn(value);};
    server.once('error', done(fail));
    server.once('exit', done(() => fail(new Error('Private demo exited. Check host.log.'))));
    server.stdout.on('data', bytes => {
      lines = (lines + bytes).slice(-8192);
      const url = lines.match(/Tolglow Studio listening on (http:\/\/127\.0\.0\.1:\d+)\s/);
      if (url) done(ok)(url[1]);
    });
  });
  const browser = await chromium.connectOverCDP(endpoint.href);
  page = await browser.contexts()[0].newPage();
  if (passes) {
    const {build} = await import('esbuild');
    const result = await build({entryPoints:['dev/preview/room-pass-worker.mjs'], bundle:true, format:'esm',
      platform:'browser', target:['es2022'], minify:true, write:false});
    const probeBytes = result.outputFiles[0].contents;
    metadata.diagnosticWorkerSha256 = hash(probeBytes);
    await page.route(origin + '/studio/preview-worker.js', route =>
      route.fulfill({contentType:'application/javascript', body:Buffer.from(probeBytes)}));
  }
  await page.bringToFront();
  await page.goto(origin);
  await page.locator('#preview').scrollIntoViewIfNeeded();
  await page.waitForFunction(() => document.querySelector('#preview')?.tolglowPreview?.ready, null, {timeout: 90000});
  // Use ordinary controls and the actual device viewport and DPR.
  const expand = page.getByRole('button', {name: 'Expand', exact: true});
  if (await expand.count()) await expand.click();
  await page.getByLabel('Preview quality', {exact: true}).selectOption(quality);
  await page.locator('#preview').scrollIntoViewIfNeeded();
  if (passes) {
    probeWorker = page.workers().find(w => new URL(w.url()).pathname === '/studio/preview-worker.js');
    if (!probeWorker || !await probeWorker.evaluate(() => typeof self.setRoomPassMode === 'function'))
      throw new Error('Diagnostic worker did not attach to the private Room page.');
  }
  async function cue(id) {
    const snapshot = await (await fetch(origin + '/api/state')).json();
    const reply = await fetch(origin + '/api/command', {method: 'POST', headers: {'Content-Type': 'application/json'},
      body: JSON.stringify({op: 'cue/launch', 'cue-id': id, launch: {mode: 'persistent'},
        'runtime-id': snapshot['runtime-id'], 'show-id': snapshot['show-id'], epoch: snapshot.epoch,
        'expected-epoch': snapshot.epoch, 'command-id': randomUUID(), source: {type: 'repl', 'session-id': 'browser-room-qualification'}})});
    const result = await reply.json();
    if ((result.ack || result).status !== 'accepted') throw new Error('The demo rejected ' + id);
  }
  async function slider(label, value) {
    await page.getByLabel(label, {exact: true}).evaluate((element, v) => {
      Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value').set.call(element, String(v));
      element.dispatchEvent(new Event('input', {bubbles: true}));
    }, value);
  }
  async function scenario(name, fog, moving, pass = null) {
    // Restore the full moving scene before selecting a different ablation, so
    // a cached-frame sample captures the current lit scenario.
    if (pass) await probeWorker.evaluate(() => self.setRoomPassMode('full'));
    await slider('Preview work light', .22); await slider('Preview fog density', fog);
    await page.locator('#preview').scrollIntoViewIfNeeded();
    await page.waitForFunction(fog => {
      const t = document.querySelector('#preview').tolglowPreview.getTelemetry();
      return t.visible && t.activeLights > 0 && t.fogAmount === fog;
    }, fog, {timeout: 60000});
    // Exclude the new tier's pipeline allocation and first lit/shadowed draws,
    // including when Safari or a laptop is still compiling a cold program.
    await page.waitForFunction(q => q === 'auto' || document.querySelector('#preview').tolglowPreview.getTelemetry().quality === q,
      quality, {timeout: 90000});
    const warmStarted = Date.now();
    const initialFrames = await page.evaluate(() => document.querySelector('#preview').tolglowPreview.getTelemetry().frames);
    await Promise.all([delay(8000), page.waitForFunction(n => document.querySelector('#preview').tolglowPreview.getTelemetry().frames >= n + 4,
      initialFrames, {timeout: 150000})]);
    if (pass) {
      await probeWorker.evaluate(choice => self.setRoomPassMode(choice), pass);
      await page.waitForFunction(choice => {
        const p = document.querySelector('#preview').tolglowPreview.getTelemetry()?.passProbe;
        return p?.mode === choice && (choice !== 'present-cached-frame' || 'replay' in p.submitMs);
      }, pass, {timeout:90000});
      await delay(2500);
      await page.evaluate(beginPassSample, pass);
    }
    const state = async () => (await (await fetch(origin + '/api/state')).json()).output?.values?.['head-1'];
    const warmupMs = Date.now() - warmStarted;
    const before = await state();
    const [measured, middle] = await Promise.all([page.evaluate(measureRoom, 20000), delay(7100).then(state)]);
    const after = await state();
    if (pass) {
      measured.pass = pass;
      measured.passTimingsMs = await page.evaluate(() => window.finishRoomPassSample());
      if (!measured.passTimingsMs.domSubmitMs?.samples) measured.invalid.push('no diagnostic presentations');
      name += '-' + String(metadata.scenarios.length+1).padStart(2,'0') + '-' + pass;
    }
    await page.screenshot({path: join(outputDir, name + '.png')});
    measured.name = name;
    measured.requestedQuality = quality;
    measured.warmupMs = warmupMs;
    if (quality !== 'auto' && (measured.qualities.some(q => q !== quality) || measured.scales.some(k => k !== 1))) {
      measured.invalid.push('manual tier or resolution scale changed during the sample');
    }
    measured.head1 = {before, middle, after};
    if (measured.end.fixtureCount !== 8 || measured.end.activeLights < 1) measured.invalid.push('demo heads or emission missing');
    if (moving && new Set([before?.pan, middle?.pan, after?.pan]).size < 2) measured.invalid.push('no pan change observed at sample boundaries');
    metadata.scenarios.push(measured);
    console.log(name, JSON.stringify({thread: measured.rendererThread, roomHz: measured.presented.hz,
      rafHz: measured.windowRaf.hz, quality: measured.qualities, backing: measured.backing, invalid: measured.invalid}));
  }
  await cue('ember');
  if (scene !== 'dense') for (const pass of passes || [null]) await scenario('lit-fixed-ambient-fog', .25, false, pass);
  if (scene !== 'fixed') {
    await cue('sweep');
    for (const pass of passes || [null]) await scenario('moving-heads-dense-fog', .7, true, pass);
  }
  metadata.qualified = metadata.scenarios.every(s => !s.invalid.length && !s.end.softwareRenderer);
  await writeFile(join(outputDir, 'room.json'), JSON.stringify(metadata, null, 2));
  if (!metadata.qualified) process.exitCode = 2;
} catch (error) {
  console.error(error); process.exitCode = 1;
} finally {
  await shutdown();
  // Closing this process disconnects CDP. Never issue Browser.close or close
  // the shared default context; the operator's other Chrome tabs stay open.
  process.exit(process.exitCode || 0);
}
