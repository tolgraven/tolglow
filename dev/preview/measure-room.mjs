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
import {readDesktopSession, watchDesktopSession} from './desktop-session.mjs';

const [cdp = 'http://127.0.0.1:9223', output = 'target/room-device'] = process.argv.slice(2);
const quality = process.env.ROOM_QUALITY || 'auto';
if (!['auto', 'mobile', 'desktop'].includes(quality)) throw new Error('ROOM_QUALITY must be auto, mobile or desktop.');
const workLight = Number(process.env.ROOM_WORK_LIGHT ?? .22);
const lightingMode = process.env.ROOM_LIGHTING_MODE || 'hold';
const minimumWarmupMs = Number(process.env.ROOM_WARMUP_MS ?? 8000);
if (!Number.isFinite(workLight) || workLight < 0 || workLight > 1) throw new Error('ROOM_WORK_LIGHT must be between zero and one.');
if (!['hold', 'interpolate'].includes(lightingMode)) throw new Error('ROOM_LIGHTING_MODE must be hold or interpolate.');
if (!Number.isInteger(minimumWarmupMs) || minimumWarmupMs < 8000 || minimumWarmupMs > 120000)
  throw new Error('ROOM_WARMUP_MS must be between 8000 and 120000.');
const passes = process.env.ROOM_PASSES?.split(',').map(s => s.trim());
const allowedPasses = ['full', 'full-no-timers', 'no-composite-fallback', 'simple-fixed-loop', 'cached-shadows', 'skip-fog-raymarch', 'present-cached-frame'];
const frozenAuto = quality === 'auto' && Boolean(passes);
const loopComparison = passes?.includes('simple-fixed-loop');
const timerComparison = passes?.includes('full-no-timers');
const compositeComparison = passes?.includes('no-composite-fallback');
const attributionComparison = frozenAuto && passes.join(',') === 'full,present-cached-frame,skip-fog-raymarch,cached-shadows,full';
const controlledComparison = loopComparison || timerComparison || compositeComparison || attributionComparison;
const requestTimerExtension = !compositeComparison && !attributionComparison;
if (passes && (passes.some(p => !allowedPasses.includes(p)) ||
    (frozenAuto && !attributionComparison && passes.some(p => !['full', 'full-no-timers', 'no-composite-fallback', 'simple-fixed-loop'].includes(p))))) {
  throw new Error('ROOM_PASSES at Auto requires a supported controlled sequence; other pass probes require a fixed quality.');
}
const scene = process.env.ROOM_SCENE || 'both';
if (!['both','dense','fixed'].includes(scene)) throw new Error('ROOM_SCENE must be both, dense or fixed.');
if (loopComparison && (!frozenAuto || scene !== 'dense' || minimumWarmupMs < 20000 ||
    passes.join(',') !== 'full,simple-fixed-loop,full'))
  throw new Error('The loop comparison requires Auto, dense moving scene, ≥20 s warmup and full,simple-fixed-loop,full.');
if (timerComparison && (!frozenAuto || scene !== 'dense' || minimumWarmupMs < 20000 ||
    passes.join(',') !== 'full,full-no-timers,full'))
  throw new Error('The timer comparison requires Auto, dense moving scene, ≥20 s warmup and full,full-no-timers,full.');
if (compositeComparison && (!frozenAuto || scene !== 'dense' || minimumWarmupMs < 20000 ||
    passes.join(',') !== 'full,no-composite-fallback,full'))
  throw new Error('The composite comparison requires Auto, dense moving scene, ≥20 s warmup and full,no-composite-fallback,full.');
if (attributionComparison && (scene !== 'dense' || minimumWarmupMs < 20000))
  throw new Error('The stage attribution comparison requires the dense moving scene and ≥20 s warmup.');
const plumeScene = process.env.ROOM_PLUME_SCENE === '1';
if (plumeScene && (scene === 'fixed' || passes)) throw new Error('ROOM_PLUME_SCENE requires the full moving renderer.');
const plume = {position: [-1, .25, .4], direction: [1, .5, -.25], duration: 1.5, strength: 4};
const endpoint = new URL(cdp);
if (endpoint.protocol !== 'http:' || !['127.0.0.1', '[::1]', 'localhost'].includes(endpoint.hostname)) {
  throw new Error('Pass the local Chrome CDP endpoint.');
}
const outputDir = resolve(output), source = process.cwd();
await mkdir(outputDir, {recursive: true});
const desktopPreflight = await readDesktopSession();
if (desktopPreflight.status !== 'unlocked') {
  const error = 'Desktop session must be verified unlocked before benchmarking: '+desktopPreflight.reason;
  await writeFile(join(outputDir, 'room-error.json'), JSON.stringify({error, platform: process.platform,
    qualified: false, cadenceQualified: false, desktopSession: {preflight: desktopPreflight}}, null, 2));
  console.error(error); process.exit(2);
}
const temp = await mkdtemp(join(tmpdir(), 'tolglow-room-device-'));
const gitHead = dir => execFileSync('git', ['-C', dir, 'rev-parse', 'HEAD'], {encoding: 'utf8'}).trim();
const fork = resolve(process.env.AFTERGLOW_ROOT || '../afterglow');
const ui = await readFile(join(source, 'resources/studio/js/main.js'));
const shader = await readFile(join(source, 'resources/studio/preview-worker.js'));
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
const metadata = {tolglow: gitHead(source), afterglow: gitHead(fork), mainJsSha256: hash(ui),
  workerJsSha256: hash(shader), platform: process.platform, architecture: process.arch,
  output: 'preview only', quality, workLight, lightingMode,
  desktopSession: {preflight: desktopPreflight, checks: [], samples: [],
    scope: 'macOS root lock flag plus one active console session; other platforms are unverified and rejected',
    polling: 'one asynchronous ioreg read at a time, 750 ms timeout, next read 1000 ms after completion; at most 32 receipts and 25 s parent sample timeout; readMs records cost'},
  warmup: {minimumSeconds: minimumWarmupMs / 1000, minimumDrawnFrames: 4}, scenarios: []};
if (passes) metadata.diagnostic = {passes, productionCadenceAcceptance: false, frozenAuto,
  timerExtensionRequested: requestTimerExtension,
  readinessTiming: 'readyForFrameMs describes the previous frame through its acknowledgement; rafDeliveryMs and workerFrameMs describe the current frame',
  scope: loopComparison ? 'adaptive traversal versus fixed loop, both fog materials; all graphics stages retained'
    : timerComparison ? 'timer queries on/off/on; full shaders and all graphics stages retained'
    : compositeComparison ? 'composite depth-edge raymarch omitted only in middle pass; changes affected pixels and is not a production fix'
    : attributionComparison ? 'full controls bracket cached display blit, omitted fog volume/depth composite, and omitted shadow-map refresh; diagnostic only'
    : 'graphics pass attribution'};
async function checkDesktop(stage) {
  const check = await readDesktopSession(); metadata.desktopSession.checks.push({stage, ...check});
  if (check.status !== 'unlocked') throw new Error('Desktop session qualification failed: '+check.reason);
}
const config = join(temp, 'room.edn');
await writeFile(config, '{:init {:components [:show] :modules [:web-server]} :show {:workspace-options nil} :web-server {:host "127.0.0.1" :port 0}}');
const leinPrefix = [];
if (passes) {
  const {build} = await import('esbuild');
  const entry = new URL('./room-pass-worker.mjs', import.meta.url).pathname;
  const result = await build({entryPoints:[entry], bundle:true, format:'esm',
    platform:'browser', target:['es2022'], minify:true, write:false,
    define: {ROOM_GPU_TIMERS: String(requestTimerExtension)}});
  const overlay = join(temp, 'private-assets');
  await mkdir(join(overlay, 'studio'), {recursive: true});
  const bytes = result.outputFiles[0].contents;
  await writeFile(join(overlay, 'studio/preview-worker.js'), bytes);
  metadata.diagnosticWorkerSha256 = hash(bytes);
  // Prepend one resource root in this demo JVM only. Chrome loads its worker
  // from the same origin and route as an ordinary release, without interception.
  leinPrefix.push('update-in', ':resource-paths', 'seq', '--',
    'update-in', ':resource-paths', 'conj', JSON.stringify(overlay), '--');
}
try {await checkDesktop('before-server');} catch (error) {
  metadata.error = error.message; metadata.qualified = metadata.cadenceQualified = false;
  await writeFile(join(outputDir, 'room-error.json'), JSON.stringify(metadata, null, 2));
  console.error(error.message); process.exit(2);
}
const server = spawn(process.env.LEIN || 'lein', [...leinPrefix, 'run', '-m', 'tolglow.application',
  '--config', config, '--output', 'preview', '--port', '0', '--host', '127.0.0.1'],
{cwd: source, env: {...process.env, AFTERGLOW_ROOT: fork}, detached: true, stdio: ['ignore', 'pipe', 'pipe']});
const log = createWriteStream(join(outputDir, 'host.log'));
server.stderr.pipe(log, {end: false}); server.stdout.pipe(log, {end: false});
let page, closing, probeWorker, frozenPolicy, comparisonView, completed = false;
const keepPage = process.env.ROOM_KEEP_PAGE === '1';
function stopServer() {
  if (server.pid && server.exitCode == null) {
    try {process.kill(-server.pid, 'SIGTERM');} catch (error) {if (error.code !== 'ESRCH') throw error;}
  }
}
const onExit = () => { stopServer(); };
process.once('exit', onExit);
async function shutdown() {
  stopServer();
  if (!closing) closing = (async () => {
    let cleanupTimeout;
    try {
      await Promise.race([(async () => {
        if (keepPage && completed && page && !page.isClosed()) {
        // Keep only the owned tab, with no live Studio/worker/GPU resources or
        // fetch loop against the stopped host. ROOM_PAGE_TITLE can reuse it.
        await page.goto('about:blank', {timeout: 2000});
        await page.setContent('<title>Tolglow GPU diagnostic (paused)</title><p>Diagnostic complete. This owned tab is retained for the next comparison.</p>', {timeout: 2000});
        metadata.retainedPage = {title: await page.title(), url: page.url()};
        await writeFile(join(outputDir, 'room.json'), JSON.stringify(metadata, null, 2));
        } else await page?.close({runBeforeUnload: false});
      })(), new Promise(resolve => {cleanupTimeout = setTimeout(() => {
        metadata.cleanupTimedOut = true; resolve();
      }, 2500);})]);
    } finally {clearTimeout(cleanupTimeout); log.end();}
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
  await checkDesktop('before-browser');
  const browser = await chromium.connectOverCDP(endpoint.href);
  const reuseTitle = process.env.ROOM_PAGE_TITLE;
  if (reuseTitle) {
    const matches = [];
    for (const candidate of browser.contexts()[0].pages()) if (await candidate.title() === reuseTitle) matches.push(candidate);
    if (matches.length !== 1) throw new Error('Expected exactly one owned benchmark page with ROOM_PAGE_TITLE.');
    page = matches[0];
    await page.evaluate(() => window.crossing?.dispose());
    const session = await page.context().newCDPSession(page);
    await session.send('Emulation.clearDeviceMetricsOverride'); await session.detach();
  } else page = await browser.contexts()[0].newPage();
  if (passes) {
    const response = await fetch(origin + '/studio/preview-worker.js');
    const received = hash(new Uint8Array(await response.arrayBuffer()));
    if (!response.ok || received !== metadata.diagnosticWorkerSha256)
      throw new Error('Private Ring worker digest mismatch: '+received+' / '+metadata.diagnosticWorkerSha256);
    metadata.servedDiagnosticWorkerSha256 = received;
  }
  const startup = metadata.browserStartup = [];
  const note = (kind, message) => {if (startup.length < 24) startup.push({kind, message: String(message).slice(0,480)});};
  page.on('pageerror', e => note('page-error',e.message));
  page.on('console', m => {if (m.type()==='error') note('console',m.text());});
  page.on('requestfailed', req => note('asset',new URL(req.url()).pathname+' '+req.failure()?.errorText));
  page.on('worker', w => {note('worker',new URL(w.url()).pathname); w.on('close', () => note('worker-closed',new URL(w.url()).pathname));});
  await page.bringToFront();
  await page.goto(origin);
  await page.evaluate(title => {document.title = title;}, attributionComparison ? 'Tolglow graphics stage diagnostic'
    : compositeComparison ? 'Tolglow composite fallback diagnostic'
    : timerComparison ? 'Tolglow timer query diagnostic'
    : loopComparison ? 'Tolglow adaptive loop diagnostic' : 'Tolglow benchmark — measuring preview');
  await page.locator('#preview').scrollIntoViewIfNeeded();
  await page.waitForFunction(() => document.querySelector('#preview')?.tolglowPreview?.ready, null, {timeout: 90000});
  // Use ordinary controls and the actual device viewport and DPR.
  const expand = page.getByRole('button', {name: 'Expand', exact: true});
  if (await expand.count()) await expand.click();
  await page.getByLabel('Preview quality', {exact: true}).selectOption(quality);
  await page.getByLabel('Preview fixture updates', {exact: true}).selectOption(lightingMode);
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
  function viewChanged(before, after) {
    return ['camera', 'target'].some(key => {
      const a = before[key], b = after[key];
      return !Array.isArray(a) || !Array.isArray(b) || a.length !== 3 || b.length !== 3 ||
        a.some((value, i) => !Number.isFinite(value) || !Number.isFinite(b[i]) || Math.abs(value - b[i]) > 1e-6);
    });
  }
  async function scenario(name, fog, moving, pass = null, emitPlumes = false) {
    await checkDesktop('before-scenario-'+name+'-'+(pass || 'production'));
    // Restore full rendering before graphics ablations. Timer-only controls
    // switch at the safe next draw boundary before their entire warmup.
    if (pass) await probeWorker.evaluate(choice => self.setRoomPassMode(choice), timerComparison ? pass : 'full');
    await slider('Preview work light', workLight); await slider('Preview fog density', fog);
    await page.locator('#preview').scrollIntoViewIfNeeded();
    await page.waitForFunction(fog => {
      const t = document.querySelector('#preview').tolglowPreview.getTelemetry();
      return t.visible && t.activeLights > 0 && t.fogAmount === fog;
    }, fog, {timeout: 60000});
    // Exclude the new tier's pipeline allocation and first lit/shadowed draws,
    // including when Safari or a laptop is still compiling a cold program.
    await page.waitForFunction(q => q === 'auto' || document.querySelector('#preview').tolglowPreview.getTelemetry().quality === q,
      quality, {timeout: 90000});
    if (emitPlumes) await page.evaluate(options => {
      const preview = document.querySelector('#preview').tolglowPreview;
      const receipt = window.roomPlumeSample = {accepted: 0, rejected: 0};
      const burst = () => receipt[preview.burst(options) ? 'accepted' : 'rejected']++;
      burst(); receipt.timer = setInterval(burst, 4000);
    }, plume);
    const warmStarted = Date.now();
    const initialFrames = await page.evaluate(() => document.querySelector('#preview').tolglowPreview.getTelemetry().frames);
    await Promise.all([delay(minimumWarmupMs), page.waitForFunction(n => document.querySelector('#preview').tolglowPreview.getTelemetry().frames >= n + 4,
      initialFrames, {timeout: 150000})]);
    if (frozenAuto && !frozenPolicy) {
      frozenPolicy = await probeWorker.evaluate(() => self.freezeRoomPassQuality());
      metadata.diagnostic.frozenPolicy = frozenPolicy;
      console.log('Auto frozen after settling', JSON.stringify(frozenPolicy));
    }
    if (controlledComparison) {
      // The constructor's startup burst must disperse before either variant is
      // measured. Below .25 no cell enables local adaptive plume subdivision.
      await page.waitForFunction(() => {
        const telemetry = document.querySelector('#preview').tolglowPreview.getTelemetry(), fog = telemetry.fog;
        return fog.maximumDensity < .24 && fog.activeSources === 0 && !fog.atmosphere && telemetry.passProbe?.detailActive === false;
      }, null, {timeout: 120000});
      if (loopComparison && !metadata.diagnostic.pixelComparison) {
        metadata.diagnostic.pixelComparison = await probeWorker.evaluate(() => self.compareRoomPassPixels());
        console.log('Frozen loop pixel comparison', JSON.stringify(metadata.diagnostic.pixelComparison));
        if (Object.values(metadata.diagnostic.pixelComparison.differences).some(d => d.nonFinite || d.maximum > .002))
          throw new Error('Fixed-loop diagnostic changed frozen haze pixels beyond half-float rounding tolerance.');
      }
      if (compositeComparison && !metadata.diagnostic.fallbackMask) {
        metadata.diagnostic.fallbackMask = await probeWorker.evaluate(() => self.readRoomFallbackMask());
        console.log('Frozen composite fallback occupancy', JSON.stringify(metadata.diagnostic.fallbackMask));
        if (metadata.diagnostic.fallbackMask.invalidPixels || metadata.diagnostic.fallbackMask.timerExtensionRequested ||
            JSON.stringify(metadata.diagnostic.fallbackMask.targetSize) !== JSON.stringify(frozenPolicy.scene))
          throw new Error('Composite fallback mask or disabled-extension evidence was invalid.');
      }
    }
    if (pass) {
      if (!timerComparison) await probeWorker.evaluate(choice => self.setRoomPassMode(choice), pass);
      await page.waitForFunction(choice => {
        const p = document.querySelector('#preview').tolglowPreview.getTelemetry()?.passProbe;
        return p?.mode === choice && (choice !== 'present-cached-frame' || 'replay' in p.submitMs);
      }, pass, {timeout:90000});
      await delay(2500);
      await page.evaluate(beginPassSample, {mode: pass, defer: true});
    }
    const diagnosticBefore = pass ? await probeWorker.evaluate(() => self.getRoomPassState()) : null;
    const state = async () => (await (await fetch(origin + '/api/state')).json()).output?.values?.['head-1'];
    const warmupMs = Date.now() - warmStarted;
    const settings = () => page.evaluate(() => {
      const preview = document.querySelector('#preview').tolglowPreview, t = preview.getTelemetry();
      return {quality: document.querySelector('[aria-label="Preview quality"]').value,
        lightingMode: t.lightingMode, workLight: t.workLight, fog: t.fogAmount};
    });
    const settingsBefore = await settings();
    const before = await state();
    const plumeWarmup = emitPlumes ? await page.evaluate(() => {
      const receipt = window.roomPlumeSample, warmup = {accepted: receipt.accepted, rejected: receipt.rejected};
      receipt.accepted = 0; receipt.rejected = 0; return warmup;
    }) : null;
    await checkDesktop('before-sample-'+name+'-'+(pass || 'production'));
    const sessionWatch = watchDesktopSession();
    const sessionReceipt = {name, pass, checks: sessionWatch.checks};
    metadata.desktopSession.samples.push(sessionReceipt);
    let measured, middle, sampleTimeout;
    try {
      [measured, middle] = await Promise.race([
        Promise.all([page.evaluate(measureRoom, 20000), delay(7100).then(state)]),
        sessionWatch.failed.then(() => {throw new Error('Desktop session became locked or unknown during sample');}),
        new Promise((_, reject) => {sampleTimeout = setTimeout(() => reject(new Error('Room sample exceeded 25 s deadline')), 25000);})
      ]);
    } finally {
      clearTimeout(sampleTimeout);
      await sessionWatch.stop();
      sessionReceipt.invalid = Boolean(sessionWatch.failure);
      if (sessionWatch.failure) sessionReceipt.failure = sessionWatch.failure;
    }
    if (sessionWatch.failure) throw new Error('Desktop session qualification failed at sample completion');
    const after = await state();
    const settingsAfter = await settings();
    measured.controls = {before: settingsBefore, after: settingsAfter};
    if (emitPlumes) {
      measured.plumes = await page.evaluate(() => {
        const {timer, ...receipt} = window.roomPlumeSample;
        clearInterval(timer); delete window.roomPlumeSample; return receipt;
      });
      Object.assign(measured.plumes, {options: plume, intervalMs: 4000, warmup: plumeWarmup});
      if (measured.plumes.rejected || measured.plumes.accepted < 5)
        measured.invalid.push('the production preview did not accept repeated local plumes');
      if (!measured.policySamples.some(s => s.fogMaximumDensity > .35))
        measured.invalid.push('the plume sample did not exercise dense local fog');
    }
    if ([settingsBefore, settingsAfter].some(s => s.quality !== quality || s.lightingMode !== lightingMode || s.workLight !== workLight || s.fog !== fog))
      measured.invalid.push('preview controls differ from the requested benchmark settings');
    if (viewChanged(measured.start, measured.end))
      measured.invalid.push('camera changed during the controlled sample');
    if (controlledComparison) {
      comparisonView ??= measured.start;
      if (viewChanged(comparisonView, measured.start))
        measured.invalid.push('camera differs from the first comparison sample');
    }
    if (pass) {
      measured.pass = pass;
      if (!measured.passDiagnostic) throw new Error('Browser sample did not return its prepared diagnostic collector');
      const {timings, accepted, raymarched, traversal, queryFrames} = measured.passDiagnostic;
      delete measured.passDiagnostic;
      measured.passTimingsMs = timings;
      measured.passEvidence = {accepted, raymarched, traversal};
      measured.queryFrames = queryFrames;
      if (accepted !== measured.presented.frames) measured.invalid.push('diagnostic and cadence frame boundaries differ');
      if (pass !== 'present-cached-frame' && timings.fogSubmitMs?.samples !== accepted)
        measured.invalid.push('fog renderer instrumented for only part of the sample');
      const expectedRaymarch = !['skip-fog-raymarch', 'present-cached-frame'].includes(pass);
      if (raymarched !== (expectedRaymarch ? accepted : 0))
        measured.invalid.push('actual fog volume draws differ from selected diagnostic mode');
      if (pass === 'present-cached-frame' && (timings.replaySubmitMs?.samples !== accepted ||
          ['opaqueSubmitMs', 'fogSubmitMs', 'postSubmitMs'].some(key => timings[key]?.samples)))
        measured.invalid.push('cached display control included ordinary rendering work');
      if (!measured.passTimingsMs.domSubmitMs?.samples) measured.invalid.push('no diagnostic presentations');
      measured.diagnosticBoundaries = {before: diagnosticBefore,
        after: await probeWorker.evaluate(() => self.getRoomPassState())};
      if (frozenAuto) {
        const same = p => JSON.stringify(p) === JSON.stringify(frozenPolicy);
        if (!Object.values(measured.diagnosticBoundaries).every(s => same(s.policy) && same(s.frozenPolicy)) ||
            traversal.missingPolicyFrames || !traversal.policies.every(same) ||
            measured.qualities.some(q => q !== frozenPolicy.tier) || measured.scales.some(k => k !== frozenPolicy.scale) ||
            measured.sceneSizes.some(size => JSON.stringify(size) !== JSON.stringify(frozenPolicy.scene)))
          measured.invalid.push('frozen Auto policy or raster changed during the diagnostic');
      }
      if (controlledComparison && (traversal.detailedFrames || !Number.isFinite(traversal.maximumDensity) || traversal.maximumDensity >= .25 ||
          measured.policySamples.some(s => s.fogMaximumDensity >= .25 || s.fogActiveSources !== 0) ||
          Object.values(measured.diagnosticBoundaries).some(s => s.maximumDensity >= .25 || s.detailActive)))
        measured.invalid.push('controlled comparison was not confined to dispersed local haze');
      if (timerComparison && (queryFrames.length !== accepted || queryFrames.some(q => !q.supported ||
          !Number.isFinite(q.readyForFrameMs) || !Number.isFinite(q.rafDeliveryMs) ||
          (pass === 'full-no-timers' ? q.enabled || q.started !== 0 || q.apiCalls !== 0 : !q.enabled || q.apiCalls < 1)) ||
          (pass !== 'full-no-timers' && !queryFrames.some(q => q.started > 0))))
        measured.invalid.push('timer query control or frame-readiness evidence was incomplete');
      if (!requestTimerExtension && (queryFrames.length !== accepted || queryFrames.some(q =>
          q.extensionRequested !== false || q.enabled || q.supported || q.started !== 0 || q.apiCalls !== 0)))
        measured.invalid.push('controlled comparison enabled diagnostic timer queries');
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
    if (plumeScene) await scenario('moving-heads-active-plumes', .7, true, null, true);
  }
  metadata.qualified = metadata.scenarios.every(s => !s.invalid.length && !s.end.softwareRenderer);
  metadata.cadenceQualified = metadata.qualified && !passes &&
    metadata.scenarios.every(s => s.cadence.withinFivePercentOfConfigured === true);
  await writeFile(join(outputDir, 'room.json'), JSON.stringify(metadata, null, 2));
  completed = true;
  if (!metadata.qualified) process.exitCode = 2;
} catch (error) {
  stopServer();
  metadata.qualified = metadata.cadenceQualified = false;
  metadata.error = String(error);
  if (page && !page.isClosed() && !metadata.desktopSession.samples.some(s => s.invalid) &&
      !metadata.desktopSession.checks.some(s => s.status !== 'unlocked')) {
    let telemetryTimeout;
    try {
      metadata.startupTelemetry = await Promise.race([page.evaluate(() => {
        const t = document.querySelector('#preview')?.tolglowPreview?.getTelemetry();
        return t ? {thread:t.thread, fallback:t.fallbackReason, failed:t.failed, ready:t.ready, quality:t.quality} : null;
      }), new Promise(resolve => {telemetryTimeout = setTimeout(() => resolve(null), 1000);})]);
    } catch {} finally {clearTimeout(telemetryTimeout);}
  }
  await writeFile(join(outputDir, 'room-error.json'), JSON.stringify(metadata, null, 2));
  console.error(error, metadata.startupTelemetry || '', metadata.browserStartup || ''); process.exitCode = 1;
} finally {
  await shutdown();
  if (metadata.cleanupTimedOut) await writeFile(join(outputDir, 'room-cleanup.json'), JSON.stringify({
    checkedAt: new Date().toISOString(), ownedBrowserCleanupTimedOut: true, ownedHostStopRequested: true}, null, 2));
  // Closing this process disconnects CDP. Never issue Browser.close or close
  // the shared default context; the operator's other Chrome tabs stay open.
  process.exit(process.exitCode || 0);
}
