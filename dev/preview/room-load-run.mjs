// Compare a released Room bundle with this checkout's on one machine. Serves
// both bundles and `room-load.mjs` on loopback, starts its own Chrome profile
// and alternates the builds through each scenario. No show, server or fixture
// output is involved.
//
//   node dev/preview/room-load-run.mjs                    # headless Chrome
//   node dev/preview/room-load-run.mjs --headed --position 0,0 --window 1172x927
//
// --baseline   git ref or directory holding the released bundles (origin/master)
// --candidate  directory holding the bundles under test (resources/studio)
// --scenario   heads=64,quality=auto,busy=45/50 and any `installRoomLoad` option;
//              repeat for several. `busy` blocks the page WORK of every PERIOD ms.
// --order      builds per scenario (old,new,new,old)
// --warm, --measure  milliseconds (4000, 8000); --port (4151); --out report.json
// --window WxH, --dpr N  CSS viewport and device scale; a headed window keeps
//              the scale of the display it opens on.
//
// Headless Chrome has a synthetic 60 Hz frame source and scans nothing out, so
// its numbers describe rendering and canvas submission only. A headed run must
// stay the focused, visible window for its whole duration; the report says
// whether it did, and which display rate the page and the worker measured.
import {execFileSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {existsSync, statSync} from 'node:fs';
import {readFile, writeFile} from 'node:fs/promises';
import {createServer} from 'node:http';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const flags = {scenario: []};
for (let args = process.argv.slice(2), i = 0; i < args.length; i++) {
  const name = args[i].replace(/^--/, '');
  if (name === 'headed') flags.headed = true;
  else if (name === 'scenario') flags.scenario.push(args[++i]);
  else flags[name] = args[++i];
}
const root = process.cwd(), files = ['preview.js', 'preview-worker.js'];
const git = (...args) => execFileSync('git', args, {cwd: root, maxBuffer: 64 << 20});
const baseline = flags.baseline || 'origin/master', candidate = resolve(flags.candidate || 'resources/studio');
const directory = path => existsSync(path) && statSync(path).isDirectory();
const load = async source => Object.fromEntries(await Promise.all(files.map(async file =>
  [file, directory(source) ? await readFile(join(source, file)) : git('show', `${source}:resources/studio/${file}`)])));
const bundles = {old: await load(baseline), new: await load(candidate)};
const sha = bytes => createHash('sha256').update(bytes).digest('hex').slice(0, 12);
const describe = build => Object.fromEntries(files.map(file => [file, sha(bundles[build][file])]));
const harness = await readFile(new URL('./room-load.mjs', import.meta.url));

const parse = text => Object.fromEntries(text.split(',').map(pair => {
  const [key, value] = pair.split('=');
  if (key === 'busy') {const [work, period] = value.split('/').map(Number); return [key, {work, period}];}
  return [key, /^-?[\d.]+$/.test(value) ? Number(value) : value === 'true' ? true : value === 'false' ? false : value];
}));
const scenarios = (flags.scenario.length ? flags.scenario : ['heads=8,quality=desktop', 'heads=8,quality=desktop,busy=45/50',
  'heads=64,quality=desktop', 'heads=64,quality=desktop,busy=45/50', 'heads=64,quality=auto']).map(text => ({text, options: parse(text)}));
const order = (flags.order || 'old,new,new,old').split(',');
const [width, height] = (flags.window || '1172x927').split('x').map(Number);
const warm = Number(flags.warm || 4000), measure = Number(flags.measure || 8000), port = Number(flags.port || 4151);

const server = createServer((request, response) => {
  const [, build, file] = request.url.match(/^\/(old|new)\/([\w.-]+)$/) || [];
  response.setHeader('Cache-Control', 'no-store');
  response.setHeader('Content-Type', request.url === '/' ? 'text/html' : 'text/javascript');
  if (request.url === '/') response.end('<!doctype html><title>Room load</title><body></body>');
  else if (request.url === '/room-load.mjs') response.end(harness);
  else if (bundles[build]?.[file]) response.end(bundles[build][file]);
  else {response.statusCode = 404; response.end();}
});
await new Promise(listening => server.listen(port, '127.0.0.1', listening));

const executablePath = process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
const args = flags.headed ? [`--window-size=${width},${height + 90}`, `--window-position=${flags.position || '0,0'}`]
  : ['--headless=new', '--enable-gpu', ...(process.platform === 'darwin' ? ['--use-angle=metal'] : [])];
const context = await chromium.launchPersistentContext(resolve(flags.profile || 'target/room-load-chrome'), {executablePath,
  headless: !flags.headed, args, ...(flags.headed ? {viewport: null} : {viewport: {width, height}, deviceScaleFactor: Number(flags.dpr || 2)})});
const report = {revision: git('rev-parse', 'HEAD').toString().trim(), baseline: directory(baseline) ? resolve(baseline) : git('rev-parse', baseline).toString().trim(),
  bundles: {old: describe('old'), new: describe('new')}, headed: !!flags.headed, platform: `${process.platform} ${process.arch}`, warmMs: warm, measureMs: measure, runs: []};
try {
  report.browser = context.browser()?.version();
  for (const scenario of scenarios) for (const build of order) {
    const page = await context.newPage();
    const errors = []; page.on('pageerror', error => errors.push(error.message));
    await page.bringToFront(); await page.goto(`http://127.0.0.1:${port}/`);
    const result = await page.evaluate(async ({base, options, warm, measure}) => {
      const {installRoomLoad} = await import('/room-load.mjs');
      const load = await installRoomLoad({base, ...options});
      const ready = await Promise.race([load.engine.whenReady, new Promise(late => setTimeout(() => late('no frame in 60 s'), 60000))]);
      // Auto's tier and scale over the whole run, as the points where either changed.
      const trace = [], started = performance.now(), sample = () => {
        const t = load.engine.getTelemetry(), last = trace.at(-1);
        if (!last || last[1] !== t.quality || last[2] !== t.renderScale) trace.push([Math.round(performance.now() - started), t.quality, t.renderScale]);
      };
      const timer = setInterval(sample, 250); sample();
      const result = await load.measure(measure, warm);
      clearInterval(timer); load.dispose();
      return {...result, ready, trace, hdrDisplay: matchMedia('(dynamic-range: high)').matches, userAgent: navigator.userAgent};
    }, {base: `/${build}/`, options: scenario.options, warm, measure});
    await page.close();
    report.runs.push({scenario: scenario.text, build, errors, ...result});
    console.error(`${scenario.text} ${build}: ${result.presentedHz}/s gap ${result.frameGapP50}/${result.frameGapP95} ms, ${result.tier} x${result.scale}, display ${result.displayHz} Hz, ${result.presentation}${errors.length ? ' ERRORS ' + errors.join('; ') : ''}`);
  }
} finally {await context.close(); await new Promise(closed => server.close(closed));}

const cell = value => value == null ? '–' : String(value);
const lines = ['| Scenario | Build | Accepted/s | Distinct refreshes/s | Gap p50 / p95 / p99 ms | Same-task pairs | Display Hz believed (target) | Page rAF Hz | Tier × scale, scene | Presents |',
  '| --- | --- | ---: | ---: | --- | ---: | --- | ---: | --- | --- |'];
for (const run of report.runs) lines.push(`| ${run.scenario} | ${run.build} | ${cell(run.presentedHz)} | ${cell(run.distinctRefreshHz)} | ${cell(run.frameGapP50)} / ${cell(run.frameGapP95)} / ${cell(run.frameGapP99)} | ${cell(run.gapsUnder2Ms)} | ${cell(run.displayHz)} (${cell(run.targetHz)}) | ${cell(run.rafHz)} | ${run.tier} × ${run.scale}, ${run.scene?.join(' × ')} | ${run.thread === 'worker' ? run.presentation : 'main thread'} |`);
const first = report.runs[0] || {};
console.log(`${report.browser}, ${first.deviceRenderer}${first.software ? ' (software)' : ''}, ${report.headed ? 'headed' : 'headless'}, ` +
  `${first.css?.join(' × ')} CSS at DPR ${first.dpr}, focused ${report.runs.every(run => run.focused)}, hidden ${report.runs.some(run => run.hidden)}`);
console.log(`candidate ${report.revision.slice(0, 8)} ${JSON.stringify(report.bundles.new)}, baseline ${String(report.baseline).slice(0, 8)} ${JSON.stringify(report.bundles.old)}\n`);
console.log(lines.join('\n'));
if (flags.out) await writeFile(flags.out, JSON.stringify(report, null, 1));
