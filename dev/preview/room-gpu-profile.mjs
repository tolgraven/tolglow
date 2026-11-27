#!/usr/bin/env node
// Engineering GPU attribution for the Room scene renderer (#782).
//
//   node dev/preview/room-gpu-profile.mjs [--label before] \
//     [--sizes 960x540,1920x1080] [--fixtures 16,64] [--configs full,no-fog] \
//     [--out target/room-gpu] [--port 19180] [--rounds 3] [--repeats 2] [--screenshots]
//
// It bundles dev/preview/room-gpu-profile-page.mjs, which draws the source
// SceneRenderer on a synthetic rig without a host, show, device or output.
// Each configuration toggles one pass; per-pass cost is the difference of
// wall-clock frame costs (see the page for why fences/timer queries are not
// used). Headless Chrome on the real GPU (ANGLE Metal) is engineering evidence
// only. It is not a foreground display, compositor, iPad Safari or
// qualification measurement, and it neither uses nor bypasses the guards in
// dev/preview/desktop-session.mjs or measure-room.mjs.
import {build} from 'esbuild';
import {chromium} from 'playwright-core';
import {createServer} from 'node:http';
import {mkdir, writeFile, rm} from 'node:fs/promises';
import {existsSync} from 'node:fs';
import {fileURLToPath} from 'node:url';
import path from 'node:path';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const arg = (name, fallback) => {
  const index = process.argv.indexOf('--' + name);
  return index < 0 ? fallback : process.argv[index + 1];
};
const flag = name => process.argv.includes('--' + name);
const label = arg('label', 'profile');
const sizes = arg('sizes', '960x540,1440x900,1920x1080,2560x1440').split(',').map(s => s.split('x').map(Number));
const fixtureCounts = arg('fixtures', '16,32,64,128').split(',').map(Number);
const out = path.resolve(root, arg('out', 'target/room-gpu'));
// Bind 127.0.0.1 only; pick a port your lane owns (ROOM_GPU_PORT or --port).
const port = Number(arg('port', process.env.ROOM_GPU_PORT || 19180));
if (!Number.isInteger(port) || port < 1024 || port > 65535) throw new Error('Use --port with an unprivileged port');
const frames = Number(arg('frames', 40)), repeats = Number(arg('repeats', 2)), rounds = Number(arg('rounds', 3));

// Named configurations, each applied to an otherwise identical scene. A
// configuration's `delta` names the pass whose cost is full minus that run.
export const CONFIGS = {
  full: {},
  'full-repeat': {},
  moving: {moving: true},
  'no-fog': {fog: 0, delta: 'fog (raymarch + composite)'},
  'no-bloom': {bloom: false, delta: 'bloom'},
  'no-shadows': {shadows: 0, delta: 'shadow maps + shadow sampling'},
  'no-spots': {spots: 0, delta: 'forward spotlights in opaque shading'},
  'no-fxaa': {fxaa: false, delta: 'FXAA'},
  msaa0: {samples: 0, delta: 'MSAA'},
  'fog-steps-24': {raySteps: 24, delta: 'fog steps 48 to 24'},
  'fog-scale-25': {fogScale: .25, delta: 'fog raster .5 to .25'},
  'fog-1mp': {fogPixels: 1000000},
  'fog-2mp': {fogPixels: 2000000},
  'lights-0': {fullLights: 0},
  'no-faked-beams': {fakedBeams: false, delta: 'faked beam cones'},
  'no-footprints': {footprints: false, delta: 'footprint atlas + material sampling'},
  'lights-8': {fullLights: 8},
  'lights-16': {fullLights: 16},
  'moving-lights-8': {moving: true, fullLights: 8},
  haze: {fogMode: 'haze'},
  raymarch: {fogMode: 'raymarch'},
  'tier-low': {tier: 'low'},
  'tier-mobile': {tier: 'mobile'},
  'tier-high': {tier: 'high'},
};
// Configurations compose with '+', e.g. lights-16+no-fog.
const configNames = arg('configs', 'full').split(',');
const configFor = name => name.split('+').reduce((merged, part) => {
  if (!CONFIGS[part]) throw new Error('Unknown config ' + part);
  return {...merged, ...CONFIGS[part], delta: name.includes('+') ? undefined : CONFIGS[part].delta};
}, {});
for (const name of configNames) configFor(name);

const executablePath = process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
if (!existsSync(executablePath)) throw new Error('Set CHROMIUM_PATH to a Chrome executable');

// --source <checkout> profiles another checkout's js/preview (e.g. the
// read-only origin/master copy) with this page, for before/after runs.
const source = arg('source', null);
const redirect = source ? [{name: 'preview-source', setup(build) {
  build.onResolve({filter: /\/js\/preview\//}, args => {
    const resolved = path.resolve(args.resolveDir, args.path);
    const relative = path.relative(path.join(root, 'js/preview'), resolved);
    return relative.startsWith('..') ? undefined : {path: path.join(path.resolve(source), 'js/preview', relative)};
  });
}}] : [];
const bundle = await build({entryPoints: [path.join(root, 'dev/preview/room-gpu-profile-page.mjs')],
  bundle: true, format: 'esm', platform: 'browser', target: ['es2022'], write: false, logLevel: 'warning',
  plugins: redirect, nodePaths: [path.join(root, 'node_modules')]});
const server = createServer((request, response) => {
  const script = request.url === '/page.js';
  response.setHeader('Content-Type', script ? 'text/javascript' : 'text/html');
  response.end(script ? bundle.outputFiles[0].text
    : '<!doctype html><meta charset="utf-8"><body style="margin:0;background:#000"><script type="module" src="/page.js"></script>');
});
await new Promise(resolve => server.listen(port, '127.0.0.1', resolve));

const userDataDir = path.join(root, `target/room-gpu-chrome-${port}`);
await mkdir(out, {recursive: true});
let context;
const results = {label, source: source ? path.resolve(source) : root,
  method: 'wall-clock cost of back-to-back presented frames ending in one GPU fence (median of batches); pass cost = full minus the run with that pass toggled off',
  conditions: (flag('force-raster') ? 'every tier forced to the requested raster and 128 meshes; ' : 'production tier rasters and mesh caps; ') + 'headless Chrome (--headless=new), ANGLE Metal, OffscreenCanvas + bitmaprenderer on the page thread, device scale 1, synthetic rig, audience camera, fog .7 after an 80-step manual burst',
  startedAt: new Date().toISOString(), runs: []};
try {
  context = await chromium.launchPersistentContext(userDataDir, {executablePath, headless: true,
    args: ['--headless=new', '--use-angle=metal', '--enable-gpu', '--ignore-gpu-blocklist',
      '--disable-gpu-vsync', '--disable-frame-rate-limit', '--disable-background-timer-throttling',
      '--disable-renderer-backgrounding'], viewport: {width: 800, height: 600}, deviceScaleFactor: 1});
  const page = context.pages()[0] || await context.newPage();
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  page.on('console', message => { if (message.type() === 'error') errors.push(message.text()); });
  await page.goto(`http://127.0.0.1:${port}/`);
  await page.waitForFunction(() => typeof window.setupRoom === 'function');
  if (flag('match') || flag('crossfade')) {
    const [width, height] = sizes[0];
    await page.evaluate(options => window.setupRoom(options), {width, height, fixtures: fixtureCounts[0]});
    if (flag('match')) {
      const {rows, images} = await page.evaluate(() => window.matchProbe());
      results.match = rows;
      for (const [name, url] of Object.entries(images)) await writeFile(path.join(out, `${label}-match-${name}.png`), Buffer.from(url.split(',')[1], 'base64'));
      for (const row of results.match) console.log(`density ${row.density}: luma full ${row.lumaFull.toFixed(1)} faked ${row.lumaFaked.toFixed(1)}, max diff ${row.maxDiff.toFixed(0)}, >16: ${(100 * row.over16).toFixed(1)}%`);
    }
    if (flag('crossfade')) {
      const frames = await page.evaluate(() => window.crossfadeFrames());
      results.crossfade = frames.map(({url, ...rest}, i) => rest);
      for (const [i, frame] of frames.entries()) {
        await writeFile(path.join(out, `${label}-crossfade-${String(i).padStart(2, '0')}.png`), Buffer.from(frame.url.split(',')[1], 'base64'));
        console.log(`frame ${i}: ${frame.target} weight ${frame.weight.toFixed(2)} ${JSON.stringify(frame.slots)}`);
      }
    }
    await page.evaluate(() => window.disposeRoom());
  } else for (const [width, height] of sizes) for (const fixtures of fixtureCounts) {
    const setup = await page.evaluate(options => window.setupRoom(options), {width, height, fixtures, force: flag('force-raster')});
    if (!results.renderer) { results.renderer = setup.renderer; results.userAgent = setup.userAgent; }
    const inventory = await page.evaluate(() => window.inventory());
    // Interleave configurations across rounds so slow drift (thermal state,
    // other GPU clients) spreads over all of them; report per-config medians.
    const group = new Map();
    for (let round = 0; round < rounds; round++) for (const name of configNames) {
      const run = await page.evaluate(config => window.profileRoom(config), {...configFor(name), name, frames, repeats});
      if (!group.has(name)) group.set(name, []);
      group.get(name).push(run);
    }
    const median = values => { const v = [...values].sort((a, b) => a - b), m = v.length >> 1; return v.length % 2 ? v[m] : (v[m - 1] + v[m]) / 2; };
    let full;
    for (const name of configNames) {
      const runs = group.get(name), run = {...runs[runs.length - 1]};
      run.frameMs = median(runs.map(r => r.frameMs)); run.rounds = runs.map(r => r.frameMs); run.cpuDrawMs = median(runs.map(r => r.cpuDrawMs));
      run.name = name; run.size = [width, height]; run.fixtures = fixtures;
      if (name === configNames[0]) run.inventory = inventory;
      if (name === 'full') full = run;
      results.runs.push(run);
      const delta = full && configFor(name).delta ? ` (${configFor(name).delta}: ${(full.frameMs - run.frameMs).toFixed(2)} ms)` : '';
      console.log(`${width}x${height} n=${fixtures} ${name}: ${run.frameMs.toFixed(2)} ms/frame [${run.rounds.map(v => v.toFixed(1)).join(' ')}]${delta}`);
    }
    if (flag('screenshots')) for (const name of configNames) {
      if (configFor(name).delta || name.includes('+')) continue;
      await page.evaluate(config => window.profileRoom(config), {...configFor(name), name, frames: 2, repeats: 1, warm: 4});
      const png = await page.evaluate(() => window.screenshot());
      await writeFile(path.join(out, `${label}-${width}x${height}-n${fixtures}-${name}.png`), Buffer.from(png.split(',')[1], 'base64'));
    }
    await page.evaluate(() => window.disposeRoom());
  }
  results.errors = errors;
  if (errors.length) console.error('Page errors:', errors);
} finally {
  await context?.close();
  await new Promise(resolve => server.close(resolve));
  await rm(userDataDir, {recursive: true, force: true});
}
results.finishedAt = new Date().toISOString();
const file = path.join(out, `${label}.json`);
await writeFile(file, JSON.stringify(results, null, 1));
console.log('Wrote', path.relative(root, file));
