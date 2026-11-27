#!/usr/bin/env node
// Engineering GPU attribution for the Room scene renderer (#782).
//
//   node dev/preview/room-gpu-profile.mjs [--label before] \
//     [--sizes 960x540,1920x1080] [--fixtures 16,64] [--configs full,no-fog] \
//     [--out target/room-gpu] [--port 19180] [--screenshots]
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
const port = Number(arg('port', 19180));
if (!(port >= 19180 && port <= 19199)) throw new Error('Use a port in 19180-19199');
const frames = Number(arg('frames', 40)), repeats = Number(arg('repeats', 5));

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
  'lights-0': {fullLights: 0},
  'no-faked-beams': {fakedBeams: false, delta: 'faked beam cones'},
  'no-footprints': {footprints: false, delta: 'footprint atlas + material sampling'},
  'lights-8': {fullLights: 8},
  'lights-16': {fullLights: 16},
  'moving-lights-8': {moving: true, fullLights: 8},
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

const bundle = await build({entryPoints: [path.join(root, 'dev/preview/room-gpu-profile-page.mjs')],
  bundle: true, format: 'esm', platform: 'browser', target: ['es2022'], write: false, logLevel: 'warning'});
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
const results = {label,
  method: 'wall-clock cost of back-to-back presented frames ending in one GPU fence (median of batches); pass cost = full minus the run with that pass toggled off',
  conditions: 'headless Chrome (--headless=new), ANGLE Metal, OffscreenCanvas + bitmaprenderer on the page thread, device scale 1, every tier forced to the requested raster, synthetic rig, audience camera, fog .7 after an 80-step manual burst',
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
  for (const [width, height] of sizes) for (const fixtures of fixtureCounts) {
    const setup = await page.evaluate(options => window.setupRoom(options), {width, height, fixtures});
    if (!results.renderer) { results.renderer = setup.renderer; results.userAgent = setup.userAgent; }
    const inventory = await page.evaluate(() => window.inventory());
    const group = [];
    for (const name of configNames) {
      const run = await page.evaluate(config => window.profileRoom(config), {...configFor(name), name, frames, repeats});
      run.name = name; run.size = [width, height]; run.fixtures = fixtures;
      if (!group.length) run.inventory = inventory;
      group.push(run); results.runs.push(run);
      const full = group.find(r => r.name === 'full');
      const delta = full && configFor(name).delta ? ` (${configFor(name).delta}: ${(full.frameMs - run.frameMs).toFixed(2)} ms)` : '';
      console.log(`${width}x${height} n=${fixtures} ${name}: ${run.frameMs.toFixed(2)} ms/frame, cpu draw ${run.cpuDrawMs.toFixed(2)} ms${delta}`);
      if (flag('screenshots') && !configFor(name).delta && !name.includes('+')) {
        const png = await page.evaluate(() => window.screenshot());
        await writeFile(path.join(out, `${label}-${width}x${height}-n${fixtures}-${name}.png`), Buffer.from(png.split(',')[1], 'base64'));
      }
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
