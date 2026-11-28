#!/usr/bin/env node
// Footprint atlas culling cost and equivalence (#782).
//
//   node dev/preview/footprint-cull-bench.mjs [--before path/to/footprints.js] \
//     [--frames 400] [--browser] [--port 19181] [--out target/footprint-cull]
//
// Times `footprintTileLights` (the per-redraw CPU culling of the footprint
// atlas) in node on synthetic moving rigs, and with --before compares every
// per-tile light list against another revision's module (for example the
// read-only base worktree's js/preview/footprints.js). --browser bundles both
// modules and repeats the timing in headless Chrome on the real GPU (ANGLE
// Metal), renders each module's FootprintAtlas into its own WebGL target and
// compares the read-back atlas texels. Headless Chrome is engineering
// evidence only, not foreground display or iPad Safari evidence.
import {build} from 'esbuild';
import {existsSync} from 'node:fs';
import {mkdir, rm, writeFile} from 'node:fs/promises';
import {createServer} from 'node:http';
import path from 'node:path';
import {fileURLToPath, pathToFileURL} from 'node:url';
import {PerformanceObserver} from 'node:perf_hooks';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const arg = (name, fallback) => {
  const index = process.argv.indexOf('--' + name);
  return index < 0 ? fallback : process.argv[index + 1];
};
const flag = name => process.argv.includes('--' + name);
const after = path.resolve(root, 'js/preview/footprints.js');
const before = arg('before', null) && path.resolve(arg('before'));
const frames = Number(arg('frames', 400));
const port = Number(arg('port', 19181));
const out = path.resolve(root, arg('out', 'target/footprint-cull'));

import {SCENES, scenario} from './footprint-scenario.mjs';

function time(module, scene, sequence, repeats = 3) {
  const stage = module.stageBounds(scene.room);
  for (const lights of sequence.slice(0, 50)) module.footprintTileLights(lights, scene.room, stage);
  const samples = [];
  for (let r = 0; r < repeats; r++) {
    const started = performance.now();
    for (const lights of sequence) module.footprintTileLights(lights, scene.room, stage);
    samples.push((performance.now() - started) / sequence.length);
  }
  samples.sort((a, b) => a - b);
  return samples[Math.floor(samples.length / 2)];
}

function compare(a, b, scene, sequence) {
  const stage = a.stageBounds(scene.room);
  let mismatches = 0, first = null, entries = 0;
  sequence.forEach((lights, frame) => {
    const x = a.footprintTileLights(lights, scene.room, stage), y = b.footprintTileLights(lights, scene.room, stage);
    x.forEach((list, tile) => {
      entries += list.length;
      if (list.length !== y[tile].length || list.some((v, i) => v !== y[tile][i])) {
        mismatches++; first ||= {frame, tile, before: [...list], after: [...y[tile]]};
      }
    });
  });
  return {mismatches, entries, first};
}

/** Minor/major GCs during `count` calls of the atlas culling path. */
async function gcDuring(module, scene, sequence) {
  if (!module.FootprintAtlas || !module.cullFootprintTiles) return null;
  const planes = module.footprintTilePlanes(scene.room, module.stageBounds(scene.room));
  const indices = new Int32Array(6 * 128), counts = new Int32Array(6);
  for (let r = 0; r < 4; r++) for (const lights of sequence) module.cullFootprintTiles(lights, lights.length, planes, indices, counts);
  let collections = 0;
  const observer = new PerformanceObserver(list => {collections += list.getEntries().length;});
  observer.observe({entryTypes: ['gc']});
  for (let r = 0; r < 20; r++) for (const lights of sequence) module.cullFootprintTiles(lights, lights.length, planes, indices, counts);
  await new Promise(resolve => setTimeout(resolve, 50));
  observer.disconnect();
  return {calls: 20 * sequence.length, collections};
}

async function nodeRun() {
  const next = await import(pathToFileURL(after));
  const previous = before ? await import(pathToFileURL(before)) : null;
  const report = [];
  for (const scene of SCENES) {
    const sequence = scenario(scene.room, scene.lights, frames);
    const row = {scene: scene.name, afterMs: time(next, scene, sequence)};
    if (previous) {
      row.beforeMs = time(previous, scene, sequence);
      row.equivalence = compare(previous, next, scene, sequence);
    }
    row.gc = await gcDuring(next, scene, sequence);
    report.push(row);
  }
  return report;
}

const page = `
import * as previous from 'before';
import * as next from 'after';
import * as THREE from 'three';
import {SCENES, scenario} from 'scenario';
const median = values => values.sort((a, b) => a - b)[Math.floor(values.length / 2)];
function time(module, scene, sequence) {
  const stage = module.stageBounds(scene.room), samples = [];
  for (const lights of sequence.slice(0, 50)) module.footprintTileLights(lights, scene.room, stage);
  for (let r = 0; r < 3; r++) {
    const started = performance.now();
    for (const lights of sequence) module.footprintTileLights(lights, scene.room, stage);
    samples.push((performance.now() - started) / sequence.length);
  }
  return median(samples);
}
function atlas(module, renderer, scene, lights) {
  const atlas = new module.FootprintAtlas(renderer, {capacity: 128});
  atlas.setRoom(scene.room); atlas.update(lights);
  const started = performance.now(); atlas.render(0); const js = performance.now() - started;
  const {width, height} = atlas.target, pixels = new Uint16Array(width * height * 4);
  renderer.readRenderTargetPixels(atlas.target, 0, 0, width, height, pixels);
  // Moving redraws: JS time of render() alone, the culling plus six draws.
  // The mean, since this page's clock is coarsened to about 0.1 ms.
  const redraw = [];
  for (let i = 0; i < 40; i++) {
    atlas.update(scene.sequence[i % scene.sequence.length]);
    const t0 = performance.now(); atlas.render(1000 * (i + 1)); redraw.push(performance.now() - t0);
  }
  renderer.getContext().finish();
  atlas.dispose();
  return {pixels, js, redrawMs: redraw.reduce((a, b) => a + b, 0) / redraw.length, draws: atlas.tileDraws};
}
window.run = async frames => {
  const canvas = new OffscreenCanvas(16, 16);
  const renderer = new THREE.WebGLRenderer({canvas, antialias: false});
  const report = [];
  for (const scene of SCENES) {
    const sequence = scenario(scene.room, scene.lights, frames);
    const row = {scene: scene.name, beforeMs: time(previous, scene, sequence), afterMs: time(next, scene, sequence)};
    let differing = 0, compared = 0, maxDifference = 0;
    for (const index of [0, 37, 111, frames - 1]) {
      const lights = sequence[index]; scene.sequence = sequence;
      const a = atlas(previous, renderer, scene, lights), b = atlas(next, renderer, scene, lights);
      compared += a.pixels.length;
      for (let i = 0; i < a.pixels.length; i++) if (a.pixels[i] !== b.pixels[i]) {
        differing++; maxDifference = Math.max(maxDifference, Math.abs(a.pixels[i] - b.pixels[i]));
      }
      row.redrawBeforeMs = a.redrawMs; row.redrawAfterMs = b.redrawMs;
      row.nonzero = (row.nonzero || 0) + a.pixels.reduce((n, v, i) => n + (i % 4 !== 3 && v !== 0 ? 1 : 0), 0);
    }
    Object.assign(row, {comparedTexelChannels: compared, differing, maxDifference});
    report.push(row);
  }
  renderer.dispose();
  return {renderer: (() => {const gl = renderer.getContext(), ext = gl.getExtension('WEBGL_debug_renderer_info');
    return ext ? gl.getParameter(ext.UNMASKED_RENDERER_WEBGL) : gl.getParameter(gl.RENDERER);})(), report};
};
`;

async function browserRun() {
  if (!before) throw new Error('--browser compares two modules: pass --before');
  const executablePath = process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
  if (!existsSync(executablePath)) throw new Error('Set CHROMIUM_PATH');
  const bundle = await build({stdin: {contents: page, resolveDir: root, loader: 'js'}, bundle: true, format: 'esm',
    platform: 'browser', target: ['es2022'], write: false, logLevel: 'error',
    alias: {before, after, scenario: path.join(root, 'dev/preview/footprint-scenario.mjs')}});
  const {chromium} = await import('playwright-core');
  await mkdir(out, {recursive: true});
  const profile = path.join(out, 'chrome-profile');
  const server = createServer((request, response) => {
    if (request.url === '/bench.js') {response.writeHead(200, {'content-type': 'text/javascript'}); response.end(bundle.outputFiles[0].text);}
    else {response.writeHead(200, {'content-type': 'text/html'}); response.end('<script type="module" src="/bench.js"></script>');}
  });
  await new Promise(resolve => server.listen(port, '127.0.0.1', resolve));
  const context = await chromium.launchPersistentContext(profile, {executablePath, headless: true,
    args: ['--headless=new', '--use-angle=metal', '--enable-gpu', '--ignore-gpu-blocklist']});
  try {
    const tab = await context.newPage(), errors = [];
    tab.on('pageerror', error => errors.push(error.message));
    await tab.goto(`http://127.0.0.1:${port}/`);
    await tab.waitForFunction(() => typeof window.run === 'function');
    const result = await tab.evaluate(n => window.run(n), frames);
    if (errors.length) throw new Error(errors.join('\n'));
    return result;
  } finally {
    await context.close(); server.close(); await rm(profile, {recursive: true, force: true});
  }
}

{
  const result = {node: await nodeRun(), browser: flag('browser') ? await browserRun() : null,
    node_version: process.version, frames};
  console.log(JSON.stringify(result, null, 2));
  if (flag('save')) {await mkdir(out, {recursive: true}); await writeFile(path.join(out, 'result.json'), JSON.stringify(result, null, 2));}
}
