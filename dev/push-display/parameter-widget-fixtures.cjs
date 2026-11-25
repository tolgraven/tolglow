'use strict';
// Offline Push display renders of fixture views through the release /push-display
// page and the shipped capture worker. No show, device or output is involved.
// The base view and its touch scenarios are the fixtures of the #627 layout
// evidence. Usage: node dev/push-display/parameter-widget-fixtures.cjs <out-dir> [static-root]
// The optional static root holds another build's resources/studio, for comparison.
const http = require('node:http');
const fs = require('node:fs');
const path = require('node:path');
const {chromium} = require('playwright-core');
const {createWorker} = require('../../resources/devices/push-display/worker.cjs');

const repository = path.resolve(__dirname, '../..');
const [outDir, staticRoot = repository] = process.argv.slice(2);
if (!outDir) throw new Error('Pass an output directory');
const executablePath = process.env.CHROMIUM_PATH || process.env.CHROMIUM || '/usr/bin/chromium';
const resources = path.join(path.resolve(staticRoot), 'resources');
const types = {'.js': 'text/javascript', '.css': 'text/css', '.html': 'text/html', '.svg': 'image/svg+xml',
               '.json': 'application/json', '.woff2': 'font/woff2', '.wasm': 'application/wasm'};

const server = http.createServer((req, res) => {
  const url = new URL(req.url, 'http://127.0.0.1');
  let file;
  if (url.pathname === '/push-display') file = path.join(resources, 'studio/index.html');
  else if (url.pathname.startsWith('/studio/')) file = path.join(resources, path.normalize(url.pathname));
  if (!file || !file.startsWith(resources) || !fs.existsSync(file)) { res.writeHead(404); return res.end(); }
  res.writeHead(200, {'content-type': types[path.extname(file)] || 'application/octet-stream'});
  fs.createReadStream(file).pipe(res);
});

const wave = f => Array.from({length: 48}, (_, i) => f(i / 47));
const tint = {name: 'Tint', schema: {type: 'color'}, value: [1, 0.42, 0.12, 1]};
const parameters = [
  {id: 'level', param: 'level', name: 'Level', schema: {type: 'number', bounds: [0, 1]}, value: 0.62,
   'preview-samples': wave(t => 0.5 + 0.45 * Math.sin(t * Math.PI * 4))},
  {id: 'speed', param: 'speed', name: 'Speed', schema: {type: 'number', bounds: [0, 4], unit: 'beats'}, value: 1.5,
   'preview-samples': wave(t => (t * 3) % 1 * 4)},
  {id: 'position', param: 'position', name: 'Position',
   schema: {type: 'vector', size: 3, bounds: [-1, 1], 'dmx-bits': 16}, value: [0.123456, -0.5, 0.8]},
  {...tint, id: 'tint', param: 'tint', component: 0, 'component-label': 'r'},
  {...tint, id: 'tint', param: 'tint', component: 1, 'component-label': 'g'},
  {...tint, id: 'tint', param: 'tint', component: 2, 'component-label': 'b'},
  null,
  {id: 'shape', param: 'shape', name: 'Shape', schema: {type: 'enum', choices: ['sine', 'saw', 'square']}, value: 'saw'}
];
const base = {title: 'Tolglow', page: 'main', status: 'ready', bpm: 124, master: 1, 'instance-id': 'a',
  cue: {name: 'Ember', active: true}, effect: {name: 'Ember wash'}, 'parameter-view': 'auto', parameters,
  labels: ['Ember', 'Drift', 'Strobe', 'Sweep', '', '', '', '']};
const scenarios = {
  untouched: base,
  'single-touch': {...base, 'touched-parameter': {'instance-id': 'a', param: 'level', component: 0}},
  'simultaneous-touch-fixture': {...base, 'touched-parameter': {'instance-id': 'a', param: 'level', component: 0},
    'touched-parameters': [{'instance-id': 'a', param: 'speed', component: 0}]},
  'compound-touch': {...base, 'touched-parameter': {'instance-id': 'a', param: 'tint', component: 0}},
  'spatial-touch': {...base, 'touched-parameter': {'instance-id': 'a', param: 'position', component: 0}}
};

server.listen(Number(process.env.PORT || 0), '127.0.0.1', async () => {
  const origin = 'http://127.0.0.1:' + server.address().port;
  let browser;
  const worker = await createWorker({url: origin + '/push-display', executablePath,
    startupTimeoutMs: 30000, captureTimeoutMs: 10000}, async o => (browser = await chromium.launch(o)));
  const page = browser.contexts()[0].pages()[0];
  const errors = [];
  page.on('pageerror', e => errors.push(e.message));
  try {
    fs.mkdirSync(outDir, {recursive: true});
    for (const [name, view] of Object.entries(scenarios)) {
      const result = await worker.capture(view);
      fs.writeFileSync(path.join(outDir, name + '.png'), Buffer.from(result.png, 'base64'));
      const overflow = await page.evaluate(() => [...document.querySelectorAll('#push-display .push-parameter, #push-display .push-compound-area')]
        .flatMap(slot => [...slot.querySelectorAll('span, output, b, small, figcaption')]
          .filter(el => el.offsetParent && el.scrollWidth > el.clientWidth + 1 && getComputedStyle(el).textOverflow !== 'ellipsis')
          .map(el => (slot.dataset.encoder ?? 'area') + ':' + el.className + ':' + el.textContent.trim())));
      const count = selector => page.locator(selector).count();
      console.log(name, JSON.stringify({ms: Math.round(result['capture-ms']), areas: await count('.push-compound-area'),
        spaces: await count('.parameter-space'), colours: await count('.parameter-color'), overflow}));
    }
    // History mode needs a sequence of active observations on one show clock.
    let result;
    for (let n = 0; n < 24; n++) {
      const t = n / 23;
      const active = parameters.map((p, i) => p && i < 3 ? {...p, 'source-state': 'active',
        value: i === 2 ? [Math.sin(t * 6) * 0.9, Math.cos(t * 6) * 0.5, t - 0.5] : 0.5 + 0.4 * Math.sin(t * 9 + i)} : p);
      result = await worker.capture({...base, parameters: active, 'display-mode': 'parameters', 'parameter-view': 'history', 'history-beats': 4,
        'runtime-id': 'r', epoch: 1, 'definition-version': 1,
        'visualization-clock': {'time-ms': 1000 + n * 250, beat: n / 2, source: 'show'}});
    }
    fs.writeFileSync(path.join(outDir, 'history.png'), Buffer.from(result.png, 'base64'));
    console.log('history', JSON.stringify({charts: await page.locator('.parameter-history').count()}));
    console.log('errors', JSON.stringify(errors));
    if (errors.length) process.exitCode = 1;
  } finally { await worker.close(); server.close(); }
});
