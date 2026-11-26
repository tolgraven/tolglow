'use strict';
// Offline Push display renders of fixture views through the release /push-display
// page and the shipped capture worker. No show, device or output is involved.
// The base view and its touch scenarios are the fixtures of the #627 layout
// evidence, with the musical timing a show supplies for each signal. Usage: node dev/push-display/parameter-widget-fixtures.cjs <out-dir> [static-root]
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
  // Two one-bar cycles and three one-beat cycles, as the show reports them.
  {id: 'level', param: 'level', name: 'Level', schema: {type: 'number', bounds: [0, 1]}, value: 0.62,
   'preview-samples': wave(t => 0.5 + 0.45 * Math.sin(t * Math.PI * 4)),
   'signal-timing': {'period-beats': 4, 'start-beat': 0, 'span-beats': 8}},
  {id: 'speed', param: 'speed', name: 'Speed', schema: {type: 'number', bounds: [0, 4], unit: 'beats'}, value: 1.5,
   'preview-samples': wave(t => (t * 3) % 1 * 4),
   'signal-timing': {'period-beats': 1, 'start-beat': 0, 'span-beats': 3}},
  {id: 'position', param: 'position', name: 'Position',
   schema: {type: 'vector', size: 3, bounds: [-1, 1], 'dmx-bits': 16}, value: [0.123456, -0.5, 0.8]},
  {...tint, id: 'tint', param: 'tint', component: 0, 'component-label': 'r'},
  {...tint, id: 'tint', param: 'tint', component: 1, 'component-label': 'g'},
  {...tint, id: 'tint', param: 'tint', component: 2, 'component-label': 'b'},
  null,
  {id: 'shape', param: 'shape', name: 'Shape', schema: {type: 'enum', choices: ['sine', 'saw', 'square']}, value: 'saw'}
];
// Registered waveform glyphs as a show projects them (tolglow.signals/glyph);
// waveform-shapes-test fails when this file and the registry disagree.
const glyphs = JSON.parse(fs.readFileSync(path.join(__dirname, 'waveform-glyphs.json'), 'utf8'));
const meter = (beats, bars) => ({'beats-per-bar': beats, 'bars-per-phrase': bars});
const base = {title: 'Tolglow', page: 'main', status: 'ready', bpm: 124, master: 1, 'instance-id': 'a',
  cue: {name: 'Ember', active: true}, effect: {name: 'Ember wash'}, 'parameter-view': 'auto', parameters,
  'musical-timing': {meter: meter(4, 4)}, 'waveform-glyphs': glyphs,
  labels: ['Ember', 'Drift', 'Strobe', 'Sweep', '', '', '', '']};
// One sine cycle per encoder. Only the period and the meter differ.
const cycle = (name, beats, own) => ({id: name, param: name, name, schema: {type: 'number', bounds: [0, 1]}, value: 0.5,
  'preview-samples': wave(t => 0.5 - 0.45 * Math.cos(t * Math.PI * 2)),
  'signal-timing': {'period-beats': beats, 'start-beat': 0, 'span-beats': beats, ...(own && {meter: own})}});
const periods = {...base, cue: {name: 'Grid periods', active: true}, 'musical-timing': {meter: meter(4, 8)},
  parameters: [cycle('1/4 beat', 0.25), cycle('1 beat', 1), cycle('1 bar', 4), cycle('4 bars', 16), cycle('1 phrase', 32),
               cycle('Phrase 4x4', 16, meter(4, 4)), cycle('Bar 7/8', 3.5, meter(3.5, 4)), cycle('Bar 3/4', 3, meter(3, 4))]};
// One direction at four magnitudes, each axis at full scale, and a positive range.
const vector = (name, value, bounds = [-1, 1]) => ({id: name, param: name, name,
  schema: {type: 'vector', size: 3, bounds}, value});
const along = amount => [0.64, 0.6, -0.48].map(c => +(c * amount).toFixed(4));
const magnitudes = {...base, cue: {name: 'Vector magnitudes', active: true},
  parameters: [vector('Mag 0.25', along(0.25)), vector('Mag 0.5', along(0.5)), vector('Mag 0.75', along(0.75)),
               vector('Mag 1', along(1)), vector('+X', [1, 0, 0]), vector('+Y', [0, 1, 0]), vector('+Z', [0, 0, 1]),
               vector('0 to 10', [3, 4, 5], [0, 10])]};
// Shape choices: plain enums, legacy oscillator names, a registered selector
// and, for contrast, a geometry enum that must stay text.
const choice = (name, schema, value) => ({id: name, param: name, name, schema, value});
const selector = {type: 'number', bounds: [0, 3], step: 1, 'semantic/type': 'waveform-selector', label: 'Waveform',
  choices: glyphs.map((g, i) => ({value: i, id: g.id, label: g.name}))};
const shapeGlyphs = {...base, cue: {name: 'Shape glyphs', active: true},
  parameters: [choice('Shape', {type: 'enum', values: ['sine', 'saw', 'square']}, 'sine'),
               choice('Shape', {type: 'enum', values: ['sine', 'saw', 'square']}, 'saw'),
               choice('Shape', {type: 'enum', values: ['sine', 'saw', 'square']}, 'square'),
               choice('Legacy osc', {type: 'enum', values: ['sine', 'triangle', 'sawtooth', 'square']}, 'triangle'),
               choice('Legacy osc', {type: 'enum', values: ['sine', 'triangle', 'sawtooth', 'square']}, 'sawtooth'),
               choice('Waveform', selector, 2),
               choice('Volume', {type: 'enum', values: ['sphere', 'rounded-box']}, 'sphere'),
               choice('Osc mode', {type: 'enum', values: ['sine', 'random']}, 'random')]};
// One known value in each colour layout (#698); the touched RGBW one shows its white track.
const layout = (type, value) => ({id: type, param: type, name: type.toUpperCase(), component: 0,
  schema: {type: 'vector', size: value.length, bounds: [0, 1], 'semantic/type': type}, value});
const rgbw = layout('rgbw', [0.5, 0, 0, 0.5]);
const colourLayouts = {...base, cue: {name: 'Colour layouts', active: true},
  parameters: [layout('rgb', [1, 0.5, 0]), layout('rgba', [0, 0, 1, 0.5]), rgbw, layout('hsl', [1 / 3, 1, 0.5]),
               layout('hsla', [2 / 3, 1, 0.25, 0.5]), layout('hsv', [0, 1, 1]), null, null]};
const scenarios = {
  untouched: base,
  'single-touch': {...base, 'touched-parameter': {'instance-id': 'a', param: 'level', component: 0}},
  'simultaneous-touch-fixture': {...base, 'touched-parameter': {'instance-id': 'a', param: 'level', component: 0},
    'touched-parameters': [{'instance-id': 'a', param: 'speed', component: 0}]},
  'compound-touch': {...base, 'touched-parameter': {'instance-id': 'a', param: 'tint', component: 0}},
  'spatial-touch': {...base, 'touched-parameter': {'instance-id': 'a', param: 'position', component: 0}},
  'grid-periods': periods,
  'vector-magnitudes': magnitudes,
  'shape-glyphs': shapeGlyphs,
  'colour-layouts': colourLayouts,
  'colour-layout-touch': {...colourLayouts, parameters: [rgbw, null, null, null, null, null, null, null],
    'touched-parameter': {'instance-id': 'a', param: 'rgbw', component: 0}}
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
        spaces: await count('.parameter-space'), colours: await count('.parameter-color'),
        glyphs: await count('.shape-glyph'), overflow}));
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
    // Sources with a known period on a running transport: a one-bar sine, a
    // one-beat saw and a two-bar orbit, observed every eighth of a beat.
    const lfo = [beat => 0.5 + 0.4 * Math.sin(beat * Math.PI / 2), beat => (beat % 1) * 4,
                 beat => [Math.sin(beat * Math.PI / 4) * 0.9, Math.cos(beat * Math.PI / 4) * 0.5, Math.sin(beat * Math.PI / 8)]];
    for (let n = 0; n <= 124; n++) {
      const beat = 4 + n / 8;
      const active = parameters.map((p, i) => p && i < 3 ? {...p, 'source-state': 'active', 'source-types': ['lfo'],
        'source-period-beats': [4, 1, 8][i], value: lfo[i](beat)} : p);
      result = await worker.capture({...base, parameters: active, 'display-mode': 'parameters', 'parameter-view': 'history', 'history-beats': 4,
        'runtime-id': 'r', epoch: 2, 'definition-version': 1,
        'visualization-clock': {'time-ms': 2000 + n * 125, beat, source: 'show'}});
    }
    fs.writeFileSync(path.join(outDir, 'history-periods.png'), Buffer.from(result.png, 'base64'));
    console.log('history-periods', JSON.stringify({grids: await page.locator('.parameter-history .musical-grid').count()}));
    console.log('errors', JSON.stringify(errors));
    if (errors.length) process.exitCode = 1;
  } finally { await worker.close(); server.close(); }
});
