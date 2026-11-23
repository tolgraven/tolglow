import test from 'node:test';
import assert from 'node:assert/strict';
import {mkdtemp, readFile, realpath, writeFile, rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {fileURLToPath, pathToFileURL} from 'node:url';
import {Worker} from 'node:worker_threads';
import {runInNewContext} from 'node:vm';

const source = await readFile(new URL('../macOS/ShaderQualification.js', import.meta.url), 'utf8');

function fixture({debug = true, worker = false} = {}) {
  const events = [], timers = [], workers = [], blobs = [];
  // WebGL is the I/O boundary. The actual injected script owns all observation,
  // filtering, bounds, worker routing and snapshot behavior under test.
  class GL {
    constructor(canvas) { this.canvas = canvas; this.links = 0; this.draws = 0; }
    getExtension(name) {
      return name === 'WEBGL_debug_shaders' && debug
        ? {getTranslatedShaderSource: shader => shader.translated} : null;
    }
    getParameter() { return 'WebKit WebGL'; }
    linkProgram() { this.links++; return 'linked'; }
    getAttachedShaders(program) { return program.shaders; }
    getShaderSource(shader) { return shader.source; }
    getShaderParameter(shader, parameter) { return parameter === this.SHADER_TYPE ? shader.stage : shader.compiled; }
    getShaderInfoLog() { return ''; }
    getProgramParameter(program) { return program.linked; }
    getProgramInfoLog() { return ''; }
    drawArrays() { this.draws++; return 'drawn'; }
    isContextLost() { return false; }
  }
  Object.assign(GL.prototype, {SHADER_TYPE: 1, VERTEX_SHADER: 2, COMPILE_STATUS: 3, LINK_STATUS: 4});
  const canvas = {width: 320, height: 200,
    tolglowPreview: {ready: true, draws: 42, getTelemetry: () => ({drawCalls: 42})}};
  const globals = {location: {href: 'http://127.0.0.1:1234/', origin: 'http://127.0.0.1:1234'},
    document: {querySelectorAll: () => [canvas]}, WebGL2RenderingContext: GL,
    webkit: {messageHandlers: {shaderQualification: {postMessage: event => events.push(event)}}},
    setTimeout: (fn, delay) => timers.push({fn, delay})};
  globals.window = globals;
  if (worker) {
    globals.Worker = class {
      constructor(url, options) { this.url = url; this.options = options; this.listeners = []; workers.push(this); }
      addEventListener(...args) { this.listeners.push(args); }
    };
    globals.URL = class extends URL {
      static createObjectURL(blob) { blobs.push(blob); return 'blob:qualification'; }
      static revokeObjectURL() {}
    };
    globals.Blob = class { constructor(parts) { this.text = parts.join(''); } };
  }
  runInNewContext(source, globals);
  return {globals, GL, canvas, events, timers, workers, blobs};
}

const program = {linked: true, shaders: [{source: '#version 300 es\nvoid main() {}',
  translated: '#include <metal_stdlib>\nusing namespace metal;', compiled: true, stage: 2}]};

test('observes real preview calls without replacing shader source, return values or telemetry', () => {
  const {globals, GL, canvas, events, timers} = fixture();
  const unrelated = new GL({});
  unrelated.linkProgram(program); unrelated.drawArrays();
  assert.equal(events.length, 0);
  const gl = new GL(canvas);
  assert.equal(gl.linkProgram(program), 'linked');
  assert.equal(gl.drawArrays(), 'drawn');
  gl.drawArrays();
  assert.equal(gl.links, 1); assert.equal(gl.draws, 2);
  assert.deepEqual(events.map(event => event.type), ['context', 'program', 'draw']);
  assert.equal(events[1].shaders[0].source, program.shaders[0].source);
  assert.equal(events[1].shaders[0].translated, program.shaders[0].translated);
  const snapshot = globals.__tolglowShaderQualificationSnapshot();
  assert.equal(snapshot.previews[0].telemetry.drawCalls, 42);
  assert.equal(snapshot.previews[0].route, 'main');
  assert.equal(snapshot.previews[0].presented, true);
  canvas.tolglowPreview = {ready: true, presented: false, fallback: {draws: 2}};
  assert.equal(globals.__tolglowShaderQualificationSnapshot().previews[0].presented, true);
  canvas.tolglowPreview.fallback.draws = 0;
  assert.equal(globals.__tolglowShaderQualificationSnapshot().previews[0].presented, false);
  assert.equal(timers[0].delay, 60000);
  timers[0].fn(); gl.linkProgram(program);
  assert.equal(events.length, 3);
});

test('the real module bootstrap preserves startup messages while the original module loads', async () => {
  const directory = await mkdtemp(join(tmpdir(), 'tolglow-metal-worker-'));
  let worker;
  try {
    const original = join(directory, 'preview-worker.mjs');
    await writeFile(original, `await new Promise(resolve => setTimeout(resolve, 30));
      self.addEventListener('message', event => postMessage({received: event.data, moduleURL: import.meta.url}));`);
    const {globals, blobs} = fixture({worker: true});
    globals.location.href = pathToFileURL(directory + '/').href;
    globals.location.origin = 'null';
    new globals.Worker(pathToFileURL(original), {type: 'module', name: 'tolglow-room-preview'});
    // Adapt only browser worker messaging at the Node worker-thread boundary;
    // execute the generated production bootstrap unchanged, including import.
    const entry = join(directory, 'bootstrap.mjs');
    await writeFile(entry, `import {parentPort} from 'node:worker_threads';
      const events = new EventTarget(); globalThis.self = globalThis;
      globalThis.addEventListener = events.addEventListener.bind(events);
      globalThis.removeEventListener = events.removeEventListener.bind(events);
      globalThis.dispatchEvent = events.dispatchEvent.bind(events);
      globalThis.postMessage = value => parentPort.postMessage(value);
      parentPort.on('message', data => events.dispatchEvent(new MessageEvent('message', {data})));
      ${blobs[0].text}`);
    worker = new Worker(pathToFileURL(entry));
    const replies = await new Promise((resolve, reject) => {
      const rows = [];
      const timer = setTimeout(() => reject(new Error('Startup messages lost during module import')), 3000);
      worker.on('error', error => { clearTimeout(timer); reject(error); });
      worker.on('message', message => {
        rows.push(message);
        if (rows.length === 2) { clearTimeout(timer); resolve(rows); }
      });
      worker.postMessage({type: 'init'});
      worker.postMessage({type: 'activity', active: true});
    });
    assert.deepEqual(replies.map(row => row.received), [{type: 'init'}, {type: 'activity', active: true}]);
    for (const row of replies) assert.equal(await realpath(fileURLToPath(row.moduleURL)), await realpath(original));
  } finally {
    await worker?.terminate();
    await rm(directory, {recursive: true, force: true});
  }
});

test('hidden translation extension and failed programs remain explicit; observation is bounded', () => {
  const {GL, canvas, events} = fixture({debug: false});
  const gl = new GL(canvas);
  const failed = {...program, linked: false};
  for (let i = 0; i < 140; i++) gl.linkProgram(failed);
  assert.equal(gl.links, 140);
  const programs = events.filter(event => event.type === 'program');
  assert.equal(programs.length, 128);
  assert.equal(events[0].translatedSourceAvailable, false);
  assert.equal(programs[0].linked, false);
  assert.equal(programs[0].shaders[0].translated, '');
  assert.equal(events.filter(event => event.type === 'limit').length, 1);
});

test('only the named same-origin production module worker is instrumented', () => {
  const {globals, workers, blobs, events} = fixture({worker: true});
  new globals.Worker('/compute-worker.js', {type: 'module', name: 'compute'});
  new globals.Worker('https://example.org/preview-worker.js', {type: 'module', name: 'tolglow-room-preview'});
  new globals.Worker('/studio/preview-worker.js', {type: 'module', name: 'tolglow-room-preview'});
  assert.equal(workers[0].url, '/compute-worker.js');
  assert.equal(workers[1].url, 'https://example.org/preview-worker.js');
  assert.equal(workers[2].url, 'blob:qualification');
  assert.match(blobs[0].text, /await import\("http:\/\/127\.0\.0\.1:1234\/studio\/preview-worker\.js"\)/);
  let stopped = false;
  workers[2].listeners.find(([name, , options]) => name === 'message' && !options)[1]({
    data: {tag: 'tolglow-shader-qualification', event: {type: 'draw'}},
    stopImmediatePropagation() { stopped = true; }});
  assert.equal(stopped, true);
  assert.equal(events[0].type, 'draw');
});
