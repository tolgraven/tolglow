import test from 'node:test';
import assert from 'node:assert/strict';
import {fileURLToPath} from 'node:url';
import {runInNewContext} from 'node:vm';
import {build} from 'esbuild';

// Keep the real worker, its two-frame render-ahead queue and the diagnostic
// probe. Replace only graphics allocation and raster work at the GPU boundary.
const bundle = await build({stdin: {
  resolveDir: fileURLToPath(new URL('../../../', import.meta.url)),
  contents: `import {HemisphereLight} from 'three';
    import {SceneRenderer} from './js/preview/scene-renderer.js';
    import {TIERS} from './js/preview/render-quality.js';
    import {FOG_FRAGMENT, FOG_COMPOSITE_FRAGMENT} from './js/preview/fog.js';
    SceneRenderer.prototype.initialize = function () {
      this.softwareRenderer = false;
      this.quality = this.autoQuality(); this.options = TIERS[this.quality];
      this.frameInterval = 1000 / this.targetHz;
      this.diffuseBounce = new HemisphereLight();
      const renderer = this.renderer = {getContext: () => graphics.gl, info: {memory: {}, programs: []},
        shadowMap: {autoUpdate: true}, render() {}, dispose() {}};
      this.fog = {amount: .5, width: 4, height: 4, budget: {steps: 32}, scene: {}, camera: {}, quad: {},
        volumeMaterial: {fragmentShader: FOG_FRAGMENT}, compositeMaterial: {fragmentShader: FOG_COMPOSITE_FRAGMENT},
        uniforms: {detailMin: {value: {x: 1, y: 1, z: 1}}, detailMax: {value: {x: -1, y: -1, z: -1}}},
        field: {stats: () => ({maximumDensity: .07})}, stats: () => ({maximumDensity: .07}),
        setAmount(value) {this.amount = value;}, setQuality() {}, setSize() {}, setRoom() {}, setAtmosphere() {},
        upload() {}, dispose() {},
        render() {
          if (this.amount <= 0) return;
          this.quad.material = this.volumeMaterial; renderer.render(this.scene, this.camera);
        }};
      this.composer = {render() {}, dispose() {}};
      this.sceneTarget = {samples: 0, dispose() {}};
    };
    SceneRenderer.prototype.rebuild = function () {
      this.lightIds = []; this.spots = []; this.diffuserTubes = []; this.fixtureSamples = [];
    };
    SceneRenderer.prototype.draw = function (dt) {
      graphics.draws++;
      this.renderer.render(this.scene, this.camera); this.fog.render(); this.composer.render(dt); this.draws++;
    };
    SceneRenderer.prototype.resize = function () {};
    // The probe wraps whichever draw exists when it loads, as it does in the
    // private worker bundle. Load it after the GPU boundary above.
    import('./dev/preview/room-pass-worker.mjs');`
}, bundle: true, format: 'iife', platform: 'browser', write: false});

async function worker() {
  let now = 0, nextId = 0, serial = 0, readyThrough = Infinity;
  const fences = new Set(), rafs = new Map(), timers = new Map(), messages = [];
  const graphics = {draws: 0, gl: {
    SYNC_GPU_COMMANDS_COMPLETE: 1, ALREADY_SIGNALED: 2, CONDITION_SATISFIED: 3, WAIT_FAILED: 4, TIMEOUT_EXPIRED: 5,
    getExtension: () => null,
    fenceSync() {const fence = ++serial; fences.add(fence); return fence;},
    clientWaitSync(fence) {return fence <= readyThrough ? this.CONDITION_SATISFIED : this.TIMEOUT_EXPIRED;},
    deleteSync(fence) {fences.delete(fence);},
    flush() {}
  }};
  const host = {graphics, AbortController, console, performance: {now: () => now, timeOrigin: 0},
    requestAnimationFrame(callback) {const id = ++nextId; rafs.set(id, callback); return id;},
    cancelAnimationFrame: id => rafs.delete(id),
    setTimeout(callback, delay) {const id = ++nextId; timers.set(id, {callback, at: now + delay}); return id;},
    clearTimeout: id => timers.delete(id),
    postMessage: message => messages.push(message), close() {},
    OffscreenCanvas: class {
      addEventListener() {}
      transferToImageBitmap() {return {close() {}};}
    }};
  host.self = host;
  runInNewContext(bundle.outputFiles[0].text, host);
  const settle = () => new Promise(resolve => setImmediate(resolve));
  await settle();
  const send = data => host.onmessage({data});
  const healthy = () => assert.deepEqual(messages.filter(message => message.type === 'error'), []);
  async function advance(time) {
    for (;;) {
      const due = [...timers].filter(([, entry]) => entry.at <= time).sort((a, b) => a[1].at - b[1].at)[0];
      if (!due) break;
      timers.delete(due[0]); now = Math.max(now, due[1].at); due[1].callback(); await settle();
    }
    now = Math.max(now, time);
  }
  send({type: 'activity', active: true, generation: 1});
  send({type: 'state', id: 1, outputOnly: false, state: {generation: 1, displayHz: 60, receivedAt: 0,
    viewport: {width: 640, height: 360, pixelRatio: 1}, quality: 'mobile', lightingMode: 'hold', paused: false, actions: [],
    camera: {position: [0, 2, 8], target: [0, 2, 0], up: [0, 1, 0], fov: 49, name: 'audience'}}});
  healthy();
  return {host, graphics, advance, healthy,
    probes: () => messages.filter(message => message.type === 'frame').map(message => message.telemetry.passProbe),
    set fenceReady(value) {readyThrough = value ? Infinity : 0;},
    acknowledge(frameId) {send({type: 'frame-ack', frameId}); healthy();},
    async refresh(timestamp) {
      await advance(timestamp + 2);
      const callbacks = [...rafs.values()]; rafs.clear();
      for (const callback of callbacks) callback(timestamp);
      await settle(); healthy();
    },
    dispose() {send({type: 'dispose'});}};
}

test('a frame published behind a later draw keeps the probe sample of its own draw', async t => {
  const w = await worker(); t.after(() => w.dispose());
  w.fenceReady = false;
  await w.refresh(1000 / 60);
  // The second draw starts, in another mode, while the first still waits on its fence.
  w.host.setRoomPassMode('skip-fog-raymarch');
  await w.refresh(2 * 1000 / 60);
  assert.equal(w.graphics.draws, 2); assert.equal(w.probes().length, 0);
  w.fenceReady = true; await w.advance(45);
  w.acknowledge(1);
  const describe = probe => [probe.frame, probe.mode, Boolean(probe.raymarched), 'fog' in probe.submitMs, Number.isFinite(probe.sentAt)];
  assert.deepEqual(w.probes().map(describe), [
    [1, 'full', true, true, true],
    [2, 'skip-fog-raymarch', false, true, true]]);
  assert.deepEqual(w.probes().map(probe => probe.maximumDensity), [.07, .07]);
});
