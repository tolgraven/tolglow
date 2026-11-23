import test from 'node:test';
import assert from 'node:assert/strict';
import {SceneRenderer} from '../../../js/preview/scene-renderer.js';
import {TIERS, rasterSizes, sceneSamples, supportedSamples, configureDisplayRange} from '../../../js/preview/render-quality.js';

test('present at panel DPR up to UHD regardless of the current fog and scene budget', () => {
  const size = (w, h, dpr, tier, scale = 1) => rasterSizes(w, h, dpr, TIERS[tier], scale, 16384);
  assert.deepEqual(size(1020, 521, 1, 'mobile'), {scene: [1020, 521], display: [1020, 521]});
  assert.deepEqual(size(1920, 1080, 2, 'high'), {scene: [3840, 2160], display: [3840, 2160]});
  assert.deepEqual(size(1920, 1080, 2, 'high', .5), {scene: [1920, 1080], display: [3840, 2160]});
  const fallback = size(3840, 2160, 1, 'low');
  assert.deepEqual(fallback.display, [3840, 2160]);
  assert.ok(fallback.scene[0] * fallback.scene[1] < 181000);
  assert.deepEqual(size(960, 160, 1, 'mobile').scene, [960, 160]);
  assert.deepEqual(rasterSizes(2048, 64, 3, TIERS.high, 1, 2048).display, [2048,64]);
});

test('multisample count requires common FP16/depth support and bounded intermediates', () => {
  const gl = {RGBA16F: 0, DEPTH_COMPONENT24: 1, getInternalformatParameter: (_, fmt) => fmt ? [8, 4] : [8, 4, 2]};
  assert.equal(supportedSamples(gl), 4);
  assert.equal(sceneSamples('mobile', 1020, 521, 4), 4);
  assert.equal(sceneSamples('high', 3840, 2160, 4), 0);
  assert.equal(sceneSamples('low', 512, 256, 4), 0);
  gl.getInternalformatParameter = () => [];
  assert.equal(supportedSamples(gl), 0);
});

test('HDR requires observed float storage and extended compositor mode', () => {
  const gl = {RGBA16F: 1, RGBA8: 2, drawingBufferFormat: 2, getExtension: () => true,
    drawingBufferStorage(format) {this.drawingBufferFormat = format;}, drawingBufferToneMapping: setting => setting};
  assert.equal(configureDisplayRange(gl, 10, 20, false), 'sdr');
  assert.equal(configureDisplayRange(gl, 10, 20, true), 'extended');
  gl.drawingBufferToneMapping = () => ({mode: 'standard'});
  assert.equal(configureDisplayRange(gl, 10, 20, true), 'sdr');
  assert.equal(gl.drawingBufferFormat, gl.RGBA8);
});

test('completed warm frames control auto quality with headroom and hysteresis', () => {
  const engine = {qualityChoice: 'auto', quality: 'desktop', options: TIERS.desktop, targetHz: 120, draws: 2, visible: true,
    renderScale: 1, scaleChangedAt: 0, applyTier(next) {this.quality = next; this.options = TIERS[next];}};
  const sample = (ms, count) => {for (let i = 0; i < count; i++) SceneRenderer.prototype.measureLoad.call(engine, ms, 3000 + i);};
  sample(3, 59);
  assert.equal(engine.quality, 'desktop');
  sample(3, 1);
  assert.equal(engine.quality, 'high');
  assert.equal(engine.renderScale, .9);
  engine.scaleChangedAt = 0; sample(20, 8);
  assert.ok(engine.renderScale < .9 && engine.resizePending);
  engine.qualityChoice = 'desktop'; const scale = engine.renderScale; sample(30, 60);
  assert.equal(engine.renderScale, scale);
});


test('a 15 ms warm workload keeps its spatial budget on a 60 Hz panel and adapts on 120 Hz', () => {
  function scene(displayHz) {
    const engine = Object.create(SceneRenderer.prototype);
    Object.assign(engine, {displayHz, quality: 'mobile', qualityChoice: 'auto', options: TIERS.mobile,
      draws: 10, visible: true, renderScale: 1, scaleChangedAt: 0});
    return engine;
  }
  const standard = scene(60), fast = scene(120);
  for (let i = 0; i < 5; i++) {
    standard.measureLoad(15, 2000 + i); fast.measureLoad(15, 2000 + i);
  }
  assert.equal(standard.renderScale, 1);
  assert.ok(fast.renderScale < 1);
});
