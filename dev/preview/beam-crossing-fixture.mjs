import * as THREE from 'three';
import {SceneRenderer} from '../../js/preview/scene-renderer.js';

const canvas = document.querySelector('canvas');
const engine = new SceneRenderer(canvas, null, {autoStart: false});
engine.visible = true;
if (!engine.renderer) engine.initialize();
engine.pause();
engine.controls.enableDamping = false;
engine.setLightingMode('hold');

const centerHeight = 2.3;
const beamHeight = 3.65;
const colors = [[.8, .18, .045], [.11, .51, .82]];
const roomColors = [[.78, .22, .045], [.21, .48, .67]];
const points = {
  controlled: {crossing: [.5, .5], shoulder: [.5, .54], lens: [.22, .80], dark: [.52, .71]},
  room: {air: [.49, .36], floor: [.47, .207], floorEdge: [.34, .207], lens: [.53, .69], dark: [.74, .24]},
};
let scene, rig, probes, revision = 0;
let stages;

function meanRGB(pixels, decode) {
  const totals = [0, 0, 0];
  for (let i = 0; i < pixels.length; i += 4) {
    for (let channel = 0; channel < 3; channel++) totals[channel] += decode(pixels[i + channel]);
  }
  return totals.map(sum => sum / (pixels.length / 4));
}

function samples(width, height, pixels, read, decode) {
  // Use the same footprint in display coordinates when a source raster shrinks.
  const sampleWidth = Math.max(1, Math.round(7 * width / canvas.width));
  const sampleHeight = Math.max(1, Math.round(7 * height / canvas.height));
  const region = pixels.subarray(0, sampleWidth * sampleHeight * 4);
  return Object.fromEntries(Object.entries(probes).map(([name, [x, y]]) => {
    const left = Math.floor(x * width) - Math.floor(sampleWidth / 2);
    const bottom = Math.floor(y * height) - Math.floor(sampleHeight / 2);
    read(left, bottom, sampleWidth, sampleHeight, region);
    return [name, meanRGB(region, decode)];
  }));
}

function targetSamples(target) {
  const size = Math.ceil(7 * target.width / canvas.width) * Math.ceil(7 * target.height / canvas.height) * 4;
  return samples(target.width, target.height, new Uint16Array(size),
    (x, y, w, h, pixels) => engine.renderer.readRenderTargetPixels(target, x, y, w, h, pixels),
    THREE.DataUtils.fromHalfFloat);
}

function screenSamples(gl) {
  return samples(gl.drawingBufferWidth, gl.drawingBufferHeight, new Uint8Array(7 * 7 * 4),
    (x, y, w, h, pixels) => gl.readPixels(x, y, w, h, gl.RGBA, gl.UNSIGNED_BYTE, pixels),
    value => value / 255);
}

for (const [pass, name] of [[engine.bloom, 'preBloom'], [engine.outputPass, 'postBloom'], [engine.edges, 'display'], [engine.film, 'edges']]) {
  const render = pass.render.bind(pass);
  pass.render = (renderer, output, input, ...options) => {
    if (stages) stages[name] = targetSamples(input);
    render(renderer, output, input, ...options);
    if (stages && pass === engine.bloom) stages.bloomSource = targetSamples(engine.bloom.renderTargetBright);
  };
}

function update(values, force = false) {
  const serial = ++revision;
  engine.update({'show-id': scene, epoch: 1, revision: serial, 'frame-number': serial, rig, output: {values}}, force);
}

function configure({width, height, quality, separation = 0, roomRig}) {
  scene = roomRig ? 'room' : 'controlled';
  canvas.style.width = width + 'px';
  canvas.style.height = height + 'px';
  engine.setQuality(quality);
  engine.visible = true;
  engine.pause();
  engine.resize();

  const fixtures = [-2.1, 2.1].map((x, i) => ({
    id: i ? 'B' : 'A', position: [x, beamHeight, i ? -separation : 0], 'beam-angle': 12, 'lens-radius-m': .09,
  }));
  rig = roomRig || {room: {width: 12, height: 5, depth: 12}, fixtures};
  probes = points[scene];
  engine.setFog(.65);
  engine.setWorkLight(roomRig ? .12 : 0);
  update({}, true);
  engine.fog.field.clear(roomRig ? .07 : .13);
  engine.fog.upload();
  if (roomRig) engine.setCamera('audience');
  return {
    quality: engine.quality,
    raster: [engine.sceneTarget.width, engine.sceneTarget.height],
    fogRaster: [engine.fog.target.width, engine.fog.target.height],
    display: [canvas.width, canvas.height],
    samples: engine.sceneTarget.samples,
    renderer: engine.deviceRenderer,
  };
}

function draw({mask = -1, gain = 1, exposure = .85, bloom = .28, grain = 0, yaw = 0, measure = true, picture = false} = {}) {
  if (scene === 'controlled') {
    engine.camera.position.set(4.8 * Math.sin(yaw), centerHeight, 4.8 * Math.cos(yaw));
    engine.controls.target.set(0, centerHeight, 0);
    engine.controls.update();
  }
  const values = Object.fromEntries(rig.fixtures.map((fixture, i) => {
    const [x, y] = fixture.position;
    const rgb = (scene === 'room' ? roomColors : colors)[i % 2];
    return [fixture.id, {
      'emitted-color': rgb.map(c => (mask & (1 << i)) ? c * gain : 0),
      direction: scene === 'room' ? [(i - 3.5) * -.22, -1, .45] : [-x, centerHeight - y, 0],
      zoom: scene === 'room' ? .16 : 0,
      focus: .2,
      'cut?': true,
    }];
  }));
  engine.setFilm({exposure, bloom, grain});
  update(values);
  const gl = engine.renderer.getContext();
  gl.finish();
  const start = performance.now();
  const before = engine.draws;
  stages = measure ? {} : null;
  engine.draw(0);
  if (engine.draws !== before + 1) throw new Error('Preview did not draw');

  // EffectComposer restores its prior target. Read the actual canvas before
  // sampling intermediary targets or returning control to the browser.
  engine.renderer.setRenderTarget(null);
  if (!stages) {
    if (picture) return canvas.toDataURL();
    gl.readPixels(0, 0, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, new Uint8Array(4));
    return performance.now() - start;
  }
  const screen = screenSamples(gl);
  const report = {
    scene: targetSamples(engine.sceneTarget),
    volume: targetSamples(engine.fog.target),
    // Follow the actual pass input even when the renderer composites fog
    // directly into the composer and bypasses its separate fogged target.
    linear: stages.preBloom ?? stages.postBloom,
    ...stages,
    screen,
    settings: {mask, gain, exposure, bloom, grain, yaw},
    picture: picture ? canvas.toDataURL() : undefined,
  };
  stages = null;
  return report;
}

window.crossing = {configure, draw, dispose: () => engine.dispose()};
