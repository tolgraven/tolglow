// Browser entry for dev/preview/room-gpu-profile.mjs. Engineering attribution
// only: it draws the source SceneRenderer on a synthetic rig with no host, show,
// device or output, and times each pass with gl.finish() fences.
import * as THREE from 'three';
import {SceneRenderer} from '../../js/preview/scene-renderer.js';
import {TIERS} from '../../js/preview/render-quality.js';
import {FOG_QUALITY} from '../../js/preview/fog-field.js';

// Compare every tier at exactly the requested raster: the profiler sets the
// viewport to that size at device scale 1, and lifts the tier pixel caps and
// mesh caps so the fixture-count axis reaches the renderer.
for (const tier of Object.values(TIERS)) {
  tier.maxPixels = 3840 * 2160; tier.dpr = Math.max(1, tier.dpr); tier.fixtures = 128;
}

const ROOM = {width: 12, height: 5, depth: 10};

export function denseRig(count) {
  const rows = Math.max(1, Math.ceil(count / 16)), perRow = Math.ceil(count / rows);
  const fixtures = [], trusses = [];
  for (let row = 0; row < rows; row++) {
    const z = rows === 1 ? -1 : -3 + row * 6 / (rows - 1);
    trusses.push({id: 'truss-' + row, position: [0, 4.35, z], length: 11});
    for (let i = 0; i < perRow && fixtures.length < count; i++) {
      const x = -5 + (perRow === 1 ? 5 : i * 10 / (perRow - 1));
      fixtures.push({id: `head-${String(fixtures.length).padStart(3, '0')}`, position: [x, 3.55, z],
        channels: {pan: 1, tilt: 2}, 'zoom-range-deg': [8, 40]});
    }
  }
  return {room: ROOM, fixtures, trusses};
}

const palette = [[1, .25, .08], [.15, .35, 1], [.9, .1, .7], [.2, 1, .45], [1, .8, .3], [.6, .2, 1]];

export function denseOutput(rig, time, moving) {
  const values = {};
  rig.fixtures.forEach((fixture, i) => {
    const phase = moving ? time * .9 + i * .7 : i * .7;
    const sweep = Math.sin(phase) * .55, tilt = .35 + .25 * Math.cos(phase * .6 + i);
    values[fixture.id] = {direction: [Math.sin(sweep) * tilt * 1.6, -1, Math.cos(sweep) * tilt * 1.2 + .15],
      'emitted-color': palette[i % palette.length].map(v => v * (.55 + .45 * ((i * 37) % 11) / 10)),
      zoom: .15 + .2 * ((i * 13) % 7) / 6};
  });
  return values;
}

function median(values) {
  const sorted = values.filter(Number.isFinite).sort((a, b) => a - b);
  if (!sorted.length) return null;
  const mid = sorted.length >> 1;
  return sorted.length % 2 ? sorted[mid] : (sorted[mid - 1] + sorted[mid]) / 2;
}

const formatName = target => {
  const t = target.texture || target.textures?.[0];
  const type = {[THREE.HalfFloatType]: 'RGBA16F', [THREE.FloatType]: 'RGBA32F', [THREE.UnsignedByteType]: 'RGBA8'}[t?.type] || t?.type;
  return `${target.width}x${target.height} ${type}${target.samples ? ` MSAA${target.samples}` : ''}${target.depthTexture ? ' +depthTex' : target.depthBuffer ? ' +depth' : ''}`;
};

let engine, canvas, presenter, rig, frameNumber = 1, sceneSamplesDefault = 0;

function snapshot(time, moving) {
  return {'show-id': 'gpu-profile', epoch: 1, revision: 1, 'frame-number': ++frameNumber, rig,
    output: {values: denseOutput(rig, time, moving)}};
}

window.setupRoom = async ({width, height, fixtures}) => {
  // The production worker path: an OffscreenCanvas renderer whose frames are
  // transferred as ImageBitmaps to a bitmaprenderer canvas.
  canvas = new OffscreenCanvas(width, height);
  engine = new SceneRenderer(canvas, null, {worker: true, autoStart: false, reducedMotion: false,
    coarsePointer: false, displayHz: 60, viewport: {width, height, pixelRatio: 1}});
  if (engine.error) throw Error(engine.error);
  engine.setQuality('desktop');
  engine.setWorkLight(.22);
  engine.setFog(.7);
  rig = denseRig(fixtures);
  engine.update(snapshot(0, false));
  engine.resize();
  engine.setCamera('audience');
  // Let the default manual fog burst develop into a dense plume, then hold it.
  for (let i = 0; i < 80; i++) { engine.clock += .05; engine.fog.update(.05, []); }
  engine.draw(0);
  sceneSamplesDefault = engine.sceneTarget.samples;
  const gl = engine.renderer.getContext();
  await gpuIdle(gl);
  return {renderer: engine.deviceRenderer, pixels: [engine.width, engine.height],
    display: [engine.displayWidth, engine.displayHeight], samples: engine.sceneTarget.samples,
    timerQuery: Boolean(gl.getExtension('EXT_disjoint_timer_query_webgl2')), userAgent: navigator.userAgent};
};

async function gpuIdle(gl) {
  const fence = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0); gl.flush();
  while (gl.clientWaitSync(fence, 0, 0) === gl.TIMEOUT_EXPIRED) await new Promise(resolve => setTimeout(resolve, 0));
  gl.deleteSync(fence);
}

function prepare(config, frame) {
  if (config.moving || frame === 0) engine.update(snapshot(frame / 60, config.moving));
}

function applyConfig(config) {
  engine.setQuality(config.tier || 'desktop');
  if (engine.edges) engine.edges.enabled = config.fxaa !== false;
  const samples = config.samples ?? sceneSamplesDefault;
  if (engine.sceneTarget.samples !== samples) { engine.sceneTarget.dispose(); engine.sceneTarget.samples = samples; }
  const fogScale = config.fogScale ?? FOG_QUALITY[engine.fog.quality]?.scale;
  if (fogScale && engine.fog.budget.scale !== fogScale) {
    engine.fog.budget = {...engine.fog.budget, scale: fogScale}; engine.fog.setSize(engine.width, engine.height);
  }
  if (config.raySteps) engine.fog.uniforms.raySteps.value = config.raySteps;
  else if (engine.fog.uniforms.raySteps) engine.fog.uniforms.raySteps.value = engine.fog.budget.steps;
  engine.setFog(config.fog ?? .7);
  engine.setFilm({bloom: config.bloom === false ? 0 : .28});
  const shadows = config.shadows ?? engine.options.shadows;
  // Visible spotlights participate in forward shading; hidden ones do not.
  engine.spots.forEach((light, i) => {
    light.visible = config.spots == null || i < config.spots;
    const cast = i < shadows;
    if (light.castShadow !== cast) { light.castShadow = cast; light.shadow.needsUpdate = true; }
  });
  if (engine.setLightBudget) engine.setLightBudget(config.fullLights ?? null);
  if (engine.beams) engine.beams.mesh.visible = config.fakedBeams !== false;
  if (engine.fog.hazeMaterial) {
    const mode = config.fogMode ?? FOG_QUALITY[engine.fog.quality].mode;
    if (engine.fog.mode !== mode) { engine.fog.mode = mode; engine.applyLighting(engine.displayedValues); }
  }
  if (engine.footprints) engine.footprints.disabled = config.footprints === false;
  else {
    // Before the LOD change the fog takes at most 16 entries in fixture order.
    engine.fogLightLimit = config.fullLights ?? 16;
    if (!engine.fogLimited) {
      const update = engine.fog.update;
      engine.fog.update = function (dt, lights) { return update.call(this, dt, lights.slice(0, engine.fogLightLimit)); };
      engine.fogLimited = true;
    }
  }
}

// Attribution is by difference: each configuration toggles one pass and the
// driver subtracts its wall-clock frame cost from the full scene's. Two
// observations rule out per-pass fences on this platform. ANGLE Metal's
// EXT_disjoint_timer_query_webgl2 results do not match wall time (a 960x540
// frame that completes in 6 ms reported 78 ms of passes), and a clear+readPixels
// fence on a private framebuffer did not wait for earlier passes (fog cost
// surfaced under the final canvas pass). performance.now() is also coarsened
// to 0.1 ms in this non-isolated page, so frames are timed in batches.
window.profileRoom = async config => {
  applyConfig(config);
  const frames = config.frames ?? 40, warm = config.warm ?? 12, repeats = config.repeats ?? 5;
  const gl = engine.renderer.getContext(), renderer = engine.renderer;
  presenter ||= Object.assign(document.createElement('canvas'), {width: 8, height: 8}).getContext('bitmaprenderer');
  const present = () => presenter.transferFromImageBitmap(canvas.transferToImageBitmap());
  // Warm: compile programs and allocate targets for this configuration.
  let frame = 0;
  for (let f = 0; f < warm; f++) { prepare(config, frame++); engine.draw(0); present(); }
  await gpuIdle(gl);
  // Back-to-back frames, each presented as the worker does, then one GPU
  // completion fence. This includes CPU submission: a wall-clock frame cost.
  const batches = [], cpu = [];
  for (let r = 0; r < repeats; r++) {
    const start = performance.now();
    for (let f = 0; f < frames; f++) { prepare(config, frame++); engine.draw(0); present(); cpu.push(engine.lastCPU); }
    await gpuIdle(gl);
    batches.push((performance.now() - start) / frames);
  }
  renderer.info.autoReset = false; renderer.info.reset();
  prepare(config, frame++); engine.draw(0); present();
  const drawCalls = renderer.info.render.calls, triangles = renderer.info.render.triangles;
  renderer.info.autoReset = true;
  await gpuIdle(gl);
  const t = engine.getTelemetry();
  return {config, frameMs: median(batches), batches, cpuDrawMs: median(cpu), drawCalls, triangles, quality: t.quality,
    scene: t.pixels, display: t.displayPixels, samples: engine.sceneTarget.samples,
    fog: {target: t.fog.targetSize, steps: engine.fog.uniforms.raySteps?.value, mode: t.fog.mode || 'raymarch',
      lights: t.fog.lightCount, shadows: t.fog.shadowCount, meanDensity: t.fog.meanDensity},
    lights: {spotsVisible: engine.spots.filter(l => l.visible).length,
      shadowMaps: t.shadowCount, active: t.activeLights, lod: t.lightLod || null},
    programs: renderer.info.programs.length, textures: renderer.info.memory.textures, geometries: renderer.info.memory.geometries};
};

window.inventory = () => {
  const targets = [['sceneTarget', engine.sceneTarget], ['compositeTarget', engine.compositeTarget],
    ['composer.read', engine.composer.readBuffer], ['composer.write', engine.composer.writeBuffer],
    ['bloom.bright', engine.bloom.renderTargetBright],
    ...engine.bloom.renderTargetsHorizontal.map((t, i) => ['bloom.h' + i, t]),
    ...engine.bloom.renderTargetsVertical.map((t, i) => ['bloom.v' + i, t]),
    ['fog.volume', engine.fog.target],
    ...engine.spots.filter(l => l.castShadow && l.shadow.map).map((l, i) => ['shadow' + i, l.shadow.map]),
    ...(engine.reflections?.lights || []).filter(l => l.castShadow && l.shadow.map).map((l, i) => ['reflectionShadow' + i, l.shadow.map])];
  let meshes = 0, casters = 0;
  engine.scene.traverse(o => { if (o.isMesh) { meshes++; if (o.castShadow) casters++; } });
  return {targets: targets.map(([name, target]) => ({name, format: formatName(target)})),
    programs: engine.renderer.info.programs.map(p => p.name + ':' + p.usedTimes),
    spots: engine.spots.length, shadowCasting: engine.spots.filter(l => l.castShadow).length, meshes, casters,
    canvas: [canvas.width, canvas.height]};
};

window.screenshot = async () => {
  engine.draw(0);
  const blob = await canvas.convertToBlob({type: 'image/png'});
  return await new Promise(resolve => { const reader = new FileReader(); reader.onload = () => resolve(reader.result); reader.readAsDataURL(blob); });
};

window.disposeRoom = () => { engine?.dispose(); engine = null; };
