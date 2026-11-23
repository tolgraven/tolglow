// Controlled scene projection only: no host, effect evaluation or device output.
import * as THREE from 'three';
import {SceneRenderer} from '../../../js/preview/scene-renderer.js';

let engine;
window.disposeShadowCacheTest = () => engine?.dispose();

window.shadowCacheTest = () => {
  const canvas = document.querySelector('canvas');
  engine = new SceneRenderer(canvas, null, {worker: true, autoStart: false, displayHz: 60,
    viewport: {width: 960, height: 540, pixelRatio: 1}});
  engine.setQuality('desktop'); engine.setWorkLight(0); engine.setFog(.7);
  engine.setFilm({bloom: .22, exposure: .85, grain: 0});
  engine.camera.position.set(3.2, 2.8, 7.6); engine.controls.target.set(0, 1.65, -1);
  const cameraPosition = engine.camera.position.clone();
  const fixtures = Array.from({length: 8}, (_, i) => ({id: `head-${i + 1}`,
    position: [(i - 3.5) * 1.1, 3.5, -1.5], channels: {pan: 1, tilt: 2},
    'beam-angle': 12, 'lens-radius-m': .119}));
  const pose = direction => {
    const axis = new THREE.Vector3(...direction).normalize();
    return {direction: axis.toArray(), 'pan-deg': Math.atan2(axis.x, axis.z) * 180 / Math.PI,
      'tilt-deg': Math.asin(axis.y) * 180 / Math.PI};
  };
  const colors = [[.85,.8,.7], [.15,.7,.85], [.85,.36,.08]];
  const values = Object.fromEntries(fixtures.map((fixture, i) => [fixture.id, {
    pose: pose([-fixture.position[0] * 1.35, -3.35, 2.8]),
    'emitted-color': colors[i % colors.length], focus: .45}]));
  const rig = {room: {width: 12, height: 5, depth: 10}, fixtures,
    trusses: [{id: 'back', position: [0, 4.15, -1.5], length: 9}]};
  let frame = 0;
  const update = (output, extra = {}) => engine.update({rig, 'show-id': 'shadow-cache-gpu', epoch: 1, revision: 1,
    'frame-number': ++frame, 'time-ms': frame * 25, output: {values: output}, ...extra});
  update(values); engine.fog.field.clear(.15); engine.fog.upload();
  const shadowCalls = new Map();
  engine.scene.traverse(object => {
    if (!object.isMesh || !object.castShadow) return;
    const previous = object.onBeforeShadow;
    object.onBeforeShadow = function(renderer, mesh, camera, shadowCamera, ...rest) {
      shadowCalls.set(shadowCamera.uuid, (shadowCalls.get(shadowCamera.uuid) || 0) + 1);
      previous.call(this, renderer, mesh, camera, shadowCamera, ...rest);
    };
  });
  const gl = engine.renderer.getContext(), debug = gl.getExtension('WEBGL_debug_renderer_info');
  const draw = () => {
    shadowCalls.clear(); engine.draw(0); engine.renderer.setRenderTarget(null);
    const pixels = new Uint8Array(gl.drawingBufferWidth * gl.drawingBufferHeight * 4);
    gl.readPixels(0, 0, gl.drawingBufferWidth, gl.drawingBufferHeight, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
    return {pixels, shadowDraws: [...shadowCalls.values()].reduce((a,b) => a + b, 0), shadowMaps: shadowCalls.size};
  };
  const difference = (a, b) => {
    let maximum = 0, changed = 0;
    for (let i = 0; i < a.length; i += 4) {
      const delta = Math.max(...[0,1,2].map(c => Math.abs(a[i+c] - b[i+c])));
      maximum = Math.max(maximum, delta); if (delta) changed++;
    }
    return {maximum, changed};
  };
  const first = draw(), initialMaps = engine.spots.filter(light => light.castShadow).map(light => light.shadow.map.depthTexture.uuid);
  update(structuredClone(values)); const identical = draw();
  const recolored = structuredClone(values); recolored['head-1']['emitted-color'] = [.85,.2,.7];
  update(recolored); const color = draw();
  engine.camera.position.x += .4; const camera = draw();
  const dark = structuredClone(recolored); dark['head-8']['emitted-color'] = [0,0,0];
  update(dark); const darkStill = draw();
  dark['head-8'].pose = pose([.2,-1,.3]); update(dark); const darkMoved = draw();
  const cached = draw();
  for (const light of engine.spots) if (light.castShadow) light.shadow.needsUpdate = true;
  const forced = draw();
  update(values); engine.camera.position.copy(cameraPosition); engine.setFog(0); const clear = draw();
  engine.setFog(.7); const fog = draw();
  const renderFog = engine.fog.render;
  const copyReference = () => {
    engine.fog.render = function(scene, camera) {
      engine.inputPass.enabled = true;
      return renderFog.call(this, scene, camera, engine.compositeTarget);
    };
    try {return draw();} finally {engine.fog.render = renderFog;}
  };
  const copied = copyReference(), directAgain = draw();
  const directInputEnabled = engine.inputPass.enabled;
  update(values, {'glass-meshes': {orb: {kind: 'sphere'}}, 'glass-volumes': [
    {id: 'orb', mesh: 'orb', position: [0,1.5,0], 'half-size': [.55,.55,.55], color: [.3,.8,1], strength: 1}]});
  let glassTargetIsComposite = false;
  engine.fog.render = function(scene, camera, target) {
    glassTargetIsComposite = target === engine.compositeTarget;
    return renderFog.call(this, scene, camera, target);
  };
  const glass = draw(), glassInputEnabled = engine.inputPass.enabled;
  engine.fog.render = renderFog;
  const glassReference = copyReference();
  update(values); draw();
  let fogBrightened = 0, fogDimmed = 0;
  for (let i = 0; i < clear.pixels.length; i += 4) {
    const delta = [0,1,2].reduce((sum,c) => sum + fog.pixels[i+c] - clear.pixels[i+c], 0);
    if (delta > 9) fogBrightened++;
    if (delta < -9) fogDimmed++;
  }
  // Keep this same production scene available for optional unaltered captures.
  window.captureShadowCacheScene = name => {
    engine.fog.field.clear(.15); engine.setFog(name === 'clear-surfaces' ? 0 : .7);
    if (name === 'fresh-plume' || name === 'dispersed-plume') {
      engine.fog.burst({position: [-1,.25,.4], direction: [1,.5,-.25], duration: 1.5, strength: 4});
      const steps = name === 'fresh-plume' ? 40 : 240;
      for (let i = 0; i < steps; i++) engine.fog.field.advance(.05);
    }
    engine.fog.upload(); draw();
    return engine.getTelemetry();
  };
  window.setShadowCaptureAutoFloor = () => {
    engine.platform.viewport = {width: 1559, height: 700, pixelRatio: 1.8};
    canvas.style.width = '1559px'; canvas.style.height = '700px';
    engine.qualityChoice = 'auto'; engine.renderScale = .5; engine.applyTier('low');
    engine.resize(); return engine.getTelemetry();
  };
  window.recordShadowMotion = async () => {
    window.captureShadowCacheScene('crossing-shafts');
    engine.fog.burst({position: [-1,.25,.4], direction: [1,.5,-.25], duration: 1.5, strength: 4});
    const stream = canvas.captureStream(60), chunks = [];
    const mimeType = ['video/webm;codecs=vp9','video/webm;codecs=vp8'].find(type => MediaRecorder.isTypeSupported(type));
    const recorder = new MediaRecorder(stream, {mimeType, videoBitsPerSecond: 6000000});
    recorder.ondataavailable = event => {if (event.data.size) chunks.push(event.data);};
    const stopped = new Promise(resolve => {recorder.onstop = resolve;});
    recorder.start(); let previous, started, frames = 0;
    try {
      await new Promise(resolve => {
        const tick = now => {
          started ??= now; const seconds = (now - started) / 1000, dt = previous == null ? 0 : Math.min(.1, (now - previous) / 1000);
          previous = now; engine.clock += dt;
          const moving = Object.fromEntries(fixtures.map((fixture, i) => [fixture.id, {...values[fixture.id],
            pose: pose([-fixture.position[0] * 1.35 + .5 * Math.sin(seconds * .8 + i * .2), -3.35, 2.8])}]));
          update(moving); engine.camera.position.x = cameraPosition.x + .35 * Math.sin(seconds * .5);
          engine.draw(dt); frames++;
          if (seconds >= 6) resolve(); else requestAnimationFrame(tick);
        };
        requestAnimationFrame(tick);
      });
    } finally {recorder.stop(); await stopped; for (const track of stream.getTracks()) track.stop();}
    const blob = new Blob(chunks, {type: mimeType});
    const data = await new Promise(resolve => {const reader = new FileReader(); reader.onload = () => resolve(reader.result); reader.readAsDataURL(blob);});
    return {data, mimeType, frames, durationMs: previous - started, telemetry: engine.getTelemetry(),
      scope: 'Real-clock standalone SceneRenderer capture at fixed Auto-equivalent floor; not Studio worker presentation throughput'};
  };
  return {renderer: debug ? gl.getParameter(debug.UNMASKED_RENDERER_WEBGL) : gl.getParameter(gl.RENDERER),
    first: {shadowDraws: first.shadowDraws, shadowMaps: first.shadowMaps},
    identical: {shadowDraws: identical.shadowDraws, difference: difference(first.pixels, identical.pixels)},
    color: {shadowDraws: color.shadowDraws, difference: difference(identical.pixels, color.pixels)},
    camera: {shadowDraws: camera.shadowDraws, difference: difference(color.pixels, camera.pixels)},
    darkStill: {shadowDraws: darkStill.shadowDraws}, darkMoved: {shadowDraws: darkMoved.shadowDraws, shadowMaps: darkMoved.shadowMaps},
    cached: {shadowDraws: cached.shadowDraws}, forced: {shadowDraws: forced.shadowDraws, shadowMaps: forced.shadowMaps,
      difference: difference(cached.pixels, forced.pixels)}, initialMaps,
    finalMaps: engine.spots.filter(light => light.castShadow).map(light => light.shadow.map.depthTexture.uuid),
    fogBrightened, fogDimmed, directCopyDifference: difference(fog.pixels, copied.pixels),
    directRepeatDifference: difference(fog.pixels, directAgain.pixels), directInputEnabled,
    glass: {inputEnabled: glassInputEnabled, targetIsComposite: glassTargetIsComposite,
      changed: difference(directAgain.pixels, glass.pixels), referenceDifference: difference(glass.pixels, glassReference.pixels)},
    glError: gl.getError(), telemetry: engine.getTelemetry()};
};
