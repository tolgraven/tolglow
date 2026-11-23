import {SceneRenderer} from '../../../../js/preview/scene-renderer.js';
import {installBloomDisplayExperiment} from '../../../../dev/preview/bloom-display-experiment.mjs';

const width = 257;
const height = 149;
const stage = {
  'show-id': 'bloom-display-reference', epoch: 1, revision: 1, 'frame-number': 1,
  rig: {room: {width: 9.4, height: 3.7, depth: 8.2}, fixtures: [
    {id: 'violet', position: [-2.45, 3, -1.82], 'beam-angle': 19},
    {id: 'magenta', position: [2.35, 2.89, -1.62], 'beam-angle': 21},
    {id: 'amber', position: [0.12, 0.81, -3.54], 'beam-angle': 12}
  ], surfaces: [{id: 'wing', point: [-2.48, 0.78, 0.5], size: [0.79, 1.56],
    normal: [0, 0, 1], 'u-axis': [1, 0, 0]}]},
  output: {values: {
    violet: {'emitted-color': [0.28, 0.009, 1], direction: [2.57, -1.55, 1.12]},
    magenta: {'emitted-color': [0.86, 0.009, 0.45], direction: [-2.23, -1.44, 0.92]},
    amber: {'emitted-color': [1, 0.24, 0.033], direction: [0.28, -0.77, 4.28]}
  }}
};

function compare(a, b) {
  let changed = 0;
  let maximum = 0;
  let total = 0;
  for (let i = 0; i < a.length; i++) {
    const delta = Math.abs(a[i] - b[i]);
    if (delta) changed++;
    maximum = Math.max(maximum, delta);
    total += delta;
  }
  return {changed, maximum, mean: total / a.length};
}

export function compareBloomPaths() {
  // A non-integer upsample exercises the original scene-size FXAA and final
  // panel-size film pass. Use the real renderer and preserve its post chain.
  const canvas = new OffscreenCanvas(width, height);
  const engine = new SceneRenderer(canvas, null, {worker: true, autoStart: false,
    viewport: {width, height, pixelRatio: 1}, displayHz: 60, reducedMotion: false});
  if (engine.error) throw new Error(engine.error);
  const experiment = installBloomDisplayExperiment(engine);
  const renderer = engine.renderer;
  const gl = renderer.getContext();
  function draw(fused) {
    experiment.setEnabled(fused);
    renderer.info.reset();
    engine.draw(0);
    const drawCalls = renderer.info.render.calls;
    const pixels = new Uint8Array(canvas.width * canvas.height * 4);
    renderer.setRenderTarget(null);
    gl.readPixels(0, 0, canvas.width, canvas.height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
    const error = gl.getError();
    if (error !== gl.NO_ERROR) throw new Error(`Bloom comparison WebGL error 0x${error.toString(16)}`);
    return {drawCalls, pixels};
  }
  function check(name) {
    const baseline = draw(false);
    const fused = draw(true);
    const repeat = draw(false);
    return {baseline, result: {name, calls: [baseline.drawCalls, fused.drawCalls],
      differences: compare(baseline.pixels, fused.pixels), repeat: compare(baseline.pixels, repeat.pixels)}};
  }
  try {
    renderer.info.autoReset = false;
    engine.setQuality('low');
    engine.setWorkLight(0);
    engine.setFog(0.38);
    engine.update(stage);
    engine.camera.position.set(-0.63, 1.76, 4.26);
    engine.controls.target.set(0, 1.62, -2.05);
    engine.clock = 0.85;
    const sdr = check('SDR haze');
    engine.setFilm({bloom: 0});
    const disabled = check('bloom disabled');
    const bloomContribution = compare(sdr.baseline.pixels, disabled.baseline.pixels);
    engine.setFilm({bloom: 0.28, exposure: 1.1, grain: 0.012});
    engine.update({...stage, revision: 2, 'frame-number': 2,
      'glass-meshes': {lens: {kind: 'sphere'}}, 'glass-volumes': [
        {id: 'lens', mesh: 'lens', position: [0.5, 1.48, 0.12], 'half-size': [0.65, 0.65, 0.65],
          color: [0.23, 0.89, 1], strength: 1}
      ]});
    if (!engine.glass.enabled) throw new Error('Glass fixture was not installed');
    // Exercise the extended transform branch. The local WebGL surface is still SDR.
    engine.outputPass.uniforms.extendedDisplay.value = true;
    const extended = check('extended transform with glass');
    engine.update({...stage, revision: 3, 'frame-number': 3, 'glass-volumes': []});
    if (engine.glass.enabled) throw new Error('Glass fixture was not retired');
    const glassContribution = compare(extended.baseline.pixels, draw(false).pixels);
    const telemetry = engine.getTelemetry();
    return {scene: telemetry.pixels, display: telemetry.displayPixels, backend: telemetry.deviceRenderer,
      bloomContribution, glassContribution, cases: [sdr.result, disabled.result, extended.result]};
  } finally {
    experiment.dispose();
    engine.dispose();
  }
}
