// Imported only by the browser qualification harness's private worker bundle.
import {FramebufferTexture, NoBlending, ShaderMaterial} from 'three';
import {FullScreenQuad} from 'three/addons/postprocessing/Pass.js';
import {SceneRenderer} from '../../js/preview/scene-renderer.js';
import '../../js/preview/worker.js';

const modes = new Set(['full', 'cached-shadows', 'skip-fog-raymarch', 'present-cached-frame']);
let mode = 'full', probe;

class FrameCopy {
  constructor(engine) {
    const {renderer, canvas} = engine;
    this.texture = new FramebufferTexture(canvas.width, canvas.height);
    this.material = new ShaderMaterial({uniforms: {frame: {value: this.texture}}, depthTest: false,
      depthWrite: false, blending: NoBlending,
      vertexShader: 'varying vec2 at; void main(){at=uv;gl_Position=vec4(position.xy,0.,1.);}',
      fragmentShader: 'varying vec2 at;uniform sampler2D frame;void main(){gl_FragColor=texture2D(frame,at);}'});
    this.quad = new FullScreenQuad(this.material);
    renderer.setRenderTarget(null);
    renderer.copyFramebufferToTexture(this.texture);
  }
  draw(renderer) {
    renderer.setRenderTarget(null);
    this.quad.render(renderer);
  }
  dispose() {this.texture.dispose(); this.material.dispose(); this.quad.dispose();}
}

function inspect(engine) {
  const renderer = engine.renderer, gl = renderer.getContext();
  const ext = gl.getExtension('EXT_disjoint_timer_query_webgl2');
  const originalShadowUpdate = renderer.shadowMap.autoUpdate;
  const phases = ['opaque', 'fog', 'post'], pending = new Map();
  let sample, frame = 0, copy;
  function observe(phase, run) {
    const timed = ext && frame % 8 === 0 && !pending.has(phase);
    const query = timed ? gl.createQuery() : null;
    if (query) gl.beginQuery(ext.TIME_ELAPSED_EXT, query);
    const start = performance.now();
    try {return run();} finally {
      sample.submitMs[phase] = performance.now() - start;
      if (query) {gl.endQuery(ext.TIME_ELAPSED_EXT); pending.set(phase, query);}
    }
  }
  function prepare() {
    frame++;
    sample = {mode, frame, submitMs: {}, gpuMs: {}, timerSupported: Boolean(ext)};
    if (!ext) return;
    const disjoint = gl.getParameter(ext.GPU_DISJOINT_EXT);
    for (const [name, query] of pending) {
      if (disjoint || gl.getQueryParameter(query, gl.QUERY_RESULT_AVAILABLE)) {
        if (!disjoint) sample.gpuMs[name] = gl.getQueryParameter(query, gl.QUERY_RESULT) / 1e6;
        gl.deleteQuery(query); pending.delete(name);
      }
    }
  }
  const opaque = renderer.render.bind(renderer);
  renderer.render = (scene, camera) => scene === engine.scene && camera === engine.camera
    ? observe(phases[0], () => opaque(scene, camera)) : opaque(scene, camera);
  const fog = engine.fog.render.bind(engine.fog);
  engine.fog.render = (...args) => observe(phases[1], () => {
    const amount = engine.fog.amount;
    if (mode === 'skip-fog-raymarch') engine.fog.amount = 0;
    try {return fog(...args);} finally {engine.fog.amount = amount;}
  });
  const compose = engine.composer.render.bind(engine.composer);
  engine.composer.render = (...args) => observe(phases[2], () => compose(...args));
  return {
    draw(dt, ordinary) {
      prepare();
      renderer.shadowMap.autoUpdate = mode === 'cached-shadows' ? false : originalShadowUpdate;
      if (mode === 'present-cached-frame' && copy) {
        const start = performance.now();
        observe('replay', () => copy.draw(renderer));
        engine.lastCPU = performance.now() - start; engine.draws++;
      } else {
        ordinary.call(engine, dt);
        if (mode === 'present-cached-frame') copy = new FrameCopy(engine);
      }
    },
    reset() {
      copy?.dispose(); copy = null;
      for (const query of pending.values()) gl.deleteQuery(query);
      pending.clear();
      renderer.shadowMap.autoUpdate = originalShadowUpdate;
    },
    sample: () => sample
  };
}

const draw = SceneRenderer.prototype.draw;
SceneRenderer.prototype.draw = function(dt) {
  if (!this.renderer || !this.fog || !this.composer || this.disposed || this.lost || !this.visible) return draw.call(this, dt);
  probe ||= inspect(this);
  return probe.draw(dt, draw);
};

const send = self.postMessage.bind(self);
self.postMessage = function(message, ...options) {
  if (message?.type === 'frame' && probe) {
    message.telemetry.passProbe = {...probe.sample(), sentAt: performance.timeOrigin + performance.now()};
  }
  return send(message, ...options);
};
self.setRoomPassMode = choice => {
  if (!modes.has(choice)) throw new Error('Unknown Room pass probe '+choice);
  probe?.reset(); mode = choice;
  return choice;
};
