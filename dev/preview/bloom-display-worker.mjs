// Bundle only into the private experiment page. Keep the normal Room worker's
// frame scheduling, completion fence and bitmap acknowledgement in both arms.
import {SceneRenderer} from '../../js/preview/scene-renderer.js';
import '../../js/preview/worker.js';
import {installBloomDisplayExperiment} from './bloom-display-experiment.mjs';

let trial = {fused: false, scale: 1};
let lastDraw;
const experiments = new WeakMap();
const originalDraw = SceneRenderer.prototype.draw;
const originalDispose = SceneRenderer.prototype.dispose;

SceneRenderer.prototype.draw = function (dt) {
  if (!this.renderer || !this.visible || this.disposed || this.lost) return originalDraw.call(this, dt);
  if (!experiments.has(this)) experiments.set(this, installBloomDisplayExperiment(this));
  if (this.renderScale !== trial.scale) {
    this.renderScale = trial.scale;
    this.resizePending = true;
  }
  experiments.get(this).setEnabled(trial.fused);
  lastDraw = trial;
  return originalDraw.call(this, dt);
};

SceneRenderer.prototype.dispose = function () {
  experiments.get(this)?.dispose();
  experiments.delete(this);
  return originalDispose.call(this);
};

const send = self.postMessage.bind(self);
self.postMessage = (message, ...options) => {
  if (message?.type === 'frame') message.telemetry.bloomExperiment = lastDraw;
  send(message, ...options);
};

self.setBloomDisplayTrial = (fused, scale) => {
  if (typeof fused !== 'boolean' || !Number.isFinite(scale) || scale <= 0 || scale > 1)
    throw new Error('Supply a fusion mode and a render scale in (0, 1].');
  trial = {fused, scale};
};
