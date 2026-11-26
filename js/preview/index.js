import {hdrDisplayAvailable} from './render-quality.js';
import {SceneRenderer} from './scene-renderer.js';
import {WorkerRoomPreview, workerSupported} from './worker-proxy.js';

// One scene implementation, with DOM controls kept off the render thread when
// OffscreenCanvas is available. The worker proxy hands the visible canvas to
// the worker after its first frame where the browser can present from there.
export class RoomPreview {
  constructor(canvas, status, options = {}) {
    return options.worker !== false && workerSupported() && !hdrDisplayAvailable()
      ? new WorkerRoomPreview(canvas, status, options) : new SceneRenderer(canvas, status);
  }
}
export {SceneRenderer};
export {CanvasPlayback, canPresentCanvas} from './playback.js';
export {PushScene} from './push-scene.js';
