import {hdrDisplayAvailable} from './render-quality.js';
import {SceneRenderer} from './scene-renderer.js';
import {WorkerRoomPreview, workerSupported} from './worker-proxy.js';

// One scene implementation, with DOM controls/presentation kept off the render
// thread when OffscreenCanvas is available. The visible canvas is never moved.
export class RoomPreview {
  constructor(canvas, status, options = {}) {
    return options.worker !== false && workerSupported() && !hdrDisplayAvailable()
      ? new WorkerRoomPreview(canvas, status, options) : new SceneRenderer(canvas, status);
  }
}
export {SceneRenderer};

export {PushScene} from './push-scene.js';
