import {SceneRenderer} from './scene-renderer.js';

let engine, canvas, stateId = 0, actionThrough = 0, frameId = 0, pendingFrame = null;
let state, timer, stopped = false, lastDraw = 0, renderOnce = false, rendering = false, deferredState = null;
const send = value => postMessage(value);
const visible = () => state && !state.hidden && state.visible && !state.paused;
function schedule(delay = 0) {
  clearTimeout(timer); timer = null;
  if (!stopped && !rendering && !pendingFrame && engine && (visible() || renderOnce)) timer = setTimeout(render, delay);
}
async function completeGpuFrame(gl) {
  const fence = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0), started = performance.now();
  if (!fence) throw new Error('Could not fence the preview GPU frame');
  gl.flush();
  try {
    while (!stopped && !engine.lost) {
      const status = gl.clientWaitSync(fence, 0, 0);
      if (status === gl.ALREADY_SIGNALED || status === gl.CONDITION_SATISFIED) return true;
      if (status === gl.WAIT_FAILED) throw new Error('Preview GPU frame wait failed');
      if (performance.now() - started > 30000) throw new Error('Preview GPU frame did not complete');
      await new Promise(resolve => setTimeout(resolve, 4));
    }
    return false;
  } finally {gl.deleteSync(fence);}
}
async function render() {
  timer = null;
  if (stopped || rendering || pendingFrame || !engine || (!visible() && !renderOnce)) return;
  if (engine.lost) {schedule(250); return;}
  rendering = true;
  try {
    const started = performance.now(), previousInterval = engine.frameInterval;
    send({type: 'render-start', frameId: frameId + 1, stateId});
    const dt = lastDraw ? Math.min(.1, (started - lastDraw) / 1000) : 0;
    lastDraw = started; engine.clock += dt; engine.draw(dt); renderOnce = false;
    const submitted = performance.now();
    // ImageBitmap transfer alone only submits shared GPU work. Wait for actual
    // completion, otherwise the worker can saturate the browser compositor's
    // command queue while its own JS timings misleadingly remain fast.
    if (!await completeGpuFrame(engine.renderer.getContext())) return;
    engine.lastGPUWait = performance.now() - submitted;
    const bitmapStarted = performance.now(), bitmap = canvas.transferToImageBitmap(); pendingFrame = ++frameId;
    const completed = performance.now();
    engine.lastBitmapTransfer = completed - bitmapStarted; engine.lastWorkerFrame = completed - started;
    const nominal = 1000 / engine.options.fps;
    engine.frameInterval = Math.max(nominal,
      Math.min(engine.lastWorkerFrame * (engine.softwareRenderer ? 2 : 1.5), previousInterval * 1.5), previousInterval * .5);
    postMessage({type: 'frame', frameId, stateId, actionThrough, bitmap,
      showId: engine.snapshot?.['show-id'], epoch: engine.snapshot?.epoch,
      telemetry: {...engine.getTelemetry(), workerFrameMs: engine.lastWorkerFrame, gpuCompletionWaitMs: engine.lastGPUWait,
        idleBudgetMs: Math.max(0, engine.frameInterval - engine.lastWorkerFrame), bitmapTransferMs: engine.lastBitmapTransfer}}, [bitmap]);
  } catch (error) {stopped = true; send({type: 'error', message: String(error?.message || error)});}
  finally {
    rendering = false;
    if (!stopped && deferredState) {const message = deferredState; deferredState = null; applyState(message);}
    if (!stopped && !pendingFrame && !timer) schedule(250);
  }
}
function applyState(message) {
  if (message.id <= stateId || stopped) return;
  stateId = message.id; state = message.state;
  if (!engine) {
    canvas = new OffscreenCanvas(1, 1);
    const status = {set textContent(text) {send({type: 'status', text});}, get textContent() {return '';}};
    engine = new SceneRenderer(canvas, status, {worker: true, autoStart: false,
      viewport: state.viewport, hidden: state.hidden, coarsePointer: state.coarsePointer, reducedMotion: state.reducedMotion});
    if (engine.error) throw new Error(engine.error);
  }
  engine.platform.viewport = state.viewport; engine.platform.hidden = state.hidden;
  engine.visible = state.visible; engine.reducedMotion.matches = state.reducedMotion;
  engine.setQuality(state.quality); engine.setWorkLight(state.workLight); engine.setFog(state.fog);
  if (state.snapshot) engine.update(state.snapshot);
  engine.camera.position.fromArray(state.camera.position); engine.controls.target.fromArray(state.camera.target);
  engine.camera.up.fromArray(state.camera.up); engine.camera.fov = state.camera.fov;
  engine.cameraName = state.camera.name; engine.controls.update(); engine.resize();
  for (const action of state.actions) if (action.id > actionThrough) {
    if (action.type === 'burst') engine.burst(action.options);
    else if (action.type === 'draw') renderOnce = true;
    actionThrough = action.id;
  }
  send({type: 'state-ack', id: stateId, actionThrough});
  if (timer === null || timer === undefined) schedule();
}
self.onmessage = event => {
  try {
    const message = event.data;
    if (message.type === 'state') {
      // One upstream message is in flight, so this slot stays bounded while a
      // GPU fence is pending. Ack only after applying the complete projection.
      if (rendering) deferredState = message;
      else applyState(message);
    }
    else if (message.type === 'frame-ack' && message.frameId === pendingFrame) {
      pendingFrame = null;
      schedule(Math.max(0, engine.frameInterval - engine.lastWorkerFrame));
    } else if (message.type === 'dispose') {stopped = true; clearTimeout(timer); engine?.dispose(); close();}
  } catch (error) {stopped = true; send({type: 'error', message: String(error?.message || error)});}
};
send({type: 'ready'});
