import {SceneRenderer} from './scene-renderer.js';

let engine, canvas, stateId = 0, actionThrough = 0, frameId = 0, pendingFrame = null;
let state, timer, stopped = false, lastDraw = 0, renderOnce = false;
const send = value => postMessage(value);
const visible = () => state && !state.hidden && state.visible && !state.paused;
function schedule(delay = 0) {
  clearTimeout(timer); timer = null;
  if (!stopped && !pendingFrame && engine && (visible() || renderOnce)) timer = setTimeout(render, delay);
}
function render() {
  timer = null;
  if (stopped || pendingFrame || !engine || (!visible() && !renderOnce)) return;
  if (engine.lost) {schedule(250); return;}
  try {
    const started = performance.now();
    send({type: 'render-start', frameId: frameId + 1, stateId});
    const dt = lastDraw ? Math.min(.1, (started - lastDraw) / 1000) : 0;
    lastDraw = started; engine.clock += dt; engine.draw(dt); renderOnce = false;
    const bitmapStarted = performance.now(), bitmap = canvas.transferToImageBitmap(); pendingFrame = ++frameId;
    const completed = performance.now();
    engine.lastBitmapTransfer = completed - bitmapStarted; engine.lastWorkerFrame = completed - started;
    postMessage({type: 'frame', frameId, stateId, actionThrough, bitmap,
      showId: engine.snapshot?.['show-id'], epoch: engine.snapshot?.epoch,
      telemetry: {...engine.getTelemetry(), workerFrameMs: engine.lastWorkerFrame, bitmapTransferMs: engine.lastBitmapTransfer}}, [bitmap]);
  } catch (error) {stopped = true; send({type: 'error', message: String(error?.message || error)});}
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
    if (message.type === 'state') applyState(message);
    else if (message.type === 'frame-ack' && message.frameId === pendingFrame) {
      pendingFrame = null;
      schedule(Math.max(0, engine.frameInterval - engine.lastWorkerFrame));
    } else if (message.type === 'dispose') {stopped = true; clearTimeout(timer); engine?.dispose(); close();}
  } catch (error) {stopped = true; send({type: 'error', message: String(error?.message || error)});}
};
send({type: 'ready'});
