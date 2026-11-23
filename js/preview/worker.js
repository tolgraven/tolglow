import {nextDisplayDeadline} from './display-clock.js';
import {SceneRenderer} from './scene-renderer.js';

let engine, canvas, stateId = 0, actionThrough = 0, frameId = 0, pendingFrame = null;
let state, stopped = false, lastDraw = 0, nextDraw = null, renderOnce = false, rendering = false, deferredState = null;
const send = value => postMessage(value);
let active = false, generation = 0;
const canDraw = () => active && state?.generation === generation && (!state.paused || renderOnce);
function schedule(deadline = performance.now()) {
  if (!engine) return;
  engine.displayClock.cancel();
  if (!stopped && !rendering && !pendingFrame && canDraw())
    engine.displayClock.at(deadline, render, engine.softwareRenderer ? .5 : Math.min(2.5, 250 / engine.targetHz));
}
async function completeGpuFrame(gl) {
  const fence = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0), started = performance.now();
  if (!fence) throw new Error('Could not fence the preview GPU frame');
  gl.flush();
  let yields = 0;
  try {
    while (!stopped && !engine.lost) {
      const status = gl.clientWaitSync(fence, 0, 0);
      if (status === gl.ALREADY_SIGNALED || status === gl.CONDITION_SATISFIED) return true;
      if (status === gl.WAIT_FAILED) throw new Error('Preview GPU frame wait failed');
      if (performance.now() - started > 30000) throw new Error('Preview GPU frame did not complete');
      // A few cooperative task yields let short hardware frames complete
      // without the timer wake-up floor. Bound both attempts and elapsed time;
      // slower frames retain cancellable timer polling and the timeout above.
      if (!engine.softwareRenderer && globalThis.scheduler?.yield && yields++ < 16 &&
          performance.now() - started < Math.min(4, 500 / engine.targetHz))
        await scheduler.yield();
      else await new Promise(resolve => setTimeout(resolve, 2));
    }
    return false;
  } finally {gl.deleteSync(fence);}
}
async function render(time) {
  if (stopped || rendering || pendingFrame || !engine || !canDraw()) return;
  if (engine.lost) {schedule(performance.now() + 250); return;}
  rendering = true;
  try {
    const started = performance.now(), previousInterval = engine.frameInterval, drawingGeneration = generation;
    send({type: 'render-start', frameId: frameId + 1, stateId});
    const dt = lastDraw ? Math.min(.1, (time - lastDraw) / 1000) : 0;
    // Keep the display phase. Callback delivery is later than its RAF timestamp;
    // adding that latency to every deadline can skip every other refresh.
    nextDraw = nextDisplayDeadline(nextDraw, time, 1000 / engine.targetHz);
    lastDraw = time; engine.clock += dt; engine.draw(dt); renderOnce = false;
    const submitted = performance.now();
    // ImageBitmap transfer alone only submits shared GPU work. Wait for actual
    // completion, otherwise the worker can saturate the browser compositor's
    // command queue while its own JS timings misleadingly remain fast.
    if (!await completeGpuFrame(engine.renderer.getContext())) return;
    if (!active || drawingGeneration !== generation) {send({type: 'render-retired'}); return;}
    engine.lastGPUWait = performance.now() - submitted;
    const bitmapStarted = performance.now(), bitmap = canvas.transferToImageBitmap(); pendingFrame = ++frameId;
    const completed = performance.now();
    engine.lastBitmapTransfer = completed - bitmapStarted; engine.lastWorkerFrame = completed - started;
    const nominal = 1000 / engine.targetHz;
    // The completion fence and bitmap acknowledgement already bound GPU work.
    // Hardware keeps the display cadence; Auto adjusts spatial cost instead
    // of adding a second throttle that silently halves a reachable refresh rate.
    engine.frameInterval = engine.softwareRenderer ? Math.max(nominal,
      Math.min(engine.lastWorkerFrame * 2, previousInterval * 1.5), previousInterval * .5) : nominal;
    postMessage({type: 'frame', frameId, stateId, actionThrough, generation, bitmap,
      runtimeId: engine.snapshot?.['runtime-id'], showId: engine.snapshot?.['show-id'], epoch: engine.snapshot?.epoch,
      showFrame: engine.snapshot?.['frame-number'] ?? 0,
      telemetry: {...engine.getTelemetry(), workerFrameMs: engine.lastWorkerFrame, rafDeliveryMs: started - time, readyForFrameMs: engine.lastReadyMs, gpuCompletionWaitMs: engine.lastGPUWait,
        idleBudgetMs: Math.max(0, engine.frameInterval - engine.lastWorkerFrame), bitmapTransferMs: engine.lastBitmapTransfer}}, [bitmap]);
  } catch (error) {stopped = true; send({type: 'error', message: String(error?.message || error)});}
  finally {
    rendering = false;
    if (!stopped && deferredState) {const message = deferredState; deferredState = null; applyState(message);}
    if (!stopped && !pendingFrame && !engine.displayClock.pending) schedule(performance.now() + 250);
  }
}
function applyState(message) {
  if (message.id <= stateId || stopped) return;
  stateId = message.id;
  if (!active || message.state.generation !== generation) {
    send({type: 'state-ack', id: stateId, actionThrough}); return;
  }
  state = message.state;
  if (!engine) {
    canvas = new OffscreenCanvas(1, 1);
    let lastStatus = '';
    const status = {set textContent(text) {lastStatus = text; send({type: 'status', text});}, get textContent() {return lastStatus;}};
    engine = new SceneRenderer(canvas, status, {worker: true, autoStart: false,
      viewport: state.viewport, displayHz: state.displayHz, coarsePointer: state.coarsePointer, reducedMotion: state.reducedMotion});
    if (engine.error) throw new Error(engine.error);
  }
  engine.setDisplayHz(state.displayHz);
  engine.platform.viewport = state.viewport; engine.visible = true;
  if (engine.restorePending) engine.restoreContext();
  engine.reducedMotion.matches = state.reducedMotion;
  engine.setQuality(state.quality); engine.setFilm(state.film); engine.setWorkLight(state.workLight); engine.setFog(state.fog);
  engine.setLightingMode(state.lightingMode);
  if (state.snapshot) {
    const snapshot = state.snapshot;
    for (const key of ['sculpt-guide', 'volume-guides', 'glass-meshes', 'glass-volumes'])
      if (!Object.hasOwn(snapshot, key)) snapshot[key] = engine.snapshot?.[key];
    if (!snapshot.rig && engine.snapshot) snapshot.rig = engine.snapshot.rig;
    if (!Object.hasOwn(snapshot, 'editor')) snapshot.editor = engine.snapshot?.editor;
    engine.update(snapshot, false, state.receivedAt - performance.timeOrigin);
  }
  engine.camera.position.fromArray(state.camera.position); engine.controls.target.fromArray(state.camera.target);
  engine.camera.up.fromArray(state.camera.up); engine.camera.fov = state.camera.fov;
  engine.cameraName = state.camera.name; engine.controls.update(); engine.resize();
  for (const action of state.actions) if (action.id > actionThrough) {
    if (action.type === 'burst') engine.burst(action.options);
    else if (action.type === 'draw') renderOnce = true;
    actionThrough = action.id;
  }
  send({type: 'state-ack', id: stateId, actionThrough});
  if (state.paused) {lastDraw = 0; nextDraw = null; engine.lighting.previous = null;}
  if (!engine.displayClock.pending || state.paused) schedule();
}
self.onmessage = event => {
  try {
    const message = event.data;
    if (message.type === 'activity') {
      active = message.active; generation = message.generation; renderOnce = false; lastDraw = 0; nextDraw = null;
      if (engine) {
        engine.visible = active; engine.platform.hidden = !active;
        if (!active) engine.fog?.showAtmosphere?.suspend();
        engine.displayClock.cancel(); engine.lighting.latest = engine.lighting.previous = null;
      }
    } else if (message.type === 'state') {
      // One upstream message is in flight, so this slot stays bounded while a
      // GPU fence is pending. Ack only after applying the complete projection.
      if (rendering) deferredState = message;
      else applyState(message);
    }
    else if (message.type === 'frame-ack' && message.frameId === pendingFrame) {
      pendingFrame = null;
      // Include callback delivery, completion and bitmap acknowledgement. A frame
      // that finishes its GPU work just before refresh can still miss presentation.
      engine.lastReadyMs = performance.now() - lastDraw;
      if (canDraw()) engine.measureLoad(engine.lastReadyMs, performance.now());
      const deadline = engine.softwareRenderer ? performance.now() + engine.frameInterval : nextDraw;
      schedule(deadline);
    } else if (message.type === 'dispose') {stopped = true; engine?.dispose(); close();}
  } catch (error) {stopped = true; send({type: 'error', message: String(error?.message || error)});}
};
send({type: 'ready'});
