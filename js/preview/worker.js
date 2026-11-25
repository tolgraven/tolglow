import {DisplayRefresh, nextDisplayDeadline} from './display-clock.js';
import {showIdentity} from './presentation.js';
import {SceneRenderer} from './scene-renderer.js';

let engine, canvas, stateId = 0, actionThrough = 0, frameId = 0;
let state, stopped = false, lastDraw = 0, nextDraw = null, renderOnce = false;
let active = false, generation = 0, deferredState = null, pendingFrame = null;
let pendingLoad = null, lastAck = null, pollTimer = null, yielding = false;
// A transferred presentation surface lets completed frames reach the page
// without a main-thread task. Until it arrives, bitmaps travel to the proxy.
let surface = null, presenter = null, expectedIdentity = null;
let proxyHz = null, workerHz = null;
const frames = [];
const send = value => postMessage(value);
const canDraw = () => active && state?.generation === generation && (!state.paused || renderOnce);
const gpuPending = () => frames.some(frame => frame.fence);
const frameIdentity = metadata => showIdentity({'runtime-id': metadata.runtimeId, 'show-id': metadata.showId, epoch: metadata.epoch});

// Animation callbacks on a busy Studio thread arrive at a fraction of the panel
// rate. This thread samples the same display; neither sample can exceed it.
const refresh = new DisplayRefresh(hz => {workerHz = hz; applyDisplayHz();}, self);

function applyDisplayHz() {
  const hz = Math.max(proxyHz ?? 0, workerHz ?? 0);
  if (engine && hz) engine.setDisplayHz(hz);
}

function canSubmit() {
  if (stopped || !engine || engine.lost || deferredState || !canDraw()) return false;
  const limit = engine.softwareRenderer || state.paused ? 1 : 2;
  return frames.length + Number(pendingFrame !== null) < limit && !(engine.resizePending && gpuPending());
}

function schedule() {
  if (canSubmit() && !engine.displayClock.pending) {
    engine.displayClock.at(nextDraw ?? performance.now(), render,
      engine.softwareRenderer ? .5 : Math.min(2.5, 250 / engine.targetHz));
  }
}

function closeFrame(frame) {
  if (frame.fence) frame.gl.deleteSync(frame.fence);
  frame.bitmap?.close();
}

function retireFrames() {
  clearTimeout(pollTimer); pollTimer = null;
  for (const frame of frames) closeFrame(frame);
  if (frames.length) {
    frames.length = 0;
    send({type: 'render-retired', rendering: false});
  }
}

function stop() {
  stopped = true; deferredState = pendingLoad = null;
  refresh.setActive(false);
  engine?.displayClock.cancel();
  retireFrames();
  engine?.dispose();
}

function fail(error) {
  stop();
  send({type: 'error', message: String(error?.message || error)});
}

function capture(frame) {
  const started = performance.now();
  frame.bitmap = canvas.transferToImageBitmap();
  frame.bitmapMs = performance.now() - started;
}

function pollAgain(frame) {
  if (pollTimer !== null || yielding) return;
  if (!engine.softwareRenderer && globalThis.scheduler?.yield && frame.yields < 16 &&
      performance.now() - frame.submitted < Math.min(4, 500 / engine.targetHz)) {
    frame.yields++;
    yielding = true;
    scheduler.yield().then(() => {yielding = false; advance();}, fail);
  } else {
    pollTimer = setTimeout(() => {pollTimer = null; advance();}, 2);
  }
}

function completed(frame, now) {
  if (active && frame.metadata.generation === generation) {
    engine.lastReadyMs = now - frame.time;
    // With render-ahead, completed frames can arrive every refresh even
    // when their individual latency spans two refreshes.
    pendingLoad = {elapsed: Math.min(engine.lastReadyMs, lastAck === null ? Infinity : now - lastAck), now};
    lastAck = now;
    if (engine.softwareRenderer) nextDraw = now + engine.frameInterval;
  }
}

function frameMessage(frame) {
  const {metadata, telemetry, time, started, duration, gpuWait, bitmapMs} = frame;
  return {type: 'frame', ...metadata, rendering: frames.length > 0,
    telemetry: {...telemetry, frameIntervalMs: engine.frameInterval,
      workerFrameMs: duration, rafDeliveryMs: started - time, readyForFrameMs: engine.lastReadyMs,
      gpuCompletionWaitMs: gpuWait, idleBudgetMs: Math.max(0, engine.frameInterval - duration),
      bitmapTransferMs: bitmapMs, presentation: presenter ? 'worker' : 'proxy', presentSubmitMs: frame.presentMs}};
}

// Show the image from this thread. An image from a show the page has already
// left is closed, exactly as the proxy would refuse it.
function present(frame) {
  const {bitmap} = frame, presented = frameIdentity(frame.metadata) === expectedIdentity;
  frame.bitmap = null;
  if (presented) {
    if (surface.width !== bitmap.width || surface.height !== bitmap.height) {
      surface.width = bitmap.width; surface.height = bitmap.height;
    }
    const begun = performance.now();
    presenter.transferFromImageBitmap(bitmap);
    frame.presentMs = performance.now() - begun;
  }
  bitmap.close();
  completed(frame, performance.now());
  send({...frameMessage(frame), presented});
}

function publish() {
  while (presenter && frames[0] && !frames[0].fence) present(frames.shift());
  const frame = frames[0];
  if (presenter || !frame || frame.fence || pendingFrame) return;
  frames.shift();
  pendingFrame = frame;
  const {bitmap} = frame;
  try {
    postMessage({...frameMessage(frame), bitmap}, [bitmap]);
    frame.bitmap = null;
  } catch (error) {
    bitmap.close();
    throw error;
  }
}

function advance() {
  if (stopped) return;
  try {
    if (engine.lost) {retireFrames(); return;}
    for (const frame of frames) {
      if (!frame.fence) continue;
      const {gl, fence} = frame, status = gl.clientWaitSync(fence, 0, 0);
      if (status === gl.WAIT_FAILED) throw new Error('Preview GPU frame wait failed');
      if (status !== gl.ALREADY_SIGNALED && status !== gl.CONDITION_SATISFIED) {
        if (performance.now() - frame.submitted > 30000) throw new Error('Preview GPU frame did not complete');
        break;
      }
      gl.deleteSync(fence); frame.fence = null;
      frame.gpuWait = performance.now() - frame.submitted;
      if (active && frame.metadata.generation === generation && !frame.bitmap) capture(frame);
      frame.duration = performance.now() - frame.started;
      if (engine.softwareRenderer) {
        const nominal = 1000 / engine.targetHz, previous = engine.frameInterval;
        engine.frameInterval = Math.max(nominal, Math.min(frame.duration * 2, previous * 1.5), previous * .5);
      }
    }
    let retired = false;
    while (frames[0] && !frames[0].fence && (!active || frames[0].metadata.generation !== generation)) {
      closeFrame(frames.shift()); retired = true;
    }
    if (retired) send({type: 'render-retired', rendering: frames.length > 0});
    publish();

    const waiting = frames.find(frame => frame.fence);
    if (waiting) pollAgain(waiting);
    else {
      clearTimeout(pollTimer); pollTimer = null;
      // Changed projections and quality tiers can replace GPU resources. Their
      // application waits for every submitted draw, even if a bitmap is ready.
      if (pendingLoad) {
        if (canDraw()) engine.measureLoad(pendingLoad.elapsed, pendingLoad.now);
        pendingLoad = null;
      }
      if (deferredState) {
        const message = deferredState; deferredState = null; applyState(message);
      }
    }
    schedule();
  } catch (error) {fail(error);}
}

function render(time) {
  if (!canSubmit()) return;
  try {
    const started = performance.now(), id = ++frameId;
    send({type: 'render-start', frameId: id, stateId});
    const dt = lastDraw ? Math.min(.1, (time - lastDraw) / 1000) : 0;
    const nominal = 1000 / engine.targetHz;
    nextDraw = nextDisplayDeadline(nextDraw, time, nominal);
    if (!engine.softwareRenderer) engine.frameInterval = nominal;
    lastDraw = time; engine.clock += dt; engine.draw(dt); renderOnce = false;
    const submitted = performance.now(), gl = engine.renderer.getContext();
    const fence = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0);
    if (!fence) throw new Error('Could not fence the preview GPU frame');
    const snapshot = engine.snapshot;
    const frame = {time, started, submitted, gl, fence, bitmap: null, yields: 0,
      metadata: {frameId: id, stateId, actionThrough, generation, runtimeId: snapshot?.['runtime-id'],
        showId: snapshot?.['show-id'], epoch: snapshot?.epoch, showFrame: snapshot?.['frame-number'] ?? 0}};
    frames.push(frame);
    gl.flush();
    // Retain this drawing buffer before recording the next frame. A hardware
    // bitmap stays in the worker until its fence and the previous canvas ack.
    if (!engine.softwareRenderer) capture(frame);
    frame.telemetry = engine.getTelemetry();
    advance();
  } catch (error) {fail(error);}
}

function applyState(message, outputOnly = false) {
  if (message.id <= stateId || stopped) return;
  stateId = message.id;
  if (!active || message.state.generation !== generation) {
    send({type: 'state-ack', id: stateId, actionThrough}); return;
  }
  state = message.state;
  if (outputOnly) {
    engine.updateEmittedOutput({...engine.snapshot, ...state.snapshot}, state.receivedAt - performance.timeOrigin);
    send({type: 'state-ack', id: stateId, actionThrough});
    schedule();
    return;
  }
  if (!engine) {
    canvas = new OffscreenCanvas(1, 1);
    let lastStatus = '';
    const status = {set textContent(text) {lastStatus = text; send({type: 'status', text});}, get textContent() {return lastStatus;}};
    engine = new SceneRenderer(canvas, status, {worker: true, autoStart: false,
      viewport: state.viewport, displayHz: state.displayHz, coarsePointer: state.coarsePointer, reducedMotion: state.reducedMotion});
    if (engine.error) throw new Error(engine.error);
    const signal = engine.abort.signal;
    canvas.addEventListener('webglcontextlost', () => {
      engine.displayClock.cancel();
      retireFrames();
    }, {signal});
    canvas.addEventListener('webglcontextrestored', () => advance(), {signal});
  }
  proxyHz = state.displayHz; applyDisplayHz();
  refresh.setActive(active && !state.paused);
  engine.platform.viewport = state.viewport; engine.visible = true;
  if (engine.restorePending) engine.restoreContext();
  engine.reducedMotion.matches = state.reducedMotion;
  engine.setQuality(state.quality); engine.setFilm(state.film); engine.setWorkLight(state.workLight); engine.setFog(state.fog);
  engine.setLightingMode(state.lightingMode);
  if (state.snapshot) {
    const snapshot = state.snapshot;
    for (const key of ['sculpt-guide', 'spatial-volumes', 'volume-guides', 'glass-meshes', 'glass-volumes'])
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
  if (state.paused) {
    lastDraw = 0; nextDraw = null; lastAck = null;
    engine.lighting.previous = null; engine.displayClock.cancel();
  }
  schedule();
}
self.onmessage = event => {
  if (stopped) return;
  try {
    const message = event.data;
    if (message.type === 'activity') {
      active = message.active; generation = message.generation;
      refresh.setActive(active && !!state && !state.paused);
      renderOnce = false; lastDraw = 0; nextDraw = lastAck = pendingLoad = null;
      if (deferredState && deferredState.state.generation !== generation) {
        const stale = deferredState; deferredState = null; applyState(stale);
      }
      if (engine) {
        engine.visible = active; engine.platform.hidden = !active;
        if (!active) engine.fog?.showAtmosphere?.suspend();
        engine.displayClock.cancel(); engine.lighting.latest = engine.lighting.previous = null;
        advance();
      }
    } else if (message.type === 'state') {
      // Emitted output can feed the next draw while a prior bitmap completes.
      // Drain for scene/viewport changes, which can replace GPU resources.
      const current = active && message.state.generation === generation;
      if (current && message.outputOnly && !deferredState && engine?.snapshot &&
          !engine.lost && !engine.softwareRenderer &&
          showIdentity(message.state.snapshot) === showIdentity(engine.snapshot))
        applyState(message, true);
      else if (current &&
          (gpuPending() || (engine?.lost && !engine.restorePending))) deferredState = message;
      else applyState(message);
    } else if (message.type === 'frame-ack' && message.frameId === pendingFrame?.metadata.frameId) {
      const acknowledged = pendingFrame; pendingFrame = null;
      completed(acknowledged, performance.now());
      advance();
    } else if (message.type === 'surface') {
      // The proxy proved a first frame and handed over the visible canvas.
      surface = message.canvas; presenter = surface.getContext('bitmaprenderer');
      if (!presenter) throw new Error('No worker bitmap presentation context');
      expectedIdentity = message.identity;
      const first = pendingFrame; pendingFrame = null;
      if (message.bitmap) {
        if (first?.metadata.frameId === message.frameId) {first.bitmap = message.bitmap; present(first);}
        else message.bitmap.close();
      } else if (first) completed(first, performance.now());
      advance();
    } else if (message.type === 'identity') {
      expectedIdentity = message.identity;
      presenter?.transferFromImageBitmap(null);
    } else if (message.type === 'dispose') {presenter?.transferFromImageBitmap(null); stop(); close();}
  } catch (error) {fail(error);}
};
send({type: 'ready'});
