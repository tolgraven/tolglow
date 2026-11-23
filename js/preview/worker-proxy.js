import {RigEditorInput} from './editor.js';
import * as THREE from 'three';
import {SceneRenderer} from './scene-renderer.js';
import {cameraControls, cameraPreset} from './camera.js';
import {PreviewActivity} from './preview-activity.js';
import {CameraNavigator} from './camera-navigation.js';
import {LatestStateChannel} from './mailbox.js';
import {presentFrame, showIdentity} from './presentation.js';
import {clamp, roomDimensions} from './math.js';

export function workerSupported() {
  return typeof Worker !== 'undefined' && typeof OffscreenCanvas !== 'undefined'
    && typeof OffscreenCanvas.prototype.transferToImageBitmap === 'function';
}

/** DOM/input owner only. Scene, fog and WebGL objects live in the worker. */
export class WorkerRoomPreview {
  constructor(canvas, status, options = {}) {
    this.canvas = canvas; canvas.tolglowPreview = this; this.status = status; this.disposed = false;
    this._ready = false; this.presented = false; this.room = [12, 5, 10]; this.snapshot = null;
    this.identity = showIdentity(null); this.visible = false; this.generation = 0; this.paused = false;
    this.filmSettings = {}; this.lightingMode = 'hold'; this.fogOverride = null; this.workLight = .12; this.qualityChoice = 'auto'; this.actions = []; this.actionId = 0;
    this.lastFrame = 0; this.discardedFrames = 0; this.telemetry = {}; this.rendering = false;
    this.navigationListeners = new Set();
    this.abort = new AbortController(); this.whenReady = new Promise(resolve => {this.resolveReady = resolve;});
    this.camera = new THREE.PerspectiveCamera(49, 1, .08, 100);
    this.controls = cameraControls(this.camera, canvas); cameraPreset(this.camera, this.controls, this.room, 'audience'); this.cameraName = 'audience';
    this.controls.addEventListener('change', () => this.sendState());
    this.reducedMotion = matchMedia('(prefers-reduced-motion: reduce)'); this.coarsePointer = matchMedia('(pointer: coarse)');
    const signal = this.abort.signal;
    this.reducedMotion.addEventListener('change', () => {this.navigation?.notify(); this.sendState();}, {signal});
    this.coarsePointer.addEventListener('change', () => this.sendState(), {signal});
    this.navigation = new CameraNavigator({camera: this.camera, controls: this.controls, canvas,
      room: () => this.room, snapshot: () => this.snapshot, visibleLights: () => this.telemetry.lightIds,
      active: () => this.visible && !this.paused && !this.disposed,
      reducedMotion: () => this.reducedMotion.matches, changed: () => this.sendState()});
    this.navigation.subscribe(state => this.notifyNavigation(state));
    this.resizeObserver = new ResizeObserver(() => this.resize()); this.resizeObserver.observe(canvas);
    this.activity = new PreviewActivity(canvas, active => this.changeActivity(active));
    this.setStatus('Starting background room renderer…');
    try {
      const createWorker = options.workerFactory || (url => new Worker(url, {type: 'module', name: 'tolglow-room-preview'}));
      this.worker = createWorker(new URL('./preview-worker.js', import.meta.url));
      this.channel = new LatestStateChannel(message => {
        const snapshot = message.state.snapshot;
        const changed = snapshot !== this.lastSentSnapshot;
        const projection = changed && snapshot ? {...snapshot} : null;
        if (projection && snapshot.rig === this.lastSentSnapshot?.rig) delete projection.rig;
        for (const key of ['editor', 'sculpt-guide', 'volume-guides', 'glass-meshes', 'glass-volumes'])
          if (projection && snapshot[key] === this.lastSentSnapshot?.[key]) delete projection[key];
        this.worker.postMessage({...message, state: {...message.state, snapshot: projection}});
        this.lastSentSnapshot = snapshot;
      });
      this.worker.onmessage = event => this.receive(event.data);
      this.worker.onerror = event => {event.preventDefault(); this.fail(event.message || 'Background renderer failed');};
      this.worker.onmessageerror = () => this.fail('Background renderer message could not be read');
      this.checkStartup = () => {
        if (this.ready || this.disposed || this.fallback) return;
        if (this.visible && !this.paused) this.fail('Background renderer did not produce its first frame');
      };
      this.startupTimeout = options.startupTimeout ?? 45000;
      this.resize();
    } catch (error) {this.fail(error);}
  }
  get ready() {return this.fallback ? this.fallback.ready : this._ready;}
  setStatus(text) {if (this.status && this.status.textContent !== text) this.status.textContent = text;}
  changeActivity(active) {
    if (this.disposed || this.fallback) return;
    this.visible = active; ++this.generation;
    this.navigation.refreshActivity(); clearTimeout(this.startupTimer);
    // Out-of-band cancellation also invalidates work already waiting on a GPU fence.
    this.worker?.postMessage({type: 'activity', active, generation: this.generation});
    this.lastSentSnapshot = null;
    if (active) {
      if (!this.ready && !this.paused) this.startupTimer = setTimeout(this.checkStartup, this.startupTimeout);
      this.sendState();
    } else {
      if (this.channel) this.channel.pending = null;
      this.actions = []; this.drawRequest?.resolve(false); this.drawRequest = null;
    }
  }
  sendState() {
    if (this.disposed || this.fallback || !this.channel || !this.visible) return;
    const bounds = this.canvas.getBoundingClientRect();
    this.camera.aspect = Math.max(1, bounds.width) / Math.max(1, bounds.height); this.camera.updateProjectionMatrix();
    this.channel.offer({snapshot: this.snapshot, receivedAt: this.receivedAt, lightingMode: this.lightingMode, viewport: {width: Math.max(1, bounds.width), height: Math.max(1, bounds.height), pixelRatio: devicePixelRatio || 1},
      generation: this.generation, paused: this.paused,
      reducedMotion: this.reducedMotion.matches, coarsePointer: this.coarsePointer.matches,
      quality: this.qualityChoice, film: this.filmSettings, fog: this.fogOverride, workLight: this.workLight,
      camera: {position: this.camera.position.toArray(), target: this.controls.target.toArray(), up: this.camera.up.toArray(), fov: this.camera.fov, name: this.cameraName},
      actions: this.actions.slice()});
  }
  receive(message) {
    if (this.disposed || this.fallback) {message.bitmap?.close(); return;}
    if (message.type === 'ready') this.channel.open();
    else if (message.type === 'state-ack') {
      this.actions = this.actions.filter(action => action.id > message.actionThrough); this.channel.acknowledge(message.id);
    } else if (message.type === 'status') this.setStatus(message.text);
    else if (message.type === 'render-start') {this.rendering = true; this.startedRendering = true;}
    else if (message.type === 'render-retired') this.rendering = false;
    else if (message.type === 'error') this.fail(message.message);
    else if (message.type === 'frame') {
      this.rendering = false;
      try {
        if (!this.visible || message.generation !== this.generation) {
          message.bitmap.close(); this.discardedFrames++; return;
        }
        const accepted = presentFrame(message, this.identity, this.lastFrame, bitmap => {
          // Do not claim the DOM canvas context before an actual worker frame.
          // Worker startup failure therefore leaves the same canvas available
          // for the shared renderer's WebGL capability fallback.
          if (!this.presenter) {
            this.presenter = this.canvas.getContext('bitmaprenderer'); this.bitmapPresenter = !!this.presenter;
            if (!this.presenter) this.presenter = this.canvas.getContext('2d', {alpha: false});
            if (!this.presenter) throw new Error('No canvas bitmap presentation context');
          }
          if (this.canvas.width !== bitmap.width || this.canvas.height !== bitmap.height) {
            this.canvas.width = bitmap.width; this.canvas.height = bitmap.height;
          }
          this.canvas.tolglowCaptureVideo?.(bitmap);
          if (this.bitmapPresenter) this.presenter.transferFromImageBitmap(bitmap);
          else this.presenter.drawImage(bitmap, 0, 0);
          this.presented = true;
        });
        this.lastFrame = Math.max(this.lastFrame, message.frameId);
        if (accepted) {
          this.telemetry = message.telemetry; this._ready = true; clearTimeout(this.startupTimer); this.resolveReady(true);
          if (this.drawRequest && message.actionThrough >= this.drawRequest.id) {this.drawRequest.resolve(true); this.drawRequest = null;}
        } else {
          this.discardedFrames++;
          // An explicit paused draw remains pending until the current show's
          // image arrives, even if its first attempt belonged to an old epoch.
          if (this.drawRequest) {
            this.drawRequest.id = ++this.actionId;
            this.actions.push({id: this.actionId, type: 'draw'}); this.sendState();
          }
        }
      } catch (error) {this.fail(error);}
      finally {if (this.worker) this.worker.postMessage({type: 'frame-ack', frameId: message.frameId});}
    }
  }
  update(snapshot) {
    if (this.fallback) return this.fallback.update(snapshot);
    if (!snapshot || this.disposed) return;
    this.editorInput ||= new RigEditorInput(this); this.editorInput.update(snapshot);
    const nextIdentity = showIdentity(snapshot);
    if (this.identity !== nextIdentity) {
      this.identity = nextIdentity; this._ready = false;
      if (this.bitmapPresenter) this.presenter.transferFromImageBitmap(null);
      else this.presenter?.clearRect(0, 0, this.canvas.width, this.canvas.height);
    }
    // Explicit graphics projection: no chat, credentials or definitions cross
    // the render boundary, and only emitted output drives the scene.
    this.receivedAt = performance.timeOrigin + performance.now();
    this.snapshot = {'runtime-id': snapshot['runtime-id'], 'time-ms': snapshot['time-ms'],
      'blackout?': snapshot['blackout?'], master: snapshot.master, 'cut?': snapshot['cut?'], editor: snapshot.editor ?? null, 'show-id': snapshot['show-id'], epoch: snapshot.epoch, revision: snapshot.revision,
      'frame-number': snapshot['frame-number'], rig: snapshot.rig, 'spatial-volumes': snapshot['spatial-volumes'],
      'glass-meshes': snapshot['glass-meshes'] ?? null, 'glass-volumes': snapshot['glass-volumes'] ?? null,
      'sculpt-guide': snapshot['sculpt-guide'] ?? null, 'volume-guides': snapshot['volume-guides'] ?? null, output: {values: snapshot.output?.values, atmosphere: snapshot.output?.atmosphere}};
    this.room = roomDimensions(snapshot.rig?.room);
    this.camera.far = Math.max(100, 3 * Math.hypot(...this.room)); this.camera.updateProjectionMatrix();
    this.sendState();
  }
  resize() {if (this.fallback) return this.fallback.resize(); this.sendState();}
  setLightingMode(mode) {
    if (!['hold', 'interpolate'].includes(mode)) throw new RangeError('Unknown preview lighting mode');
    this.lightingMode = mode; if (this.fallback) return this.fallback.setLightingMode(mode); this.sendState();
  }
  setQuality(value = 'auto') {
    if (!['auto', 'low', 'mobile', 'desktop', 'high'].includes(value)) throw new Error('Unknown preview quality');
    this.qualityChoice = value; if (this.fallback) return this.fallback.setQuality(value); this.sendState();
  }
  get fogAmount() {return this.fogOverride ?? (this.snapshot?.output?.atmosphere ? 1 : .3);}
  setFog(value) {
    this.fogOverride = value == null ? null : clamp(Number(value) || 0, 0, 1);
    if (this.fallback) return this.fallback.setFog(this.fogOverride);
    this.sendState();
  }
  setFilm(settings) {
    Object.assign(this.filmSettings, settings);
    if (this.fallback) return this.fallback.setFilm(this.filmSettings);
    this.sendState();
  }
  setWorkLight(value) {this.workLight = clamp(Number(value) || 0, 0, 1); if (this.fallback) return this.fallback.setWorkLight(value); this.sendState();}
  burst(options) {
    if (this.fallback) return this.fallback.burst(options);
    if (!this.visible || this.snapshot?.output?.atmosphere || this.actions.length >= 32) return false;
    this.actions.push({id: ++this.actionId, type: 'burst', options}); this.sendState(); return true;
  }
  setCamera(name) {
    if (this.fallback) {this.fallback.setCamera(name); this.cameraName = name; return;}
    this.navigation.setMode('orbit');
    cameraPreset(this.camera, this.controls, this.room, name); this.cameraName = name; this.sendState();
  }
  resetCamera() {this.setCamera('audience');}
  setNavigationMode(mode) {if (this.fallback) return this.fallback.setNavigationMode(mode); this.navigation.setMode(mode);}
  setNavigationSpeed(speed) {if (this.fallback) return this.fallback.setNavigationSpeed(speed); this.navigation.setSpeed(speed);}
  setInteractionActive(active) {if (this.fallback) return this.fallback.setInteractionActive(active); this.navigation.setInteractionActive(active);}
  moveCamera(direction, pressed) {if (this.fallback) return this.fallback.moveCamera(direction, pressed); this.navigation.move(direction, pressed);}
  getNavigationState() {return this.fallback ? this.fallback.getNavigationState() : this.navigation.state();}
  notifyNavigation(state) {for (const callback of this.navigationListeners) callback(state);}
  onNavigationChange(callback) {
    this.navigationListeners.add(callback); callback(this.getNavigationState());
    return () => this.navigationListeners.delete(callback);
  }
  focusFixture(id) {
    if (this.fallback) return this.fallback.focusFixture(id);
    const fixture = this.snapshot?.rig?.fixtures?.find(entry => String(entry.id) === String(id));
    if (!fixture?.position) return false;
    this.navigation.setMode('orbit');
    this.controls.target.fromArray(fixture.position); this.camera.position.fromArray(fixture.position).add(new THREE.Vector3(1.3, .5, 2));
    this.controls.update(); this.cameraName = 'fixture'; this.sendState(); return true;
  }
  pause() {this.paused = true; if (this.fallback) return this.fallback.pause(); this.navigation.refreshActivity(); this.sendState();}
  resume() {this.paused = false; if (this.fallback) return this.fallback.resume(); this.navigation.refreshActivity(); this.sendState();}
  draw() {
    if (this.disposed || this.error) return Promise.resolve(false);
    if (this.fallback) return Promise.resolve(this.fallback.draw() !== false);
    if (!this.visible) return Promise.resolve(false);
    if (this.drawRequest) return this.drawRequest.promise;
    const id = ++this.actionId; let resolve; const promise = new Promise(done => {resolve = done;});
    this.drawRequest = {id, promise, resolve}; this.actions.push({id, type: 'draw'}); this.sendState(); return promise;
  }
  getTelemetry() {
    if (this.fallback) return {...this.fallback.getTelemetry(), thread: 'main', fallbackReason: this.fallbackReason};
    return {...this.telemetry, ready: this.ready, failed: this.error || null, thread: 'worker', rendering: this.rendering,
      pendingState: !!this.channel?.pending, inFlightState: this.channel?.inFlight, coalescedStates: this.channel?.coalesced || 0,
      discardedFrames: this.discardedFrames, visible: this.visible, hidden: document.hidden, paused: this.paused,
      navigation: this.getNavigationState(), camera: this.camera.position.toArray(), target: this.controls.target.toArray()};
  }
  stopProxy() {
    this.editorInput?.dispose(); this.editorInput = null;
    clearTimeout(this.startupTimer); this.navigation?.dispose(); this.abort.abort();
    this.resizeObserver?.disconnect(); this.activity?.dispose(); this.controls.dispose(); this.channel?.close();
    this.worker?.terminate(); this.worker = null;
  }
  fail(error) {
    if (this.disposed || this.fallback) return;
    const navigation = this.navigation?.state(); this.stopProxy(); this.rendering = false;
    const reason = String(error?.message || error);
    if (!this.presenter && !this.startedRendering) {
      this.fallbackReason = reason; this.fallback = new SceneRenderer(this.canvas, this.status); this.canvas.tolglowPreview = this;
      if (this.fallback.error) {this.resolveReady(false); this.drawRequest?.resolve(false); this.drawRequest = null; return;}
      this.fallback.setLightingMode(this.lightingMode); this.fallback.setQuality(this.qualityChoice); this.fallback.setFilm(this.filmSettings); this.fallback.setWorkLight(this.workLight); this.fallback.setFog(this.fogOverride);
      if (this.snapshot) this.fallback.update(this.snapshot);
      this.fallback.camera.position.copy(this.camera.position); this.fallback.controls.target.copy(this.controls.target); this.fallback.controls.update();
      this.camera = this.fallback.camera; this.controls = this.fallback.controls;
      if (navigation) {
        this.fallback.setNavigationMode(navigation.mode); this.fallback.setNavigationSpeed(navigation.speed);
        this.fallback.setInteractionActive(navigation.interacting);
      }
      this.fallback.onNavigationChange(state => this.notifyNavigation(state));
      if (this.paused) this.fallback.pause();
      this.fallback.whenReady.then(this.resolveReady);
    } else {
      this.error = reason; this._ready = false; this.resolveReady(false); this.setStatus(`Preview unavailable: ${reason}`);
    }
    this.drawRequest?.resolve(false); this.drawRequest = null;
  }
  dispose() {
    if (this.disposed) return; this.disposed = true; this._ready = false; this.stopProxy(); this.fallback?.dispose();
    this.navigationListeners.clear();
    if (this.bitmapPresenter) this.presenter.transferFromImageBitmap(null);
    else this.presenter?.clearRect(0, 0, this.canvas.width, this.canvas.height);
    this.resolveReady(false); this.drawRequest?.resolve(false); this.drawRequest = null;
    if (this.canvas.tolglowPreview === this) delete this.canvas.tolglowPreview;
  }
}
