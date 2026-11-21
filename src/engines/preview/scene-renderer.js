import {RigEditorInput} from './editor.js';
import {RigOverlay} from './overlay.js';
import * as THREE from 'three';
import {cameraControls, cameraPreset} from './camera.js';
import {CameraNavigator} from './camera-navigation.js';
import {RoomEnvironment} from 'three/addons/environments/RoomEnvironment.js';
import {EffectComposer} from 'three/addons/postprocessing/EffectComposer.js';
import {TexturePass} from 'three/addons/postprocessing/TexturePass.js';
import {UnrealBloomPass} from 'three/addons/postprocessing/UnrealBloomPass.js';
import {OutputPass} from 'three/addons/postprocessing/OutputPass.js';
import {createMaterials, disposeMaterials} from './materials.js';
import {createRoom} from './room.js';
import {createMovingHead, createTruss, disposeGeometry} from './geometry.js';
import {clamp, fixtureDirection, emittedColor, beamAngle, structuralKey, axisDegrees} from './math.js';
import {VolumetricFog} from './fog.js';
import {apertureRadius, opticalFrustum, finiteApertureMaterial} from './optics.js';

const TIERS = {
  low: {dpr: .75, maxPixels: 180000, shadows: 1, shadowSize: 256, fps: 10, fixtures: 32, fog: 'phone'},
  mobile: {dpr: 1, maxPixels: 500000, shadows: 2, shadowSize: 512, fps: 24, fixtures: 32},
  desktop: {dpr: 1.5, maxPixels: 1500000, shadows: 4, shadowSize: 1024, fps: 30, fixtures: 64},
  high: {dpr: 1.75, maxPixels: 2800000, shadows: 4, shadowSize: 2048, fps: 30, fixtures: 128},
};
const DOWN = new THREE.Vector3(0, -1, 0);
const MAX_LIGHTS = 16;

/** One lifecycle-owned graphics engine. The JVM owns all lighting output. */
export class SceneRenderer {
  constructor(canvas, status, platform = {}) {
    this.platform = platform;
    this.canvas = canvas; canvas.tolglowPreview = this; this.status = status; this.ready = false; this.disposed = false;
    this.room = [12, 5, 10]; this.snapshot = null; this.key = null; this.fixtures = [];
    this.quality = this.autoQuality(); this.options = TIERS[this.quality]; this.qualityChoice = 'auto';
    this.fogAmount = .3; this.workLight = .12; this.visible = true; this.lost = false;
    this.lastFrame = 0; this.clock = 0; this.fogAnimationUntil = 0; this.draws = 0; this.lastCPU = 0;
    this.abort = new AbortController(); this.reducedMotion = platform.worker
      ? {matches: Boolean(platform.reducedMotion)} : matchMedia('(prefers-reduced-motion: reduce)');
    this.whenReady = new Promise(resolve => { this.resolveReady = resolve; });
    this.setStatus('Building venue and lighting shaders…');
    try { this.initialize(); } catch (error) { this.fail(error); return; }
    if (!platform.worker) {
      this.resizeObserver = new ResizeObserver(() => this.resize()); this.resizeObserver.observe(canvas);
      this.intersectionObserver = new IntersectionObserver(entries => { this.visible = entries.some(entry => entry.isIntersecting); this.lastFrame = 0; }); this.intersectionObserver.observe(canvas);
    }
    const signal = this.abort.signal;
    canvas.addEventListener('webglcontextlost', event => { event.preventDefault(); this.lost = true; this.ready = false; this.setStatus('Graphics context lost · waiting for recovery'); }, {signal});
    canvas.addEventListener('webglcontextrestored', () => {
      try { this.refreshEnvironment(); this.fog?.upload(); this.lost = false; this.ready = false; this.lastFrame = 0; this.setStatus('Restoring venue shaders…'); }
      catch (error) { this.fail(error); }
    }, {signal});
    if (!platform.worker) {
      this.navigation = new CameraNavigator({camera: this.camera, controls: this.controls, canvas,
        room: () => this.room, snapshot: () => this.snapshot,
        active: () => !this.paused && !this.disposed && !document.hidden && this.visible && !!canvas.getClientRects().length,
        reducedMotion: () => this.reducedMotion.matches});
      document.addEventListener('visibilitychange', () => { this.lastFrame = 0; }, {signal});
      this.reducedMotion.addEventListener('change', () => this.navigation.notify(), {signal});
    }
    if (platform.autoStart !== false) this.scheduleFrame(0);
  }
  autoQuality() {
    if (this.softwareRenderer) return 'low';
    return (this.platform.worker ? this.platform.coarsePointer || this.platform.viewport.width < 700
      : matchMedia('(pointer: coarse)').matches || innerWidth < 700) ? 'mobile' : 'desktop';
  }
  setStatus(text) { if (this.status && this.status.textContent !== text) this.status.textContent = text; }
  initialize() {
    this.renderer = new THREE.WebGLRenderer({canvas: this.canvas, alpha: false, antialias: true, powerPreference: 'high-performance', preserveDrawingBuffer: false});
    const context = this.renderer.getContext(), debug = context.getExtension('WEBGL_debug_renderer_info');
    this.deviceRenderer = debug ? context.getParameter(debug.UNMASKED_RENDERER_WEBGL) : context.getParameter(context.RENDERER);
    this.softwareRenderer = /swiftshader|llvmpipe|softpipe|software rasterizer/i.test(this.deviceRenderer || '');
    this.quality = this.autoQuality(); this.options = TIERS[this.quality]; this.frameInterval = 1000 / this.options.fps;
    this.renderer.outputColorSpace = THREE.SRGBColorSpace; this.renderer.toneMapping = THREE.ACESFilmicToneMapping; this.renderer.toneMappingExposure = .85;
    this.renderer.shadowMap.enabled = true; this.renderer.shadowMap.type = THREE.PCFShadowMap;
    this.renderer.debug.onShaderError = (gl, program, vertex, fragment) => {
      const text = gl.getProgramInfoLog(program) || gl.getShaderInfoLog(fragment) || gl.getShaderInfoLog(vertex);
      throw new Error(`Preview shader rejected: ${String(text).slice(0, 500)}`);
    };
    this.scene = new THREE.Scene(); this.scene.background = new THREE.Color(.0015, .002, .003);
    this.camera = new THREE.PerspectiveCamera(49, 1, .08, 100);
    this.controls = this.platform.worker
      ? {target: new THREE.Vector3(), update: () => this.camera.lookAt(this.controls.target), dispose() {}, touches: {}}
      : cameraControls(this.camera, this.canvas);
    this.materials = createMaterials();
    this.apertureUniform = {value: new Float32Array(MAX_LIGHTS)};
    for (const material of Object.values(this.materials)) finiteApertureMaterial(material, this.apertureUniform);
    this.refreshEnvironment();
    this.hemisphere = new THREE.HemisphereLight(0xc5d9f0, 0x3a2c20, 0); this.scene.add(this.hemisphere);
    this.workLamp = new THREE.DirectionalLight(0xffe3c4, 0); this.workLamp.position.set(2, 6, 4); this.scene.add(this.workLamp);
    this.setWorkLight(this.workLight);
    this.sceneTarget = new THREE.WebGLRenderTarget(1, 1, {type: THREE.HalfFloatType, minFilter: THREE.LinearFilter, magFilter: THREE.LinearFilter, depthBuffer: true});
    this.sceneTarget.texture.colorSpace = THREE.LinearSRGBColorSpace;
    this.sceneTarget.depthTexture = new THREE.DepthTexture(1, 1, THREE.UnsignedIntType);
    this.compositeTarget = new THREE.WebGLRenderTarget(1, 1, {type: THREE.HalfFloatType, depthBuffer: false});
    this.compositeTarget.texture.colorSpace = THREE.LinearSRGBColorSpace;
    this.fog = new VolumetricFog(this.renderer, {quality: this.options.fog || this.quality}); this.fog.setRoom(this.room); this.fog.setAmount(this.fogAmount);
    this.composer = new EffectComposer(this.renderer, new THREE.WebGLRenderTarget(1, 1, {type: THREE.HalfFloatType, depthBuffer: false}));
    this.inputPass = new TexturePass(this.compositeTarget.texture); this.composer.addPass(this.inputPass);
    this.bloom = new UnrealBloomPass(new THREE.Vector2(1, 1), .28, .4, 1.1); this.composer.addPass(this.bloom);
    this.outputPass = new OutputPass(); this.composer.addPass(this.outputPass);
    this.rebuild({room: {width: 12, height: 5, depth: 10}, fixtures: [], trusses: []});
    this.setCamera('audience'); this.resize();
  }
  refreshEnvironment() {
    this.environmentTarget?.dispose();
    const environment = new RoomEnvironment(), generator = new THREE.PMREMGenerator(this.renderer);
    this.environmentTarget = generator.fromScene(environment, .04); this.scene.environment = this.environmentTarget.texture;
    environment.dispose(); generator.dispose();
  }
  setWorkLight(value) {
    this.workLight = clamp(Number(value) || 0, 0, 1);
    if (this.hemisphere) this.hemisphere.intensity = this.workLight * 1.8;
    if (this.workLamp) this.workLamp.intensity = this.workLight * 3.6;
    if (this.scene) this.scene.environmentIntensity = this.workLight * 1.25;
    if (this.ready) this.updateStatus();
  }
  setFog(value) { this.fogAmount = clamp(Number(value) || 0, 0, 1); this.fog?.setAmount(this.fogAmount); }
  burst(options) { this.fog?.burst(options); this.fogAnimationUntil = this.clock + 8; }
  setQuality(value = 'auto') {
    if (!['auto', ...Object.keys(TIERS)].includes(value)) throw new Error('Unknown preview quality');
    this.qualityChoice = value; const next = value === 'auto' ? this.autoQuality() : value;
    if (next === this.quality) return;
    const previousFog = this.fog, nextFog = new VolumetricFog(this.renderer, {quality: TIERS[next].fog || next});
    if (previousFog) { nextFog.field.copyFrom(previousFog.field); nextFog.uniforms.room.value.fromArray(this.room); nextFog.upload(); }
    else nextFog.setRoom(this.room);
    nextFog.setAmount(this.fogAmount);
    this.quality = next; this.options = TIERS[next]; this.frameInterval = 1000 / this.options.fps;
    this.fog = nextFog; previousFog?.dispose();
    for (const entry of this.fixtures) if (entry.light) {
      entry.light.shadow.map?.dispose(); entry.light.shadow.map = null;
      entry.light.shadow.mapSize.set(this.options.shadowSize, this.options.shadowSize);
    }
    if (this.snapshot) this.update(this.snapshot, true);
    this.resize();
  }
  update(snapshot, force = false) {
    if (!snapshot || this.disposed || !this.renderer) return;
    this.snapshot = snapshot;
    if (!this.platform.worker) {this.editorInput ||= new RigEditorInput(this); this.editorInput.update(snapshot);}
    this.editorOverlay ||= new RigOverlay(); this.editorOverlay.update(snapshot);
    const rig = snapshot.rig || {};
    const key = structuralKey(rig);
    if (force || this.key !== key) { this.ready = false; this.rebuild(rig); this.key = key; }
    for (const entry of this.trusses || []) {
      const spec = (rig.trusses || []).find(t => String(t.id) === entry.id);
      if (spec) {entry.root.position.fromArray(spec.position); entry.root.rotation.set(...(spec['rotation-deg'] || [0, 0, 0]).map(v => v * Math.PI / 180), 'ZYX');}
    }
    const output = snapshot.output?.values || {};
    const currentFixtures = new Map((rig.fixtures || []).slice(0, this.options.fixtures).map(f => [String(f.id), f]));
    const illuminated = entry => Math.max(...emittedColor(output[entry.id] || {})) > .00001;
    // Keep shader light/shadow counts stable across blackout and cue launches.
    // Zero-colour slots emit no light. Prefer active lights for the fixed shadow
    // budget, filling unused slots with dark fixtures before the first frame.
    // Otherwise the first lit cue compiles new PBR variants on the GPU process,
    // which can stall browser compositing even when JS runs in a worker.
    const shadowLights = new Set(this.fixtures.filter(entry => entry.light)
      .sort((a, b) => Number(illuminated(b)) - Number(illuminated(a))).slice(0, this.options.shadows));
    let shadowCount = 0;
    this.activeLights = 0;
    this.lightEntries = [];
    for (const entry of this.fixtures) {
      entry.fixture = currentFixtures.get(entry.id) || entry.fixture;
      entry.root.position.fromArray(entry.fixture.position || [0, 0, 0]);
      entry.root.rotation.set(...(entry.fixture['rotation-deg'] || [0, 0, 0]).map(v => v * Math.PI / 180), 'ZYX');
      const resolved = output[entry.id] || {}, direction = fixtureDirection(entry.fixture, resolved), rgb = emittedColor(resolved);
      const world = new THREE.Quaternion().setFromUnitVectors(DOWN, new THREE.Vector3(...direction));
      entry.head.quaternion.copy(entry.root.quaternion).invert().multiply(world);
      entry.yoke.rotation.y = axisDegrees(entry.fixture, resolved, 'pan') * Math.PI / 180;
      entry.lensMaterial.emissive.setRGB(...rgb).multiplyScalar(8);
      if (!entry.light) continue;
      const light = entry.light;
      if (Math.max(...rgb) > .00001) this.activeLights++;
      light.color.setRGB(...rgb); light.intensity = 145; light.angle = Math.min(Math.PI * .45, beamAngle(entry.fixture, resolved) * 1.35);
      light.penumbra = clamp(.72 - (Number(resolved.focus) || 0) * .52, .2, .8);
      const optics = opticalFrustum(entry.fixture.position, direction, light.angle, apertureRadius(entry.fixture));
      light.position.copy(optics.virtual);
      if (light.shadow.camera.near !== optics.apexDistance + .006) {
        light.shadow.camera.near = optics.apexDistance + .006; light.shadow.camera.updateProjectionMatrix();
      }
      light.shadow.camera.far = light.distance; light.userData.apexDistance = optics.apexDistance;
      light.target.position.copy(light.position).add(new THREE.Vector3(...direction)); light.target.updateMatrixWorld();
      light.castShadow = shadowLights.has(entry);
      if (light.castShadow) shadowCount++;
      light.visible = true;
      this.lightEntries.push({position: optics.optical, virtualPosition: optics.virtual, apexDistance: optics.apexDistance, apertureRadius: optics.radius, direction: optics.axis, color: light.color,
        angle: light.angle, penumbra: light.penumbra, intensity: light.intensity, distance: light.distance, shadow: light.castShadow ? light.shadow : null});
    }
    // Low-order room bounce estimate: only actual emitted, floor-directed light
    // contributes. This illuminates airborne density, not a second fixture output.
    const bounce = new THREE.Color(0, 0, 0);
    for (const entry of this.lightEntries) {
      const downward = Math.max(0, -entry.direction.y);
      bounce.add(entry.color.clone().multiplyScalar(downward * .045));
    }
    this.bounceColor = bounce;
    const visibleLights = this.fixtures.map(entry => entry.light).filter(light => light?.visible)
      .sort((a, b) => Number(b.castShadow) - Number(a.castShadow));
    this.apertureUniform.value.fill(0);
    visibleLights.forEach((light, index) => { this.apertureUniform.value[index] = light.userData.apexDistance || 0; });
    this.shadowCount = shadowCount;
    if (this.ready) this.updateStatus();
  }
  rebuild(rig) {
    if (this.content) {
      this.scene.remove(this.content); disposeGeometry(this.content);
      for (const entry of this.fixtures) { entry.dispose(); entry.light?.dispose(); }
    }
    const r = rig.room || {}; this.room = [r.width || 12, r.height || 5, r.depth || 10].map(v => clamp(Number(v) || 1, 1, 100));
    this.fog?.setRoom(this.room);
    this.content = new THREE.Group(); this.scene.add(this.content); this.content.add(createRoom(this.room, this.materials));
    this.trusses = [];
    for (const spec of (rig.trusses || []).slice(0, 24)) {
      if (Array.isArray(spec.position) && spec.position.every(Number.isFinite) && Number.isFinite(spec.length) && spec.length > 0)
        {const root = createTruss(spec, this.materials); this.content.add(root); this.trusses.push({id: String(spec.id), root});}
    }
    this.fixtures = (rig.fixtures || []).slice(0, this.options.fixtures).map((fixture, index) => {
      const entry = createMovingHead(fixture, this.materials); finiteApertureMaterial(entry.lensMaterial, this.apertureUniform); entry.id = String(fixture.id); entry.fixture = fixture;
      const rotation = (fixture['rotation-deg'] || [0, 0, 0]).map(v => (Number(v) || 0) * Math.PI / 180);
      entry.root.rotation.set(...rotation, 'ZYX'); this.content.add(entry.root);
      if (index < MAX_LIGHTS) {
        const light = new THREE.SpotLight(0x000000, 145, Math.max(...this.room) * 2.5, Math.PI / 20, .15, 2);
        light.position.copy(entry.root.position); light.shadow.mapSize.set(this.options.shadowSize, this.options.shadowSize);
        light.shadow.camera.near = .06; light.shadow.camera.far = light.distance;
        light.shadow.bias = -.00008; light.shadow.normalBias = .012;
        this.content.add(light, light.target); entry.light = light;
      }
      return entry;
    });
    this.totalFixtureCount = (rig.fixtures || []).length;
    this.lightEntries = [];
  }
  resize() {
    if (!this.renderer || this.disposed) return;
    const bounds = this.platform.worker ? this.platform.viewport : this.canvas.getBoundingClientRect();
    if (!bounds.width || !bounds.height) return;
    const pixelRatio = this.platform.worker ? bounds.pixelRatio : devicePixelRatio;
    const dpr = Math.min(pixelRatio || 1, this.options.dpr, Math.sqrt(this.options.maxPixels / (bounds.width * bounds.height)));
    const width = Math.max(1, Math.round(bounds.width * dpr)), height = Math.max(1, Math.round(bounds.height * dpr));
    this.camera.aspect = bounds.width / bounds.height; this.camera.updateProjectionMatrix();
    if (width === this.width && height === this.height) return;
    this.renderer.setPixelRatio(1); this.renderer.setSize(width, height, false);
    this.sceneTarget.setSize(width, height); this.compositeTarget.setSize(width, height);
    this.composer.setPixelRatio(1); this.composer.setSize(width, height); this.fog.setSize(width, height);
    this.width = width; this.height = height;
  }
  setCamera(name) {
    if (!this.camera) return;
    this.navigation?.setMode('orbit');
    cameraPreset(this.camera, this.controls, this.room, name); this.cameraName = name;
  }
  resetCamera() { this.setCamera('audience'); }
  setNavigationMode(mode) {this.navigation?.setMode(mode);}
  setNavigationSpeed(speed) {this.navigation?.setSpeed(speed);}
  setInteractionActive(active) {this.navigation?.setInteractionActive(active);}
  moveCamera(direction, pressed) {this.navigation?.move(direction, pressed);}
  getNavigationState() {return this.navigation?.state() || null;}
  onNavigationChange(callback) {return this.navigation?.subscribe(callback) || (() => {});}
  focusFixture(id) {
    const entry = this.fixtures.find(f => f.id === String(id)); if (!entry) return false;
    this.navigation?.setMode('orbit');
    this.controls.target.copy(entry.root.position); this.camera.position.copy(entry.root.position).add(new THREE.Vector3(1.3, .5, 2)); this.controls.update(); this.cameraName = 'fixture'; return true;
  }
  scheduleFrame(delay) {
    clearTimeout(this.frameTimer); globalThis.cancelAnimationFrame?.(this.frame);
    if (this.disposed || this.paused) return;
    this.nextFrameAt = performance.now() + delay;
    this.frameTimer = setTimeout(() => {
      this.frame = globalThis.requestAnimationFrame ? requestAnimationFrame(time => this.animate(time)) : setTimeout(() => this.animate(performance.now()), 0);
    }, delay);
  }
  pause() { this.paused = true; this.navigation?.clearInput(); clearTimeout(this.frameTimer); globalThis.cancelAnimationFrame?.(this.frame); }
  resume() { if (!this.disposed) {this.paused = false; this.lastFrame = 0; this.scheduleFrame(0);} }
  animate(time) {
    if (this.disposed || this.paused) return;
    if (this.lost || (this.platform.worker ? this.platform.hidden : document.hidden)
        || !this.visible || (!this.platform.worker && !this.canvas.getClientRects().length)) {
      this.lastFrame = 0; this.scheduleFrame(250); return;
    }
    const dt = this.lastFrame ? Math.min(.1, (time - this.lastFrame) / 1000) : 0;
    this.lastFrame = time; this.clock += dt;
    try {
      this.draw(dt);
      // The idle budget starts at completion, not at the previous RAF timestamp.
      // Even a very slow driver must yield a real input-processing window.
      this.scheduleFrame(Math.max(0, this.frameInterval - this.lastCPU));
    } catch (error) { this.fail(error); }
  }
  draw(dt = 0) {
    if (this.disposed || this.lost || !this.renderer) return false;
    const started = performance.now(); if (this.platform.worker) this.controls.update();
    const simulate = !this.reducedMotion.matches || this.clock < this.fogAnimationUntil;
    // Both preview worklights illuminate airborne material: the sky hemisphere
    // alone made dense water droplets unnaturally blue/dark against the warm
    // floor lit by the directional worklamp.
    const ambient = this.hemisphere.color.clone().multiplyScalar(this.workLight * .8)
      .add(this.workLamp.color.clone().multiplyScalar(this.workLight * .65));
    if (this.bounceColor) ambient.add(this.bounceColor);
    this.fog.setAmbient?.({color: ambient, intensity: 1, groundColor: ambient.clone().multiplyScalar(.5)});
    this.fog.update(simulate ? dt : 0, this.lightEntries || []);
    this.renderer.setRenderTarget(this.sceneTarget); this.renderer.clear(); this.renderer.render(this.scene, this.camera);
    this.renderedShadows = this.fixtures.filter(entry => entry.light?.castShadow && entry.light.shadow.map).length;
    this.fog.render(this.sceneTarget, this.camera, this.compositeTarget);
    this.composer.render(dt); this.editorOverlay?.render(this.renderer, this.camera); this.lastCPU = performance.now() - started; this.draws++;
    // Backpressure leaves main-thread time for cue/parameter input when a
    // driver blocks on compilation/submission. Recover gradually after it clears.
    const nominal = 1000 / this.options.fps;
    const previous = this.frameInterval || nominal;
    // The worker computes its budget after the GPU completion fence; command
    // submission time alone cannot establish render throughput.
    if (!this.platform.worker) this.frameInterval = Math.max(nominal, this.lastCPU * (this.softwareRenderer ? 4 : 1.5), previous * .95);
    if (!this.ready) { this.ready = true; this.resolveReady?.(true); }
    this.updateStatus();
  }
  updateStatus() {
      const capped = this.totalFixtureCount > MAX_LIGHTS ? ` · first ${MAX_LIGHTS}/${this.totalFixtureCount} lights` : '';
      const automatic = this.qualityChoice === 'auto' && this.softwareRenderer ? 'low · software GPU' : this.quality;
      this.setStatus(`${automatic} · ${this.renderedShadows || 0} shadows · ${this.workLight ? 'work light' : 'dark show'}${capped}`);
  }
  getTelemetry() {
    return {ready: this.ready, failed: this.error || null, renderer: `Three.js r${THREE.REVISION}`, quality: this.quality,
      deviceRenderer: this.deviceRenderer, softwareRenderer: this.softwareRenderer,
      pixels: [this.width, this.height], frameCPUms: this.lastCPU, frameIntervalMs: this.frameInterval,
      idleBudgetMs: Math.max(0, this.frameInterval - this.lastCPU), nextFrameAt: this.nextFrameAt, frames: this.draws,
      geometryCount: this.renderer?.info.memory.geometries || 0, textureCount: this.renderer?.info.memory.textures || 0,
      programCount: this.renderer?.info.programs?.length || 0, activeLights: this.activeLights || 0,
      fixtureCount: this.totalFixtureCount, visibleFixtures: this.fixtures.length, lightCap: MAX_LIGHTS,
      shadowCount: this.renderedShadows || 0, workLight: this.workLight, fogAmount: this.fogAmount,
      camera: this.camera?.position.toArray(), target: this.controls?.target.toArray(), fog: this.fog?.stats(),
      visible: this.visible, hidden: this.platform.worker ? this.platform.hidden : document.hidden, lost: this.lost,
      epoch: this.snapshot?.epoch, revision: this.snapshot?.revision, frameNumber: this.snapshot?.['frame-number']};
  }
  fail(error) {
    this.error = String(error?.message || error); this.ready = false; this.setStatus(`Preview unavailable: ${this.error}`);
    this.pause(); this.resolveReady?.(false);
    console.error('Tolglow preview', error);
  }
  dispose() {
    if (this.disposed) return; this.disposed = true; this.ready = false;
    this.pause(); this.abort.abort(); this.resizeObserver?.disconnect(); this.intersectionObserver?.disconnect();
    this.editorInput?.dispose(); this.editorOverlay?.dispose();
    this.navigation?.dispose(); this.controls?.dispose(); this.fog?.dispose();
    for (const entry of this.fixtures) { entry.dispose(); entry.light?.dispose(); }
    if (this.content) disposeGeometry(this.content);
    this.inputPass?.dispose(); this.bloom?.dispose(); this.outputPass?.dispose(); this.composer?.dispose();
    this.sceneTarget?.dispose(); this.compositeTarget?.dispose(); this.environmentTarget?.dispose();
    if (this.materials) disposeMaterials(this.materials);
    this.renderer?.dispose(); this.resolveReady?.(false);
    if (this.canvas.tolglowPreview === this) delete this.canvas.tolglowPreview;
  }
}
