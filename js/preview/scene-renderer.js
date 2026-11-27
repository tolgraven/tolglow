import {showIdentity} from './presentation.js';
import {RigEditorInput} from './editor.js';
import {RigOverlay} from './overlay.js';
import * as THREE from 'three';
import {cameraControls, cameraPreset} from './camera.js';
import {CameraNavigator} from './camera-navigation.js';
import {RoomEnvironment} from 'three/addons/environments/RoomEnvironment.js';
import {EffectComposer} from 'three/addons/postprocessing/EffectComposer.js';
import {TexturePass} from 'three/addons/postprocessing/TexturePass.js';
import {UnrealBloomPass} from 'three/addons/postprocessing/UnrealBloomPass.js';
import {displayTransform, EdgePass, FilmPass} from './film.js';
import {TIERS, rasterBudget, rasterSizes, supportedSamples, sceneSamples, presentationHz, adjacentQuality, hdrDisplayAvailable, configureDisplayRange, GpuTimer, lightBudget, MAX_FULL_LIGHTS} from './render-quality.js';
import {createMaterials, disposeMaterials} from './materials.js';
import {createRoom} from './room.js';
import {createDiffuserTubes} from './diffuser-tubes.js';
import {createMovingHead, createTruss, disposeGeometry} from './geometry.js';
import {clamp, roomDimensions, fixtureDirection, emittedColor, beamAngle, structuralKey, axisDegrees} from './math.js';
import {SurfaceReflections} from './reflections.js';
import {GlassPass} from './glass.js';
import {VolumetricFog} from './fog.js';
import {apertureRadius, opticalFrustum, finiteApertureMaterial} from './optics.js';

import {PreviewActivity} from './preview-activity.js';
import {DisplayClock, DisplayRefresh} from './display-clock.js';
import {fixtureSlots} from './fixture-selection.js';
import {LightingSamples} from './lighting-samples.js';
import {rankLights, assignSlots, viewFromMatrix} from './light-lod.js';
import {FakedBeams, roomExit} from './beams.js';
import {FootprintAtlas, footprintMaterial} from './footprints.js';

const DOWN = new THREE.Vector3(0, -1, 0);
const MAX_LIGHTS = MAX_FULL_LIGHTS;
const FAKED_CAPACITY = 128;

function updateRigPose(root, spec) {
  const position = spec.position || [0, 0, 0];
  const rotation = (spec['rotation-deg'] || [0, 0, 0]).map(value => value * Math.PI / 180);
  if (!position.some((value, axis) => value !== root.position.getComponent(axis)) &&
      root.rotation.order === 'ZYX' && root.rotation.x === rotation[0] &&
      root.rotation.y === rotation[1] && root.rotation.z === rotation[2]) return false;
  root.position.fromArray(position); root.rotation.set(...rotation, 'ZYX');
  return true;
}

function invalidateShadows(spots, reflectedLights = []) {
  for (const light of spots) if (light.castShadow) light.shadow.needsUpdate = true;
  for (const light of reflectedLights) if (light.castShadow) light.shadow.needsUpdate = true;
}

/** One lifecycle-owned graphics engine. The JVM owns all lighting output. */
export class SceneRenderer {
  constructor(canvas, status, platform = {}) {
    this.displayHz = platform.displayHz ?? (platform.worker ? null : 60);
    this.platform = platform; this.displayClock = new DisplayClock(); this.lighting = new LightingSamples();
    this.canvas = canvas; canvas.tolglowPreview = this; this.status = status; this.ready = false; this.disposed = false;
    this.room = [12, 5, 10]; this.snapshot = null; this.key = null; this.fixtures = [];
    this.filmSettings = {bloom: .28, exposure: .85, grain: .0015};
    this.quality = this.autoQuality(); this.options = TIERS[this.quality]; this.qualityChoice = 'auto';
    this.fogOverride = null; this.workLight = .12; this.visible = Boolean(platform.worker); this.lost = false;
    this.lastFrame = 0; this.renderScale = 1; this.scaleChangedAt = 0; this.clock = 0; this.fogAnimationUntil = 0; this.draws = 0; this.lastCPU = 0;
    this.abort = new AbortController(); this.reducedMotion = platform.worker
      ? {matches: Boolean(platform.reducedMotion)} : matchMedia('(prefers-reduced-motion: reduce)');
    this.whenReady = new Promise(resolve => { this.resolveReady = resolve; });
    this.setStatus('Building venue and lighting shaders…');
    this.camera = new THREE.PerspectiveCamera(49, 1, .08, 100);
    this.controls = platform.worker
      ? {target: new THREE.Vector3(), update: () => this.camera.lookAt(this.controls.target), dispose() {}, touches: {}}
      : cameraControls(this.camera, this.canvas);
    this.setCamera('audience');
    if (platform.worker) {try {this.initialize();} catch (error) {this.fail(error); return;}}
    if (!platform.worker) {
      this.refresh = new DisplayRefresh(hz => this.setDisplayHz(hz));
      this.resizeObserver = new ResizeObserver(() => this.resize()); this.resizeObserver.observe(canvas);
      this.activity = new PreviewActivity(canvas, active => this.changeActivity(active));
    }
    const signal = this.abort.signal;
    canvas.addEventListener('webglcontextlost', event => { event.preventDefault(); this.lost = true; this.ready = false; this.setStatus('Graphics context lost · waiting for recovery'); }, {signal});
    canvas.addEventListener('webglcontextrestored', () => {
      try {this.restorePending = true; if (this.visible) {this.restoreContext(); this.scheduleFrame(0);}}
      catch (error) { this.fail(error); }
    }, {signal});
    if (!platform.worker) {
      this.navigation = new CameraNavigator({camera: this.camera, controls: this.controls, canvas,
        room: () => this.room, snapshot: () => this.snapshot, visibleLights: () => this.lightIds,
        active: () => this.visible && !this.paused && !this.disposed,
        reducedMotion: () => this.reducedMotion.matches});
      this.reducedMotion.addEventListener('change', () => this.navigation.notify(), {signal});
    }
    if (platform.autoStart !== false) this.scheduleFrame(0);
  }
  get targetHz() {return presentationHz(this.quality, this.qualityChoice, this.softwareRenderer, this.displayHz);}
  setDisplayHz(hz) {
    if (!hz || hz === this.displayHz) return;
    this.displayHz = hz;
    this.overBudget = this.underBudget = 0;
  }
  autoQuality(width = this.platform.viewport?.width ?? this.canvas.getBoundingClientRect().width) {
    if (this.softwareRenderer) return 'low';
    const coarse = this.platform.worker ? this.platform.coarsePointer : matchMedia('(pointer: coarse)').matches;
    // Hysteresis keeps resizable panes and mobile browser chrome from
    // reallocating shadow maps and fog targets around the layout threshold.
    return coarse || width < (this.autoLayoutTier === 'mobile' ? 740 : 660) ? 'mobile' : 'desktop';
  }
  setStatus(text) { if (this.status && this.status.textContent !== text) this.status.textContent = text; }
  initialize() {
    this.hdrRequested = !this.platform.worker && hdrDisplayAvailable();
    this.renderer = new THREE.WebGLRenderer({canvas: this.canvas, alpha: this.hdrRequested, antialias: false, powerPreference: 'high-performance', preserveDrawingBuffer: false});
    const context = this.renderer.getContext(), debug = context.getExtension('WEBGL_debug_renderer_info');
    this.maxDimension = Math.min(context.getParameter(context.MAX_RENDERBUFFER_SIZE), context.getParameter(context.MAX_TEXTURE_SIZE));
    this.samples = supportedSamples(context);
    this.gpuTimer = this.platform.worker ? null : new GpuTimer(context);
    this.deviceRenderer = debug ? context.getParameter(debug.UNMASKED_RENDERER_WEBGL) : context.getParameter(context.RENDERER);
    this.softwareRenderer = /swiftshader|llvmpipe|softpipe|software rasterizer/i.test(this.deviceRenderer || '');
    this.quality = this.autoLayoutTier = this.autoQuality(); this.options = TIERS[this.quality]; this.frameInterval = 1000 / this.targetHz;
    this.renderer.outputColorSpace = THREE.SRGBColorSpace; this.renderer.toneMapping = THREE.ACESFilmicToneMapping; this.renderer.toneMappingExposure = .85;
    this.renderer.shadowMap.enabled = true; this.renderer.shadowMap.type = THREE.PCFShadowMap;
    this.renderer.debug.onShaderError = (gl, program, vertex, fragment) => {
      const text = gl.getProgramInfoLog(program) || gl.getShaderInfoLog(fragment) || gl.getShaderInfoLog(vertex);
      throw new Error(`Preview shader rejected: ${String(text).slice(0, 500)}`);
    };
    this.scene = new THREE.Scene(); this.scene.background = new THREE.Color(.0015, .002, .003);
    this.materials = createMaterials();
    this.apertureUniform = {value: new Float32Array(MAX_LIGHTS + 2)};
    this.patchRadii = {value: new Float32Array(MAX_LIGHTS + 2)};
    for (const material of Object.values(this.materials)) finiteApertureMaterial(material, this.apertureUniform, this.patchRadii);
    // Faked lights paint the floor and walls through one irradiance atlas.
    this.footprints = new FootprintAtlas(this.renderer, {capacity: FAKED_CAPACITY});
    for (const name of ['floor', 'wall', 'stage', 'curtain']) footprintMaterial(this.materials[name], this.footprints.materialUniforms);
    this.beams = new FakedBeams(FAKED_CAPACITY);
    this.refreshEnvironment();
    this.hemisphere = new THREE.HemisphereLight(0xc5d9f0, 0x3a2c20, 0); this.scene.add(this.hemisphere);
    this.diffuseBounce = new THREE.HemisphereLight(0, 0, 1); this.scene.add(this.diffuseBounce);
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
    this.glass = new GlassPass(this); this.composer.addPass(this.glass);
    this.bloom = new UnrealBloomPass(new THREE.Vector2(1, 1), .28, .4, 1.1); this.composer.addPass(this.bloom);
    this.outputPass = displayTransform(); this.composer.addPass(this.outputPass);
    this.edges = new EdgePass(); this.composer.addPass(this.edges);
    this.film = new FilmPass(); this.composer.addPass(this.film); this.setFilm(this.filmSettings);
    this.rebuild({room: {width: 12, height: 5, depth: 10}, fixtures: [], trusses: []});
    // Link the faked-beam program now so the first cue that needs it does not
    // compile a shader mid-show.
    this.renderer.compile(this.beams.scene, this.camera);
    this.resize();
  }
  refreshEnvironment() {
    this.environmentTarget?.dispose();
    const environment = new RoomEnvironment(), generator = new THREE.PMREMGenerator(this.renderer);
    this.environmentTarget = generator.fromScene(environment, .04); this.scene.environment = this.environmentTarget.texture;
    environment.dispose(); generator.dispose();
  }
  setFilm(settings) {
    for (const [key, maximum] of [['bloom', 2], ['exposure', 3], ['grain', .08]]) {
      if (Number.isFinite(settings?.[key])) this.filmSettings[key] = clamp(settings[key], 0, maximum);
    }
    if (!this.film) return;
    this.renderer.toneMappingExposure = this.filmSettings.exposure;
    this.bloom.strength = this.filmSettings.bloom;
    this.bloom.enabled = this.bloom.strength > 0;
    this.film.uniforms.grain.value = this.filmSettings.grain;
  }
  setWorkLight(value) {
    this.workLight = clamp(Number(value) || 0, 0, 1);
    if (this.hemisphere) this.hemisphere.intensity = this.workLight * 1.8;
    if (this.workLamp) this.workLamp.intensity = this.workLight * 3.6;
    if (this.scene) this.scene.environmentIntensity = this.workLight * 1.25;
    if (this.ready) this.updateStatus();
  }
  get fogAmount() {return this.fogOverride ?? (this.snapshot?.output?.atmosphere ? 1 : .3);}
  setFog(value) {
    this.fogOverride = value == null ? null : clamp(Number(value) || 0, 0, 1);
    this.fog?.setAmount(this.fogAmount);
  }
  burst(options) {
    if (!this.visible || !this.fog?.burst(options)) return false;
    this.fogAnimationUntil = this.clock + 8; return true;
  }
  setQuality(value = 'auto') {
    if (!['auto', ...Object.keys(TIERS)].includes(value)) throw new Error('Unknown preview quality');
    const choiceChanged = this.qualityChoice !== value;
    if (choiceChanged) {
      this.renderScale = 1; this.overBudget = this.underBudget = 0;
      this.scaleChangedAt = performance.now();
    }
    this.qualityChoice = value; if (!this.visible || !this.renderer) return;
    if (value === 'auto' && choiceChanged) this.autoLayoutTier = this.autoQuality();
    const next = value === 'auto' ? (choiceChanged ? this.autoLayoutTier : this.quality) : value;
    if (next === this.quality) {if (choiceChanged) this.resize(); return;}
    this.applyTier(next);
  }
  applyTier(next) {
    this.fog.setQuality(TIERS[next].fog || next);
    this.quality = next; this.options = TIERS[next]; this.frameInterval = 1000 / this.targetHz;
    if (this.width) this.fog.setSize(this.width, this.height);
    if (this.snapshot) this.update(this.snapshot, true);
    this.resize();
  }
  setLightingMode(mode) {
    if (mode === this.lighting.mode) return;
    this.lighting.setMode(mode);
    if (this.visible && this.renderer && this.snapshot) this.applyLighting(this.snapshot.output?.values || {});
  }
  /** Number of full lights (real SpotLight + raymarched fog). null = tier default. */
  setLightBudget(value = null) {
    const next = value == null ? null : Math.max(0, Math.min(MAX_LIGHTS, Math.floor(Number(value) || 0)));
    if (next === this.lightBudgetOverride) return;
    this.lightBudgetOverride = next;
    if (this.visible && this.renderer && this.snapshot) this.update(this.snapshot, true);
  }
  get lightBudget() {return lightBudget(this.options, this.lightBudgetOverride);}
  update(snapshot, force = false, receivedAt = performance.now()) {
    if (!snapshot || this.disposed) return;
    if (!this.platform.worker) {this.editorInput ||= new RigEditorInput(this); this.editorInput.update(snapshot);}
    if (!this.visible || !this.renderer) {this.snapshot = snapshot; return;}
    const opticalShow = showIdentity(snapshot);
    if (this.opticalShow !== opticalShow) this.glass?.dispose();
    this.opticalShow = opticalShow;
    if (this.glass) {
      this.glass.update(snapshot['glass-meshes'], snapshot['glass-volumes']);
      // Composer swaps HDR images. Depth storage is only needed while surfaces
      // can overlap. Recreate the attachments before the next input pass.
      for (const target of [this.composer.readBuffer, this.composer.writeBuffer]) {
        if (target.depthBuffer === this.glass.enabled) continue;
        target.dispose();
        target.depthBuffer = this.glass.enabled;
      }
    }
    this.editorOverlay ||= new RigOverlay(); this.editorOverlay.update(snapshot);
    this.snapshot = snapshot; const rig = snapshot.rig || {};
    const key = rig === this.lastRig ? this.key : structuralKey(rig);
    if (rig !== this.lastRig) {
      this.fixtureById = new Map((rig.fixtures || []).map(f => [String(f.id), f]));
      this.beamById = new Map([...this.fixtureById].filter(([, f]) => !(f['pixel-layout-id'] && f['diffuser-radius-m'])));
    }
    this.lastRig = rig;
    if (force || this.key !== key) { this.ready = false; this.rebuild(rig); this.key = key; }
    let castersChanged = false;
    for (const entry of this.trusses || []) {
      const spec = (rig.trusses || []).find(t => String(t.id) === entry.id);
      if (spec && updateRigPose(entry.root, spec)) castersChanged = true;
    }
    this.updateEmittedOutput(snapshot, receivedAt, castersChanged);
  }
  // The scene and its resource bindings have already been applied. These
  // updates are CPU-side inputs to draws in the same ordered WebGL context.
  updateEmittedOutput(snapshot, receivedAt, castersChanged = false) {
    this.snapshot = snapshot;
    this.fog?.setAtmosphere(this.opticalShow, snapshot.output?.atmosphere);
    this.fog?.setAmount(this.fogAmount);
    this.lighting.accept(snapshot, this.lastRig, receivedAt);
    const selection = snapshot.editor?.selection;
    const selected = selection?.kind === 'fixtures' ? String(selection.id) : null;
    // Mesh identities follow committed emission; light detail is ranked per
    // view in assignLights.
    const slots = fixtureSlots(this.beamById, snapshot.output?.values || {}, selected,
      this.fixtures.map(entry => entry.id), [], 0);
    this.selectedFixture = selected;
    this.fixtures.forEach((entry, index) => {
      const id = slots.meshes[index], fixture = this.fixtureById.get(id);
      if (entry.fixture !== fixture) {
        const radius = entry.lensRadius;
        entry.setAperture(fixture);
        if (entry.lensRadius !== radius) castersChanged = true;
        entry.fixture = fixture; entry.id = id; entry.root.name = 'fixture:' + id;
      }
      if (updateRigPose(entry.root, fixture)) castersChanged = true;
    });
    this.applyLighting(this.lighting.values(this.fixtureSamples, receivedAt), castersChanged);
  }
  // Rank emitting fixtures by prominence from the current camera and map the
  // most prominent onto the persistent SpotLight slots (#782).
  assignLights(lit) {
    if (!this.spots.length) {
      for (const entry of this.fixtures) entry.light = null;
      this.lod = {budget: 0, full: 0, faked: lit.length}; return;
    }
    const camera = this.camera;
    camera.updateMatrixWorld();
    const matrix = (this.lodMatrix ||= new THREE.Matrix4()).multiplyMatrices(camera.projectionMatrix, camera.matrixWorldInverse);
    const eye = new THREE.Vector3().setFromMatrixPosition(camera.matrixWorld).toArray();
    const view = viewFromMatrix(matrix.elements, eye, Math.tan(camera.fov * Math.PI / 360));
    const candidates = lit.map(entry => ({id: entry.id, level: entry.beam.level, pinned: entry.id === this.selectedFixture,
      position: entry.beam.optics.optical.toArray(), direction: entry.beam.optics.axis.toArray(),
      angle: entry.beam.angle, length: entry.beam.length}));
    const ranked = rankLights(candidates, view, this.spots.length, this.lodState, performance.now() / 1000);
    this.lodState = ranked.state;
    const shadowSlots = this.spots.filter(light => light.castShadow).length;
    const slots = assignSlots(this.lightIds, ranked.full, this.spots.length, shadowSlots);
    // Keep the array identity while the assignment holds; camera tours key on it.
    if (slots.length !== this.lightIds.length || slots.some((id, i) => id !== this.lightIds[i])) this.lightIds = slots;
    const spotById = new Map();
    this.lightIds.forEach((id, index) => { if (id != null) spotById.set(id, this.spots[index]); });
    for (const entry of this.fixtures) entry.light = spotById.get(entry.id) || null;
    this.lod = {budget: this.spots.length, full: ranked.full.length, faked: ranked.faked.length};
  }
  applyLighting(output, castersChanged = false) {
    this.displayedValues = output;
    for (const tube of this.diffuserTubes) tube.update(output);
    this.lightEntries = [];
    const candidates = [], lit = [];
    const head = new THREE.Quaternion(), inverseRoot = new THREE.Quaternion(), axis = new THREE.Vector3();
    for (const entry of this.fixtures) {
      const resolved = output[entry.id] || {}, direction = fixtureDirection(entry.fixture, resolved), rgb = emittedColor(resolved);
      head.setFromUnitVectors(DOWN, axis.fromArray(direction)).premultiply(inverseRoot.copy(entry.root.quaternion).invert());
      const pan = axisDegrees(entry.fixture, resolved, 'pan') * Math.PI / 180;
      // Even an unlit fixture can occlude another fixture's beam.
      if (!entry.head.quaternion.equals(head) || entry.yoke.rotation.y !== pan) {
        castersChanged = true;
        entry.head.quaternion.copy(head); entry.yoke.rotation.y = pan;
      }
      const brightness = entry.fixture['relative-brightness'] ?? 1;
      entry.lensMaterial.emissive.setRGB(...rgb).multiplyScalar(8 * brightness);
      entry.beam = null;
      if (!(brightness > 0 && Math.max(...rgb) > 0)) continue;
      const angle = beamAngle(entry.fixture, resolved);
      const optics = opticalFrustum(entry.fixture.position, direction, angle, entry.lensRadius);
      entry.beam = {rgb, brightness, angle, optics, direction, resolved, level: Math.max(...rgb) * brightness,
        penumbra: clamp(.72 - (Number(resolved.focus) || 0) * .52, .2, .8),
        length: roomExit(optics.optical.toArray(), optics.axis.toArray(), this.room)};
      lit.push(entry);
    }
    this.activeLights = lit.length;
    this.assignLights(lit);
    const haze = this.fog?.mode === 'haze', faked = [], footprints = [];
    for (const light of this.spots) if (!this.fixtures.some(entry => entry.light === light)) {
      if (light.intensity !== 0) { light.intensity = 0; light.color.setRGB(0, 0, 0); }
    }
    for (const entry of this.fixtures) {
      const light = entry.light, beam = entry.beam;
      if (light && !beam) { light.intensity = 0; light.color.setRGB(0, 0, 0); continue; }
      if (!beam) continue;
      const {angle, optics, rgb, brightness} = beam;
      const distance = Math.max(...this.room) * 2.5 + optics.apexDistance;
      const intensity = 145 * brightness;
      if (!light || haze) {
        faked.push({position: optics.optical.toArray(), direction: optics.axis.toArray(),
          color: rgb.map(v => v * intensity), radius: optics.radius, tangent: Math.tan(angle),
          length: beam.length, apexDistance: optics.apexDistance});
      }
      if (!light) {
        footprints.push({virtualPosition: optics.virtual.toArray(), apexDistance: optics.apexDistance,
          direction: optics.axis.toArray(), outerCos: Math.cos(angle), innerCos: Math.cos(angle * (1 - beam.penumbra)),
          color: rgb.map(v => v * intensity), range: distance});
        continue;
      }
      const near = optics.apexDistance + .006;
      const target = axis.fromArray(beam.direction).add(optics.virtual);
      if (light.castShadow && (light.angle !== angle || light.distance !== distance || light.shadow.camera.near !== near ||
          !light.position.equals(optics.virtual) || !light.target.position.equals(target))) light.shadow.needsUpdate = true;
      light.color.setRGB(...rgb); light.intensity = intensity; light.angle = angle;
      light.penumbra = beam.penumbra;
      light.position.copy(optics.virtual);
      light.distance = distance;
      if (light.shadow.camera.near !== near || light.shadow.camera.far !== distance) {
        light.shadow.camera.near = near; light.shadow.camera.far = distance;
        light.shadow.camera.updateProjectionMatrix();
      }
      light.userData.apexDistance = optics.apexDistance;
      light.target.position.copy(target); light.target.updateMatrixWorld();
      const entryBeam = {position: optics.optical, virtualPosition: optics.virtual, apexDistance: optics.apexDistance, apertureRadius: optics.radius, direction: optics.axis, color: light.color,
        angle: light.angle, penumbra: light.penumbra, intensity: light.intensity, distance: light.distance, shadow: light.castShadow ? light.shadow : null, pseudo: entry.fixture['pseudo-fixture']};
      this.lightEntries.push(entryBeam);
      this.reflections.addCandidate(candidates, entry, entryBeam, beam.resolved['beam-footprint']);
    }
    this.beams?.update(faked); this.footprints?.update(footprints);
    this.fakedEntries = faked;
    // Approximate one diffuse floor bounce from beams whose axes hit the
    // room floor. Use the same energy for surfaces and airborne haze.
    // Faked lights contribute the same bounce as full ones.
    const bounce = new THREE.Color(0, 0, 0);
    for (const entry of this.fixtures) {
      const beam = entry.beam;
      if (!beam || entry.fixture['pseudo-fixture'] || beam.optics.axis.y >= 0) continue;
      const position = beam.optics.optical, direction = beam.optics.axis;
      const distance = -position.y / direction.y;
      const x = position.x + distance * direction.x;
      const z = position.z + distance * direction.z;
      if (distance <= 0 || Math.abs(x) > this.room[0] / 2 || Math.abs(z) > this.room[2] / 2) continue;
      const weight = -direction.y * .045 * beam.brightness / (1 + distance * distance / 24);
      bounce.r += beam.rgb[0] * weight; bounce.g += beam.rgb[1] * weight; bounce.b += beam.rgb[2] * weight;
    }
    this.diffuseBounce.color.copy(bounce).multiplyScalar(.14);
    this.diffuseBounce.groundColor.copy(bounce).multiplyScalar(.55);
    this.bounceColor = bounce;
    if (castersChanged) invalidateShadows(this.spots, this.reflections?.lights);
    // The permanent spot order matches Three's shadow-first uniform order.
    if (this.spots.length) {
      if (candidates.length) this.content.updateMatrixWorld(true);
      this.reflections.resolve(candidates);
      this.reflections.illuminate(this.fog.field, this.fogAmount, this.fog.extinctionPerM);
      this.spotUniformOrder.forEach((light, i) => {
        this.apertureUniform.value[i] = light.userData.apexDistance || 0;
        this.patchRadii.value[i] = light.userData.patchRadius || 0;
      });
    }
    if (this.ready) this.updateStatus();
  }
  rebuild(rig) {
    if (this.content) {
      this.scene.remove(this.content); disposeGeometry(this.content);
      for (const entry of this.fixtures) entry.dispose();
      for (const tube of this.diffuserTubes || []) tube.dispose();
      for (const light of this.spots || []) light.dispose();
      this.reflections?.dispose();
    }
    this.room = roomDimensions(rig.room);
    this.camera.far = Math.max(100, 3 * Math.hypot(...this.room)); this.camera.updateProjectionMatrix();
    this.fog?.setRoom(this.room);
    this.content = new THREE.Group(); this.scene.add(this.content); this.content.add(createRoom(this.room, this.materials, rig.surfaces || []));
    this.trusses = [];
    for (const spec of (rig.trusses || []).slice(0, 24)) {
      if (Array.isArray(spec.position) && spec.position.every(Number.isFinite) && Number.isFinite(spec.length) && spec.length > 0)
        {const root = createTruss(spec, this.materials); this.content.add(root); this.trusses.push({id: String(spec.id), root});}
    }
    this.spots = []; this.lightIds = [];
    const fixtures = rig.fixtures || [];
    this.diffuserTubes = createDiffuserTubes(fixtures.filter(f => f['pixel-layout-id'] && f['diffuser-radius-m']), this.content);
    this.fixtures = fixtures.filter(f => !(f['pixel-layout-id'] && f['diffuser-radius-m']))
      .slice(0, this.options.fixtures).map((fixture, index) => {
      const entry = createMovingHead(fixture, this.materials); finiteApertureMaterial(entry.lensMaterial, this.apertureUniform, this.patchRadii); entry.id = String(fixture.id); entry.fixture = fixture;
      const rotation = (fixture['rotation-deg'] || [0, 0, 0]).map(v => (Number(v) || 0) * Math.PI / 180);
      entry.root.rotation.set(...rotation, 'ZYX'); this.content.add(entry.root);
      entry.light = null;
      return entry;
    });
    // A persistent pool of full-light slots. Its size fixes the forward-shading
    // light count (one program per count), so ranking never recompiles shaders.
    for (let index = 0; index < Math.min(this.lightBudget, this.fixtures.length); index++) {
      const light = new THREE.SpotLight(0x000000, 0, Math.max(...this.room) * 2.5, Math.PI / 20, .15, 2);
      light.position.set(0, this.room[1], 0); light.shadow.mapSize.set(this.options.shadowSize, this.options.shadowSize);
      light.shadow.camera.near = .06; light.shadow.camera.far = light.distance;
      light.shadow.bias = -.00008; light.shadow.normalBias = .012;
      light.castShadow = index < this.options.shadows;
      light.shadow.autoUpdate = false; light.shadow.needsUpdate = true;
      this.spots.push(light); this.lightIds.push(null);
      this.content.add(light, light.target);
    }
    this.lodState = null;
    this.footprints?.setRoom(this.room);
    this.fixtureSamples = this.fixtures.concat(this.diffuserTubes.flatMap(tube => tube.rows));
    const optedIn = fixtures.filter(f => f['pseudo-fixture']).length;
    const reflectedCap = this.quality === 'mobile' || this.quality === 'low' ? 1 : 2;
    this.reflections = new SurfaceReflections(this.content, Math.min(reflectedCap, optedIn), Math.min(512,this.options.shadowSize), this.fixtures);
    this.spotUniformOrder = [...this.spots.filter(l => l.castShadow), ...this.reflections.lights, ...this.spots.filter(l => !l.castShadow)];
    this.totalFixtureCount = (rig.fixtures || []).length;
    this.lightEntries = [];
  }
  measureLoad(elapsed, now) {
    // Shader compilation is a one-time cost. Use consecutive warm samples and
    // hysteresis so the targets do not resize on every display callback.
    if (this.qualityChoice !== 'auto' || this.draws < 2 || !this.visible) return;
    this.measuredFrameMs = elapsed;
    const budget = 1000 / this.targetHz;
    this.overBudget = elapsed > budget * 1.35 ? (this.overBudget || 0) + 1 : 0;
    this.underBudget = elapsed < budget * .65 ? (this.underBudget || 0) + 1 : 0;
    if (now - this.scaleChangedAt < 1000) return;
    let scale = this.renderScale;
    if (this.overBudget >= 4) scale = Math.max(.5, Math.min(scale * .85, scale * Math.sqrt(budget / elapsed)));
    else if (this.underBudget >= 60) scale = Math.min(1, scale + .1);
    if (scale === this.renderScale && !this.softwareRenderer) {
      const direction = this.overBudget >= 8 && scale === .5 ? -1 : this.underBudget >= 60 && scale === 1 ? 1 : 0;
      const next = adjacentQuality(this.quality, direction);
      if (next !== this.quality) {
        const sameRaster = rasterBudget(next, this.qualityChoice, this.softwareRenderer)
          === rasterBudget(this.quality, this.qualityChoice, this.softwareRenderer);
        this.renderScale = direction < 0 ? (sameRaster ? scale : .65) : .9;
        this.scaleChangedAt = now; this.overBudget = this.underBudget = 0;
        this.applyTier(next);
        return;
      }
    }
    if (scale !== this.renderScale) {
      this.renderScale = scale; this.scaleChangedAt = now; this.overBudget = this.underBudget = 0;
      this.resizePending = true;
    }
  }
  resize() {
    if (!this.visible || !this.renderer || this.disposed) return;
    this.resizePending = false;
    const bounds = this.platform.worker ? this.platform.viewport : this.canvas.getBoundingClientRect();
    if (!bounds.width || !bounds.height) return;
    if (this.qualityChoice === 'auto') {
      const layoutTier = this.autoQuality(bounds.width);
      if (layoutTier !== this.autoLayoutTier) {
        this.autoLayoutTier = layoutTier;
        this.renderScale = 1; this.overBudget = this.underBudget = 0;
        this.scaleChangedAt = performance.now();
        if (layoutTier !== this.quality) {this.applyTier(layoutTier); return;}
      }
    }
    const pixelRatio = this.platform.worker ? bounds.pixelRatio : devicePixelRatio;
    const {scene: [width, height], display: [displayWidth, displayHeight]} = rasterSizes(
      bounds.width, bounds.height, pixelRatio, rasterBudget(this.quality, this.qualityChoice, this.softwareRenderer), this.renderScale, this.maxDimension);
    this.camera.aspect = bounds.width / bounds.height; this.camera.updateProjectionMatrix();
    if (displayWidth !== this.displayWidth || displayHeight !== this.displayHeight) {
      this.renderer.setSize(displayWidth, displayHeight, false);
      this.displayWidth = displayWidth; this.displayHeight = displayHeight;
      this.displayRange = configureDisplayRange(this.renderer.getContext(), displayWidth, displayHeight, this.hdrRequested);
      this.outputPass.uniforms.extendedDisplay.value = this.displayRange === 'extended';
    }
    const samples = sceneSamples(this.quality, width, height, this.samples);
    if (width === this.width && height === this.height && this.sceneTarget.samples === samples) return;
    if (samples !== this.sceneTarget.samples) {
      this.sceneTarget.dispose();
      this.sceneTarget.samples = samples;
    }
    this.sceneTarget.setSize(width, height); this.compositeTarget.setSize(width, height);
    this.composer.setSize(width, height); this.fog.setSize(width, height);
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
    const fixture = this.fixtureById?.get(String(id)); if (!fixture) return false;
    this.navigation?.setMode('orbit');
    this.controls.target.fromArray(fixture.position || [0, 0, 0]); this.camera.position.copy(this.controls.target).add(new THREE.Vector3(1.3, .5, 2)); this.controls.update(); this.cameraName = 'fixture'; return true;
  }
  changeActivity(active) {
    this.refresh?.setActive(active && !this.paused);
    this.visible = active; if (!active) this.fog?.showAtmosphere?.suspend(); this.lastFrame = 0; this.lighting.latest = this.lighting.previous = null;
    this.displayClock.cancel(); if (!active) this.gpuTimer?.dispose(); this.navigation?.refreshActivity();
    if (!active || this.disposed || this.error) return;
    try {
      if (!this.renderer) this.initialize();
      if (this.restorePending) this.restoreContext();
      this.setQuality(this.qualityChoice);
      if (this.snapshot) this.update(this.snapshot);
      this.resize(); this.scheduleFrame(0);
    } catch (error) {this.fail(error);}
  }
  restoreContext() {
    this.restorePending = false;
    this.displayWidth = this.displayHeight = null;
    this.gpuTimer?.dispose();
    if (!this.platform.worker) this.gpuTimer = new GpuTimer(this.renderer.getContext());
    this.resize(); this.refreshEnvironment(); this.fog?.upload();
    invalidateShadows(this.spots, this.reflections?.lights);
    this.lost = false; this.ready = false; this.lastFrame = 0; this.setStatus('Restoring venue shaders…');
  }
  scheduleFrame(delay) {
    if (this.disposed || this.paused || !this.visible || this.lost || this.platform.autoStart === false) return;
    this.nextFrameAt = performance.now() + delay;
    this.displayClock.at(this.nextFrameAt, time => this.animate(time));
  }
  pause() {this.paused = true; this.refresh?.setActive(false); this.gpuTimer?.dispose(); this.navigation?.refreshActivity(); this.lighting.previous = null; this.displayClock.cancel();}
  resume() {if (!this.disposed) {this.paused = false; this.refresh?.setActive(this.visible); this.navigation?.refreshActivity(); this.lastFrame = 0; this.scheduleFrame(0);}}
  animate(time) {
    if (this.disposed || this.paused || !this.visible || this.lost) return;
    const dt = this.lastFrame ? Math.min(.1, (time - this.lastFrame) / 1000) : 0;
    this.lastFrame = time; this.clock += dt;
    try {
      const gpuMs = this.gpuTimer?.sample();
      const timed = this.gpuTimer?.begin();
      try { this.draw(dt); }
      finally { if (timed) this.gpuTimer.end(); }
      if (gpuMs != null) {
        this.lastGpuMs = gpuMs;
        this.measureLoad(Math.max(this.lastCPU, gpuMs || 0), performance.now());
      }
      // The idle budget starts at completion, not at the previous RAF timestamp.
      // Even a very slow driver must yield a real input-processing window.
      this.scheduleFrame(Math.max(0, time + this.frameInterval - performance.now()));
    } catch (error) { this.fail(error); }
  }
  draw(dt = 0) {
    if (this.disposed || this.lost || !this.visible || !this.renderer) return false;
    if (this.resizePending) this.resize();
    const started = performance.now(); if (this.platform.worker) this.controls.update();
    let relit = false;
    if (this.lighting.latest) {
      const values = this.lighting.values(this.fixtureSamples, started);
      if (values !== this.displayedValues) {this.applyLighting(values); relit = true;}
    }
    // Light detail follows the view: re-rank when the camera moved.
    this.camera.updateMatrixWorld();
    const view = this.camera.matrixWorld.elements, last = this.lodView ||= new Float32Array(16).fill(NaN);
    if (!view.every((value, i) => value === last[i])) {
      last.set(view);
      if (!relit && this.displayedValues) this.applyLighting(this.displayedValues);
    }
    const simulate = !this.reducedMotion.matches || this.clock < this.fogAnimationUntil;
    // Both preview worklights illuminate airborne material: the sky hemisphere
    // alone made dense water droplets unnaturally blue/dark against the warm
    // floor lit by the directional worklamp.
    const ambient = this.hemisphere.color.clone().multiplyScalar(this.workLight * .8)
      .add(this.workLamp.color.clone().multiplyScalar(this.workLight * .65));
    if (this.bounceColor) ambient.add(this.bounceColor);
    this.fog.setAmbient?.({color: ambient, intensity: 1, groundColor: ambient.clone().multiplyScalar(.5)});
    this.fog.update(simulate ? dt : 0, this.lightEntries || []);
    this.reflections.illuminate(this.fog.field, this.fogAmount, this.fog.extinctionPerM);
    this.footprints.render();
    this.renderer.setRenderTarget(this.sceneTarget); this.renderer.clear(); this.renderer.render(this.scene, this.camera);
    // Allocated shadow maps stay with their slots while ranking moves lights.
    this.renderedShadows = this.spots.filter(light => light.castShadow && light.shadow.map).length;
    // Glass reads an immutable HDR source while drawing into the composer.
    // When there is no glass, write fog directly into the composer input.
    this.inputPass.enabled = this.glass.enabled;
    const fogged = this.glass.enabled ? this.compositeTarget : this.composer.readBuffer;
    // Faked beams go into the fog's reduced raster and share its depth-aware
    // upsampling, so their cost does not grow with the display.
    this.fog.render(this.sceneTarget, this.camera, fogged, target => this.beams.render(this.renderer, this.camera, target,
      this.sceneTarget, {room: this.room, amount: this.fog.visibleAmount, extinctionPerM: this.fog.extinctionPerM,
        time: this.fog.field.time, densityTexture: this.fog.density}));
    this.film.uniforms.phase.value = this.reducedMotion.matches ? 0 : Math.floor(this.clock * 24) % 1024;
    this.composer.render(dt); this.editorOverlay?.render(this.renderer, this.camera); this.lastCPU = performance.now() - started; this.draws++;
    this.canvas.tolglowCaptureVideo?.(this.canvas);
    this.observedFrame?.(this.canvas, this.snapshot?.['show-id'], this.snapshot?.epoch, this.snapshot?.['frame-number'] ?? 0);
    // Backpressure leaves main-thread time for cue/parameter input when a
    // driver blocks on compilation/submission. Recover gradually after it clears.
    const nominal = 1000 / this.targetHz;
    const previous = this.frameInterval || nominal;
    // The worker computes its budget after the GPU completion fence; command
    // submission time alone cannot establish render throughput.
    if (!this.platform.worker) {
      this.frameInterval = Math.max(nominal, this.lastCPU * (this.softwareRenderer ? 4 : 1.5), previous * .95);
    }
    if (!this.ready) { this.ready = true; this.resolveReady?.(true); }
    this.updateStatus();
  }
  updateStatus() {
      const capped = this.activeLights > this.spots.length ? ` · ${this.activeLights} lit · ${this.spots.length} full lights` : '';
      const automatic = this.qualityChoice === 'auto' && this.softwareRenderer ? 'low · software GPU' : this.quality;
      this.setStatus(`${automatic} · ${this.renderedShadows || 0} shadows · ${this.workLight ? 'work light' : 'dark show'}${capped}`);
  }
  getTelemetry() {
    return {lightingMode: this.lighting.mode, targetRefreshHz: this.targetHz, tierMaxHz: this.options.fps, displayHz: this.displayHz, ready: this.ready, failed: this.error || null, renderer: `Three.js r${THREE.REVISION}`, quality: this.quality,
      deviceRenderer: this.deviceRenderer, softwareRenderer: this.softwareRenderer,
      pixels: [this.width, this.height], displayPixels: [this.displayWidth, this.displayHeight],
      sceneSamples: this.sceneTarget?.samples, edgeFilter: 'FXAA', displayRange: this.displayRange, gpuTimeMs: this.lastGpuMs,
      film: {...this.filmSettings}, measuredFrameMs: this.measuredFrameMs,
      renderScale: this.renderScale, frameCPUms: this.lastCPU, frameIntervalMs: this.frameInterval,
      idleBudgetMs: Math.max(0, this.frameInterval - this.lastCPU), nextFrameAt: this.nextFrameAt, frames: this.draws,
      geometryCount: this.renderer?.info.memory.geometries || 0, textureCount: this.renderer?.info.memory.textures || 0,
      programCount: this.renderer?.info.programs?.length || 0, activeLights: this.activeLights || 0,
      reflectedPatches: this.reflections?.patches.map(p => ({fixture:p.id, surface:p.surface, center:p.position.toArray(),
        majorRadius:p.major.length(), minorRadius:p.minor.length()})) || [],
      lightIds: this.lightIds, fixtureCount: this.totalFixtureCount, visibleFixtures: this.fixtures.length, lightCap: this.spots?.length ?? 0,
      lightLod: this.lod ? {...this.lod, fakedBeams: this.beams?.count || 0, footprints: this.footprints?.count || 0} : null,
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
    this.pause(); this.abort.abort(); this.resizeObserver?.disconnect(); this.activity?.dispose();
    this.editorInput?.dispose(); this.editorOverlay?.dispose();
    this.navigation?.dispose(); this.controls?.dispose(); this.fog?.dispose(); this.beams?.dispose(); this.footprints?.dispose();
    for (const entry of this.fixtures) entry.dispose();
    for (const tube of this.diffuserTubes || []) tube.dispose();
    for (const light of this.spots || []) light.dispose();
    this.reflections?.dispose();
    if (this.content) disposeGeometry(this.content);
    this.glass?.dispose(); this.inputPass?.dispose(); this.bloom?.dispose(); this.outputPass?.dispose(); this.edges?.dispose(); this.film?.dispose(); this.gpuTimer?.dispose(); this.composer?.dispose();
    this.sceneTarget?.dispose(); this.compositeTarget?.dispose(); this.environmentTarget?.dispose();
    if (this.materials) disposeMaterials(this.materials);
    this.renderer?.dispose(); this.resolveReady?.(false);
    if (this.canvas.tolglowPreview === this) delete this.canvas.tolglowPreview;
  }
}
