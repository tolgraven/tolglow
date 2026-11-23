// Imported only by the browser qualification harness's private worker bundle.
import {DataUtils, FramebufferTexture, NoBlending, ShaderMaterial} from 'three';
import {FullScreenQuad} from 'three/addons/postprocessing/Pass.js';
import {SceneRenderer} from '../../js/preview/scene-renderer.js';
import '../../js/preview/worker.js';

const modes = new Set(['full', 'full-no-timers', 'no-composite-fallback', 'simple-fixed-loop', 'cached-shadows', 'skip-fog-raymarch', 'present-cached-frame']);
// The composite control omits extension enablement altogether, not just calls.
const timersAllowed = typeof ROOM_GPU_TIMERS === 'undefined' || ROOM_GPU_TIMERS;
let mode = 'full', probe;

export function withoutCompositeFallback(source, mask = false) {
  const from = ' if(total>.00001)fog/=total;else integrateFog(fog);';
  if (source.split(from).length !== 2) throw new Error('Room composite fallback probe source no longer matches.');
  if (!mask) return source.replace(from, ' if(total>.00001)fog/=total;else fog=vec4(0.,0.,0.,1.);');
  const output = ' compositeOutput=vec4(color.rgb*fog.a+fog.rgb,color.a);';
  if (source.split(output).length !== 2) throw new Error('Room composite output probe source no longer matches.');
  // The original amount<=0 early return must produce an empty mask too. The
  // depth gather and its weights stay exact at the actual composite raster.
  const disabled = ' if(amount<=0.){compositeOutput=color;return;}';
  if (source.split(disabled).length !== 2) throw new Error('Room composite amount probe source no longer matches.');
  return source.replace(from, '').replace(output, ' compositeOutput=vec4(vec3(total>.00001?0.:1.),1.);')
    .replace(disabled, ' if(amount<=0.){compositeOutput=vec4(0.,0.,0.,1.);return;}');
}

// Diagnostic only: remove adaptive traversal from BOTH copies of integrateFog.
// Keep the entire density, incident-light and composite fallback calculation.
export function fixedLoop(source) {
  const replace = (from, to) => {
    if (source.split(from).length !== 2) throw new Error('Room loop probe shader no longer matches its exact source.');
    source = source.replace(from, to);
  };
  replace(` float baseStep=(finish-start)/float(raySteps);
 vec2 detailSpan=vec2(1.,0.);
 if(all(lessThan(detailMin,detailMax)))detailSpan=boxHit(eye,ray,detailMin,detailMax);
 // Desktop has fewer spare evaluations than dense base intervals can occupy.
 // Distribute those subdivisions along the whole plume instead of refining
 // only its near end. Integer differences keep the total at most 72.
 int detailFirst=int(clamp(floor((detailSpan.x-start)/baseStep),0.,float(raySteps)));
 int detailLast=int(clamp(ceil((detailSpan.y-start)/baseStep),0.,float(raySteps)));
 int detailCount=max(0,detailLast-detailFirst),extra=min(72-raySteps,detailCount);`,
  ' float stepSize=(finish-start)/float(raySteps);');
  replace(` int baseIndex=0,subdivision=0,subdivisions=1;
 float stepSize=baseStep;
 for(int n=0;n<72;n++){
  if(baseIndex>=raySteps||transmission<.002)break;
  float baseBegin=start+float(baseIndex)*baseStep;
  if(subdivision==0){
   // Upload bounds every cell that can enable procedural plume detail. Spend
   // the existing 72-sample ceiling there; uniform haze keeps its base budget.
   bool detailed=detailSpan.y>detailSpan.x && baseBegin<detailSpan.y && baseBegin+baseStep>detailSpan.x;
   subdivisions=detailed?max(1,72/raySteps):1;
   if(detailed && raySteps>36 && raySteps<72 && detailCount>0){
    int local=baseIndex-detailFirst;
    // Floating overlap can include a touching neighbor outside floor/ceil's
    // integer range. Only that range owns extra samples; otherwise a local=-1
    // interval can spend a 25th extra sample and truncate the far end of a ray.
    if(local>=0 && local<detailCount)
     subdivisions=1+(local+1)*extra/detailCount-local*extra/detailCount;
   }
   stepSize=baseStep/float(subdivisions);
  }
  float begin=baseBegin+float(subdivision)*stepSize;`,
  ` for(int n=0;n<72;n++){
  if(n>=raySteps||transmission<.002)break;
  float begin=start+float(n)*stepSize;`);
  replace(`  subdivision++;
  if(subdivision>=subdivisions){baseIndex++;subdivision=0;}\n`, '');
  return source;
}

class FrameCopy {
  constructor(engine) {
    const {renderer, canvas} = engine;
    this.texture = new FramebufferTexture(canvas.width, canvas.height);
    this.material = new ShaderMaterial({uniforms: {frame: {value: this.texture}}, depthTest: false,
      depthWrite: false, blending: NoBlending,
      vertexShader: 'varying vec2 at; void main(){at=uv;gl_Position=vec4(position.xy,0.,1.);}',
      fragmentShader: 'varying vec2 at;uniform sampler2D frame;void main(){gl_FragColor=texture2D(frame,at);}'});
    this.quad = new FullScreenQuad(this.material);
    renderer.setRenderTarget(null);
    renderer.copyFramebufferToTexture(this.texture);
  }
  draw(renderer) {
    renderer.setRenderTarget(null);
    this.quad.render(renderer);
  }
  dispose() {this.texture.dispose(); this.material.dispose(); this.quad.dispose();}
}

function inspect(engine) {
  const renderer = engine.renderer, gl = renderer.getContext();
  const ext = timersAllowed ? gl.getExtension('EXT_disjoint_timer_query_webgl2') : null;
  const originalShadowUpdate = renderer.shadowMap.autoUpdate;
  const pending = new Map(), instrumentedFog = new WeakMap();
  let sample, frame = 0, copy, frozenPolicy, comparison, resetPending = false;
  const policy = () => ({choice: engine.qualityChoice, tier: engine.quality, scale: engine.renderScale,
    scene: [engine.width, engine.height], display: [engine.displayWidth, engine.displayHeight],
    fog: [engine.fog.width, engine.fog.height], raySteps: engine.fog.budget.steps,
    sceneSamples: engine.sceneTarget.samples, targetHz: engine.targetHz});
  const detailActive = () => {
    const low = engine.fog.uniforms.detailMin.value, high = engine.fog.uniforms.detailMax.value;
    return low.x < high.x && low.y < high.y && low.z < high.z;
  };
  const measureLoad = engine.measureLoad.bind(engine);
  engine.measureLoad = (elapsed, now) => {
    if (frozenPolicy) engine.measuredFrameMs = elapsed;
    else measureLoad(elapsed, now);
  };
  function observe(phase, run) {
    const timed = sample.timerQueriesEnabled && frame % 8 === 0 && !pending.has(phase);
    if (timed) sample.timerApiCalls++;
    const query = timed ? gl.createQuery() : null;
    if (query) {gl.beginQuery(ext.TIME_ELAPSED_EXT, query); sample.timerApiCalls++; sample.timerQueriesStarted++;}
    const start = performance.now();
    try {return run();} finally {
      sample.submitMs[phase] = performance.now() - start;
      if (query) {gl.endQuery(ext.TIME_ELAPSED_EXT); sample.timerApiCalls++; pending.set(phase, query);}
    }
  }
  function prepare() {
    frame++;
    sample = {mode, frame, submitMs: {}, gpuMs: {}, timerSupported: Boolean(ext), frozenPolicy,
      policy: policy(), timerExtensionRequested: timersAllowed, timerQueriesEnabled: Boolean(ext) && mode !== 'full-no-timers',
      timerQueriesStarted: 0, timerApiCalls: 0};
    if (!sample.timerQueriesEnabled) return;
    sample.timerApiCalls++;
    const disjoint = gl.getParameter(ext.GPU_DISJOINT_EXT);
    for (const [name, query] of pending) {
      if (!disjoint) sample.timerApiCalls++;
      if (disjoint || gl.getQueryParameter(query, gl.QUERY_RESULT_AVAILABLE)) {
        if (!disjoint) {sample.gpuMs[name] = gl.getQueryParameter(query, gl.QUERY_RESULT) / 1e6; sample.timerApiCalls++;}
        sample.timerApiCalls++;
        gl.deleteQuery(query); pending.delete(name);
      }
    }
  }
  const opaque = renderer.render.bind(renderer);
  renderer.render = (scene, camera) => {
    if (scene === engine.scene && camera === engine.camera) return observe('opaque', () => opaque(scene, camera));
    if (sample && scene === engine.fog.scene && engine.fog.quad.material === engine.fog.volumeMaterial)
      sample.raymarched = true;
    return opaque(scene, camera);
  };
  function observeFog(current) {
    let shaders = instrumentedFog.get(current);
    if (shaders) {
      for (const shader of shaders) {
        const source = mode === 'simple-fixed-loop' ? shader.simple
          : mode === 'no-composite-fallback' && shader.withoutFallback ? shader.withoutFallback : shader.original;
        if (shader.material.fragmentShader !== source) {
          shader.material.fragmentShader = source; shader.material.needsUpdate = true;
        }
      }
      return;
    }
    shaders = [current.volumeMaterial, current.compositeMaterial].map(material =>
      ({material, original: material.fragmentShader, simple: fixedLoop(material.fragmentShader),
        withoutFallback: material === current.compositeMaterial ? withoutCompositeFallback(material.fragmentShader) : null}));
    instrumentedFog.set(current, shaders);
    const render = current.render.bind(current);
    current.render = (...args) => observe('fog', () => {
      const amount = current.amount;
      if (mode === 'skip-fog-raymarch') current.amount = 0;
      try {return render(...args);} finally {current.amount = amount;}
    });
    observeFog(current);
  }
  const compose = engine.composer.render.bind(engine.composer);
  engine.composer.render = (...args) => observe('post', () => compose(...args));
  return {
    draw(dt, ordinary) {
      if (resetPending) {this.reset(); resetPending = false;}
      prepare();
      // Selecting a tier replaces the fog target and its shader owner.
      observeFog(engine.fog);
      if (comparison) {
        const request = comparison; comparison = null;
        try {request.resolve(request.mask ? this.fallbackMaskNow() : this.comparePixelsNow());} catch (error) {request.reject(error);}
      }
      renderer.shadowMap.autoUpdate = mode === 'cached-shadows' ? false : originalShadowUpdate;
      if (mode === 'present-cached-frame' && copy) {
        const start = performance.now();
        observe('replay', () => copy.draw(renderer));
        engine.lastCPU = performance.now() - start; engine.draws++;
      } else {
        ordinary.call(engine, dt);
        if (mode === 'present-cached-frame') copy = new FrameCopy(engine);
      }
    },
    reset() {
      copy?.dispose(); copy = null;
      for (const query of pending.values()) gl.deleteQuery(query);
      pending.clear();
      renderer.shadowMap.autoUpdate = originalShadowUpdate;
    },
    requestReset() {resetPending = true;},
    sample: () => ({...sample, policy: policy(), detailActive: detailActive()}),
    freezeQuality() {
      if (engine.qualityChoice !== 'auto' || engine.resizePending) throw new Error('Room Auto policy is not ready to freeze.');
      frozenPolicy ||= policy();
      return frozenPolicy;
    },
    comparePixels() {
      if (comparison) throw new Error('Room pixel comparison already queued.');
      return new Promise((resolve, reject) => {comparison = {resolve, reject};});
    },
    fallbackMask() {
      if (comparison) throw new Error('Room pixel comparison already queued.');
      return new Promise((resolve, reject) => {comparison = {resolve, reject, mask: true};});
    },
    fallbackMaskNow() {
      const fog = engine.fog, material = fog.compositeMaterial;
      const original = material.fragmentShader, previousMaterial = fog.quad.material;
      if (!frozenPolicy || mode !== 'full' || timersAllowed || detailActive())
        throw new Error('Frozen full renderer without timer extension or active detail is required for the mask.');
      try {
        material.fragmentShader = withoutCompositeFallback(original, true); material.needsUpdate = true;
        fog.render(engine.sceneTarget, engine.camera, engine.compositeTarget);
        const target = engine.compositeTarget, pixels = new Uint16Array(target.width * target.height * 4);
        renderer.readRenderTargetPixels(target, 0, 0, target.width, target.height, pixels);
        let fallbackPixels = 0, invalidPixels = 0;
        for (let i = 0; i < pixels.length; i += 4) {
          if (pixels[i] === 0x3c00) fallbackPixels++;
          else if (pixels[i] !== 0) invalidPixels++;
        }
        return {fallbackPixels, invalidPixels, totalPixels: target.width * target.height,
          targetSize: [target.width, target.height], fraction: fallbackPixels / (target.width * target.height),
          amount: fog.compositeUniforms.amount.value,
          fieldTime: fog.field.time, policy: policy(), timerExtensionRequested: timersAllowed};
      } finally {
        material.fragmentShader = original; material.needsUpdate = true; fog.quad.material = previousMaterial;
      }
    },
    comparePixelsNow() {
      const fog = engine.fog, previousMode = mode, previousMaterial = fog.quad.material;
      const density = fog.field.stats().maximumDensity;
      if (!frozenPolicy || density >= .25 || fog.showAtmosphere || detailActive())
        throw new Error('Frozen local dispersed haze is required for loop pixel comparison.');
      const read = target => {
        const pixels = new Uint16Array(target.width * target.height * 4);
        renderer.readRenderTargetPixels(target, 0, 0, target.width, target.height, pixels);
        return pixels;
      };
      const capture = choice => {
        mode = choice; observeFog(fog);
        // No field update, lighting interpolation or clock advance. The opaque
        // target, camera, uniforms and time are identical for these two draws.
        fog.render(engine.sceneTarget, engine.camera, engine.compositeTarget);
        return {volume: read(fog.target), composite: read(engine.compositeTarget)};
      };
      try {
        const full = capture('full'), simple = capture('simple-fixed-loop');
        const differences = {};
        for (const name of ['volume', 'composite']) {
          let changed = 0, nonFinite = 0, maximum = 0, squared = 0;
          for (let i = 0; i < full[name].length; i++) {
            const a = DataUtils.fromHalfFloat(full[name][i]), b = DataUtils.fromHalfFloat(simple[name][i]);
            const delta = Math.abs(a - b);
            if (!Number.isFinite(a) || !Number.isFinite(b)) nonFinite++;
            if (delta) changed++;
            maximum = Math.max(maximum, delta); squared += delta * delta;
          }
          differences[name] = {channels: full[name].length, changed, nonFinite, maximum,
            rmse: Math.sqrt(squared / full[name].length)};
        }
        return {maximumDensity: density, detailActive: detailActive(), fieldTime: fog.field.time, policy: policy(), differences};
      } finally {
        mode = previousMode; observeFog(fog); fog.quad.material = previousMaterial;
      }
    },
    state: () => ({mode, frozenPolicy, policy: policy(), detailActive: detailActive(),
      maximumDensity: engine.fog.field.stats().maximumDensity})
  };
}

const draw = SceneRenderer.prototype.draw;
SceneRenderer.prototype.draw = function(dt) {
  if (!this.renderer || !this.fog || !this.composer || this.disposed || this.lost || !this.visible) return draw.call(this, dt);
  probe ||= inspect(this);
  return probe.draw(dt, draw);
};

const send = self.postMessage.bind(self);
self.postMessage = function(message, ...options) {
  if (message?.type === 'frame' && probe) {
    message.telemetry.passProbe = {...probe.sample(), maximumDensity: message.telemetry.fog.maximumDensity,
      sentAt: performance.timeOrigin + performance.now()};
  }
  return send(message, ...options);
};
self.setRoomPassMode = choice => {
  if (!modes.has(choice)) throw new Error('Unknown Room pass probe '+choice);
  probe?.requestReset(); mode = choice;
  return choice;
};
self.freezeRoomPassQuality = () => {
  if (!probe) throw new Error('Room renderer has not drawn.');
  return probe.freezeQuality();
};
self.getRoomPassState = () => probe?.state();
self.compareRoomPassPixels = () => probe.comparePixels();
self.readRoomFallbackMask = () => probe.fallbackMask();
