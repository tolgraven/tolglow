import * as THREE from 'three';
import {SceneRenderer} from '../../js/preview/scene-renderer.js';

const canvas = document.querySelector('canvas');
const engine = new SceneRenderer(canvas, null, {autoStart: false});
engine.visible = true;
if (!engine.renderer) engine.initialize();
engine.pause();
engine.controls.enableDamping = false;
engine.setLightingMode('hold');

const centerHeight = 2.3;
const beamHeight = 3.65;
const colors = [[.8, .18, .045], [.11, .51, .82]];
const roomColors = [[.78, .22, .045], [.21, .48, .67]];
const points = {
  controlled: {crossing: [.5, .5], shoulder: [.5, .54], lens: [.22, .80], dark: [.52, .71]},
  room: {air: [.49, .36], floor: [.47, .207], floorEdge: [.34, .207], lens: [.53, .69], dark: [.74, .24]},
};
let scene, rig, probes, revision = 0, shadowValues;
let stages;
let extrema;

function meanRGB(pixels, decode) {
  const totals = [0, 0, 0];
  for (let i = 0; i < pixels.length; i += 4) {
    for (let channel = 0; channel < 3; channel++) totals[channel] += decode(pixels[i + channel]);
  }
  return totals.map(sum => sum / (pixels.length / 4));
}

function samples(width, height, pixels, read, decode) {
  // Use the same footprint in display coordinates when a source raster shrinks.
  const sampleWidth = Math.max(1, Math.round(7 * width / canvas.width));
  const sampleHeight = Math.max(1, Math.round(7 * height / canvas.height));
  const region = pixels.subarray(0, sampleWidth * sampleHeight * 4);
  return Object.fromEntries(Object.entries(probes).map(([name, [x, y]]) => {
    const left = Math.floor(x * width) - Math.floor(sampleWidth / 2);
    const bottom = Math.floor(y * height) - Math.floor(sampleHeight / 2);
    read(left, bottom, sampleWidth, sampleHeight, region);
    return [name, meanRGB(region, decode)];
  }));
}

function targetSamples(target) {
  const size = Math.ceil(7 * target.width / canvas.width) * Math.ceil(7 * target.height / canvas.height) * 4;
  return samples(target.width, target.height, new Uint16Array(size),
    (x, y, w, h, pixels) => engine.renderer.readRenderTargetPixels(target, x, y, w, h, pixels),
    THREE.DataUtils.fromHalfFloat);
}

function targetExtrema(target) {
  const pixels = new Uint16Array(target.width * target.height * 4);
  engine.renderer.readRenderTargetPixels(target, 0, 0, target.width, target.height, pixels);
  const result = {minimum: [Infinity, Infinity, Infinity], maximum: [-Infinity, -Infinity, -Infinity],
    negative: 0, nonfinite: 0, peakPixel: null, dimensions: [target.width, target.height]};
  let peak = -Infinity;
  for (let i = 0; i < pixels.length; i += 4) for (let c = 0; c < 3; c++) {
    const value = THREE.DataUtils.fromHalfFloat(pixels[i + c]);
    if (!Number.isFinite(value)) { result.nonfinite++; continue; }
    if (value < 0) result.negative++;
    result.minimum[c] = Math.min(result.minimum[c], value);
    result.maximum[c] = Math.max(result.maximum[c], value);
    if (value > peak) {
      peak = value; result.peakPixel = {x: (i / 4) % target.width, y: Math.floor(i / 4 / target.width), channel: c, value};
    }
  }
  return result;
}

function screenPicture(gl) {
  const width = gl.drawingBufferWidth, height = gl.drawingBufferHeight;
  const pixels = new Uint8Array(width * height * 4);
  gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
  const image = new ImageData(width, height), stride = width * 4;
  for (let y = 0; y < height; y++)
    image.data.set(pixels.subarray(y * stride, (y + 1) * stride), (height - 1 - y) * stride);
  const exported = document.createElement('canvas'); exported.width = width; exported.height = height;
  exported.getContext('2d').putImageData(image, 0, 0);
  return exported.toDataURL('image/png');
}

function screenSamples(gl) {
  return samples(gl.drawingBufferWidth, gl.drawingBufferHeight, new Uint8Array(7 * 7 * 4),
    (x, y, w, h, pixels) => gl.readPixels(x, y, w, h, gl.RGBA, gl.UNSIGNED_BYTE, pixels),
    value => value / 255);
}

for (const [pass, name] of [[engine.bloom, 'preBloom'], [engine.outputPass, 'postBloom'], [engine.edges, 'display'], [engine.film, 'edges']]) {
  const render = pass.render.bind(pass);
  pass.render = (renderer, output, input, ...options) => {
    if (stages) stages[name] = targetSamples(input);
    if (extrema) extrema[name] = targetExtrema(input);
    render(renderer, output, input, ...options);
    if (stages && pass === engine.bloom) stages.bloomSource = targetSamples(engine.bloom.renderTargetBright);
  };
}

function update(values, force = false) {
  const serial = ++revision;
  engine.update({'show-id': scene, epoch: 1, revision: serial, 'frame-number': serial, rig, output: {values}}, force);
}

function configure({width, height, quality, separation = 0, roomRig}) {
  shadowValues = null;
  scene = roomRig ? 'room' : 'controlled';
  canvas.style.width = width + 'px';
  canvas.style.height = height + 'px';
  engine.setQuality(quality);
  engine.visible = true;
  engine.pause();
  engine.resize();

  const fixtures = [-2.1, 2.1].map((x, i) => ({
    id: i ? 'B' : 'A', position: [x, beamHeight, i ? -separation : 0], 'beam-angle': 12, 'lens-radius-m': .09,
  }));
  rig = roomRig || {room: {width: 12, height: 5, depth: 12}, fixtures};
  probes = points[scene];
  engine.setFog(.65);
  engine.setWorkLight(roomRig ? .12 : 0);
  update({}, true);
  engine.fog.field.clear(roomRig ? .07 : .13);
  engine.fog.upload();
  if (roomRig) engine.setCamera('audience');
  return {
    quality: engine.quality,
    raster: [engine.sceneTarget.width, engine.sceneTarget.height],
    fogRaster: [engine.fog.target.width, engine.fog.target.height],
    display: [canvas.width, canvas.height],
    samples: engine.sceneTarget.samples,
    renderer: engine.deviceRenderer,
  };
}

function configureShadow({auto = false, phase = 'fresh-plume'} = {}) {
  scene = 'shadow-crossing';
  engine.platform.worker = true;
  engine.platform.viewport = auto ? {width: 1559, height: 700, pixelRatio: 1.8}
    : {width: 960, height: 540, pixelRatio: 1};
  canvas.style.width = engine.platform.viewport.width + 'px';
  canvas.style.height = engine.platform.viewport.height + 'px';
  engine.visible = true; engine.pause();
  engine.qualityChoice = auto ? 'auto' : 'desktop'; engine.renderScale = auto ? .5 : 1;
  engine.applyTier(auto ? 'low' : 'desktop'); engine.resize();
  engine.setWorkLight(0); engine.setFog(phase === 'clear-surfaces' ? 0 : .7);
  engine.setFilm({bloom: .22, exposure: .85, grain: 0});
  engine.camera.position.set(3.2, 2.8, 7.6); engine.controls.target.set(0, 1.65, -1); engine.controls.update();
  const fixtures = Array.from({length: 8}, (_, i) => ({id: `head-${i + 1}`,
    position: [(i - 3.5) * 1.1, 3.5, -1.5], channels: {pan: 1, tilt: 2},
    'beam-angle': 12, 'lens-radius-m': .119}));
  const colors = [[.85,.8,.7], [.15,.7,.85], [.85,.36,.08]];
  shadowValues = Object.fromEntries(fixtures.map((fixture, i) => {
    const axis = new THREE.Vector3(-fixture.position[0] * 1.35, -3.35, 2.8).normalize();
    return [fixture.id, {pose: {direction: axis.toArray(),
      'pan-deg': Math.atan2(axis.x, axis.z) * 180 / Math.PI, 'tilt-deg': Math.asin(axis.y) * 180 / Math.PI},
      'emitted-color': colors[i % colors.length], focus: .45}];
  }));
  rig = {room: {width: 12, height: 5, depth: 10}, fixtures,
    trusses: [{id: 'back', position: [0, 4.15, -1.5], length: 9}]};
  update(shadowValues, true);
  // The original shadow fixture restores all eight poses/colors, the camera,
  // and an empty glass list before capturing. Reproduce that final state here.
  engine.fog.field.clear(.15);
  if (phase === 'fresh-plume' || phase === 'dispersed-plume') {
    engine.fog.burst({position: [-1,.25,.4], direction: [1,.5,-.25], duration: 1.5, strength: 4});
    for (let i = 0; i < (phase === 'fresh-plume' ? 40 : 240); i++) engine.fog.field.advance(.05);
  }
  engine.fog.upload(); engine.camera.updateMatrixWorld();
  const world = {crossing: [0, 3.5 - 3.35 / 1.35, -1.5 + 2.8 / 1.35],
    shoulder: [0, 1.6, .3], floor: [0, .02, -1.5 + 2.8 * 3.5 / 3.35],
    floorEdge: [1.2, .02, 1.4], lens: fixtures[3].position, dark: [-4, 1.5, 1]};
  probes = Object.fromEntries(Object.entries(world).map(([name, point]) => {
    const screen = new THREE.Vector3(...point).project(engine.camera);
    return [name, [(screen.x + 1) / 2, (screen.y + 1) / 2]];
  }));
  return {phase, auto, world, probes, telemetry: engine.getTelemetry(), rig, values: shadowValues};
}

function draw({mask = -1, gain = 1, exposure = .85, bloom = .28, grain = 0, yaw = 0, measure = true, picture = false, diagnose = false} = {}) {
  if (scene === 'controlled') {
    engine.camera.position.set(4.8 * Math.sin(yaw), centerHeight, 4.8 * Math.cos(yaw));
    engine.controls.target.set(0, centerHeight, 0);
    engine.controls.update();
  }
  const values = Object.fromEntries(rig.fixtures.map((fixture, i) => {
    const [x, y] = fixture.position;
    const resolved = shadowValues?.[fixture.id];
    const rgb = resolved?.['emitted-color'] || (scene === 'room' ? roomColors : colors)[i % 2];
    return [fixture.id, {
      ...resolved,
      'emitted-color': rgb.map(c => (mask & (1 << i)) ? c * gain : 0),
      direction: scene === 'room' ? [(i - 3.5) * -.22, -1, .45] : [-x, centerHeight - y, 0],
      zoom: scene === 'room' ? .16 : 0,
      focus: resolved?.focus ?? .2,
      'cut?': true,
    }];
  }));
  engine.setFilm({exposure, bloom, grain});
  update(values);
  const gl = engine.renderer.getContext();
  gl.finish();
  const start = performance.now();
  const before = engine.draws;
  stages = measure ? {} : null;
  extrema = diagnose ? {} : null;
  engine.draw(0);
  if (engine.draws !== before + 1) throw new Error('Preview did not draw');

  // EffectComposer restores its prior target. Read the actual canvas before
  // sampling intermediary targets or returning control to the browser.
  engine.renderer.setRenderTarget(null);
  const captured = picture ? screenPicture(gl) : undefined;
  if (!stages) {
    if (picture) return captured;
    gl.readPixels(0, 0, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, new Uint8Array(4));
    return performance.now() - start;
  }
  const screen = screenSamples(gl);
  const report = {
    scene: targetSamples(engine.sceneTarget),
    volume: targetSamples(engine.fog.target),
    // Follow the actual pass input even when the renderer composites fog
    // directly into the composer and bypasses its separate fogged target.
    linear: stages.preBloom ?? stages.postBloom,
    ...stages,
    extrema: extrema && {scene: targetExtrema(engine.sceneTarget), volume: targetExtrema(engine.fog.target), ...extrema},
    screen,
    settings: {mask, gain, exposure, bloom, grain, yaw},
    lightIds: engine.lightIds,
    lightShadows: engine.fixtures.filter(fixture => fixture.light?.castShadow).map(fixture => fixture.id),
    picture: captured,
    glError: gl.getError(),
  };
  stages = null; extrema = null;
  return report;
}

function measureQuadrature({auto = true} = {}) {
  const config = configureShadow({auto, phase:'fresh-plume'}), fog = engine.fog;
  const materials = [fog.volumeMaterial, fog.compositeMaterial];
  const shaders = materials.map(material => material.fragmentShader);
  const baseSteps = fog.uniforms.raySteps.value;
  const readVolume = () => {
    const data = new Uint16Array(fog.target.width * fog.target.height * 4);
    engine.renderer.readRenderTargetPixels(fog.target, 0, 0, fog.target.width, fog.target.height, data);
    return Float32Array.from(data, THREE.DataUtils.fromHalfFloat);
  };
  const sample = steps => {
    fog.uniforms.raySteps.value = steps;
    draw({mask:-1,bloom:.22,measure:false});
    return readVolume();
  };
  try {
    fog.uniforms.detailMin.value.set(1,1,1); fog.uniforms.detailMax.value.set(-1,-1,-1);
    const coarse = sample(baseSteps);
    fog.upload(); const adaptive = sample(baseSteps);
    // The reference must change BOTH the hard loop ceiling and requested steps.
    // Merely setting raySteps=192 would integrate only the first 72/192 of a ray.
    for (const material of materials) {
      if (!material.fragmentShader.includes('n<72')) throw new Error('Unknown fog loop bound');
      material.fragmentShader = material.fragmentShader.replace('n<72','n<192'); material.needsUpdate = true;
    }
    const reference = sample(192), [width,height] = [fog.target.width,fog.target.height];
    const error = pixels => {
      let count=0,squared=0,absolute=0,maximumOver=0; const relative=[];
      for(let y=Math.floor(height*.18);y<height*.6;y++)for(let x=Math.floor(width*.34);x<width*.6;x++) {
        const i=(y*width+x)*4;
        const luminance=p=>p[i]*.2126+p[i+1]*.7152+p[i+2]*.0722;
        const expected=luminance(reference),actual=luminance(pixels);
        if(expected<=.02)continue;
        const delta=actual-expected; count++; squared+=delta*delta; absolute+=Math.abs(delta);
        relative.push(Math.abs(delta)/expected); maximumOver=Math.max(maximumOver,delta);
      }
      relative.sort((a,b)=>a-b);
      return {count,mae:absolute/count,rmse:Math.sqrt(squared/count),p95relative:relative[Math.floor(relative.length*.95)],maximumOver};
    };
    materials.forEach((material,i)=>{material.fragmentShader=shaders[i]; material.needsUpdate=true;});
    // Count executed production integration intervals, without substituting its
    // stepping logic. The readback is diagnostic; it is never a cadence sample.
    const counted = shaders[0].replace('float transmission=1.;','float transmission=1.;float samples=0.;')
      .replace('transmission*=segment;','transmission*=segment;samples+=1.;')
      .replace('volumeOutput=vec4(radiance,transmission);','volumeOutput=vec4(samples,0.,0.,1.);');
    if(counted===shaders[0])throw new Error('Unknown fog counter insertion point');
    fog.volumeMaterial.fragmentShader=counted; fog.volumeMaterial.needsUpdate=true;
    const counts = steps => {
      const pixels=sample(steps), histogram={};
      for(let i=0;i<pixels.length;i+=4) histogram[pixels[i]]=(histogram[pixels[i]]||0)+1;
      const positive=Object.keys(histogram).map(Number).filter(value=>value>0);
      return {minimum:Math.min(...positive),maximum:Math.max(...positive),histogram,
        mean:positive.reduce((sum,value)=>sum+value*histogram[value],0)/positive.reduce((sum,value)=>sum+histogram[value],0)};
    };
    const plumeCounts=counts(24), mobilePlumeCounts=counts(32), desktopPlumeCounts=counts(48);
    fog.field.clear(.15); fog.upload();
    const hazeCounts=counts(24), mobileHazeCounts=counts(32), desktopHazeCounts=counts(48), highHazeCounts=counts(72);
    // Whole-room detail reaches the declared upper bound without early opaque
    // termination; it must still integrate every interval exactly once.
    engine.setFog(.05); fog.field.clear(.5); fog.upload();
    const fullCounts=counts(24), mobileFullCounts=counts(32), desktopFullCounts=counts(48), highFullCounts=counts(72);
    return {config,coarse:error(coarse),adaptive:error(adaptive),
      counts:{plume:plumeCounts,mobilePlume:mobilePlumeCounts,desktopPlume:desktopPlumeCounts,
        haze:hazeCounts,mobileHaze:mobileHazeCounts,desktopHaze:desktopHazeCounts,highHaze:highHazeCounts,
        full:fullCounts,mobileFull:mobileFullCounts,desktopFull:desktopFullCounts,highFull:highFullCounts},
      finiteNonnegative:[coarse,adaptive,reference].every(pixels=>pixels.every(value=>Number.isFinite(value)&&value>=0))};
  } finally {
    materials.forEach((material,i)=>{material.fragmentShader=shaders[i]; material.needsUpdate=true;});
    fog.uniforms.raySteps.value=baseSteps;
    configureShadow({auto,phase:'fresh-plume'}); draw({mask:-1,bloom:.22,measure:false});
  }
}

window.crossing = {configure, configureShadow, draw, measureQuadrature, dispose: () => engine.dispose()};
