// Deterministic browser fixture. No host/show/device access.
import * as THREE from 'three';
import {VolumetricFog} from '../../../js/preview/fog.js';

// Both full and half-resolution targets have an exact centre ray.
const width = 99, height = 99;
const renderer = new THREE.WebGLRenderer({antialias: false});
document.body.append(renderer.domElement); renderer.setSize(width, height);
renderer.outputColorSpace = THREE.LinearSRGBColorSpace; renderer.toneMapping = THREE.NoToneMapping;
renderer.shadowMap.enabled = true; renderer.shadowMap.type = THREE.PCFShadowMap;
renderer.debug.onShaderError = (gl, program, vertex, fragment) => {
  throw new Error(gl.getProgramInfoLog(program) || gl.getShaderInfoLog(fragment) || gl.getShaderInfoLog(vertex));
};
const scene = new THREE.Scene(), camera = new THREE.PerspectiveCamera(50, 1, .08, 80);
camera.position.set(0, 2, 4); camera.lookAt(0, 2, 0); camera.updateMatrixWorld();
const target = new THREE.WebGLRenderTarget(width, height, {type: THREE.HalfFloatType});
target.depthTexture = new THREE.DepthTexture(width, height, THREE.UnsignedIntType);
const output = new THREE.WebGLRenderTarget(width, height, {type: THREE.HalfFloatType, depthBuffer: false});
const fog = new VolumetricFog(renderer, {quality: 'desktop'}); fog.setRoom([8, 4, 8]); fog.setSize(width, height);
const pixels = renderTarget => {
  const data = new Uint16Array(width * height * 4); renderer.readRenderTargetPixels(renderTarget, 0, 0, width, height, data);
  return Array.from(data, value => THREE.DataUtils.fromHalfFloat(value));
};
const sum = data => data.reduce((total, value, i) => total + (i % 4 === 3 ? 0 : value), 0);
const difference = (a, b) => a.reduce((largest, value, i) => Math.max(largest, Math.abs(value - b[i])), 0);
const render = (lights = []) => {
  fog.update(0, lights); renderer.setRenderTarget(target); renderer.render(scene, camera);
  fog.render(target, camera, output); return pixels(output);
};
const light = new THREE.SpotLight(0xffffff, 200, 12, .32, .12, 2);
light.position.set(0, 3, 2); light.target.position.set(0, 1.2, -4);
light.castShadow = true; light.shadow.mapSize.set(256, 256); light.shadow.bias = -.0001;
scene.add(light, light.target);
const entry = () => ({position: light.position, direction: light.target.position.clone().sub(light.position).normalize(),
  color: light.color, angle: light.angle, intensity: light.intensity, distance: light.distance, shadow: light.shadow});

window.fogTest = () => {
  const gl = renderer.getContext(), debug = gl.getExtension('WEBGL_debug_renderer_info');
  const result = {renderer: debug ? gl.getParameter(debug.UNMASKED_RENDERER_WEBGL) : gl.getParameter(gl.RENDERER)};
  scene.background = new THREE.Color(.125, .25, .5);
  fog.field.clear(.5); fog.upload(); fog.setAmount(0);
  const zero = render([entry()]); result.zeroAmountDifference = difference(zero, pixels(target));
  fog.field.clear(0); fog.upload(); fog.setAmount(1);
  const empty = render([entry()]); result.emptyFieldDifference = difference(empty, pixels(target));
  fog.field.clear(.35); fog.upload();
  const extinguished = render([]), original = pixels(target);
  result.extinctionRatio = sum(extinguished) / sum(original);
  result.noInventedRadiance = extinguished.every((value, i) => i % 4 === 3 || value <= original[i]);

  // A complete opaque plane in front of all density must exclude the entire volume.
  const wall = new THREE.Mesh(new THREE.PlaneGeometry(20, 20), new THREE.MeshBasicMaterial({color: new THREE.Color(.125, .25, .5)}));
  wall.position.set(0, 2, 2); scene.add(wall); fog.field.clear(0);
  const [nx, ny, nz] = fog.field.grid;
  for (let z = 0; z < nz; z++) if ((z + .5) / nz * 8 - 4 < .5)
    fog.field.data.fill(.8, z * nx * ny, (z + 1) * nx * ny);
  fog.upload(); const hidden = render([entry()]); result.behindOpaqueDifference = difference(hidden, pixels(target));
  scene.remove(wall); wall.geometry.dispose(); wall.material.dispose();

  scene.background = new THREE.Color(0, 0, 0); fog.field.clear(.3); fog.upload();
  // An empty light path must retain all radiance when shadow sampling is enabled.
  // Merely checking that an occluder dims something misses attachment/format
  // migrations that make every shadowed light incorrectly disappear.
  const clearUnshadowed = render([{...entry(), shadow: null}]);
  const clearShadowed = render([entry()]);
  result.clearShadowDifference = difference(clearUnshadowed, clearShadowed);
  result.clearShadowRatio = sum(clearShadowed) / sum(clearUnshadowed);
  result.comparisonDepthTexture = light.shadow.map.depthTexture?.compareFunction === THREE.LessEqualCompare;
  const blocker = new THREE.Mesh(new THREE.BoxGeometry(1.1, 1.1, .15), new THREE.MeshBasicMaterial({color: 0x000000}));
  blocker.position.set(0, 2.45, .5); blocker.castShadow = true; scene.add(blocker);
  camera.position.set(3, 2, 4); camera.lookAt(0, 2, 0); camera.updateMatrixWorld();
  const unshadowed = render([{...entry(), shadow: null}]), shadowed = render([entry()]);
  result.unshadowedRadiance = sum(unshadowed); result.shadowedRadiance = sum(shadowed);
  result.shadowSlots = fog.stats().shadowCount;
  result.shadowRatio = result.shadowedRadiance / result.unshadowedRadiance;
  result.finiteNonnegative = shadowed.every(value => Number.isFinite(value) && value >= 0);
  const dark = render([{...entry(), color: new THREE.Color(0, 0, 0)}]); result.blackoutRadiance = sum(dark);
  fog.setAmbient({color: new THREE.Color(.5, .6, .7), groundColor: new THREE.Color(.15, .1, .08), intensity: .2});
  const ambient = render([]); result.ambientRadiance = sum(ambient);
  fog.field.clear(0); fog.upload(); result.emptyAmbientRadiance = sum(render([]));
  fog.setAmbient(); fog.field.clear(.3); fog.upload(); scene.remove(blocker);
  light.position.set(0, 3.5, 0); light.target.position.set(0, 0, 0); light.angle = .015;
  camera.position.set(0, 2.4, 4); camera.lookAt(0, 2.4, 0); camera.updateMatrixWorld();
  const narrow = render([{...entry(), shadow: null}]);
  // Odd target dimensions place this column precisely through the beam axis.
  // These rays cross a shaft thinner than one world-ray marching step.
  const column = [];
  for (let row = 30; row <= 65; row++) column.push(narrow[(row * width + (width - 1) / 2) * 4]);
  result.narrowShaftMin = Math.min(...column); result.narrowShaftMax = Math.max(...column);
  result.narrowShaftCoverage = column.filter(value => value > 0).length / column.length;
  // Finite emitting aperture: real optical plane stays y3, virtual apex is
  // above it. A ray above the aperture sees no light; a ray just below spans
  // the lens width even though a punctual cone would be subpixel there.
  const radius = .2, angle = .2, apexDistance = radius / Math.tan(angle);
  const finiteEntry = {...entry(), position: new THREE.Vector3(0, 3, 0),
    virtualPosition: new THREE.Vector3(0, 3 + apexDistance, 0),
    direction: new THREE.Vector3(0, -1, 0), angle, apexDistance, apertureRadius: radius,
    penumbra: .3, shadow: null};
  camera.position.set(0, 3.2, 3); camera.lookAt(0, 3.2, 0); camera.updateMatrixWorld();
  const behindLens = render([finiteEntry]);
  result.behindLensRadiance = behindLens[(49 * width + 49) * 4];
  camera.position.set(0, 2.98, 3); camera.lookAt(0, 2.98, 0); camera.updateMatrixWorld();
  const aperture = render([finiteEntry]);
  result.apertureCoverage = [47, 48, 49, 50, 51].map(x => aperture[(49 * width + x) * 4]);
  result.glError = gl.getError();
  renderer.setRenderTarget(null); renderer.clear();
  scene.remove(blocker); blocker.geometry.dispose(); blocker.material.dispose();
  fog.dispose(); target.dispose(); output.dispose(); light.dispose(); renderer.dispose();
  return result;
};
