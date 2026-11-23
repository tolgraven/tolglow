// In-browser measurement of the release fog pass. The second shader differs only
// at the two named controls (phase and a fixed world-space scattering region).
import * as THREE from 'three';
import { FOG_FRAGMENT, VolumetricFog } from '../../../js/preview/fog.js';
import { intersectCone, intersectRoom } from '../../../js/preview/fog-field.js';

const room = [16, 8, 16],
  density = 0.08,
  orbitRadius = 4;
const angles = [5, 25, 45, 65, 90, 115, 135, 155, 175];
const renderer = new THREE.WebGLRenderer({ antialias: false });
renderer.toneMapping = THREE.NoToneMapping;
renderer.outputColorSpace = THREE.LinearSRGBColorSpace;
renderer.debug.onShaderError = (gl) => {
  throw new Error('Fog shader failed, GL error ' + gl.getError());
};
const camera = new THREE.PerspectiveCamera(48, 1, 0.05, 80),
  scene = new THREE.Scene();
scene.background = new THREE.Color(0, 0, 0);
const fog = new VolumetricFog(renderer, { quality: 'desktop' });
fog.setRoom(room);
fog.field.clear(density);
fog.upload();
fog.setAmount(1);
const light = {
  position: new THREE.Vector3(0, 4, -3.5),
  virtualPosition: new THREE.Vector3(0, 4, -4),
  apexDistance: 0.5,
  direction: new THREE.Vector3(0, 0, 1),
  color: new THREE.Color(1, 0.32, 0.66),
  angle: 0.1,
  penumbra: 0.45,
  intensity: 120,
  distance: 9,
};
const optical = fog.volumeMaterial;
const probe = optical.clone();
probe.uniforms = {
  ...optical.uniforms,
  retainPhase: { value: 1 },
  regionRadius: { value: 0 },
  regionCenter: { value: new THREE.Vector3() },
};
const phaseTerm = '*bulkPhase*visibility',
  intervalEarlyExit = 'if(hi<=lo)continue;';
if (!FOG_FRAGMENT.includes(phaseTerm) || !FOG_FRAGMENT.includes(intervalEarlyExit))
  throw new Error('Unknown fog shader');
probe.fragmentShader = FOG_FRAGMENT.replace(
  'in vec2 vUv;',
  'in vec2 vUv; uniform float retainPhase,regionRadius; uniform vec3 regionCenter;',
)
  .replace(
    phaseTerm,
    '*mix(bulkPhase, .8556/(12.5663706*pow(1.1444-.76*dot(travel,normalize(eye-position)),1.5)),retainPhase)*visibility',
  )
  .replace(
    intervalEarlyExit,
    `if(regionRadius>0.){
     vec3 oc=eye-regionCenter;float b=dot(oc,ray),disc=b*b-dot(oc,oc)+regionRadius*regionRadius;
     if(disc<=0.)continue;float root=sqrt(disc);lo=max(lo,-b-root);hi=min(hi,-b+root);
   }${intervalEarlyExit}`,
  );
const target = new THREE.WebGLRenderTarget(1, 1, { type: THREE.HalfFloatType });
target.depthTexture = new THREE.DepthTexture(1, 1, THREE.UnsignedIntType);
const output = new THREE.WebGLRenderTarget(1, 1, { type: THREE.HalfFloatType, depthBuffer: false });
function size(width, height) {
  renderer.setSize(width, height);
  fog.setSize(width, height);
  target.setSize(width, height);
  output.setSize(width, height);
  camera.aspect = width / height;
  camera.updateProjectionMatrix();
}
function pixels(target, center = true) {
  const w = center ? 1 : target.width,
    h = center ? 1 : target.height,
    bytes = new Uint16Array(w * h * 4);
  renderer.readRenderTargetPixels(
    target,
    center ? Math.floor(target.width / 2) : 0,
    center ? Math.floor(target.height / 2) : 0,
    w,
    h,
    bytes,
  );
  return Float32Array.from(bytes, THREE.DataUtils.fromHalfFloat);
}
function move(angle, offset) {
  const a = (angle * Math.PI) / 180,
    pivot = new THREE.Vector3(0, 4 + offset, 0);
  camera.position.set(orbitRadius * Math.sin(a), pivot.y, orbitRadius * Math.cos(a));
  camera.lookAt(pivot);
  camera.updateMatrixWorld();
  probe.uniforms.regionCenter.value.copy(pivot);
  const origin = camera.position.toArray(),
    ray = pivot.clone().sub(camera.position).normalize().toArray();
  const bounds = intersectRoom(origin, ray, room);
  const cone = intersectCone(
    origin,
    ray,
    light.virtualPosition.toArray(),
    light.direction.toArray(),
    light.angle,
    bounds,
    light.distance,
    light.apexDistance,
  );
  return { roomPath: bounds[1] - bounds[0], corePath: cone ? cone[1] - cone[0] : 0 };
}
function draw({ mode = 'full', radius = 0, phase = 1, lights = [light] } = {}) {
  fog.volumeMaterial = mode === 'full' ? optical : probe;
  probe.uniforms.regionRadius.value = radius;
  probe.uniforms.retainPhase.value = phase;
  fog.update(0, lights);
  renderer.setRenderTarget(target);
  renderer.render(scene, camera);
  fog.render(target, camera, output);
  return { raw: Array.from(pixels(fog.target)), composited: Array.from(pixels(output)) };
}
function capture() {
  const rgb = pixels(output, false),
    image = new ImageData(output.width, output.height);
  for (let y = 0; y < output.height; y++)
    for (let x = 0; x < output.width; x++) {
      const i = 4 * (y * output.width + x),
        s = 4 * ((output.height - y - 1) * output.width + x);
      for (let c = 0; c < 3; c++) {
        // Fixed exposure and sRGB display transfer, identical in every photograph.
        const value = Math.max(0, 2 * rgb[s + c]),
          mapped = value / (1 + value);
        image.data[i + c] =
          255 * (mapped <= 0.0031308 ? 12.92 * mapped : 1.055 * mapped ** (1 / 2.4) - 0.055);
      }
      image.data[i + 3] = 255;
    }
  const canvas = document.createElement('canvas');
  canvas.width = output.width;
  canvas.height = output.height;
  canvas.getContext('2d').putImageData(image, 0, 0);
  return canvas.toDataURL('image/png').split(',')[1];
}
window.measureFogOrbit = ({ width, height, offset }) => {
  size(width, height);
  const samples = [];
  for (const angle of angles) {
    const path = move(angle, offset),
      full = draw(),
      picture = width > 100 && offset === 0 ? capture() : null;
    const repeated = angle === 90 ? draw().raw[0] : null;
    const historical = draw({ mode: 'probe', phase: 1 });
    const fixed = draw({ mode: 'probe', radius: 0.125, phase: 0 });
    const fixedHistorical = draw({ mode: 'probe', radius: 0.125, phase: 1 });
    samples.push({
      angle,
      ...path,
      raw: full.raw[0],
      repeated,
      composited: full.composited[0],
      transmittance: full.raw[3],
      historical: historical.raw[0],
      fixed: fixed.raw[0],
      fixedHistorical: fixedHistorical.raw[0],
      picture,
    });
  }
  return {
    width,
    height,
    volume: [fog.width, fog.height],
    offset,
    density,
    extinction: fog.extinctionPerM,
    samples,
  };
};
function pixel(target, x, y) {
  const bytes = new Uint16Array(4);
  renderer.readRenderTargetPixels(target, x, y, 1, 1, bytes);
  return THREE.DataUtils.fromHalfFloat(bytes[0]);
}
function setVolumeSize(w, h) {
  fog.width = w;
  fog.height = h;
  fog.target.setSize(w, h);
  fog.compositeUniforms.volumeSize.value.set(w, h);
}
window.measureFogSilhouette = () => {
  const width = 323,
    height = 183;
  size(width, height);
  move(90, 0);
  fog.setAmbient({ color: new THREE.Color(0.3, 0.3, 0.3), intensity: 0.6 });
  const x = Math.floor(width / 2) + 1,
    y = Math.floor(height / 2),
    surfaceDistance = 3;
  const metersPerPixel = (2 * surfaceDistance * Math.tan(THREE.MathUtils.degToRad(camera.fov / 2))) / height;
  const edge = new THREE.Mesh(
    new THREE.PlaneGeometry(0.85 * metersPerPixel, 1.2),
    new THREE.MeshBasicMaterial({ color: 0 }),
  );
  const viewPosition = new THREE.Vector3(metersPerPixel, 0, -surfaceDistance);
  edge.position.copy(camera.localToWorld(viewPosition));
  edge.quaternion.copy(camera.quaternion);
  scene.add(edge);
  draw({ lights: [] });
  const half = pixel(output, x, y);
  setVolumeSize(width, height);
  draw({ lights: [] });
  const native = pixel(output, x, y);
  scene.remove(edge);
  edge.geometry.dispose();
  edge.material.dispose();
  fog.setAmbient();
  return { half, native, surfaceDistance, metersPerPixel };
};
window.measureSubpixelShaft = () => {
  const width = 31,
    height = 19,
    y = Math.floor(height / 2) + 1,
    x = Math.floor(width / 2);
  size(width, height);
  move(90, 0);
  const metersPerPixel = (2 * orbitRadius * Math.tan(THREE.MathUtils.degToRad(camera.fov / 2))) / height;
  const values = [];
  const angle = light.angle;
  light.angle = 0.025;
  for (const fraction of [0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2]) {
    light.position.y = 4 + fraction * metersPerPixel;
    light.virtualPosition.y = light.position.y;
    setVolumeSize(Math.floor(width / 2), Math.floor(height / 2));
    draw();
    const half = pixel(output, x, y);
    setVolumeSize(width, height);
    draw();
    const native = pixel(output, x, y);
    values.push({ fraction, half, native });
  }
  light.position.y = 4;
  light.virtualPosition.y = 4;
  light.angle = angle;
  return { width, height, angle: 0.025, metersPerPixel, values };
};
window.closeFogOrbit = () => {
  fog.volumeMaterial = optical;
  probe.dispose();
  fog.dispose();
  target.dispose();
  output.dispose();
  renderer.dispose();
};
