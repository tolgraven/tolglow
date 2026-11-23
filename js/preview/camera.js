import * as THREE from 'three';
import {OrbitControls} from 'three/addons/controls/OrbitControls.js';

export function cameraControls(camera, canvas) {
  const controls = new OrbitControls(camera, canvas);
  controls.enableDamping = true; controls.dampingFactor = .1;
  controls.minDistance = 1; controls.maxDistance = 45; controls.maxPolarAngle = Math.PI * .6;
  controls.enablePan = true; controls.screenSpacePanning = true;
  controls.touches.ONE = THREE.TOUCH.ROTATE; controls.touches.TWO = THREE.TOUCH.DOLLY_PAN;
  return controls;
}
export function cameraPreset(camera, controls, room, name) {
  const [w, h, d] = room;
  if (name === 'overview') {camera.position.set(w * .58, h * 1.25, d * .77); controls.target.set(0, h * .3, -d * .08);}
  else if (name === 'audience') {camera.position.set(w * .09, h * .48, d * .76); controls.target.set(0, h * .38, -d * .28);}
  else throw new Error('Unknown preview camera');
  controls.update();
}
export function cameraKey(camera, controls, room, key) {
  if (key === 'Home') {cameraPreset(camera, controls, room, 'audience'); return true;}
  if (!['+', '=', '-'].includes(key)) return false;
  controls.dollyIn(key === '-' ? 1.1 : .9);
  return true;
}
