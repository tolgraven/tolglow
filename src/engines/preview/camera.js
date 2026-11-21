import * as THREE from 'three';
import {OrbitControls} from 'three/addons/controls/OrbitControls.js';
import {clamp} from './math.js';

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
  if (!['ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown', '+', '=', '-', 'Home'].includes(key)) return false;
  if (key === 'Home') {cameraPreset(camera, controls, room, 'audience'); return true;}
  const offset = camera.position.clone().sub(controls.target), sphere = new THREE.Spherical().setFromVector3(offset);
  if (key === 'ArrowLeft') sphere.theta -= .1;
  if (key === 'ArrowRight') sphere.theta += .1;
  if (key === 'ArrowUp') sphere.phi = clamp(sphere.phi - .07, .08, Math.PI * .59);
  if (key === 'ArrowDown') sphere.phi = clamp(sphere.phi + .07, .08, Math.PI * .59);
  if (key === '+' || key === '=') sphere.radius = Math.max(1, sphere.radius * .9);
  if (key === '-') sphere.radius = Math.min(45, sphere.radius * 1.1);
  camera.position.copy(controls.target).add(new THREE.Vector3().setFromSpherical(sphere)); controls.update(); return true;
}
