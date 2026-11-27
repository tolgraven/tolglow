import * as THREE from 'three';
import {rotateFixture} from './math.js';

// Placement picking for the rig editor. Receivers come from the show; the
// ceiling is derived from the room. Trusses are picked along their axis and
// fixtures hang below them. Nothing here touches scene objects or the GPU.

const ray = (camera, bounds, x, y) => {
  camera.updateMatrixWorld();
  const picker = new THREE.Raycaster();
  picker.setFromCamera(new THREE.Vector2((x - bounds.left) * 2 / bounds.width - 1, 1 - (y - bounds.top) * 2 / bounds.height), camera);
  return picker.ray;
};

export function ceiling(room) {
  const width = Number(room?.width) || 12, height = Number(room?.height) || 5, depth = Number(room?.depth) || 10;
  return {id: 'ceiling', point: [0, height, 0], normal: [0, -1, 0], 'u-axis': [1, 0, 0], size: [width, depth]};
}

function surfaceHit(r, surface) {
  const center = new THREE.Vector3().fromArray(surface.point);
  const normal = new THREE.Vector3().fromArray(surface.normal).normalize();
  const incidence = normal.dot(r.direction);
  if (incidence >= -1e-8) return null;
  const distance = center.clone().sub(r.origin).dot(normal) / incidence;
  if (!(distance > 0)) return null;
  const point = r.at(distance, new THREE.Vector3()), delta = point.clone().sub(center);
  const u = new THREE.Vector3().fromArray(surface['u-axis']).projectOnPlane(normal).normalize();
  const v = new THREE.Vector3().crossVectors(normal, u);
  if (Math.abs(delta.dot(u)) > surface.size[0] / 2 + 1e-8 || Math.abs(delta.dot(v)) > surface.size[1] / 2 + 1e-8) return null;
  return {distance, point: point.toArray(), normal: normal.toArray(), kind: 'surface', 'surface-id': surface.id};
}

function trussHit(r, truss, tolerance) {
  const half = Math.min(40, Number(truss.length) || 1) / 2;
  const axis = new THREE.Vector3(...rotateFixture([1, 0, 0], truss['rotation-deg']));
  const center = new THREE.Vector3().fromArray(truss.position);
  // Closest approach between the camera ray and the truss segment.
  const w = r.origin.clone().sub(center), b = r.direction.dot(axis), d = r.direction.dot(w), e = axis.dot(w);
  const denominator = 1 - b * b;
  const t = Math.max(-half, Math.min(half, denominator > 1e-9 ? (e - b * d) / denominator : e));
  const onTruss = center.clone().addScaledVector(axis, t);
  const s = r.direction.dot(onTruss.clone().sub(r.origin)), onRay = r.at(Math.max(0, s), new THREE.Vector3());
  const gap = onRay.distanceTo(onTruss);
  if (!(s > 0) || gap > tolerance) return null;
  return {distance: s - tolerance, point: onTruss.toArray(), normal: rotateFixture([0, -1, 0], truss['rotation-deg']),
          kind: 'truss', 'truss-id': String(truss.id)};
}

/** Nearest front-facing mount for a new fixture under the pointer. */
export function pickPlacement(snapshot, camera, bounds, x, y, tolerance = .25) {
  if (!(bounds.width > 0 && bounds.height > 0)) return null;
  const r = ray(camera, bounds, x, y), hits = [];
  for (const surface of [...(snapshot?.['receiver-surfaces'] || []), ceiling(snapshot?.rig?.room)]) {
    const hit = surfaceHit(r, surface); if (hit) hits.push(hit);
  }
  for (const truss of (snapshot?.rig?.trusses || []).slice(0, 24)) {
    if (!Array.isArray(truss.position) || truss.position.length !== 3) continue;
    const hit = trussHit(r, truss, tolerance); if (hit) hits.push(hit);
  }
  const best = hits.sort((a, b) => a.distance - b.distance)[0];
  if (!best) return null;
  const {distance, ...hit} = best;
  return hit;
}

/** Local beam (+Z) and mounting (+Y) axes of a fixture rotation, in the world. */
export const mountAxes = rotation => ({forward: rotateFixture([0, 0, 1], rotation), up: rotateFixture([0, 1, 0], rotation)});
