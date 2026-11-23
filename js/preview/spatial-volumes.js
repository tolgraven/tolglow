import * as THREE from 'three';
import {SculptGuide} from './sculpt.js';
import {finitePoint} from './editor.js';

function centre(spec) {
  if (finitePoint(spec.signal?.position)) return spec.signal.position;
  if (finitePoint(spec.position)) return spec.position;
  if (spec.shape === 'box' && finitePoint(spec.min) && finitePoint(spec.max))
    return spec.min.map((n, i) => (n + spec.max[i]) / 2);
  return null;
}
export function volumePose(spec) {
  const position = centre(spec), radius = spec.signal?.radius ?? spec.radius;
  if (!position) return null;
  if (spec.shape === 'box' && finitePoint(spec.min) && finitePoint(spec.max))
    return {position, scale: spec.max.map((n, i) => Math.max(0, n - spec.min[i]))};
  if (!['ball', 'sphere', 'cylinder'].includes(spec.shape) || !Number.isFinite(radius) || radius <= 0) return null;
  if (spec.shape === 'cylinder') return {position, scale: [radius, spec.height, radius]};
  const compression = Math.min(.5, Math.max(0, spec.signal?.deformation || 0));
  const expansion = 1 / Math.sqrt(1 - compression);
  return {position, scale: [radius * expansion, radius * (1 - compression), radius * expansion]};
}
function wireGeometry(shape) {
  if (shape === 'box' || shape === 'cylinder') {
    const shell = shape === 'box' ? new THREE.BoxGeometry(1, 1, 1) : new THREE.CylinderGeometry(1, 1, 1, 40);
    const lines = new THREE.EdgesGeometry(shell); shell.dispose(); return lines;
  }
  const points = [];
  for (let axis = 0; axis < 3; axis++) {
    for (let i = 0; i < 48; i++) {
      for (const t of [i, i + 1]) {
        const p = [0, 0, 0];
        p[(axis + 1) % 3] = Math.cos(t * Math.PI / 24); p[(axis + 2) % 3] = Math.sin(t * Math.PI / 24);
        points.push(new THREE.Vector3(...p));
      }
    }
  }
  return new THREE.BufferGeometry().setFromPoints(points);
}

/** Separate guides from the optical pass. A moving ball changes transforms only. */
export class VolumeOverlay {
  constructor() { this.group = new THREE.Group(); this.byId = new Map(); this.fields = new Map(); }
  remove(id) {
    const obj = this.byId.get(id);
    obj.geometry.dispose(); obj.material.dispose(); this.group.remove(obj); this.byId.delete(id);
  }
  update(volumes = [], guides) {
    const seen = new Set(), shaped = new Set();
    for (const spec of volumes) {
      const guide = guides?.[spec.id];
      if (guide) {
        let field = this.fields.get(spec.id);
        if (!field) {field = new SculptGuide(); this.fields.set(spec.id, field); this.group.add(field.group);}
        field.update(guide);
        if (guide['moving?']) field.group.position.fromArray(spec.signal?.position || spec.position);
        shaped.add(spec.id); continue;
      }
      const pose = volumePose(spec); if (!pose) continue;
      seen.add(spec.id);
      let obj = this.byId.get(spec.id);
      if (obj && obj.userData.shape !== spec.shape) {this.remove(spec.id); obj = null;}
      if (!obj) {
        obj = new THREE.LineSegments(wireGeometry(spec.shape), new THREE.LineBasicMaterial({
          color: spec.shape === 'ball' ? 0xffbd78 : 0x8fd9cb, transparent: true, opacity: .84, depthTest: false, toneMapped: false
        }));
        obj.userData.shape = spec.shape; this.group.add(obj); this.byId.set(spec.id, obj);
      }
      obj.position.fromArray(pose.position); obj.scale.fromArray(pose.scale);
      if (finitePoint(spec.signal?.rotation)) obj.rotation.set(...spec.signal.rotation);
    }
    for (const id of this.byId.keys()) if (!seen.has(id)) this.remove(id);
    for (const [id, field] of this.fields) if (!shaped.has(id)) {
      field.dispose(); this.group.remove(field.group); this.fields.delete(id);
    }
  }
  dispose() {this.update([]);}
}
