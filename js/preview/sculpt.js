import * as THREE from 'three';

const axisColors = [0xeb796e, 0x8cdb99, 0x8daef2];

export function sculptHandles(guide) {
  if (!guide?.center) return [];
  const handles = [{position: guide.center, kind: 'centre'}];
  for (let axis = 0; axis < 3; axis++) {
    const extent = (guide.max[axis] - guide.min[axis]) / 2;
    const offset = Math.max(0.32, extent);
    const position = [...guide.center];
    position[axis] += offset;
    handles.push({position, kind: 'stretch', axis, extent});
  }
  return handles;
}

export class SculptGuide {
  constructor() {this.group = new THREE.Group(); this.key = null;}
  clear() {
    for (const object of this.group.children) {
      object.geometry.dispose(); object.material.dispose();
    }
    this.group.clear();
  }
  update(guide) {
    if (!guide) {if (this.group.children.length) this.clear(); this.key = null; return;}
    if (guide.key === this.key) return;
    this.key = guide.key; this.clear();
    const line = (points, color, opacity = 1) => {
      if (!points?.length) return;
      const vertices = new Float32Array(points.length * 3);
      points.forEach((p, i) => vertices.set(p, i * 3));
      const geometry = new THREE.BufferGeometry();
      geometry.setAttribute('position', new THREE.BufferAttribute(vertices, 3));
      const material = new THREE.LineBasicMaterial({color, opacity, transparent: opacity < 1, depthTest: false, toneMapped: false});
      this.group.add(new THREE.LineSegments(geometry, material));
    };
    const color = new THREE.Color().fromArray(guide.color || [0.4, 0.9, 0.8]);
    line(guide.outer, color, 0.28);
    line(guide.surface, color, 0.9);
    if (guide['handles?'] === false) return;
    for (const {position, kind, axis} of sculptHandles(guide)) {
      const centre = kind === 'centre', tint = centre ? 0xffdbb0 : axisColors[axis];
      if (!centre) line([guide.center, position], tint);
      const shape = centre ? new THREE.SphereGeometry(.11, 12, 8) : new THREE.BoxGeometry(.13, .13, .13);
      const object = new THREE.Mesh(shape, new THREE.MeshBasicMaterial({color: tint, depthTest: false, toneMapped: false}));
      object.position.fromArray(position); this.group.add(object);
    }
  }
  dispose() {this.clear(); this.key = null;}
}
