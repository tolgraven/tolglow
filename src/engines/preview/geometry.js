import * as THREE from 'three';
import {apertureRadius} from './optics.js';
import {RoundedBoxGeometry} from 'three/addons/geometries/RoundedBoxGeometry.js';

export function box(parent, dimensions, position, material, radius = 0) {
  const geometry = radius ? new RoundedBoxGeometry(...dimensions, 2, radius) : new THREE.BoxGeometry(...dimensions);
  const mesh = new THREE.Mesh(geometry, material); mesh.position.set(...position); mesh.castShadow = mesh.receiveShadow = true; parent.add(mesh); return mesh;
}
export function tube(parent, a, b, radius, material, segments = 8) {
  const delta = new THREE.Vector3().subVectors(new THREE.Vector3(...b), new THREE.Vector3(...a));
  const mesh = new THREE.Mesh(new THREE.CylinderGeometry(radius, radius, delta.length(), segments), material);
  mesh.position.set(...a).addScaledVector(delta, .5); mesh.quaternion.setFromUnitVectors(new THREE.Vector3(0, 1, 0), delta.normalize());
  mesh.castShadow = mesh.receiveShadow = true; parent.add(mesh); return mesh;
}
export function createTruss(spec, materials) {
  const group = new THREE.Group(); group.name = `truss:${spec.id}`; group.position.fromArray(spec.position);
  const rotation = (spec['rotation-deg'] || [0, 0, 0]).map(v => v * Math.PI / 180); group.rotation.set(...rotation, 'ZYX');
  const length = Math.min(40, Math.max(.3, spec.length)), half = length / 2, r = .17;
  // One instanced tube mesh: four chords and cross-braces on all four faces.
  const members = [];
  for (const y of [-r, r]) for (const z of [-r, r]) members.push([[-half, y, z], [half, y, z], .025]);
  const count = Math.ceil(length / .55), step = length / count;
  for (let i = 0; i < count; i++) {
    const a = -half + i * step, b = a + step, flip = i % 2 ? -1 : 1;
    for (const side of [-r, r]) {
      members.push([[a, -r * flip, side], [b, r * flip, side], .012]);
      members.push([[a, side, -r * flip], [b, side, r * flip], .012]);
    }
  }
  for (let i = 0; i <= count; i++) {
    const x = -half + i * step;
    members.push([[x, -r, -r], [x, r, -r], .012], [[x, -r, r], [x, r, r], .012], [[x, -r, -r], [x, -r, r], .012], [[x, r, -r], [x, r, r], .012]);
  }
  const mesh = new THREE.InstancedMesh(new THREE.CylinderGeometry(1, 1, 1, 8), materials.metal, members.length);
  const dummy = new THREE.Object3D(), direction = new THREE.Vector3(), up = new THREE.Vector3(0, 1, 0);
  members.forEach(([a, b, radius], i) => {
    direction.set(b[0] - a[0], b[1] - a[1], b[2] - a[2]); const length = direction.length();
    dummy.position.set((a[0] + b[0]) / 2, (a[1] + b[1]) / 2, (a[2] + b[2]) / 2);
    dummy.quaternion.setFromUnitVectors(up, direction.normalize()); dummy.scale.set(radius, length, radius); dummy.updateMatrix(); mesh.setMatrixAt(i, dummy.matrix);
  });
  mesh.castShadow = mesh.receiveShadow = true; group.add(mesh); return group;
}
export function createMovingHead(fixture, materials) {
  const radius = apertureRadius(fixture);
  const root = new THREE.Group(); root.name = `fixture:${fixture.id}`; root.position.fromArray(fixture.position || [0, 0, 0]);
  // Fixture origin is the optical origin, unchanged by decorative geometry.
  const base = box(root, [.52, .15, .36], [0, .62, 0], materials.housing, .025);
  box(root, [.44, .035, .27], [0, .71, 0], materials.blackMetal, .012);
  for (const x of [-.17, .17]) box(root, [.055, .05, .11], [x, .73, 0], materials.rubber, .008);
  const yoke = new THREE.Group(); root.add(yoke);
  for (const x of [-.22, .22]) {
    box(yoke, [.065, .36, .11], [x, .39, 0], materials.housing, .02);
    const bolt = new THREE.Mesh(new THREE.CylinderGeometry(.055, .055, .085, 16), materials.metal);
    bolt.rotation.z = Math.PI / 2; bolt.position.set(x, .23, 0); yoke.add(bolt);
  }
  const head = new THREE.Group(); root.add(head);
  // Lens at optical origin; local -Y is the head's optical axis.
  box(head, [.34, .36, .3], [0, .24, 0], materials.housing, .055);
  const barrel = new THREE.Mesh(new THREE.CylinderGeometry(radius * 1.22, radius * 1.01, .16, 32), materials.blackMetal);
  barrel.position.y = .075; barrel.castShadow = barrel.receiveShadow = true; head.add(barrel);
  const lensMaterial = new THREE.MeshPhysicalMaterial({color: 0x20313a, metalness: .35, roughness: .1, clearcoat: 1, clearcoatRoughness: .04, emissive: new THREE.Color(0, 0, 0), emissiveIntensity: 1});
  const lens = new THREE.Mesh(new THREE.CircleGeometry(radius, 40), lensMaterial); lens.rotation.x = Math.PI / 2; lens.position.y = -.007; head.add(lens);
  const ring = new THREE.Mesh(new THREE.TorusGeometry(radius + .014, .012, 8, 40), materials.metal); ring.rotation.x = Math.PI / 2; ring.position.y = -.002; head.add(ring);
  for (let n = 0; n < 6; n++) box(head, [.27, .012, .012], [0, .17 + n * .034, .156], materials.rubber);
  // Small manufacturer display is non-emissive: no invented output light.
  box(base, [.12, .035, .004], [0, -.015, .182], materials.rubber, .003);
  return {root, head, yoke, lensMaterial, dispose() { lensMaterial.dispose(); }};
}
export function disposeGeometry(root) {
  const geometries = new Set(); root.traverse(object => { if (object.geometry) geometries.add(object.geometry); });
  for (const geometry of geometries) geometry.dispose();
}
