import * as THREE from 'three';
import {box, tube} from './geometry.js';

export function receiverMaterial(wall, reading) {
  if (reading?.method !== 'bracketed-neutral-card' || !Number.isFinite(reading.estimate) || reading.estimate < 0) return wall;
  const material = wall.clone();
  // Camera measured neutral diffuse luminance. A colored wall texture would tint it a second time.
  material.map = null;
  material.metalness = 0;
  material.roughness = 1;
  material.color.setScalar(Math.min(1, reading.estimate));
  material.userData.meanMapReflectance = 1;
  material.onBeforeCompile = wall.onBeforeCompile;
  material.customProgramCacheKey = wall.customProgramCacheKey;
  return material;
}

export function createRoom(dimensions, materials, surfaces = []) {
  const [width, height, depth] = dimensions, root = new THREE.Group(); root.name = 'venue-context';
  const floor = new THREE.Mesh(new THREE.PlaneGeometry(width, depth), materials.floor);
  floor.userData.receiver = 'floor'; floor.rotation.x = -Math.PI / 2; floor.receiveShadow = true; root.add(floor);
  box(root, [width + .2, .16, depth + .2], [0, -.09, 0], materials.blackMetal);
  // Audience side is an explicit cutaway. Side/back surfaces remain real depth
  // occluders; the camera does not peer through arbitrary invisible walls.
  box(root, [width, height, .18], [0, height / 2, -depth / 2 - .09], materials.wall).userData.receiver = 'back';
  for (const x of [-width / 2, width / 2]) {
    box(root, [.18, height, depth], [x + Math.sign(x) * .09, height / 2, 0], materials.wall).userData.receiver = x < 0 ? 'left' : 'right';
    for (const z of [-depth * .4, depth * .12, depth * .4]) {
      box(root, [.2, height, .2], [x - Math.sign(x) * .13, height / 2, z], materials.blackMetal, .008);
      box(root, [.36, .08, .36], [x - Math.sign(x) * .13, .04, z], materials.metal, .008);
    }
    tube(root, [x - Math.sign(x) * .22, height - .25, -depth / 2], [x - Math.sign(x) * .22, height - .25, depth / 2], .075, materials.blackMetal, 12);
  }
  // A low modular stage, seams, front edge and a pair of access steps.
  const stageWidth = width * .68, stageDepth = Math.min(2.3, depth * .27), stageZ = -depth / 2 + stageDepth / 2 + .16;
  box(root, [stageWidth, .42, stageDepth], [0, .21, stageZ], materials.stage, .025).userData.receiver = 'stage';
  box(root, [stageWidth, .025, .045], [0, .435, stageZ + stageDepth / 2 - .02], materials.metal, .008);
  for (let x = -stageWidth / 2 + 1; x < stageWidth / 2; x += Math.max(1.2, stageWidth / 64))
    box(root, [.008, .003, stageDepth - .08], [x, .423, stageZ], materials.rubber);
  for (let step = 0; step < 2; step++) box(root, [.85, .14 * (step + 1), .34], [stageWidth / 2 + .35, .07 * (step + 1), stageZ + stageDepth / 2 - step * .34], materials.stage, .015);
  // Pleated dark curtain: actual displaced surface, lit and shadowed by fixtures.
  const curtainWidth = width * .7, curtainHeight = height * .72;
  const curtainGeometry = new THREE.PlaneGeometry(curtainWidth, curtainHeight, 120, 12);
  const positions = curtainGeometry.attributes.position;
  for (let i = 0; i < positions.count; i++) {
    const x = positions.getX(i), y = positions.getY(i);
    positions.setZ(i, .08 * Math.sin(x * 19) + .018 * Math.sin(x * 37 + y * .5));
  }
  curtainGeometry.computeVertexNormals();
  const curtain = new THREE.Mesh(curtainGeometry, materials.curtain);
  curtain.position.set(0, .45 + curtainHeight / 2, -depth / 2 + .15); curtain.castShadow = curtain.receiveShadow = true; root.add(curtain);
  // Passive speaker stacks provide metre-scale context; no sound/light state.
  for (const side of [-1, 1]) {
    const x = side * width * .395, z = -depth * .31;
    for (let unit = 0; unit < 2; unit++) {
      box(root, [.68, .86, .58], [x, .43 + unit * .88, z], materials.housing, .035);
      box(root, [.59, .72, .014], [x, .43 + unit * .88, z + .3], materials.grille, .01);
      for (const dx of [-.25, .25]) box(root, [.02, .07, .018], [x + dx, .18 + unit * .88, z + .315], materials.metal);
    }
    box(root, [.85, .5, .8], [x, .25, z + 1], materials.rubber, .035);
    box(root, [.72, .37, .015], [x, .26, z + 1.41], materials.grille, .012);
  }
  // Ceiling beams and cable conduits keep the cutaway architectural, not a cube.
  for (const z of [-depth * .38, depth * .05, depth * .4]) {
    box(root, [width, .17, .1], [0, height - .12, z], materials.blackMetal);
    tube(root, [-width / 2, height - .3, z + .14], [width / 2, height - .3, z + .14], .025, materials.metal);
  }
  const cable = new THREE.CatmullRomCurve3([
    new THREE.Vector3(-width * .4, .025, -depth * .1), new THREE.Vector3(-width * .39, .025, depth * .2), new THREE.Vector3(-width * .28, .025, depth * .35), new THREE.Vector3(-width * .1, .025, depth * .38)]);
  const cableMesh = new THREE.Mesh(new THREE.TubeGeometry(cable, 40, .012, 5, false), materials.rubber); root.add(cableMesh);
  for (const surface of surfaces) {
    const {id, point, normal, size} = surface;
    if (!Array.isArray(size) || !size.every(value => Number.isFinite(value) && value > 0)) continue;
    const n = new THREE.Vector3(...normal).normalize(), u = new THREE.Vector3(...surface['u-axis']);
    u.addScaledVector(n, -u.dot(n)).normalize();
    const material = receiverMaterial(materials.wall, surface.reflectance);
    const mesh = new THREE.Mesh(new THREE.PlaneGeometry(...size), material);
    if (material !== materials.wall) mesh.userData.ownsReceiverMaterial = true;
    mesh.position.fromArray(point);
    mesh.quaternion.setFromRotationMatrix(new THREE.Matrix4().makeBasis(u, n.clone().cross(u), n));
    mesh.userData.receiver = String(id); mesh.receiveShadow = mesh.castShadow = true;
    root.add(mesh);
  }
  return root;
}
