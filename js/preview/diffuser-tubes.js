import * as THREE from 'three';
import {emittedColor} from './math.js';

function sleeve(pixels, parent) {
  if (pixels.length < 2) return null;
  const last = pixels.length - 1;
  const samples = Math.min(256, last);
  const points = Array.from({length: samples + 1}, (_, i) =>
    new THREE.Vector3(...pixels[Math.round(last * i / samples)].position));
  const path = new THREE.CurvePath();
  for (let i = 1; i < points.length; i++)
    if (!points[i - 1].equals(points[i])) path.add(new THREE.LineCurve3(points[i - 1], points[i]));
  if (!path.curves.length) return null;

  const rings = samples * 2, sides = 12;
  const geometry = new THREE.TubeGeometry(path, rings, pixels[0]['diffuser-radius-m'], sides);
  const color = new THREE.BufferAttribute(new Float32Array(geometry.attributes.position.count * 3), 3);
  color.setUsage(THREE.DynamicDrawUsage);
  geometry.setAttribute('color', color);

  const material = new THREE.MeshStandardMaterial({color: 0x81868b, roughness: .72, vertexColors: true});
  material.onBeforeCompile = shader => {
    // Vertex colours represent emission transmitted through the rough sleeve.
    // Keep its neutral reflection independent from the LED circuit.
    shader.fragmentShader = shader.fragmentShader
      .replace('#include <color_fragment>', '')
      .replace('#include <emissivemap_fragment>', 'totalEmissiveRadiance += 5.0 * vColor.rgb;');
  };
  const mesh = new THREE.Mesh(geometry, material);
  mesh.name = 'diffuser:' + pixels[0]['pixel-layout-id'];
  mesh.receiveShadow = true;
  parent.add(mesh);
  const rows = pixels.map(fixture => ({id: String(fixture.id), fixture}));
  return {
    rows,
    update(output) {
      const colors = rows.map(({id}) => emittedColor(output[id]));
      for (let ring = 0; ring <= rings; ring++) {
        const at = last * ring / rings;
        const lower = Math.floor(at), mix = at - lower;
        // A small three-tap diffusion kernel softens every LED before
        // interpolation across the continuous sleeve.
        const a = colors[Math.max(0, lower - 1)], b = colors[lower];
        const c = colors[Math.min(last, lower + 1)], d = colors[Math.min(last, lower + 2)];
        const washed = b.map((value, axis) =>
          (1 - mix) * (.16 * a[axis] + .68 * value + .16 * c[axis]) +
          mix * (.16 * value + .68 * c[axis] + .16 * d[axis]));
        for (let side = 0; side <= sides; side++) color.setXYZ(ring * (sides + 1) + side, ...washed);
      }
      color.needsUpdate = true;
    },
    dispose() {material.dispose();}
  };
}

export function createDiffuserTubes(fixtures, parent) {
  const groups = new Map();
  for (const pixel of fixtures) {
    const id = pixel['pixel-layout-id'];
    const list = groups.get(id);
    if (list) list.push(pixel);
    else groups.set(id, [pixel]);
  }
  return [...groups.values()].map(group =>
    sleeve(group.sort((a, b) => a['pixel-index'] - b['pixel-index']), parent)).filter(Boolean);
}
