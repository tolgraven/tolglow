import * as THREE from 'three';

const canvasFor = (width, height) => {
  if (typeof document === 'undefined') return new OffscreenCanvas(width, height);
  const canvas = document.createElement('canvas'); canvas.width = width; canvas.height = height; return canvas;
};

function noise(x, y) {
  const n = Math.sin(x * 127.1 + y * 311.7) * 43758.5453123;
  return n - Math.floor(n);
}
function surfaceTexture(size, kind) {
  const canvas = canvasFor(size, size);
  const ctx = canvas.getContext('2d'), image = ctx.createImageData(size, size);
  for (let y = 0; y < size; y++) for (let x = 0; x < size; x++) {
    const n = noise(x, y), broad = noise(Math.floor(x / 24), Math.floor(y / 24));
    let value = kind === 'floor' ? 108 + n * 28 + broad * 10 : 139 + n * 28 + broad * 10;
    if (kind === 'floor' && (x < 2 || y < 2)) value *= .62;
    const i = (y * size + x) * 4;
    image.data[i] = value; image.data[i + 1] = value * .98; image.data[i + 2] = value * .94; image.data[i + 3] = 255;
  }
  ctx.putImageData(image, 0, 0);
  // Irregular wear marks are deterministic surface variation, never data marks.
  ctx.strokeStyle = 'rgba(26,26,25,.12)'; ctx.lineWidth = 1;
  for (let n = 0; n < 40; n++) { const x = noise(n, 6) * size, y = noise(n, 8) * size; ctx.beginPath(); ctx.moveTo(x, y); ctx.lineTo(x + noise(n, 2) * 80, y + noise(n, 1) * 5); ctx.stroke(); }
  const map = new THREE.CanvasTexture(canvas); map.wrapS = map.wrapT = THREE.RepeatWrapping; map.colorSpace = THREE.SRGBColorSpace; map.anisotropy = 4;
  const bump = map.clone(); bump.colorSpace = THREE.NoColorSpace;
  return {map, bump};
}
function grille() {
  const canvas = canvasFor(64, 64);
  const c = canvas.getContext('2d'); c.fillStyle = '#353a3c'; c.fillRect(0, 0, 64, 64);
  c.fillStyle = '#080a0b';
  for (let y = 0; y < 64; y += 8) for (let x = 0; x < 64; x += 8) { c.beginPath(); c.arc(x + (y % 16 ? 4 : 0), y, 2.5, 0, Math.PI * 2); c.fill(); }
  const map = new THREE.CanvasTexture(canvas); map.wrapS = map.wrapT = THREE.RepeatWrapping; map.repeat.set(3, 5); map.colorSpace = THREE.SRGBColorSpace;
  return map;
}
export function createMaterials() {
  const floor = surfaceTexture(512, 'floor'), wall = surfaceTexture(512, 'wall');
  floor.map.repeat.set(5, 4); floor.bump.repeat.copy(floor.map.repeat);
  wall.map.repeat.set(3, 2); wall.bump.repeat.copy(wall.map.repeat);
  const materials = {
    floor: new THREE.MeshStandardMaterial({color: 0xc5c1b8, map: floor.map, bumpMap: floor.bump, bumpScale: .028, roughness: .42, metalness: .12}),
    wall: new THREE.MeshStandardMaterial({color: 0xb6b0a4, map: wall.map, bumpMap: wall.bump, bumpScale: .035, roughness: .91, metalness: .02}),
    metal: new THREE.MeshStandardMaterial({color: 0x8b9299, roughness: .28, metalness: .88}),
    blackMetal: new THREE.MeshStandardMaterial({color: 0x23272a, roughness: .34, metalness: .75}),
    housing: new THREE.MeshStandardMaterial({color: 0x1b1e22, roughness: .43, metalness: .32}),
    rubber: new THREE.MeshStandardMaterial({color: 0x111315, roughness: .85, metalness: 0}),
    curtain: new THREE.MeshStandardMaterial({color: 0x282b2f, roughness: 1, metalness: 0, side: THREE.DoubleSide}),
    stage: new THREE.MeshStandardMaterial({color: 0x202226, roughness: .73, metalness: .14}),
    grille: new THREE.MeshStandardMaterial({color: 0x929697, map: grille(), roughness: .7, metalness: .55}),
    yellow: new THREE.MeshStandardMaterial({color: 0x9b793d, roughness: .65, metalness: .05}),
  };
  // Mean linear albedo of the procedural sRGB concrete maps.
  materials.floor.userData.meanMapReflectance = .22;
  materials.wall.userData.meanMapReflectance = .34;
  return materials;
}
export function disposeMaterials(materials) {
  const textures = new Set();
  for (const material of Object.values(materials)) {
    for (const value of Object.values(material)) if (value?.isTexture) textures.add(value);
    material.dispose();
  }
  for (const texture of textures) texture.dispose();
}
