import * as THREE from 'three';

/** Video surfaces (#674): a projection of the show's `:video :surfaces` view.
 * Studio builds each mesh with the same tolglow.video.surface functions the
 * JVM samples with, so the Room draws the geometry the heads read. This file
 * only uploads it: the latched frame as a texture on the mesh, and a marker
 * on every head in contact, brighter with its contact weight. Fixture colours
 * themselves stay the committed output drawn by the scene. */

export function surfaceKey(surface) {
  return `${surface.id}|${surface.columns}x${surface.rows}`;
}

export function contactColors(weights) {
  const colors = new Float32Array(weights.length * 3);
  weights.forEach((w, i) => {
    const k = .35 + .65 * Math.min(1, Math.max(0, Number(w) || 0));
    colors.set([k, k * .86, k * .32], i * 3);
  });
  return colors;
}

function finiteArray(values, size) {
  return Array.isArray(values) && values.length % size === 0 && values.every(Number.isFinite);
}

async function loadBitmap(url) {
  const response = await fetch(url, {cache: 'no-store'});
  if (!response.ok) throw new Error(`Video frame ${response.status}`);
  return createImageBitmap(await response.blob());
}

export class VideoSurfaceOverlay {
  constructor(load = loadBitmap) {
    this.group = new THREE.Group(); this.byId = new Map(); this.textures = new Map(); this.load = load;
  }
  texture(url) {
    if (!url) return null;
    let entry = this.textures.get(url);
    if (!entry) {
      entry = {texture: null, users: 0, failed: false};
      this.textures.set(url, entry);
      this.load(url).then(bitmap => {
        if (this.textures.get(url) !== entry) {bitmap.close?.(); return;}
        const texture = new THREE.Texture(bitmap);
        // Source v runs down the frame; an unflipped bitmap's first row is v = 0.
        texture.flipY = false; texture.colorSpace = THREE.SRGBColorSpace; texture.needsUpdate = true;
        entry.texture = texture;
        for (const obj of this.byId.values()) if (obj.wantUrl === url) this.show(obj, url, texture);
        this.releaseTextures();
      }).catch(() => {entry.failed = true;});
    }
    return entry;
  }
  show(obj, url, texture) {
    obj.shownUrl = url;
    obj.mesh.material.map = texture; obj.mesh.material.color.set(0xffffff); obj.mesh.material.needsUpdate = true;
  }
  releaseTextures() {
    const active = new Set();
    for (const obj of this.byId.values()) {active.add(obj.wantUrl); active.add(obj.shownUrl);}
    for (const [url, entry] of this.textures) if (!active.has(url)) {
      entry.texture?.image?.close?.(); entry.texture?.dispose(); this.textures.delete(url);
    }
  }
  remove(id) {
    const obj = this.byId.get(id);
    for (const part of [obj.mesh, obj.edge, obj.markers]) {part.geometry.dispose(); part.material.dispose();}
    this.group.remove(obj.root); this.byId.delete(id);
  }
  build(surface) {
    const root = new THREE.Group();
    const mesh = new THREE.Mesh(new THREE.BufferGeometry(), new THREE.MeshBasicMaterial({
      color: 0x58636e, side: THREE.DoubleSide, transparent: true, opacity: .82, toneMapped: false, depthWrite: false}));
    const edge = new THREE.LineSegments(new THREE.BufferGeometry(), new THREE.LineBasicMaterial({
      color: 0x8fd9cb, transparent: true, opacity: .7, toneMapped: false}));
    const markers = new THREE.Points(new THREE.BufferGeometry(), new THREE.PointsMaterial({
      size: 9, sizeAttenuation: false, vertexColors: true, depthTest: false, toneMapped: false}));
    root.add(mesh, edge, markers); this.group.add(root);
    return {root, mesh, edge, markers, wantUrl: null, shownUrl: null};
  }
  update(surfaces = []) {
    const seen = new Set();
    for (const surface of surfaces || []) {
      if (!surface || !finiteArray(surface.positions, 3) || !finiteArray(surface.uvs, 2)) continue;
      const id = String(surface.id); seen.add(id);
      let obj = this.byId.get(id);
      if (!obj) {obj = this.build(surface); this.byId.set(id, obj);}
      if (obj.positions !== surface.positions) {
        obj.positions = surface.positions;
        const geometry = obj.mesh.geometry;
        geometry.setAttribute('position', new THREE.Float32BufferAttribute(surface.positions, 3));
        geometry.setAttribute('uv', new THREE.Float32BufferAttribute(surface.uvs, 2));
        geometry.setIndex(surface.indices);
        geometry.computeBoundingSphere();
        obj.edge.geometry.dispose(); obj.edge.geometry = new THREE.EdgesGeometry(geometry, 1);
      }
      if (obj.contacts !== surface.contacts) {
        obj.contacts = surface.contacts;
        const points = (surface.contacts || []).filter(c => finiteArray(c, 4));
        const geometry = obj.markers.geometry;
        geometry.setAttribute('position', new THREE.Float32BufferAttribute(points.flatMap(c => c.slice(0, 3)), 3));
        geometry.setAttribute('color', new THREE.BufferAttribute(contactColors(points.map(c => c[3])), 3));
        geometry.computeBoundingSphere();
      }
      if (obj.wantUrl !== surface.texture) {
        // Keep showing the previous frame until the next one has loaded.
        obj.wantUrl = surface.texture;
        const entry = this.texture(surface.texture);
        if (entry?.texture) this.show(obj, surface.texture, entry.texture);
        else if (!surface.texture) {
          obj.shownUrl = null; obj.mesh.material.map = null;
          obj.mesh.material.color.set(0x58636e); obj.mesh.material.needsUpdate = true;
        }
      }
    }
    for (const id of [...this.byId.keys()]) if (!seen.has(id)) this.remove(id);
    this.releaseTextures();
  }
  dispose() {this.update([]);}
}
