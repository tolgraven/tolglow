import * as THREE from 'three';
import {finitePoint, planeAxis} from './editor.js';
import {VolumeOverlay} from './spatial-volumes.js';

/** Graphics-only authoring hints, drawn after fog; never fixture output. */
export class RigOverlay {
  constructor() {
    this.scene = new THREE.Scene(); this.rig = new THREE.Group(); this.volumes = new VolumeOverlay();
    this.scene.add(this.rig, this.volumes.group); this.key = null;
  }
  clear() {this.rig.traverse(o => {o.geometry?.dispose(); o.material?.dispose();}); this.rig.clear();}
  update(snapshot) {
    this.volumes.update(snapshot?.['spatial-volumes']);
    const editor = snapshot?.editor, selection = editor?.selection;
    if (!editor) {if (this.key !== null) {this.clear(); this.key = null;} return;}
    if (this.markerRig !== snapshot.rig) {
      this.markerRig = snapshot.rig;
      this.markers = (snapshot.rig?.fixtures || []).slice(0, 2048).map(f => f.position).filter(finitePoint);
      this.key = null;
    }
    const item = snapshot?.rig?.[selection?.kind]?.find(f => String(f.id) === String(selection.id));
    const markers = this.markers;
    const key = JSON.stringify([editor, item?.position, item?.['rotation-deg'], item?.length]);
    if (key === this.key) return; this.key = key; this.clear();
    const line = (points, color, opacity = 1) => {
      const geometry = new THREE.BufferGeometry().setFromPoints(points.map(p => new THREE.Vector3(...p)));
      this.rig.add(new THREE.Line(geometry, new THREE.LineBasicMaterial({color, transparent: opacity < 1, opacity, depthTest: false, toneMapped: false})));
    };
    if (markers.length) {
      const geometry = new THREE.BufferGeometry().setFromPoints(markers.map(p => new THREE.Vector3(...p)));
      this.rig.add(new THREE.Points(geometry, new THREE.PointsMaterial({color: 0xa2b6c9, size: 6, sizeAttenuation: false, depthTest: false, toneMapped: false})));
    }
    const points = (editor.points || []).filter(finitePoint).slice(0, 256);
    if (points.length > 1) line(editor['closed?'] ? [...points, points[0]] : points, 0xb6c9a9);
    points.forEach((point, index) => {
      const dot = new THREE.Mesh(new THREE.SphereGeometry(index === editor['selected-point'] ? .11 : .075, 10, 6),
        new THREE.MeshBasicMaterial({color: index === editor['selected-point'] ? 0xffc980 : 0xb6c9a9, depthTest: false, toneMapped: false}));
      dot.position.fromArray(point); this.rig.add(dot);
    });
    if (item && finitePoint(item.position)) {
      const box = new THREE.BoxGeometry(selection.kind === 'trusses' ? Math.min(40, item.length || 1) : .7, .9, .65);
      const edges = new THREE.EdgesGeometry(box); box.dispose();
      const outline = new THREE.LineSegments(edges, new THREE.LineBasicMaterial({color: 0xffc980, depthTest: false, toneMapped: false}));
      outline.position.fromArray(item.position); outline.rotation.set(...(item['rotation-deg'] || [0, 0, 0]).map(n => n * Math.PI / 180), 'ZYX');
      this.rig.add(outline);
    }
    if (editor.tool === 'draw') {
      const axis = planeAxis(editor.plane), other = [0, 1, 2].filter(n => n !== axis), value = Number(editor['plane-value']) || 0;
      for (let i = -10; i <= 10; i++) for (let direction = 0; direction < 2; direction++) {
        const a = [0, 0, 0], b = [0, 0, 0]; a[axis] = b[axis] = value;
        a[other[direction]] = b[other[direction]] = i; a[other[1 - direction]] = -10; b[other[1 - direction]] = 10;
        line([a, b], 0xb6c9a9, .14);
      }
    }
  }
  render(renderer, camera) {
    if (!this.rig.children.length && !this.volumes.group.children.length) return;
    // EffectComposer restores the caller's render target after its screen pass.
    // Authoring hints belong on the presented canvas, not the fog composite.
    const auto = renderer.autoClear, target = renderer.getRenderTarget();
    try {renderer.setRenderTarget(null); renderer.autoClear = false; renderer.clearDepth(); renderer.render(this.scene, camera);}
    finally {renderer.autoClear = auto; renderer.setRenderTarget(target);}
  }
  dispose() {this.clear(); this.volumes.dispose();}
}
