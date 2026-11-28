import * as THREE from 'three';
import {finitePoint, planeAxis} from './editor.js';
import {mountAxes} from './placement.js';
import {SculptGuide} from './sculpt.js';
import {VolumeOverlay} from './spatial-volumes.js';
import {VideoSurfaceOverlay} from './video-surfaces.js';

/** Graphics-only authoring hints, drawn after fog; never fixture output. */
export class RigOverlay {
  constructor() {
    this.scene = new THREE.Scene(); this.rig = new THREE.Group(); this.volumes = new VolumeOverlay();
    this.sculpt = new SculptGuide(); this.videoSurfaces = new VideoSurfaceOverlay();
    this.scene.add(this.rig, this.volumes.group, this.sculpt.group, this.videoSurfaces.group); this.key = null;
  }
  clear() {this.rig.traverse(o => {o.geometry?.dispose(); o.material?.dispose();}); this.rig.clear();}
  update(snapshot) {
    this.volumes.update(snapshot?.['spatial-volumes'], snapshot?.['volume-guides']);
    this.sculpt.update(snapshot?.['sculpt-guide']);
    this.videoSurfaces.update(snapshot?.['video-surfaces'], snapshot?.output?.values);
    const editor = snapshot?.editor, selection = editor?.selection;
    if (!editor) {if (this.key !== null) {this.clear(); this.key = null;} return;}
    if (this.markerRig !== snapshot.rig) {
      this.markerRig = snapshot.rig;
      this.markers = (snapshot.rig?.fixtures || []).slice(0, 2048).filter(f => f['pixel-layout-id'] == null).map(f => f.position).filter(finitePoint);
      this.key = null;
    }
    const item = snapshot?.rig?.[selection?.kind]?.find(f => String(f.id) === String(selection.id));
    const markers = this.markers;
    if (this.key === editor && this.item === item) return;
    this.key = editor; this.item = item; this.clear();
    const line = (points, color, opacity = 1) => {
      const geometry = new THREE.BufferGeometry().setFromPoints(points.map(p => new THREE.Vector3(...p)));
      this.rig.add(new THREE.Line(geometry, new THREE.LineBasicMaterial({color, transparent: opacity < 1, opacity, depthTest: false, toneMapped: false})));
    };
    const dots = (positions, color, size) => {
      if (!positions.length) return;
      const geometry = new THREE.BufferGeometry().setFromPoints(positions.map(p => new THREE.Vector3(...p)));
      this.rig.add(new THREE.Points(geometry, new THREE.PointsMaterial({color, size, sizeAttenuation: false, depthTest: false, toneMapped: false})));
    };
    dots(markers, 0xa2b6c9, 6);
    if (editor.tool === 'pixels') {
      dots(editor['pixel-positions'] || [], 0x78e5d2, 5);
      if (editor['pixel-mode'] === 'pixel' && finitePoint(editor['pixel-position'])) dots([editor['pixel-position']], 0xffc980, 12);
    }
    const points = (editor.points || []).filter(finitePoint).slice(0, 256);
    if (points.length > 1) line(editor['closed?'] ? [...points, points[0]] : points, 0xb6c9a9);
    if (editor['pixel-mode'] !== 'pixel') points.forEach((point, index) => {
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
      if (selection.kind === 'fixtures' && ['mount', 'place', 'calibrate'].includes(editor.tool)) {
        // Home beam (pan 0, tilt 0) and mounting axis: the handle the Aim home tool drags.
        const {forward, up} = mountAxes(item['rotation-deg'] || [0, 0, 0]), at = (v, k) => item.position.map((n, i) => n + v[i] * k);
        line([item.position, at(forward, 1.6)], 0xffc980);
        dots([at(forward, 1.6)], 0xffc980, 11);
        line([item.position, at(up, .55)], 0x78b4e5);
      }
      if (selection.kind === 'fixtures' && editor.tool === 'calibrate' && finitePoint(editor['calibration-target'])) {
        line([item.position, editor['calibration-target']], 0x78e5d2, .8);
      }
    }
    if (editor.tool === 'video-surface') this.surfaceHandles(editor, line);
    if (editor.tool === 'calibrate') {
      dots((editor['calibration-points'] || []).filter(finitePoint).slice(0, 64), 0xb6c9a9, 9);
      if (finitePoint(editor['calibration-target'])) dots([editor['calibration-target']], 0x78e5d2, 13);
    }
    if (editor.tool === 'draw' || editor.tool === 'pixels') {
      const axis = planeAxis(editor.plane), other = [0, 1, 2].filter(n => n !== axis), value = Number(editor['plane-value']) || 0;
      for (let i = -10; i <= 10; i++) for (let direction = 0; direction < 2; direction++) {
        const a = [0, 0, 0], b = [0, 0, 0]; a[axis] = b[axis] = value;
        a[other[direction]] = b[other[direction]] = i; a[other[1 - direction]] = -10; b[other[1 - direction]] = 10;
        line([a, b], 0xb6c9a9, .14);
      }
    }
  }
  /** Video surface handles (#674): a disc at each surface's centre, a raise
   * handle above it and a turn handle beside it, joined by guide lines.
   * The selected surface's handles are amber. */
  surfaceHandles(editor, line) {
    const handles = (editor['surface-handles'] || []).filter(h => finitePoint(h.position)).slice(0, 96);
    const centres = new Map(handles.filter(h => h.kind === 'move').map(h => [h.id, h.position]));
    for (const handle of handles) {
      const selected = handle.id === editor['selected-surface'];
      const color = selected ? 0xffc980 : {move: 0xb6c9a9, raise: 0x78b4e5, turn: 0x78e5d2}[handle.kind] ?? 0xb6c9a9;
      const centre = centres.get(handle.id);
      if (centre && handle.kind !== 'move') line([centre, handle.position], color, selected ? .9 : .5);
      const geometry = handle.kind === 'raise' ? new THREE.ConeGeometry(.09, .2, 12)
        : handle.kind === 'turn' ? new THREE.TorusGeometry(.09, .025, 6, 16) : new THREE.SphereGeometry(selected ? .13 : .1, 12, 8);
      const mesh = new THREE.Mesh(geometry, new THREE.MeshBasicMaterial({color, depthTest: false, toneMapped: false}));
      mesh.position.fromArray(handle.position);
      if (handle.kind === 'turn') mesh.rotation.x = Math.PI / 2;
      mesh.userData.surfaceHandle = `${handle.id}/${handle.kind}`;
      this.rig.add(mesh);
    }
  }
  render(renderer, camera) {
    if (!this.rig.children.length && !this.volumes.group.children.length && !this.sculpt.group.children.length &&
        !this.videoSurfaces.group.children.length) return;
    // EffectComposer restores the caller's render target after its screen pass.
    // Authoring hints belong on the presented canvas, not the fog composite.
    const auto = renderer.autoClear, target = renderer.getRenderTarget();
    try {renderer.setRenderTarget(null); renderer.autoClear = false; renderer.clearDepth(); renderer.render(this.scene, camera);}
    finally {renderer.autoClear = auto; renderer.setRenderTarget(target);}
  }
  dispose() {this.clear(); this.volumes.dispose(); this.sculpt.dispose(); this.videoSurfaces.dispose();}
}
