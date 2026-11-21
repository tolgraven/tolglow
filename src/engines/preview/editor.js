import * as THREE from 'three';
import {rotateFixture} from './math.js';

export const planeAxis = plane => ({xz: 1, xy: 2, yz: 0}[plane] ?? 1);
export const editorIdentity = snapshot => JSON.stringify([snapshot?.['runtime-id'], snapshot?.['show-id'], snapshot?.epoch]);
export const finitePoint = p => Array.isArray(p) && p.length === 3 && p.every(n => Number.isFinite(n) && Math.abs(n) <= 10000);
export function planePoint(camera, bounds, x, y, plane = 'xz', value = 0, snap = 0) {
  if (!(bounds.width > 0 && bounds.height > 0) || !Number.isFinite(value)) return null;
  camera.updateMatrixWorld();
  const ray = new THREE.Raycaster(); ray.setFromCamera(new THREE.Vector2(2 * (x - bounds.left) / bounds.width - 1, 1 - 2 * (y - bounds.top) / bounds.height), camera);
  const axis = planeAxis(plane), normal = new THREE.Vector3().setComponent(axis, 1), point = new THREE.Vector3();
  if (!ray.ray.intersectPlane(new THREE.Plane(normal, -value), point)) return null;
  const result = point.toArray().map((n, i) => i === axis ? value : snap > 0 ? Math.round(n / snap) * snap : n);
  return finitePoint(result) ? result : null;
}
export function projectPoint(camera, bounds, point) {
  if (!finitePoint(point)) return null;
  const projected = new THREE.Vector3(...point).project(camera);
  return projected.z >= -1 && projected.z <= 1 ? {x: bounds.left + (projected.x + 1) * bounds.width / 2,
    y: bounds.top + (1 - projected.y) * bounds.height / 2, depth: projected.z} : null;
}
const segmentDistance = (x, y, a, b) => {
  const dx = b.x - a.x, dy = b.y - a.y, t = Math.max(0, Math.min(1, ((x - a.x) * dx + (y - a.y) * dy) / (dx * dx + dy * dy || 1)));
  return Math.hypot(x - a.x - t * dx, y - a.y - t * dy);
};
/** Screen-space proxies are bounded independently of geometry and GPU completion. */
export function pickEditor(snapshot, camera, bounds, x, y) {
  camera.updateMatrixWorld();
  const editor = snapshot?.editor || {}, candidates = [];
  const add = (point, selection, radius = 24) => {
    const p = projectPoint(camera, bounds, point); if (!p) return;
    const distance = Math.hypot(p.x - x, p.y - y);
    if (distance <= radius) candidates.push({...selection, position: point, score: distance / radius + p.depth * .001});
  };
  if (editor.tool === 'draw') (editor.points || []).slice(0, 256).forEach((point, index) => add(point, {kind: 'points', index}, 22));
  else {
    (snapshot?.rig?.fixtures || []).slice(0, 2048).forEach(f => add(f.position, {kind: 'fixtures', id: String(f.id)}));
    (snapshot?.rig?.trusses || []).slice(0, 24).forEach(t => {
      if (!finitePoint(t.position)) return;
      const offset = rotateFixture([Math.min(40, t.length || 1) / 2, 0, 0], t['rotation-deg']);
      const a = projectPoint(camera, bounds, t.position.map((n, i) => n - offset[i])), b = projectPoint(camera, bounds, t.position.map((n, i) => n + offset[i]));
      if (a && b) { const d = segmentDistance(x, y, a, b); if (d <= 18) candidates.push({kind: 'trusses', id: String(t.id), position: t.position, score: d / 18 + a.depth * .001 + .2}); }
    });
  }
  return candidates.sort((a, b) => a.score - b.score)[0] || null;
}

/** Owned pointer boundary. All authoring transitions are emitted to re-frame. */
export class RigEditorInput {
  constructor(engine) {
    this.engine = engine; this.canvas = engine.canvas; this.abort = new AbortController(); this.sequence = 0;
    const options = {signal: this.abort.signal, capture: true};
    this.canvas.addEventListener('pointerdown', event => this.down(event), options);
    this.canvas.addEventListener('pointermove', event => this.move(event), options);
    this.canvas.addEventListener('pointerup', event => this.finish(event, false), options);
    this.canvas.addEventListener('pointercancel', event => this.finish(event, true), options);
    this.canvas.addEventListener('lostpointercapture', event => this.finish(event, true), options);
    this.canvas.addEventListener('keydown', event => {if (event.key === 'Escape' && this.snapshot?.editor) {this.cancel(); this.emit('navigate');}}, options);
    globalThis.addEventListener?.('blur', () => this.cancel(), {signal: this.abort.signal});
    globalThis.document?.addEventListener('visibilitychange', () => {if (document.hidden) this.cancel();}, {signal: this.abort.signal});
  }
  emit(type, detail = {}) {
    this.canvas.dispatchEvent(new CustomEvent('tolglow-editor', {detail: {...detail, type, identity: this.identity, token: this.gesture?.token}}));
  }
  update(snapshot) {
    const identity = editorIdentity(snapshot);
    if (this.identity !== identity || snapshot?.editor?.tool !== this.snapshot?.editor?.tool) this.cancel();
    this.identity = identity; this.snapshot = snapshot;
    const active = Boolean(snapshot?.editor && snapshot.editor.tool !== 'navigate');
    if (active && !this.interacting) {
      this.previousInteraction = Boolean(this.engine.getNavigationState?.().interacting);
      this.engine.setInteractionActive?.(true);
    } else if (!active && this.interacting) this.engine.setInteractionActive?.(this.previousInteraction);
    this.interacting = active;
    this.canvas.style.cursor = !active ? '' : snapshot.editor.tool === 'draw' ? 'crosshair' : 'grab';
  }
  down(event) {
    const e = this.snapshot?.editor;
    if (!e || e.tool === 'navigate' || event.button !== 0 || this.gesture) return;
    event.preventDefault(); event.stopImmediatePropagation(); this.canvas.focus();
    const bounds = this.canvas.getBoundingClientRect(), hit = pickEditor(this.snapshot, this.engine.camera, bounds, event.clientX, event.clientY);
    if (e.tool !== 'draw' && !hit) {this.emit('select', {selection: null}); return;}
    const value = hit ? hit.position[planeAxis(e.plane)] : e['plane-value'];
    const point = planePoint(this.engine.camera, bounds, event.clientX, event.clientY, e.plane, value, e.snap);
    if (!point) {this.emit('unreachable'); return;}
    this.gesture = {token: ++this.sequence, pointer: event.pointerId, hit, point, x: event.clientX, value, tool: e.tool,
      offset: hit ? hit.position.map((n, i) => n - point[i]) : [0, 0, 0]};
    this.canvas.setPointerCapture(event.pointerId);
    this.emit('begin', {selection: hit, point});
  }
  move(event) {
    const g = this.gesture; if (!g || event.pointerId !== g.pointer) return;
    event.preventDefault(); event.stopImmediatePropagation();
    const e = this.snapshot.editor, point = planePoint(this.engine.camera, this.canvas.getBoundingClientRect(), event.clientX, event.clientY, e.plane, g.value, e.snap);
    if (point) this.emit('move', {point: g.hit && (g.tool === 'move' || g.tool === 'draw') ? point.map((n, i) => n + g.offset[i]) : point, angle: (event.clientX - g.x) * .5});
  }
  finish(event, cancelled) {
    if (!this.gesture || event.pointerId !== this.gesture.pointer) return;
    event.preventDefault(); event.stopImmediatePropagation();
    this.emit(cancelled ? 'cancel' : 'end'); const pointer = this.gesture.pointer; this.gesture = null;
    if (this.canvas.hasPointerCapture?.(pointer)) this.canvas.releasePointerCapture(pointer);
  }
  cancel() {
    if (!this.gesture) return;
    this.emit('cancel'); const pointer = this.gesture.pointer; this.gesture = null;
    if (this.canvas.hasPointerCapture?.(pointer)) this.canvas.releasePointerCapture(pointer);
  }
  dispose() {this.cancel(); this.abort.abort(); if (this.interacting) this.engine.setInteractionActive?.(this.previousInteraction); this.interacting = false; this.canvas.style.cursor = '';}
}
