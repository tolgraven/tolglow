import * as THREE from 'three';
import {sculptHandles} from './sculpt.js';
import {rotateFixture} from './math.js';
import {pickPlacement} from './placement.js';

/** Tools whose gesture is a single tap: a drag or second finger cancels it. */
export const tapTools = new Set(['aim', 'place', 'calibrate']);

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
// Pick the nearest bounded receiving plane facing the camera. The show supplies
// the geometry to both the worker preview and its main-thread input controller.
export function pickSurface(snapshot, camera, bounds, x, y) {
  if (!(bounds.width > 0 && bounds.height > 0)) return null;
  camera.updateMatrixWorld();
  const picker = new THREE.Raycaster();
  picker.setFromCamera(new THREE.Vector2((x - bounds.left) * 2 / bounds.width - 1,
    1 - (y - bounds.top) * 2 / bounds.height), camera);
  const {origin, direction} = picker.ray;
  let nearest = Infinity, hit = null;
  for (const surface of snapshot?.['receiver-surfaces'] || []) {
    const center = new THREE.Vector3().fromArray(surface.point);
    const normal = new THREE.Vector3().fromArray(surface.normal).normalize();
    const incidence = normal.dot(direction);
    if (incidence >= -1e-8) continue;
    const distance = center.clone().sub(origin).dot(normal) / incidence;
    if (distance <= 0 || distance >= nearest) continue;
    const point = picker.ray.at(distance, new THREE.Vector3()), delta = point.clone().sub(center);
    const u = new THREE.Vector3().fromArray(surface['u-axis']).projectOnPlane(normal).normalize();
    const v = new THREE.Vector3().crossVectors(normal, u);
    if (Math.abs(delta.dot(u)) <= surface.size[0] / 2 + 1e-8 &&
        Math.abs(delta.dot(v)) <= surface.size[1] / 2 + 1e-8) {
      nearest = distance;
      hit = {point: point.toArray(), 'surface-id': surface.id};
    }
  }
  return hit;
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
  if (editor.tool === 'sculpt-volume') {
    for (const handle of sculptHandles(snapshot['sculpt-guide'])) add(handle.position, handle, 22);
  } else if (editor.tool === 'video-surface') {
    // Video surface handles (#674): centre, raise and turn, from Studio's projection.
    for (const handle of (editor['surface-handles'] || []).slice(0, 96)) add(handle.position, {...handle, kind: 'surface', handle: handle.kind}, 26);
  } else if (editor.tool === 'volume-select') {
    for (const volume of snapshot['spatial-volumes'] || []) {
      const field = snapshot['volume-guides']?.[volume.id];
      const center = (field?.['moving?'] ? volume.signal?.position || volume.position : field?.center) || volume.signal?.position || volume.position || (finitePoint(volume.min) && finitePoint(volume.max)
        ? volume.min.map((v, i) => (v + volume.max[i]) / 2) : null);
      if (center) add(center, {kind: 'volumes', id: volume.id}, 32);
    }
  } else if (editor.tool === 'pixels' && editor['pixel-mode'] === 'pixel') {
    const indices = editor['pixel-indices'] || [];
    (editor['pixel-positions'] || []).forEach((point, i) => add(point, {kind: 'pixels', index: indices[i]}, 22));
    add(editor['pixel-position'], {kind: 'pixels', index: editor['selected-pixel']}, 22);
  } else if (editor.tool === 'draw' || editor.tool === 'pixels') (editor.points || []).slice(0, 256).forEach((point, index) => add(point, {kind: 'points', index: editor['point-indices']?.[index] ?? index}, 22));
  else {
    (snapshot?.rig?.fixtures || []).slice(0, 2048).filter(f => f['pixel-layout-id'] == null).forEach(f => add(f.position, {kind: 'fixtures', id: String(f.id)}));
    (snapshot?.rig?.trusses || []).slice(0, 24).forEach(t => {
      if (!finitePoint(t.position)) return;
      const offset = rotateFixture([Math.min(40, t.length || 1) / 2, 0, 0], t['rotation-deg']);
      const a = projectPoint(camera, bounds, t.position.map((n, i) => n - offset[i])), b = projectPoint(camera, bounds, t.position.map((n, i) => n + offset[i]));
      if (a && b) { const d = segmentDistance(x, y, a, b); if (d <= 18) candidates.push({kind: 'trusses', id: String(t.id), position: t.position, score: d / 18 + a.depth * .001 + .2}); }
    });
  }
  return candidates.sort((a, b) => a.score - b.score)[0] || null;
}

/** Begin a video surface handle drag (#674). A handle with an axis drags
 * along that world axis as projected on screen; the others drag in the
 * editor's plane through the handle. Returns null when the camera looks
 * along the plane or axis, so the drag could not be measured. */
export function surfaceGesture(camera, bounds, hit, event, editor) {
  const base = {x: event.clientX, y: event.clientY, snap: Number(editor.snap) || 0};
  if (hit.axis != null) {
    const start = projectPoint(camera, bounds, hit.position), unit = [...hit.position];
    unit[hit.axis] += 1;
    const end = start && projectPoint(camera, bounds, unit);
    if (!start || !end) return null;
    const dx = end.x - start.x, dy = end.y - start.y;
    return dx * dx + dy * dy < 4 ? null : {...base, axis: hit.axis, dx, dy, start: hit.position};
  }
  const plane = editor.plane || 'xz', value = hit.position[planeAxis(plane)];
  const start = planePoint(camera, bounds, event.clientX, event.clientY, plane, value);
  return start ? {...base, plane, value, start} : null;
}

/** One step of a surface handle drag: {point} on the plane, or {delta}
 * metres along the axis, snapped like the other editor tools. */
export function surfaceStep(camera, bounds, g, event) {
  if (g.axis != null) {
    const distance = ((event.clientX - g.x) * g.dx + (event.clientY - g.y) * g.dy) / (g.dx * g.dx + g.dy * g.dy);
    return {delta: g.snap > 0 ? Math.round(distance / g.snap) * g.snap : distance};
  }
  const point = planePoint(camera, bounds, event.clientX, event.clientY, g.plane, g.value);
  if (!point) return null;
  return {point: g.snap > 0 ? point.map((n, i) => g.start[i] + Math.round((n - g.start[i]) / g.snap) * g.snap) : point};
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
    const editor = snapshot?.editor, prior = this.snapshot?.editor;
    if (this.identity !== identity || editor?.tool !== prior?.tool || editor?.['layout-id'] !== prior?.['layout-id']
      || editor?.['pixel-mode'] !== prior?.['pixel-mode'] || editor?.plane !== prior?.plane) this.cancel();
    this.identity = identity; this.snapshot = snapshot;
    const active = Boolean(snapshot?.editor && snapshot.editor.tool !== 'navigate');
    if (active && !this.interacting) {
      this.previousInteraction = Boolean(this.engine.getNavigationState?.().interacting);
      this.engine.setInteractionActive?.(true);
    } else if (!active && this.interacting) this.engine.setInteractionActive?.(this.previousInteraction);
    this.interacting = active;
    this.canvas.style.cursor = !active ? '' : ['draw', 'pixels', ...tapTools].includes(snapshot.editor.tool) ? 'crosshair' : 'grab';
  }
  down(event) {
    const e = this.snapshot?.editor;
    if (tapTools.has(this.gesture?.tool) && event.pointerId !== this.gesture.pointer) {
      // Adding a finger turns the gesture into a multi-touch interaction.
      this.gesture.dragged = true;
      event.preventDefault(); event.stopImmediatePropagation();
      return;
    }
    if (!e || e.tool === 'navigate' || event.button !== 0 || this.gesture) return;
    event.preventDefault(); event.stopImmediatePropagation(); this.canvas.focus();
    if (tapTools.has(e.tool)) {
      this.gesture = {tool: e.tool, token: ++this.sequence, pointer: event.pointerId,
        x: event.clientX, y: event.clientY, dragged: false};
      this.canvas.setPointerCapture(event.pointerId);
      return;
    }
    if (e.tool === 'throw') {
      const point = planePoint(this.engine.camera, this.canvas.getBoundingClientRect(), event.clientX, event.clientY, e.plane, e['plane-value'], e.snap);
      if (!point) {this.emit('unreachable'); return;}
      this.gesture = {tool: 'throw', token: ++this.sequence, pointer: event.pointerId, point, start: event.timeStamp,
        x: event.clientX, y: event.clientY, plane: e.plane, value: e['plane-value']};
      this.canvas.setPointerCapture(event.pointerId); return;
    }
    const bounds = this.canvas.getBoundingClientRect(), hit = pickEditor(this.snapshot, this.engine.camera, bounds, event.clientX, event.clientY);
    if (e.tool === 'sculpt-volume') {
      if (!hit) return;
      const value = hit.position[planeAxis(e.plane)];
      const point = planePoint(this.engine.camera, bounds, event.clientX, event.clientY, e.plane, value, e.snap);
      const start = projectPoint(this.engine.camera, bounds, hit.position);
      const unit = [...hit.position];
      if (hit.axis != null) unit[hit.axis] += 1;
      const end = projectPoint(this.engine.camera, bounds, unit);
      if (!point || !start || !end) {this.emit('unreachable'); return;}
      const dx = end.x - start.x, dy = end.y - start.y;
      if (hit.kind === 'stretch' && dx * dx + dy * dy < 4) {this.emit('unreachable'); return;}
      this.gesture = {token: ++this.sequence, tool: e.tool, pointer: event.pointerId, x: event.clientX, y: event.clientY,
        axis: hit.axis, extent: hit.extent, value, plane: e.plane, snap: e.snap, dx, dy};
      this.canvas.setPointerCapture(event.pointerId);
      this.emit('sculpt-begin', {point});
      return;
    }
    if (e.tool === 'video-surface') {
      if (!hit) return;
      const gesture = surfaceGesture(this.engine.camera, bounds, hit, event, e);
      if (!gesture) {this.emit('unreachable'); return;}
      this.gesture = {...gesture, token: ++this.sequence, tool: e.tool, pointer: event.pointerId};
      this.canvas.setPointerCapture(event.pointerId);
      this.emit('surface-begin', {id: hit.id, kind: hit.handle, point: gesture.start});
      return;
    }
    if (e.tool === 'volume-select') {if (hit) this.emit('select-volume', {'volume-id': hit.id}); return;}
    if (e.tool === 'pixels' && !hit && !e['append?']) return;
    if (e.tool !== 'draw' && e.tool !== 'pixels' && !hit) {this.emit('select', {selection: null}); return;}
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
    if (g.tool === 'throw') return;
    if (g.tool === 'sculpt-volume') {
      if (g.axis != null) {
        const distance = ((event.clientX - g.x) * g.dx + (event.clientY - g.y) * g.dy) / (g.dx * g.dx + g.dy * g.dy);
        const delta = g.snap > 0 ? Math.round(distance / g.snap) * g.snap : distance;
        this.emit('sculpt-move', {axis: g.axis, delta, 'half-extent': g.extent});
      } else {
        const point = planePoint(this.engine.camera, this.canvas.getBoundingClientRect(), event.clientX, event.clientY, g.plane, g.value, g.snap);
        if (point) this.emit('sculpt-move', {point});
      }
      return;
    }
    if (tapTools.has(g.tool)) {
      if (Math.hypot(event.clientX - g.x, event.clientY - g.y) > 10) g.dragged = true;
      return;
    }
    if (g.tool === 'video-surface') {
      const step = surfaceStep(this.engine.camera, this.canvas.getBoundingClientRect(), g, event);
      if (step) this.emit('surface-move', step);
      return;
    }
    const e = this.snapshot.editor, point = planePoint(this.engine.camera, this.canvas.getBoundingClientRect(), event.clientX, event.clientY, e.plane, g.value, e.snap);
    if (point) this.emit('move', {point: g.hit && ['move', 'draw', 'pixels'].includes(g.tool) ? point.map((n, i) => n + g.offset[i]) : point, angle: (event.clientX - g.x) * .5});
  }
  finish(event, cancelled) {
    if (!this.gesture || event.pointerId !== this.gesture.pointer) return;
    event.preventDefault(); event.stopImmediatePropagation();
    const g = this.gesture;
    if (tapTools.has(g.tool)) {
      if (!cancelled && !g.dragged && Math.hypot(event.clientX - g.x, event.clientY - g.y) <= 10) {
        const bounds = this.canvas.getBoundingClientRect();
        if (g.tool === 'place') {
          const hit = pickPlacement(this.snapshot, this.engine.camera, bounds, event.clientX, event.clientY);
          this.emit(hit ? 'place' : 'missed-place', hit || {});
        } else {
          const hit = pickSurface(this.snapshot, this.engine.camera, bounds, event.clientX, event.clientY);
          this.emit(hit ? 'surface' : 'missed-surface', hit || {});
        }
      }
    } else if (g.tool === 'throw') {
      if (!cancelled && Math.hypot(event.clientX - g.x, event.clientY - g.y) >= 8) {
        const end = planePoint(this.engine.camera, this.canvas.getBoundingClientRect(), event.clientX, event.clientY, g.plane, g.value);
        if (end) {
          const seconds = Math.max(.08, (event.timeStamp - g.start) / 1000);
          const velocity = end.map((n, i) => (n - g.point[i]) / seconds);
          const scale = Math.min(1, 50 / (Math.hypot(...velocity) || 1));
          this.emit('throw', {point: g.point, velocity: velocity.map(v => v * scale)});
        }
      }
    } else if (g.tool === 'sculpt-volume') this.emit(cancelled ? 'sculpt-cancel' : 'sculpt-end');
    else if (g.tool === 'video-surface') this.emit(cancelled ? 'surface-cancel' : 'surface-end');
    else this.emit(cancelled ? 'cancel' : 'end');
    const pointer = g.pointer; this.gesture = null;
    if (this.canvas.hasPointerCapture?.(pointer)) this.canvas.releasePointerCapture(pointer);
  }
  cancel() {
    if (!this.gesture) return;
    this.emit({'sculpt-volume': 'sculpt-cancel', 'video-surface': 'surface-cancel'}[this.gesture.tool] || 'cancel'); const pointer = this.gesture.pointer; this.gesture = null;
    if (this.canvas.hasPointerCapture?.(pointer)) this.canvas.releasePointerCapture(pointer);
  }
  dispose() {this.cancel(); this.abort.abort(); if (this.interacting) this.engine.setInteractionActive?.(this.previousInteraction); this.interacting = false; this.canvas.style.cursor = '';}
}
