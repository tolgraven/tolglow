import * as THREE from 'three';
import {CameraGesture} from './camera-gesture.js';
import {cameraKey} from './camera.js';
import {clamp, emittedColor} from './math.js';

const MOVEMENT = {
  KeyW: 'forward', KeyK: 'forward', ArrowUp: 'forward',
  KeyS: 'backward', KeyJ: 'backward', ArrowDown: 'backward',
  KeyA: 'left', KeyH: 'left', ArrowLeft: 'left',
  KeyD: 'right', KeyL: 'right', ArrowRight: 'right',
  KeyQ: 'down', KeyE: 'up'
};
const DIRECTIONS = new Set(Object.values(MOVEMENT));
const vector = value => Array.isArray(value) && value.length === 3 && value.every(Number.isFinite);
const roomSize = room => [room?.[0] || 12, room?.[1] || 5, room?.[2] || 10].map(value => clamp(value, 1, 100));
const typing = target => target?.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(target?.tagName || '');
const cellKey = (x, y, z) => `${x},${y},${z}`;

/** Only real emitted light makes a fixture a tour subject. No browser effects. */
function fixtureIndex(fixtures) {
  return new Map(fixtures.map(fixture => [String(fixture.id), fixture]));
}
export function tourSubjects(snapshot, index = fixtureIndex(snapshot?.rig?.fixtures || []), visible) {
  const values = snapshot?.output?.values || {}, subjects = [];
  for (const id of visible || Object.keys(values)) {
    const position = index.get(id)?.position;
    if (!vector(position)) continue;
    const rgb = emittedColor(values[id]), energy = rgb[0] * .2126 + rgb[1] * .7152 + rgb[2] * .0722;
    if (energy > .0001) subjects.push({id, position: position.slice(), energy});
  }
  return subjects.sort((a, b) => b.energy - a.energy || a.id.localeCompare(b.id)).slice(0, 16);
}

/** Camera-only clearance. Outside cutaway presets approach the room gradually. */
export function constrainCamera(position, room, fixtures = [], previous = position) {
  const [w, h, d] = roomSize(room), low = [-w / 2 + .25, .2, -d / 2 + .25], high = [w / 2 - .25, h - .2, d / 2 - .25];
  let result = position.map((value, axis) => clamp(value, Math.min(low[axis], previous[axis]), Math.max(high[axis], previous[axis])));
  for (const fixture of fixtures) {
    if (!vector(fixture.position)) continue;
    const delta = result.map((value, axis) => value - fixture.position[axis]), distance = Math.hypot(...delta);
    if (distance < .45) {
      const direction = distance > .00001 ? delta.map(value => value / distance) : [0, -1, 0];
      result = fixture.position.map((value, axis) => value + direction[axis] * .45);
    }
  }
  return result.map((value, axis) => clamp(value, Math.min(low[axis], previous[axis]), Math.max(high[axis], previous[axis])));
}

export function tourPose(subject, room, phase = 0) {
  const [w, h, d] = roomSize(room);
  if (!subject) return {position: [w * .28, h * .55, d * .38], target: [0, h * .4, -d * .12]};
  const [x, y, z] = subject.position;
  const angle = Math.atan2(-z || 1, -x) + Math.sin(phase) * .32;
  const radius = clamp(Math.min(w, d) * .22, 1.1, 2.4);
  return {position: constrainCamera([x + Math.cos(angle) * radius, clamp(y - .65, .5, h - .4), z + Math.sin(angle) * radius], room, [], [0, h / 2, 0]),
    target: subject.position.slice()};
}

/** One DOM camera owner, shared by worker presentation and main-thread fallback. */
export class CameraNavigator {
  constructor({camera, controls, canvas, room, snapshot, visibleLights, active, reducedMotion, changed = () => {}}) {
    Object.assign(this, {camera, controls, canvas, room, snapshot, visibleLights, active, reducedMotion, changed});
    this.mode = 'orbit'; this.speed = 2; this.keys = new Set(); this.buttons = new Set(); this.listeners = new Set();
    this.velocity = new THREE.Vector3();
    this.swipe = new THREE.Vector2(); this.look = new THREE.Vector2();
    this.gesture = new CameraGesture(); this.zoom = 0;
    this.interacting = false; this.disposed = false; this.clock = 0; this.subjectId = null; this.visitStarted = 0;
    this.abort = new AbortController(); const signal = this.abort.signal;
    canvas.addEventListener('keydown', event => this.keydown(event), {signal});
    canvas.addEventListener('keyup', event => {this.keys.delete(event.code); this.boost = event.shiftKey;}, {signal});
    canvas.addEventListener('blur', () => this.clearInput(), {signal});
    window.addEventListener('blur', () => this.clearInput(), {signal});
    document.addEventListener('visibilitychange', () => {this.clearInput(); this.lastTime = null;}, {signal});
    canvas.addEventListener('pointerdown', event => this.pointerDown(event), {signal, capture: true});
    canvas.addEventListener('pointermove', event => this.pointerMove(event), {signal, capture: true});
    for (const name of ['pointerup', 'pointercancel', 'lostpointercapture']) canvas.addEventListener(name, event => this.pointerUp(event), {signal, capture: true});
    canvas.addEventListener('wheel', event => this.wheel(event), {signal, capture: true, passive: false});
    this.animate = now => {
      this.frame = null;
      if (this.disposed || !this.active()) return;
      this.tick(now); this.refreshActivity();
    };
    this.refreshActivity();
  }
  refreshActivity() {
    if (this.disposed || !this.active()) {
      if (this.frame != null) cancelAnimationFrame(this.frame);
      this.frame = null; this.lastTime = null; this.clearInput(); this.controls.enabled = false;
    } else if (this.frame == null) {
      this.controls.enabled = this.mode === 'orbit' && !this.interacting;
      this.frame = requestAnimationFrame(this.animate);
    }
  }
  state() {
    return {mode: this.mode, speed: this.speed, interacting: this.interacting,
      'reduced-motion': Boolean(this.reducedMotion()), 'tour-fixture': this.subjectId,
      'tour-waiting': this.mode === 'tour' && !this.subjectId};
  }
  notify() {
    const state = this.state(), key = JSON.stringify(state);
    if (key === this.lastNotice) return;
    this.lastNotice = key; for (const listener of this.listeners) listener(state);
  }
  subscribe(listener) {this.listeners.add(listener); listener(this.state()); return () => this.listeners.delete(listener);}
  setMode(mode, keepTouch = false) {
    if (!['orbit', 'walk', 'tour'].includes(mode)) throw new Error('Unknown camera navigation mode');
    this.clearInput(keepTouch); this.mode = mode; this.lastTime = null; this.subjectId = null; this.visitStarted = this.clock;
    this.controls.enabled = mode === 'orbit' && !this.interacting; this.notify();
  }
  setSpeed(speed) {
    if (!Number.isFinite(speed) || speed < .25 || speed > 8) throw new Error('Camera speed must be 0.25–8 metres per second');
    this.speed = speed; this.notify();
  }
  setInteractionActive(active) {
    this.interacting = Boolean(active); this.clearInput(); this.lastTime = null;
    this.controls.enabled = this.mode === 'orbit' && !this.interacting; this.notify();
  }
  clearInput(keepTouch = false) {
    if (!keepTouch) {
      const ids = [...this.gesture.points.keys()];
      this.gesture.points.clear(); this.gesture.rebase();
      for (const id of ids) if (this.canvas.hasPointerCapture?.(id)) this.canvas.releasePointerCapture(id);
    }
    this.velocity.set(0, 0, 0); this.swipe.set(0, 0); this.look.set(0, 0); this.zoom = 0;
    this.keys.clear(); this.buttons.clear(); this.boost = false;
    if (this.pointer && this.canvas.hasPointerCapture?.(this.pointer.id)) this.canvas.releasePointerCapture(this.pointer.id);
    this.pointer = null;
  }
  move(direction, pressed) {
    if (!DIRECTIONS.has(direction)) throw new Error('Unknown camera direction');
    if (!pressed) {this.buttons.delete(direction); return;}
    if (this.interacting || !this.active()) return;
    if (this.mode !== 'walk') this.setMode('walk');
    this.buttons.add(direction);
  }
  keydown(event) {
    if (typing(event.target) || event.altKey || event.metaKey || event.ctrlKey || this.interacting || !this.active()) return;
    if (MOVEMENT[event.code]) {
      event.preventDefault();
      if (this.mode !== 'walk') this.setMode('walk');
      this.keys.add(event.code); this.boost = event.shiftKey; return;
    }
    if (event.key === 'Escape') {this.setMode('orbit'); return;}
    if (['+', '=', '-', 'Home'].includes(event.key)) {
      if (this.mode !== 'orbit') this.setMode('orbit');
      if (cameraKey(this.camera, this.controls, this.room(), event.key)) {event.preventDefault(); this.changed();}
    }
  }
  queueSwipe(x, y) {
    const scale = this.speed * 2 / Math.max(1, this.canvas.clientHeight);
    this.swipe.x += x * scale; this.swipe.y -= y * scale;
  }
  wheel(event) {
    if (!this.active() || this.interacting || event.altKey || event.metaKey) return;
    // Precision touchpads report two-finger swipes as wheel events and pinches
    // as Ctrl+wheel. A line-mode wheel retains OrbitControls' dolly in Orbit.
    if (!event.ctrlKey && event.deltaMode !== 0 && this.mode === 'orbit') return;
    event.preventDefault(); event.stopImmediatePropagation();
    if (event.ctrlKey) {this.zoom -= clamp(event.deltaY * .01, -1, 1); return;}
    if (this.mode !== 'walk') this.setMode('walk');
    const unit = event.deltaMode === 1 ? 16 : event.deltaMode === 2 ? this.canvas.clientHeight : 1;
    this.queueSwipe(event.deltaX * unit, event.deltaY * unit);
  }
  pointerDown(event) {
    if (this.interacting || !this.active() || event.button !== 0) return;
    this.canvas.focus({preventScroll: true});
    if (this.mode === 'tour') this.setMode('walk');
    if (event.pointerType === 'touch') {
      event.preventDefault(); event.stopImmediatePropagation();
      this.gesture.add(event.pointerId, event.clientX, event.clientY);
      this.canvas.setPointerCapture(event.pointerId); return;
    }
    if (this.mode !== 'walk' || this.pointer) return;
    event.preventDefault(); event.stopImmediatePropagation();
    this.pointer = {id: event.pointerId, x: event.clientX, y: event.clientY};
    this.canvas.setPointerCapture(event.pointerId);
  }
  pointerMove(event) {
    const touch = this.gesture.points.get(event.pointerId);
    const p = touch || (this.pointer?.id === event.pointerId ? this.pointer : null);
    if (!p || this.interacting) return;
    event.preventDefault(); event.stopImmediatePropagation();
    if (!touch || this.gesture.points.size === 1) {
      this.look.x += event.clientX - p.x; this.look.y += event.clientY - p.y;
    }
    p.x = event.clientX; p.y = event.clientY;
  }
  pointerUp(event) {
    if (this.gesture.points.has(event.pointerId)) {
      if (event.type === 'pointerup') this.sampleGesture();
      this.gesture.remove(event.pointerId);
      if (event.type === 'pointercancel') {this.swipe.set(0, 0); this.zoom = 0; this.look.set(0, 0);}
    } else if (this.pointer?.id === event.pointerId) this.pointer = null;
    else return;
    if (this.canvas.hasPointerCapture?.(event.pointerId)) this.canvas.releasePointerCapture(event.pointerId);
  }
  sampleGesture() {
    const gesture = this.gesture.sample();
    if (!gesture) return;
    if (gesture.kind === 'pinch') {this.zoom += gesture.zoom; return;}
    if (this.mode !== 'walk') this.setMode('walk', true);
    this.queueSwipe(gesture.x, gesture.y);
  }
  updateView(dt, fixtures) {
    const blend = this.reducedMotion() ? 1 : -Math.expm1(-24 * dt);
    if (Math.abs(this.zoom) > .0001) {
      const step = this.zoom * blend; this.zoom -= step;
      if (this.mode === 'orbit') this.controls.dollyIn(Math.exp(-step));
      else this.translate(this.camera.getWorldDirection(new THREE.Vector3()).multiplyScalar(step * this.speed), fixtures);
      this.changed();
    }
    if (this.look.lengthSq() < .0001) return;
    const dx = this.look.x * blend, dy = this.look.y * blend;
    this.look.multiplyScalar(1 - blend);
    if (this.mode === 'orbit') {
      const scale = 2 * Math.PI / Math.max(1, this.canvas.clientHeight);
      this.controls.rotateLeft(dx * scale); this.controls.rotateUp(dy * scale);
    } else {
      const euler = new THREE.Euler().setFromQuaternion(this.camera.quaternion, 'YXZ');
      euler.y -= dx * .004;
      euler.x = clamp(euler.x - dy * .004, -Math.PI * .48, Math.PI * .48);
      this.camera.quaternion.setFromEuler(euler);
      this.controls.target.copy(this.camera.position).add(this.camera.getWorldDirection(new THREE.Vector3()).multiplyScalar(2));
    }
    this.changed();
  }
  walk(dt, fixtures) {
    if (!this.keys.size && !this.buttons.size && this.velocity.lengthSq() < 1e-7 && this.swipe.lengthSq() < 1e-9) return;
    const held = new Set([...this.buttons, ...Array.from(this.keys, key => MOVEMENT[key])]);
    const target = new THREE.Vector3(Number(held.has('right')) - Number(held.has('left')),
      Number(held.has('up')) - Number(held.has('down')), Number(held.has('forward')) - Number(held.has('backward')));
    target.normalize().multiplyScalar(this.speed * (this.boost ? 2 : 1));
    const rate = target.lengthSq() ? 13 : 19;
    const blend = -Math.expm1(-rate * dt), difference = this.velocity.clone().sub(target);
    // Integrate the velocity curve over the display interval, including braking.
    const travel = target.clone().multiplyScalar(dt).addScaledVector(difference, blend / rate);
    this.velocity.lerp(target, blend);
    const panBlend = this.reducedMotion() ? 1 : -Math.expm1(-24 * dt);
    travel.x += this.swipe.x * panBlend; travel.z += this.swipe.y * panBlend;
    this.swipe.multiplyScalar(1 - panBlend);
    const forward = this.camera.getWorldDirection(new THREE.Vector3()); forward.y = 0; forward.normalize();
    const right = new THREE.Vector3().crossVectors(forward, this.camera.up).normalize();
    const world = forward.multiplyScalar(travel.z).addScaledVector(right, travel.x); world.y = travel.y;
    this.translate(world, fixtures); this.changed();
  }
  tick(now) {
    const active = this.active() && !this.interacting;
    if (!active && this.wasActive) this.clearInput();
    this.wasActive = active;
    const dt = active && this.lastTime != null ? clamp((now - this.lastTime) / 1000, 0, .05) : 0;
    this.lastTime = active ? now : null;
    this.controls.enabled = active && this.mode === 'orbit';
    if (!active) return;
    this.sampleGesture();
    if (!dt) return;
    const snapshot = this.snapshot(), fixtures = snapshot?.rig?.fixtures || [];
    this.updateView(dt, fixtures);
    if (this.mode === 'orbit') {this.controls.update(dt); return;}
    if (this.mode === 'walk') {this.walk(dt, fixtures); return;}
    if (this.reducedMotion()) return;
    this.clock += dt;
    const visible = this.visibleLights?.();
    if (snapshot !== this.subjectSnapshot || visible !== this.subjectLights) {
      if (fixtures !== this.subjectFixtures) {
        this.subjectFixtures = fixtures; this.fixtureIndex = fixtureIndex(fixtures);
      }
      this.subjectLights = visible; this.subjectSnapshot = snapshot;
      this.subjects = tourSubjects(snapshot, this.fixtureIndex, visible);
    }
    const subjects = this.subjects || [];
    let subject = subjects.find(entry => entry.id === this.subjectId);
    if (!subject || this.clock - this.visitStarted >= 8) {
      const index = subject ? (subjects.indexOf(subject) + 1) % subjects.length : 0;
      subject = subjects[index]; this.subjectId = subject?.id || null; this.visitStarted = this.clock; this.notify();
    }
    const pose = tourPose(subject, this.room(), this.clock * .22), destination = new THREE.Vector3(...pose.position);
    const delta = destination.sub(this.camera.position), distance = delta.length();
    if (distance > 0) delta.multiplyScalar(Math.min(1, this.speed * dt / distance));
    this.translate(delta, fixtures);
    this.controls.target.lerp(new THREE.Vector3(...pose.target), 1 - Math.exp(-dt * 2));
    this.camera.lookAt(this.controls.target); this.changed();
  }
  translate(delta, fixtures) {
    if (fixtures !== this.collisionFixtures) {
      this.collisionFixtures = fixtures; this.collisionGrid = new Map();
      for (const fixture of fixtures) if (vector(fixture.position)) {
        const key = cellKey(...fixture.position.map(Math.floor));
        if (!this.collisionGrid.has(key)) this.collisionGrid.set(key, []);
        this.collisionGrid.get(key).push(fixture);
      }
    }
    const steps = Math.max(1, Math.ceil(delta.length() / .15)), step = delta.clone().divideScalar(steps);
    for (let i = 0; i < steps; i++) {
      const previous = this.camera.position.clone(), next = previous.clone().add(step);
      const [x, y, z] = next.toArray().map(Math.floor), nearby = [];
      for (let dx = -1; dx <= 1; dx++) for (let dy = -1; dy <= 1; dy++) for (let dz = -1; dz <= 1; dz++)
        nearby.push(...(this.collisionGrid.get(cellKey(x + dx, y + dy, z + dz)) || []));
      const constrained = new THREE.Vector3(...constrainCamera(next.toArray(), this.room(), nearby, previous.toArray()));
      // A freshly edited fixture can surround the camera. Escape gradually
      // instead of teleporting when the next authoritative snapshot arrives.
      const movement = constrained.sub(previous).clampLength(0, step.length());
      this.camera.position.copy(previous).add(movement);
      this.controls.target.add(this.camera.position.clone().sub(previous));
    }
  }
  dispose() {
    if (this.disposed) return;
    this.disposed = true; this.clearInput(); this.abort.abort(); cancelAnimationFrame(this.frame); this.listeners.clear();
  }
}
