import test from 'node:test';
import assert from 'node:assert/strict';
import * as THREE from 'three';
import {projectPoint, pickEditor, RigEditorInput, surfaceGesture, surfaceStep} from '../../../js/preview/editor.js';
import {RigOverlay} from '../../../js/preview/overlay.js';

// Video surface handles (#674): Studio projects a centre, raise and turn
// handle per surface; the engine picks them and reports plane points or
// axis distances, never surface poses.
const bounds = {left: 0, top: 0, width: 1000, height: 800};
const camera = new THREE.PerspectiveCamera(49, 1.25, .1, 100); camera.position.set(0, 8, 10); camera.lookAt(0, 0, 0); camera.updateMatrixWorld();
const handles = [
  {kind: 'move', id: 'sheet', position: [0, 2, 0]},
  {kind: 'raise', id: 'sheet', position: [0, 3.5, 0], axis: 1},
  {kind: 'turn', id: 'sheet', position: [2.35, 2, 0]},
];
const snapshot = {rig: {fixtures: []}, 'show-id': 'show', epoch: 1,
  editor: {tool: 'video-surface', plane: 'xz', snap: 0, 'surface-handles': handles, 'selected-surface': 'sheet'}};

test('surface handles are picked by kind under the pointer', () => {
  for (const handle of handles) {
    const p = projectPoint(camera, bounds, handle.position);
    const hit = pickEditor(snapshot, camera, bounds, p.x, p.y);
    assert.equal(hit.handle, handle.kind);
    assert.equal(hit.id, 'sheet');
  }
  assert.equal(pickEditor(snapshot, camera, bounds, 5, 5), null);
});

test('a centre drag reports plane points through the handle height; a raise drag reports metres up', () => {
  const p = projectPoint(camera, bounds, [0, 2, 0]);
  const move = surfaceGesture(camera, bounds, {position: [0, 2, 0]}, {clientX: p.x, clientY: p.y}, snapshot.editor);
  assert.ok(Math.hypot(...move.start.map((n, i) => n - [0, 2, 0][i])) < 1e-6);
  const target = projectPoint(camera, bounds, [1, 2, -1]);
  const step = surfaceStep(camera, bounds, move, {clientX: target.x, clientY: target.y});
  step.point.forEach((n, i) => assert.ok(Math.abs(n - [1, 2, -1][i]) < 1e-6));
  const snapped = surfaceStep(camera, bounds, {...move, snap: .5}, {clientX: target.x, clientY: target.y});
  assert.deepEqual(snapped.point.map(n => Math.round(n * 1000) / 1000), [1, 2, -1]);

  const r = projectPoint(camera, bounds, [0, 3.5, 0]), up = projectPoint(camera, bounds, [0, 4.5, 0]);
  const raise = surfaceGesture(camera, bounds, {position: [0, 3.5, 0], axis: 1}, {clientX: r.x, clientY: r.y}, snapshot.editor);
  const lifted = surfaceStep(camera, bounds, raise, {clientX: r.x + (up.x - r.x) / 2, clientY: r.y + (up.y - r.y) / 2});
  assert.ok(Math.abs(lifted.delta - .5) < 1e-6);
  const top = new THREE.PerspectiveCamera(); top.position.set(0, 20, 0); top.lookAt(0, 0, 0); top.updateMatrixWorld();
  const c = projectPoint(top, bounds, [0, 3.5, 0]);
  assert.equal(surfaceGesture(top, bounds, {position: [0, 3.5, 0], axis: 1}, {clientX: c.x, clientY: c.y}, snapshot.editor), null,
    'looking straight down the axis, a raise cannot be measured');
});

test('the input emits one begin, moves and one end or cancel per drag', () => {
  const messages = [], canvas = new EventTarget();
  Object.assign(canvas, {style: {}, focus(){}, getBoundingClientRect: () => bounds, setPointerCapture(){}, hasPointerCapture: () => false});
  canvas.addEventListener('tolglow-editor', e => messages.push(e.detail));
  const input = new RigEditorInput({canvas, camera, setInteractionActive(){}});
  input.update(snapshot);
  const p = projectPoint(camera, bounds, [2.35, 2, 0]), q = projectPoint(camera, bounds, [0, 2, -2.35]);
  const event = {pointerId: 3, clientX: p.x, clientY: p.y, button: 0, preventDefault(){}, stopImmediatePropagation(){}};
  input.down(event);
  input.move({...event, clientX: q.x, clientY: q.y});
  input.finish({...event, clientX: q.x, clientY: q.y}, false);
  assert.deepEqual(messages.map(m => m.type), ['surface-begin', 'surface-move', 'surface-end']);
  assert.equal(messages[0].kind, 'turn'); assert.equal(messages[0].id, 'sheet');
  assert.ok(messages.every(m => m.token === messages[0].token));
  input.down(event); input.cancel();
  assert.equal(messages.at(-1).type, 'surface-cancel');
  messages.length = 0; input.down({...event, clientX: 3, clientY: 3});
  assert.deepEqual(messages, [], 'a press away from every handle starts nothing');
  input.dispose();
});

test('the overlay draws each handle and a guide to its centre', () => {
  const overlay = new RigOverlay();
  overlay.update(snapshot);
  const tagged = overlay.rig.children.filter(o => o.userData.surfaceHandle);
  assert.deepEqual(tagged.map(o => o.userData.surfaceHandle), ['sheet/move', 'sheet/raise', 'sheet/turn']);
  assert.equal(overlay.rig.children.filter(o => o.isLine).length, 2);
  overlay.dispose();
});
