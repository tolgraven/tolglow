import test from 'node:test';
import assert from 'node:assert/strict';
import * as THREE from 'three';
import {planePoint, projectPoint, pickEditor, RigEditorInput} from '../../../js/preview/editor.js';
import {structuralKey} from '../../../js/preview/math.js';
import {SceneRenderer} from '../../../js/preview/scene-renderer.js';
import {RigOverlay} from '../../../js/preview/overlay.js';
import {LightingSamples} from '../../../js/preview/lighting-samples.js';
const bounds = {left: 0, top: 0, width: 1000, height: 800};
const camera = new THREE.PerspectiveCamera(49, 1.25, .1, 100); camera.position.set(0, 8, 10); camera.lookAt(0, 0, 0); camera.updateMatrixWorld();
const rig = {fixtures: [{id: 'a', position: [0, 3, 0], 'rotation-deg': [0, 0, 0]}], trusses: [{id: 't', position: [0, 4, 0], length: 4}]};
test('editing rays meet the selected world plane and reject edge-on or backwards intersections', () => {
  const point = planePoint(camera, bounds, 500, 400, 'xz', 0, .1);
  assert.ok(Math.hypot(...point) < .00001);
  assert.equal(planePoint(camera, bounds, 500, 400, 'xz', 20), null);
  assert.equal(planePoint(camera, {...bounds, width: 0}, 500, 400), null);
  const front = new THREE.PerspectiveCamera(); front.position.set(0, 0, 10); front.lookAt(0, 0, 0); front.updateMatrixWorld();
  assert.equal(planePoint(front, bounds, 500, 400, 'xz', 1), null);
});
test('picking covers fixture, rotated truss and editable point proxies without scene objects', () => {
  for (const [kind, position] of [['fixtures', [0, 3, 0]], ['trusses', [1.5, 4, 0]]]) {
    const p = projectPoint(camera, bounds, position); assert.equal(pickEditor({rig, editor: {tool: 'move'}}, camera, bounds, p.x, p.y).kind, kind);
  }
  const p = projectPoint(camera, bounds, [2, 0, 0]);
  assert.equal(pickEditor({rig, editor: {tool: 'draw', points: [[2, 0, 0]]}}, camera, bounds, p.x, p.y).index, 0);
  assert.equal(pickEditor({rig}, camera, bounds, -100, -100), null);
});
test('fixture and truss pose changes preserve actual geometry instances and calibrated direction', () => {
  const changed = {fixtures: [{...rig.fixtures[0], position: [1, 3, 2], 'rotation-deg': [0, 45, 0]}], trusses: [{...rig.trusses[0], position: [2, 5, 0], 'rotation-deg': [0, 0, 90]}]};
  assert.equal(structuralKey(rig), structuralKey(changed));
  assert.notEqual(structuralKey(rig), structuralKey({...rig, trusses: [{...rig.trusses[0], length: 6}]}));
  const root = new THREE.Group(), truss = new THREE.Group();
  const engine = {renderer: {}, visible: true, lighting: new LightingSamples(), applyLighting: SceneRenderer.prototype.applyLighting, platform: {worker: true}, key: structuralKey(rig), editorOverlay: {update(){}}, options: {fixtures: 16, shadows: 0},
    spots: [], lightIds: [], fixtures: [{id: 'a', fixture: rig.fixtures[0], setAperture() {}, root, head: new THREE.Group(), yoke: new THREE.Group(), lensMaterial: {emissive: new THREE.Color()}}],
    trusses: [{id: 't', root: truss}], apertureUniform: {value: new Float32Array(16)}, rebuild(){throw new Error('Pose edit rebuilt scene');}};
  SceneRenderer.prototype.update.call(engine, {rig: changed});
  assert.equal(engine.fixtures[0].root, root); assert.deepEqual(root.position.toArray(), [1, 3, 2]);
  assert.deepEqual(truss.position.toArray(), [2, 5, 0]); assert.equal(truss.rotation.z, Math.PI / 2);
});
test('overlay reuses hints until authoring changes and disposes removed geometry', () => {
  const overlay = new RigOverlay(), snapshot = {rig, editor: {tool: 'draw', points: [[0, 0, 0], [1, 0, 0]], plane: 'xz'}};
  overlay.update(snapshot); const first = overlay.scene.children[0]; let disposed = false; first.geometry.addEventListener('dispose', () => {disposed = true;});
  overlay.update(snapshot); assert.equal(first, overlay.scene.children[0]);
  overlay.update({rig}); assert.equal(overlay.scene.children.length, 0); assert.equal(disposed, true); overlay.dispose();
});
test('pointer gestures retain offsets, reject unrelated fingers, cancel on show replacement and release navigation on disposal', () => {
  const messages = [], canvas = new EventTarget(); Object.assign(canvas, {style: {}, focus(){}, getBoundingClientRect: () => bounds, setPointerCapture(){}, hasPointerCapture: () => false});
  canvas.addEventListener('tolglow-editor', e => messages.push(e.detail));
  const active = [], engine = {canvas, camera, setInteractionActive: value => active.push(value)}, input = new RigEditorInput(engine);
  const snapshot = {rig, 'show-id': 'show', epoch: 1, editor: {tool: 'move', plane: 'xz', snap: .1}}; input.update(snapshot);
  const point = projectPoint(camera, bounds, rig.fixtures[0].position), event = {pointerId: 5, clientX: point.x, clientY: point.y, button: 0, preventDefault(){}, stopImmediatePropagation(){}};
  input.down(event); input.move({...event, pointerId: 6, clientX: point.x + 100}); assert.deepEqual(messages.map(x => x.type), ['begin']);
  input.move({...event, clientX: point.x + 100}); assert.equal(messages.at(-1).type, 'move'); assert.equal(messages.at(-1).point[1], 3);
  input.update({...snapshot, epoch: 2}); assert.equal(messages.at(-1).type, 'cancel'); assert.equal(input.gesture, null);
  input.dispose(); assert.equal(active.at(-1), false); assert.equal(canvas.style.cursor, '');
});

test('moving an existing path handle retains its height and grab offset', () => {
  const messages = [], canvas = new EventTarget(); Object.assign(canvas, {style: {}, focus(){}, getBoundingClientRect: () => bounds, setPointerCapture(){}, hasPointerCapture: () => false});
  canvas.addEventListener('tolglow-editor', e => messages.push(e.detail));
  const input = new RigEditorInput({canvas, camera, setInteractionActive(){}});
  input.update({rig, 'show-id': 'show', epoch: 1, editor: {tool: 'draw', plane: 'xz', 'plane-value': 0, snap: 0, points: [[0, 2, 0]]}});
  const p = projectPoint(camera, bounds, [0, 2, 0]), event = {pointerId: 1, clientX: p.x + 10, clientY: p.y, button: 0, preventDefault(){}, stopImmediatePropagation(){}};
  input.down(event); input.move(event);
  assert.ok(Math.hypot(...messages.at(-1).point.map((n, i) => n - [0, 2, 0][i])) < 1e-10);
  input.move({...event, clientX: p.x + 50}); assert.equal(messages.at(-1).point[1], 2); input.dispose();
});

test('authoring hints render onto the presented canvas after composer and restore renderer ownership', () => {
  const overlay = new RigOverlay(); overlay.update({rig, editor: {tool: 'draw', points: [[0, 0, 0], [1, 0, 0]]}});
  const composite = {name: 'fog composite'}, draws = [], renderer = {autoClear: true, target: composite,
    getRenderTarget(){return this.target;}, setRenderTarget(target){this.target = target;}, clearDepth(){},
    render(scene){draws.push({target: this.target, autoClear: this.autoClear, children: scene.children.length});}};
  overlay.render(renderer, camera);
  assert.equal(draws[0].target, null); assert.equal(draws[0].autoClear, false); assert.ok(draws[0].children > 0);
  assert.equal(renderer.target, composite); assert.equal(renderer.autoClear, true); overlay.dispose();
});

test('an inactive editor does not unlock another interaction owner, including fallback startup', () => {
  const canvas = new EventTarget(); Object.assign(canvas, {style: {}, hasPointerCapture: () => false});
  const calls = [], engine = {canvas, camera, getNavigationState: () => ({interacting: true}), setInteractionActive: active => calls.push(active)};
  const input = new RigEditorInput(engine); input.update({rig}); assert.deepEqual(calls, []);
  input.update({rig, editor: {tool: 'move'}}); input.update({rig, editor: {tool: 'navigate'}});
  assert.deepEqual(calls, [true, true]); input.dispose(); assert.deepEqual(calls, [true, true]);
});

test('aim selects the nearest bounded receiver only on a completed tap', () => {
  const messages = [], canvas = new EventTarget();
  Object.assign(canvas, {style: {}, focus(){}, getBoundingClientRect: () => bounds, setPointerCapture(){}, hasPointerCapture: () => false});
  canvas.addEventListener('tolglow-editor', event => messages.push(event.detail));
  const input = new RigEditorInput({canvas, camera, setInteractionActive(){}});
  const floor = {id: 'floor', point: [0, 0, 0], normal: [0, 1, 0], 'u-axis': [1, 0, 0], size: [10, 10]};
  const stage = {...floor, id: 'stage', point: [0, .4, 0], size: [2, 2]};
  input.update({rig, 'receiver-surfaces': [floor, stage], epoch: 2, editor: {tool: 'aim'}});
  const center = projectPoint(camera, bounds, [0, .4, 0]);
  const event = {button: 0, pointerId: 1, clientX: center.x, clientY: center.y, preventDefault(){}, stopImmediatePropagation(){}};
  input.down(event); assert.equal(messages.length, 0);
  input.finish(event, false); assert.equal(messages[0]['surface-id'], 'stage');
  assert.ok(messages[0].point.every((v,i) => Math.abs(v - stage.point[i]) < 1e-6));
  input.down(event); input.finish(event, true);
  input.down(event); input.move({...event, clientX: center.x + 25}); input.finish(event, false);
  assert.equal(messages.length, 1);
  const elsewhere = projectPoint(camera, bounds, [4, 0, 1]);
  const tap = {...event, clientX: elsewhere.x, clientY: elsewhere.y};
  input.down(tap); input.finish(tap, false);
  assert.equal(messages.at(-1)['surface-id'], 'floor');
  input.update({rig, epoch: 2, 'receiver-surfaces': [], editor: {tool: 'aim'}});
  input.down(tap); input.finish(tap, false);
  assert.equal(messages.at(-1).type, 'missed-surface');
  input.dispose();
});
