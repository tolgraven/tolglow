import test from 'node:test';
import assert from 'node:assert/strict';
import * as THREE from 'three';
import {projectPoint, RigEditorInput} from '../../../js/preview/editor.js';
import {pickPlacement, mountAxes, ceiling} from '../../../js/preview/placement.js';
import {RigOverlay} from '../../../js/preview/overlay.js';

const bounds = {left: 0, top: 0, width: 1000, height: 800};
const close = (a, b, e = 1e-6) => a.length === b.length && a.every((v, i) => Math.abs(v - b[i]) < e);
const view = (position, target) => {
  const camera = new THREE.PerspectiveCamera(49, 1.25, .1, 100);
  camera.position.set(...position); camera.lookAt(...target); camera.updateMatrixWorld(); return camera;
};
const floor = {id: 'floor', point: [0, 0, 0], normal: [0, 1, 0], 'u-axis': [1, 0, 0], size: [12, 10]};
const back = {id: 'back', point: [0, 2.5, -5], normal: [0, 0, 1], 'u-axis': [1, 0, 0], size: [12, 5]};
const room = {width: 12, height: 5, depth: 10};

test('placement rays land on the floor, walls and the derived ceiling with inward normals', () => {
  const above = view([0, 8, 10], [0, 0, 0]);
  const centre = projectPoint(above, bounds, [1, 0, 2]);
  const hit = pickPlacement({rig: {room}, 'receiver-surfaces': [floor, back]}, above, bounds, centre.x, centre.y);
  assert.equal(hit.kind, 'surface'); assert.equal(hit['surface-id'], 'floor');
  assert.ok(close(hit.point, [1, 0, 2])); assert.deepEqual(hit.normal, [0, 1, 0]);
  const wall = projectPoint(above, bounds, [2, 3, -5]);
  assert.equal(pickPlacement({rig: {room}, 'receiver-surfaces': [floor, back]}, above, bounds, wall.x, wall.y)['surface-id'], 'back');
  const below = view([0, 1.5, 4], [0, 5, -1]);
  const up = projectPoint(below, bounds, [0.5, 5, -1]);
  const roof = pickPlacement({rig: {room}, 'receiver-surfaces': [floor, back]}, below, bounds, up.x, up.y);
  assert.equal(roof['surface-id'], 'ceiling'); assert.deepEqual(roof.normal, [0, -1, 0]);
  assert.ok(close(roof.point, [0.5, 5, -1]));
  assert.deepEqual(ceiling(undefined).point, [0, 5, 0], 'The default room has a 5 m ceiling');
  assert.equal(pickPlacement({rig: {room}, 'receiver-surfaces': []}, above, {...bounds, width: 0}, 1, 1), null);
});

test('a truss in front of the ceiling takes the placement on its axis and hangs fixtures below it', () => {
  const camera = view([0, 2, 6], [0, 4, 0]);
  const truss = {id: 'front-truss', position: [0, 4, 0], length: 6, 'rotation-deg': [0, 0, 0]};
  const snapshot = {rig: {room, trusses: [truss]}, 'receiver-surfaces': [floor, back]};
  const at = projectPoint(camera, bounds, [1.5, 4, 0]);
  const hit = pickPlacement(snapshot, camera, bounds, at.x, at.y);
  assert.equal(hit.kind, 'truss'); assert.equal(hit['truss-id'], 'front-truss');
  assert.ok(close(hit.point, [1.5, 4, 0], 1e-3)); assert.ok(close(hit.normal, [0, -1, 0]));
  const past = projectPoint(camera, bounds, [3.6, 4, 0]);
  assert.notEqual(pickPlacement(snapshot, camera, bounds, past.x, past.y)?.kind, 'truss', 'Beyond the truss end');
  const turned = {...truss, 'rotation-deg': [0, 90, 0]};
  const along = projectPoint(camera, bounds, [0, 4, 1]);
  const rotated = pickPlacement({...snapshot, rig: {room, trusses: [turned]}}, camera, bounds, along.x, along.y);
  assert.equal(rotated.kind, 'truss'); assert.ok(close(rotated.point, [0, 4, 1], 1e-3));
});

test('mount axes follow the renderer Euler order for the mounts Studio computes', () => {
  // Expected rotations come from tolglow.rig.placement/mount-rotation, whose
  // directions the JVM tests check against Afterglow's fixture pose.
  const cases = [[[0, 0, 0], [0, 1, 0], [0, 0, 1]], [[0, 0, 180], [0, -1, 0], [0, 0, 1]],
    [[90, 0, 0], [0, 0, 1], [0, -1, 0]], [[-90, 0, 180], [0, 0, -1], [0, -1, 0]],
    [[90, 90, 0], [1, 0, 0], [0, -1, 0]], [[90, -90, 0], [-1, 0, 0], [0, -1, 0]]];
  for (const [rotation, up, forward] of cases) {
    const axes = mountAxes(rotation);
    assert.ok(close(axes.up, up), `${rotation} up ${axes.up}`); assert.ok(close(axes.forward, forward), `${rotation} forward ${axes.forward}`);
  }
});

test('placing is a completed tap; a drag, second finger or miss places nothing', () => {
  const camera = view([0, 8, 10], [0, 0, 0]), messages = [], canvas = new EventTarget();
  Object.assign(canvas, {style: {}, focus(){}, getBoundingClientRect: () => bounds, setPointerCapture(){}, hasPointerCapture: () => false});
  canvas.addEventListener('tolglow-editor', event => messages.push(event.detail));
  const input = new RigEditorInput({canvas, camera, setInteractionActive(){}});
  input.update({rig: {room, fixtures: []}, 'receiver-surfaces': [floor], epoch: 3, editor: {tool: 'place'}});
  assert.equal(canvas.style.cursor, 'crosshair');
  const at = projectPoint(camera, bounds, [2, 0, 1]);
  const event = {button: 0, pointerId: 1, clientX: at.x, clientY: at.y, preventDefault(){}, stopImmediatePropagation(){}};
  input.down(event); input.finish(event, false);
  assert.equal(messages.length, 1); assert.equal(messages[0].type, 'place'); assert.equal(messages[0]['surface-id'], 'floor');
  assert.ok(close(messages[0].point, [2, 0, 1])); assert.ok(messages[0].identity.includes('3'));
  input.down(event); input.move({...event, clientX: at.x + 30}); input.finish(event, false);
  input.down(event); input.down({...event, pointerId: 2}); input.finish(event, false);
  input.down(event); input.finish(event, true);
  assert.equal(messages.length, 1);
  const sky = {...event, clientY: 5};
  input.down(sky); input.finish(sky, false);
  assert.equal(messages.at(-1).type, 'missed-place');
  input.dispose();
});

test('the overlay draws the selected fixture home beam and the calibration target', () => {
  const overlay = new RigOverlay();
  const fixture = {id: 'spot', position: [0, 4, 0], 'rotation-deg': [0, 0, 180]};
  overlay.update({rig: {fixtures: [fixture]}, editor: {tool: 'calibrate', selection: {kind: 'fixtures', id: 'spot'},
    'calibration-target': [1, 0, 2], 'calibration-points': [[2, 0, 0]]}});
  const lines = overlay.rig.children.filter(o => o.isLine).map(o => Array.from(o.geometry.attributes.position.array));
  assert.ok(lines.some(p => close(p, [0, 4, 0, 0, 4, 1.6])), 'Home beam along +Z');
  assert.ok(lines.some(p => close(p, [0, 4, 0, 0, 3.45, 0])), 'Mounting axis points down when hanging');
  assert.ok(lines.some(p => close(p, [0, 4, 0, 1, 0, 2])), 'Line to the calibration target');
  overlay.dispose();
});
