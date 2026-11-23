import test from 'node:test';
import assert from 'node:assert/strict';
import * as THREE from 'three';
import {RigEditorInput, projectPoint} from '../../../js/preview/editor.js';
import {SculptGuide} from '../../../js/preview/sculpt.js';

const guide = {key: 'draft', center: [0, 1, 0], min: [-1, 0, -1], max: [1, 2, 1],
  color: [1, 0.5, 0.2], surface: [[-1, 0, 0], [1, 0, 0]], outer: []};

test('shape outlines and handles persist across show updates and dispose on close', () => {
  const view = new SculptGuide(); view.update(guide);
  const shape = view.group.children[0];
  let disposed = false; shape.geometry.addEventListener('dispose', () => {disposed = true;});
  view.update({...guide}); assert.equal(view.group.children[0], shape);
  assert.equal(view.group.children.filter(child => child.isMesh).length, 4);
  view.update({...guide, key: 'saved', 'handles?': false});
  assert.equal(disposed, true); assert.equal(view.group.children.length, 1);
  view.dispose(); assert.equal(view.group.children.length, 0);
});

test('axis drag and pointer cancellation are delivered as one sculpt gesture', () => {
  const camera = new THREE.PerspectiveCamera(45, 1, 0.1, 100);
  camera.position.set(0, 5, 10); camera.lookAt(0, 1, 0); camera.updateMatrixWorld();
  const bounds = {left: 0, top: 0, width: 600, height: 600}, canvas = new EventTarget(), messages = [];
  Object.assign(canvas, {style:{}, focus(){}, setPointerCapture(){}, hasPointerCapture: () => false, getBoundingClientRect: () => bounds});
  canvas.addEventListener('tolglow-editor', e => messages.push(e.detail));
  const input = new RigEditorInput({canvas, camera, setInteractionActive(){}});
  input.update({epoch: 2, editor:{tool:'sculpt-volume',plane:'xz',snap:0}, 'sculpt-guide': guide});
  const start = projectPoint(camera,bounds,[1,1,0]), end = projectPoint(camera,bounds,[1.5,1,0]);
  const touch = {button:0,pointerId:3,clientX:start.x,clientY:start.y,preventDefault(){},stopImmediatePropagation(){}};
  input.down(touch); input.move({...touch,clientX:end.x,clientY:end.y}); input.finish(touch,true);
  assert.deepEqual(messages.map(x => x.type), ['sculpt-begin','sculpt-move','sculpt-cancel']);
  assert.equal(messages[1].axis,0); assert.ok(Math.abs(messages[1].delta - 0.5) < 1e-8);
  assert.equal(messages[0].token,messages[2].token);
  input.dispose();
});
