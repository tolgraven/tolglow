import test from 'node:test';
import assert from 'node:assert/strict';
import * as THREE from 'three';
import {SceneRenderer} from '../../../js/preview/scene-renderer.js';
import {LightingSamples} from '../../../js/preview/lighting-samples.js';
import {disposeGeometry} from '../../../js/preview/geometry.js';

function fixture(t) {
  // Exercise real rig construction, fixture selection and lighting projection.
  // Only WebGL allocation and atmosphere I/O are absent from this Node check.
  const engine = Object.create(SceneRenderer.prototype);
  Object.assign(engine, {renderer: {}, visible: true, platform: {worker: true}, scene: new THREE.Scene(),
    camera: new THREE.PerspectiveCamera(), lighting: new LightingSamples(), quality: 'desktop',
    options: {fixtures: 16, shadows: 2, shadowSize: 256}, fixtures: [], spots: [], lightIds: [],
    editorOverlay: {update() {}}, apertureUniform: {value: new Float32Array(18)}, patchRadii: {value: new Float32Array(18)},
    fog: {setRoom() {}, setAtmosphere() {}, setAmount() {}, upload() {}, field: {}},
    materials: Object.fromEntries(['floor','wall','blackMetal','metal','housing','rubber','stage','curtain','grille']
      .map(name => [name, new THREE.MeshStandardMaterial()]))});
  const rig = {room: {width: 12, height: 5, depth: 10},
    fixtures: ['lit','dark'].map((id, i) => ({id, position: [i * 2, 3.5, -1], channels: {pan: 1, tilt: 2}})),
    trusses: [{id: 'truss', position: [0, 4.2, -1], length: 6}]};
  const values = {lit: {pan: .5, tilt: .2, zoom: 0, 'emitted-color': [.7,.02,.01]},
    dark: {pan: .5, tilt: .2, zoom: 0, 'emitted-color': [0,0,0]}};
  const snapshot = {rig, 'show-id': 'shadow-cache', epoch: 1, 'frame-number': 1, 'time-ms': 25, output: {values}};
  engine.update(snapshot);
  // Three clears needsUpdate after completing each shadow render. Advancing
  // this explicit boundary lets the tests inspect the next invalidation.
  const completed = () => {for (const light of engine.spots) light.shadow.needsUpdate = false;};
  const dirty = () => engine.spots.map(light => light.shadow.needsUpdate);
  t.after(() => {
    disposeGeometry(engine.content);
    for (const entry of engine.fixtures) entry.dispose();
    for (const light of engine.spots) light.dispose();
    engine.reflections.dispose();
    for (const material of Object.values(engine.materials)) material.dispose();
  });
  return {engine, snapshot, values, completed, dirty};
}

test('shadow maps start dirty and survive identical snapshots, emission changes and camera motion', t => {
  const {engine, snapshot, completed, dirty} = fixture(t);
  assert.deepEqual(engine.spots.map(light => light.shadow.autoUpdate), [false, false]);
  assert.deepEqual(dirty(), [true, true]);
  completed();
  const roots = engine.fixtures.map(entry => entry.root);
  engine.update({...structuredClone(snapshot), 'frame-number': 2, 'time-ms': 50});
  assert.deepEqual(engine.fixtures.map(entry => entry.root), roots);
  assert.deepEqual(dirty(), [false, false]);
  const next = structuredClone(snapshot);
  next.output.values.lit['emitted-color'] = [.2,.4,.6]; next.output.values.lit.focus = .9;
  engine.update(next);
  assert.deepEqual(dirty(), [false, false]);
  engine.camera.position.set(2, 3, 8);
  engine.applyLighting(next.output.values);
  assert.deepEqual(dirty(), [false, false]);
});

test('a dark moving housing invalidates all maps while zoom changes only its own light map', t => {
  const {engine, values, completed, dirty} = fixture(t);
  completed();
  const moved = {...values, dark: {...values.dark, pan: .6}};
  engine.applyLighting(moved);
  assert.deepEqual(dirty(), [true, true]);
  completed(); engine.applyLighting(moved);
  assert.deepEqual(dirty(), [false, false]);
  engine.applyLighting({...moved, lit: {...values.lit, zoom: .5}});
  assert.deepEqual(dirty(), [true, false]);
});

test('fixture and truss edits invalidate existing maps; structural and forced rebuilds start fresh', t => {
  const {engine, snapshot, completed, dirty} = fixture(t);
  completed();
  const roots = engine.fixtures.map(entry => entry.root), truss = engine.trusses[0].root;
  const moved = structuredClone(snapshot); moved.rig.fixtures[1].position[0] += .3;
  engine.update(moved);
  assert.deepEqual(engine.fixtures.map(entry => entry.root), roots);
  assert.deepEqual(dirty(), [true, true]);
  completed();
  const rotated = structuredClone(moved); rotated.rig.trusses[0]['rotation-deg'] = [0, 0, 12];
  engine.update(rotated);
  assert.equal(engine.trusses[0].root, truss);
  assert.deepEqual(dirty(), [true, true]);
  completed();
  const resized = structuredClone(rotated); resized.rig.fixtures[1]['lens-radius-m'] = .2;
  engine.update(resized);
  assert.notEqual(engine.fixtures[1].root, roots[1]);
  assert.deepEqual(dirty(), [true, true]);
  completed(); const old = engine.spots[0];
  engine.update(resized, true);
  assert.notEqual(engine.spots[0], old);
  assert.deepEqual(dirty(), [true, true]);
});

test('context restoration invalidates both fixture and reflected shadow maps', t => {
  const {engine, completed, dirty} = fixture(t);
  const reflected = new THREE.SpotLight(); reflected.castShadow = true; reflected.shadow.needsUpdate = false;
  engine.reflections.lights.push(reflected);
  completed();
  engine.resize = () => {}; engine.refreshEnvironment = () => {};
  engine.restoreContext();
  assert.deepEqual(dirty(), [true, true]);
  assert.equal(reflected.shadow.needsUpdate, true);
});
