import test from 'node:test';
import assert from 'node:assert/strict';
import {contactColors, VideoSurfaceOverlay} from '../../../js/preview/video-surfaces.js';

const quad = (z = 0) => ({
  id: ':sheet', columns: 1, rows: 1,
  positions: [-4, 2, z, 4, 2, z, -4, -2, z, 4, -2, z],
  uvs: [0, 0, 1, 0, 0, 1, 1, 1],
  indices: [0, 2, 1, 1, 2, 3],
  contacts: [[0, 0, .1, 1], [1, 0, .2, .5]],
  'contact-ids': ['a', 'b'],
  texture: '/api/video/frame?source=:grid&width=512&sequence=1'
});

function loader() {
  const calls = [], pending = new Map();
  const load = url => new Promise(resolve => {
    calls.push(url); pending.set(url, () => resolve({width: 2, height: 1, close() {this.closed = true;}}));
  });
  return {load, calls, finish: async url => {pending.get(url)(); await new Promise(r => setTimeout(r, 0));}};
}

test('a surface mesh carries the projected geometry, source uvs and contact markers', async () => {
  const {load, calls, finish} = loader();
  const overlay = new VideoSurfaceOverlay(load);
  const surface = quad();
  overlay.update([surface]);
  const obj = overlay.byId.get(':sheet');
  assert.deepEqual([...obj.mesh.geometry.getAttribute('position').array], surface.positions);
  assert.deepEqual([...obj.mesh.geometry.getAttribute('uv').array], surface.uvs);
  assert.deepEqual([...obj.mesh.geometry.index.array], surface.indices);
  assert.equal(obj.markers.geometry.getAttribute('position').count, 2);
  assert.equal(obj.mesh.material.map, null, 'grey until the frame arrives');
  assert.deepEqual(calls, [surface.texture]);
  await finish(surface.texture);
  assert.equal(obj.mesh.material.map.flipY, false, 'source v runs down the frame');
  assert.equal(obj.mesh.material.map.image.width, 2);
  const first = obj.mesh.material.map;

  // A new frame keeps the old texture on screen until it has loaded.
  const next = {...surface, texture: surface.texture.replace('sequence=1', 'sequence=2')};
  overlay.update([next]);
  assert.equal(obj.mesh.material.map, first);
  await finish(next.texture);
  assert.notEqual(obj.mesh.material.map, first);
  assert.equal(first.image.closed, true, 'the replaced bitmap is released');
  assert.equal(overlay.textures.size, 1);

  // Moving the surface replaces geometry; the same arrays are not reuploaded.
  const geometry = obj.mesh.geometry.getAttribute('position');
  overlay.update([{...next, contacts: next.contacts}]);
  assert.equal(obj.mesh.geometry.getAttribute('position'), geometry);
  overlay.update([{...quad(.5), texture: next.texture}]);
  assert.equal(obj.mesh.geometry.getAttribute('position').array[2], .5);

  let freed = false; obj.mesh.geometry.addEventListener('dispose', () => {freed = true;});
  overlay.update([]);
  assert.ok(freed); assert.equal(overlay.byId.size, 0); assert.equal(overlay.textures.size, 0);
});

test('each marker disc shows its head\'s committed emitted colour', () => {
  const overlay = new VideoSurfaceOverlay(() => new Promise(() => {}));
  const surface = quad();
  overlay.update([surface], {a: {'emitted-color': [1, 0, 0]}, b: {'emitted-color': [0, .214, 0]}});
  const head = overlay.byId.get(':sheet').markers.geometry.getAttribute('headColor').array;
  assert.deepEqual([...head.slice(0, 4)], [1, 0, 0, 1]);
  assert.ok(Math.abs(head[5] - .5) < .01, 'linear 0.214 displays near half');
  overlay.update([surface], {a: {'emitted-color': [0, 0, 1]}});
  assert.deepEqual([...head.slice(0, 8)], [0, 0, 1, 1, 0, 0, 0, 0], 'a head without a projected value leaves its disc empty');
});

test('contact markers brighten with their weight and invalid data is skipped', () => {
  const colors = contactColors([0, 1]);
  assert.ok(colors[3] > colors[0]);
  const overlay = new VideoSurfaceOverlay(() => new Promise(() => {}));
  overlay.update([{id: 'bad', positions: [0, 0], uvs: [0, 0]}, null]);
  assert.equal(overlay.byId.size, 0);
});
