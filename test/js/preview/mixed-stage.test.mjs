import test from 'node:test';
import assert from 'node:assert/strict';
import * as THREE from 'three';
import {SceneRenderer} from '../../../js/preview/scene-renderer.js';
import {createDiffuserTubes} from '../../../js/preview/diffuser-tubes.js';
import {createMovingHead, disposeGeometry} from '../../../js/preview/geometry.js';
import {opticalFrustum} from '../../../js/preview/optics.js';

test('Auto selects the wider viewport tier after fullscreen crosses the hysteresis band', () => {
  const scene = Object.create(SceneRenderer.prototype);
  scene.platform = {worker: true, coarsePointer: false};
  scene.autoLayoutTier = 'mobile';
  assert.equal(scene.autoQuality(720), 'mobile');
  assert.equal(scene.autoQuality(1817), 'desktop');
  scene.autoLayoutTier = 'desktop';
  assert.equal(scene.autoQuality(680), 'desktop');
  assert.equal(scene.autoQuality(522), 'mobile');
  scene.platform.coarsePointer = true;
  assert.equal(scene.autoQuality(1817), 'mobile');
});

test('moving head optical plane and clear mesh radius match after an aperture change', () => {
  const material = new THREE.MeshStandardMaterial();
  const fixture = {id: 'wash', position: [0, 3, 0], 'lens-radius-m': .16};
  const entry = createMovingHead(fixture, Object.fromEntries(
    ['housing', 'blackMetal', 'rubber', 'metal'].map(name => [name, material])));
  const lens = entry.head.children.find(child => child.geometry?.type === 'CircleGeometry');
  for (const radius of [.16, .085]) {
    entry.setAperture({...fixture, 'lens-radius-m': radius});
    const beam = opticalFrustum(fixture.position, [0, -1, 0], .25, entry.lensRadius);
    assert.equal(lens.position.y, 0);
    assert.ok(Math.abs(lens.geometry.parameters.radius * lens.scale.x - beam.radius) < 1e-10);
    assert.ok(Math.abs(beam.apexDistance * Math.tan(.25) - radius) < 1e-10);
  }
  disposeGeometry(entry.root);
  entry.dispose();
  material.dispose();
});

test('one continuous diffuser spreads a single lit cell into neighboring surface rings', () => {
  const stage = new THREE.Group();
  const fixtures = Array.from({length: 3}, (_, i) =>
    ({id: 'pixel-' + i, position: [i, 1, 0], 'pixel-layout-id': 'sleeve', 'pixel-index': i, 'diffuser-radius-m': .035}));
  const [tube] = createDiffuserTubes(fixtures, stage);
  assert.equal(stage.children.length, 1);
  assert.equal(tube.rows.length, 3);
  const color = stage.children[0].geometry.attributes.color;
  tube.update({'pixel-1': {'emitted-color': [1, 0, 0]}});
  assert.ok(Math.abs(color.getX(0) - .16) < 1e-6);
  assert.ok(Math.abs(color.getX(26) - .68) < 1e-6);
  assert.equal(color.getY(26), 0);
  tube.update({});
  assert.equal(color.getX(26), 0);
  disposeGeometry(stage);
  tube.dispose();
});
