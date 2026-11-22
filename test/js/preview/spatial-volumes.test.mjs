import test from 'node:test';
import assert from 'node:assert/strict';
import {volumePose, VolumeOverlay} from '../../../js/preview/spatial-volumes.js';

test('moving volume transforms reuse guides and close on removal', () => {
  const overlay = new VolumeOverlay();
  const ball = {id:'a', shape:'ball', radius:.5, position:[0,1,0]};
  const box = {id:'b', shape:'box', min:[0,0,0], max:[2,4,6]};
  overlay.update([ball,box]);
  assert.deepEqual(volumePose(box), {position:[1,2,3],scale:[2,4,6]});
  const object = overlay.byId.get('a');
  const geometry = object.geometry;
  overlay.update([{...ball, signal:{position:[1,2,3], radius:1, deformation:.3}}]);
  assert.deepEqual(object.position.toArray(), [1,2,3]); assert.equal(object.geometry, geometry);
  assert.equal(object.scale.y,.7); assert.equal(overlay.byId.has('b'), false);
  let freed = false; geometry.addEventListener('dispose', () => {freed=true;});
  overlay.dispose(); assert.ok(freed); assert.equal(overlay.byId.size,0);
});
