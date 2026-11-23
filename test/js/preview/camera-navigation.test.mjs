import test from 'node:test';
import assert from 'node:assert/strict';
import {constrainCamera, tourPose, tourSubjects} from '../../../js/preview/camera-navigation.js';

test('tour selects only emitted light, ranks deterministically and bounds candidate count', () => {
  const fixtures = [{id: 'dark', position: [0, 3, 0]}, {id: 'green', position: [1, 3, 0]}, {id: 'red', position: [-1, 3, 0]}];
  const snapshot = {rig: {fixtures}, instances: [{targets: ['dark']}], output: {values: {
    dark: {color: [1, 1, 1], dimmer: 1}, green: {'emitted-color': [0, 1, 0]}, red: {'emitted-color': [1, 0, 0]}}}};
  assert.deepEqual(tourSubjects(snapshot).map(subject => subject.id), ['green', 'red']);
  assert.deepEqual(snapshot.rig.fixtures, fixtures);
  assert.deepEqual(tourSubjects({...snapshot, output: {values: {}}}), []);
  assert.equal(tourSubjects({rig: {fixtures: Array.from({length: 100}, (_, id) => ({id, position: [0, 3, 0]}))},
    output: {values: Object.fromEntries(Array.from({length: 100}, (_, id) => [id, {'emitted-color': [1, 1, 1]}]))}}).length, 16);
});

test('tour views stay inside rooms and keep targets at authoritative fixture positions', () => {
  for (const position of [[0, 2, 0], [5.8, 4.7, 4.8], [-5.8, .3, -4.8]]) {
    const pose = tourPose({position}, [12, 5, 10], 100);
    assert.deepEqual(pose.target, position);
    assert.ok(pose.position[0] >= -5.75 && pose.position[0] <= 5.75);
    assert.ok(pose.position[1] >= .2 && pose.position[1] <= 4.8);
    assert.ok(pose.position[2] >= -4.75 && pose.position[2] <= 4.75);
  }
  assert.deepEqual(tourPose(null, [12, 5, 10]), {position: [3.3600000000000003, 2.75, 3.8], target: [0, 2, -1.2]});
});

test('manual movement bounds room walls, preserves gradual entry and clears fixture bodies', () => {
  assert.deepEqual(constrainCamera([100, -1, 100], [12, 5, 10], [], [0, 2, 0]), [5.75, .2, 4.75]);
  assert.deepEqual(constrainCamera([0, 2, 7.5], [12, 5, 10], [], [0, 2, 7.6]), [0, 2, 7.5]);
  assert.deepEqual(constrainCamera([0, 2, 8], [12, 5, 10], [], [0, 2, 7.6]), [0, 2, 7.6]);
  const result = constrainCamera([0, 2, 0], [12, 5, 10], [{position: [0, 2, 0]}], [0, 2, 1]);
  assert.ok(Math.hypot(result[0], result[1] - 2, result[2]) >= .449);
});

test('walking and braking cover the same distance on 60 and 120 Hz displays', async () => {
  const {Vector3} = await import('three');
  const {integrateWalk} = await import('../../../js/preview/camera-navigation.js');
  function simulate(rate) {
    const velocity = new Vector3(), distance = new Vector3(), target = new Vector3(0, 0, 2);
    const advance = () => distance.add(integrateWalk(velocity, target, 1 / rate));
    advance();
    assert.ok(velocity.z > 0 && velocity.z < 1, 'accelerates from rest');
    for (let n = 1; n < rate; n++) advance();
    const before = distance.z; target.set(0, 0, 0); advance();
    assert.ok(distance.z > before && velocity.z < 2, 'releases gradually');
    for (let n = 1; n < rate; n++) advance();
    return distance.z;
  }
  assert.ok(Math.abs(simulate(60) - simulate(120)) < 1e-10);
});

test('paired touches distinguish midpoint travel from pinch and rebase when contact changes', async () => {
  const {CameraGesture} = await import('../../../js/preview/camera-gesture.js');
  const g = new CameraGesture();
  g.add(1, 20, 60); g.add(2, 60, 60);
  g.points.get(1).y = 58; g.points.get(2).y = 58;
  assert.equal(g.sample(), null);
  g.points.get(1).y = 20; g.points.get(2).y = 20;
  assert.deepEqual(g.sample(), {kind:'walk', x:0, y:-40, zoom:0});
  g.remove(2); assert.equal(g.sample(), null);
  g.add(3, 60, 20);
  g.points.get(1).x -= 10; g.points.get(3).x += 10;
  const pinch = g.sample();
  assert.equal(pinch.kind, 'pinch'); assert.equal(pinch.x, 0); assert.ok(pinch.zoom > 0);
});
