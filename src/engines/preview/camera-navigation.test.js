import test from 'node:test';
import assert from 'node:assert/strict';
import {constrainCamera, tourPose, tourSubjects} from './camera-navigation.js';

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
