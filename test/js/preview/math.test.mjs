import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import {apertureRadius, opticalFrustum} from '../../../js/preview/optics.js';
import {fixtureDirection, emittedColor, axisDegrees, beamAngle, structuralKey} from '../../../js/preview/math.js';
const near = (a, b) => a.forEach((v, i) => assert.ok(Math.abs(v - b[i]) < 1e-10, `${a} != ${b}`));
test('resolved direction/aim use exact world coordinates before servo angles', () => {
  near(fixtureDirection({position: [2, 4, 1]}, {direction: [0, -3, 4], pan: 0}), [0, -.6, .8]);
  near(fixtureDirection({position: [2, 4, 1]}, {aim: [2, 0, 1]}), [0, -1, 0]);
});
test('normalized servo angles invert CPU elevation convention and calibration', () => {
  near(fixtureDirection({}, {pan: .5, tilt: .5}), [0, 0, 1]);
  near(fixtureDirection({}, {pan: .5, tilt: 0}), [0, -1, 0]);
  near(fixtureDirection({}, {pan: .75, tilt: .5}), [1, 0, 0]);
  near(fixtureDirection({'rotation-deg': [90, 0, 0]}, {pan: .5, tilt: .5}), [0, -1, 0]);
  assert.equal(axisDegrees({calibration: {pan: {'min-deg': -270, 'max-deg': 270}}}, {pan: .75}, 'pan'), 135);
  assert.equal(axisDegrees({calibration: {pan: [128, 100]}}, {pan: 228 / 255}, 'pan'), 180);
});
test('resolved emitted RGB is authoritative and never multiplied by master again', () => {
  assert.deepEqual(emittedColor({'emitted-color': [.2, .3, .4], intensity: .1, color: [1, 1, 1]}), [.2, .3, .4]);
  assert.deepEqual(emittedColor({color: [1, 1, 1], dimmer: 1}), [0, 0, 0]);
  assert.deepEqual(emittedColor({'emitted-color': [NaN, 1, 0]}), [0, 0, 0]);
});
test('zoom projects declared angle range and output changes do not rebuild structure', () => {
  assert.equal(beamAngle({'zoom-range-deg': [10, 50]}, {zoom: .5}), Math.PI / 12);
  const rig = {fixtures: [{id: 'a', position: [0, 3, 0]}]};
  assert.equal(structuralKey(rig), structuralKey({...rig, output: {}}));
});

test('captured actual JVM renderer oracle round-trips both geometric and servo projections', () => {
  const oracle = JSON.parse(fs.readFileSync(new URL('./fixtures/aim-oracle.json', import.meta.url)));
  assert.equal(oracle.source, 'afterglow.render.frame/resolve-frame');
  for (const sample of oracle.cases) {
    near(fixtureDirection(sample.fixture, sample.output), sample.expected);
    near(fixtureDirection(sample.fixture, sample['servo-output']), sample.expected);
  }
});

test('finite optical aperture keeps rig origin while matching lens radius at clipped plane', () => {
  const radius = apertureRadius({}), profile = opticalFrustum([2, 3, -1], [0, -1, 0], .2, radius);
  assert.equal(radius, .119);
  assert.deepEqual(profile.optical.toArray(), [2, 3, -1]);
  assert.ok(Math.abs(profile.apexDistance * Math.tan(.2) - radius) < 1e-12);
  assert.ok(Math.abs(profile.virtual.y - profile.apexDistance - 3) < 1e-12);
});
