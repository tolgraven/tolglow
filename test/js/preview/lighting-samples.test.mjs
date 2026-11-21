import test from 'node:test';
import assert from 'node:assert/strict';
import {LightingSamples} from '../../../js/preview/lighting-samples.js';
import {DisplayClock} from '../../../js/preview/display-clock.js';

const rig = {id: 'rig'}, fixtures = [{id: 'head', fixture: {calibration: {pan: {'min-deg': -270, 'max-deg': 270}}}}];
const first = {'emitted-color': [0,.25,1], dimmer: .2, intensity: .2, pan: .2, tilt: .25, shutter: 5, gobo: 1, focus: .1, direction: [0,1,0]};
const next = {'emitted-color': [1,.75,0], dimmer: .8, intensity: .8, pan: .3, tilt: .75, shutter: 5, gobo: 2, focus: .6, direction: [0,0,1]};
const frame = (n, head) => ({'runtime-id': 'host', 'show-id': 'room', epoch: 1, revision: 8,
  'frame-number': n, 'time-ms': 25*n, output: {values: {head}}});

test('hold publishes exact output across display ticks; smoothing only samples continuous fields', () => {
  const samples = new LightingSamples();
  samples.accept(frame(1, first), rig, 100);
  const f = frame(2, next); samples.accept(f, rig, 125);
  assert.equal(samples.values(fixtures, 129), f.output.values);
  samples.setMode('interpolate'); samples.accept(frame(3, first), rig, 150);
  const middle = samples.values(fixtures, 162.5).head;
  assert.deepEqual(middle['emitted-color'], [.5,.5,.5]);
  assert.equal(middle.intensity, .5); assert.equal(middle.dimmer, .5);
  assert.equal(middle.pan, .25); assert.equal(middle['pan-deg'], -135);
  assert.equal(middle.tilt, .5); assert.equal(middle['tilt-deg'], 0);
  assert.equal(middle.shutter, 5); assert.equal(middle.gobo, 1); assert.equal(middle.focus, .1);
  assert.equal(middle.direction, undefined);
  assert.equal(samples.values(fixtures, 250).head, first, 'stall clamps to exact latest');
  assert.deepEqual(first.direction, [0,1,0], 'sampling never mutates committed data');
});

test('snap on missing frames, reset, emergency output, temporal or structural discontinuity', () => {
  const origin = frame(1, first);
  for (const [patch, key, receipt] of [
    [{'frame-number': 3}, rig, 125], [{epoch: 2}, rig, 125], [{'runtime-id': 'next'}, rig, 125],
    [{'show-id': 'next'}, rig, 125], [{revision: 9}, rig, 125], [{'cut?': true}, rig, 125],
    [{'time-ms': 0}, rig, 125], [{'time-ms': 200}, rig, 125], [{}, {}, 125], [{}, rig, 230],
    [{'blackout?': true}, rig, 125], [{master: 0}, rig, 125]
  ]) {
    const samples = new LightingSamples(); samples.setMode('interpolate');
    samples.accept(origin, rig, 100);
    const f = {...frame(2, next), ...patch}; samples.accept(f, key, receipt);
    assert.equal(samples.values(fixtures, receipt + 5), f.output.values);
  }
  const samples = new LightingSamples(); samples.setMode('interpolate');
  samples.accept({...origin, 'blackout?': true}, rig, 100);
  samples.accept(frame(2, next), rig, 125);
  assert.equal(samples.values(fixtures, 130).head, next);
});
test('strobe, missing controls, and pan reversals retain discrete behavior', () => {
  const sample = (a, b) => {
    const samples = new LightingSamples(); samples.setMode('interpolate');
    samples.accept(frame(1,a), rig, 100); samples.accept(frame(2,b), rig, 125);
    return samples.values(fixtures, 137.5).head;
  };
  assert.equal(sample({...first, strobe: .8}, next), next);
  assert.equal(sample({...first, channels: {strobe: .8}}, next), next);
  assert.equal(sample({...first, shutter: 15}, next), next);
  assert.equal(sample({...first, channels: {shutter: 1}}, next), next);
  assert.equal(sample({...first, pan: 0}, {...next, pan: 1}).pan, 1);
  assert.deepEqual(sample({...first, pan: undefined}, next).direction, next.direction);
  assert.deepEqual(sample({}, next)['emitted-color'], next['emitted-color']);
});
test('display pacing follows 120Hz and 60Hz RAFs, caps faster displays and cancels pending ticks', () => {
  function frames(hz) {
    let now = 0, pending, drawn = 0;
    const clock = new DisplayClock({now: () => now, raf: fn => (pending = fn, 1), cancelRaf: () => pending = null});
    const draw = time => {drawn++; clock.at(time + 1000/120, draw);};
    clock.at(0, draw);
    for (let i=1; i<=hz; i++) {now = i*(1000/hz); pending(now);}
    clock.cancel(); assert.equal(pending, null); assert.equal(clock.pending, false);
    return drawn;
  }
  assert.equal(frames(120), 120); assert.equal(frames(60), 60); assert.equal(frames(240), 120);
});
test('worker without supported requestAnimationFrame uses positive cancellable delay', () => {
  let now=50, delay, pending, draws=0;
  const clock = new DisplayClock({now: () => now, raf: () => {throw Object.assign(new Error(),{name:'NotSupportedError'});},
    timer: (fn, ms) => {pending = fn; delay = ms; return 1;}, cancelTimer: () => pending = null});
  clock.at(58, () => draws++); assert.equal(delay,8);
  now=58; pending(); assert.equal(draws,1);
  clock.at(58, () => draws++); assert.equal(delay,1); clock.cancel(); assert.equal(pending,null);
});

test('pan sampled at the same requested bearing respects the committed end-stop reversal', () => {
  const samples = new LightingSamples();
  samples.setMode('interpolate');
  const requested = {direction: [0,0,-1], 'pan-deg': 180, 'tilt-deg': 0};
  const old = {...requested, pan: (180+270)/540, tilt: .5, pose: {'pan-deg': 180, 'tilt-deg': 0, direction: [0,0,-1]}};
  const current = {...requested, pan: (-180+270)/540, tilt: .5, pose: {'pan-deg': -180, 'tilt-deg': 0, direction: [0,0,-1]}};
  samples.accept(frame(1, old), rig, 100);
  samples.accept(frame(2, current), rig, 125);
  const middle = samples.values(fixtures, 137.5).head;
  assert.equal(middle.pan, current.pan);
  assert.equal(middle.pose, current.pose);
});

test('the interpolated pose drops the current endpoint direction and uses both mechanical angles', () => {
  const samples = new LightingSamples();
  samples.setMode('interpolate');
  const make = (level, degrees) => ({channels:{pan:level, tilt:.5}, 'pan-deg':90, 'tilt-deg':90,
    pose:{'pan-deg':degrees,'tilt-deg':0,direction:[0,0,1]}});
  samples.accept(frame(1, make(.5, 0)), rig, 100);
  samples.accept(frame(2, make(5/9, 30)), rig, 125);
  const middle = samples.values(fixtures, 137.5).head;
  assert.deepEqual(middle.pose, {'pan-deg':15,'tilt-deg':0});
});
