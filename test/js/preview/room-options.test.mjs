import test from 'node:test';
import assert from 'node:assert/strict';
import {parseRoomOptions} from '../../../dev/preview/room-options.mjs';

const controlled = {ROOM_SCENE: 'dense', ROOM_WARMUP_MS: '20000'};

test('empty Room variables mean their defaults and flags accept true and false', () => {
  const defaults = parseRoomOptions({});
  assert.deepEqual([defaults.quality, defaults.workLight, defaults.lightingMode, defaults.minimumWarmupMs, defaults.scene,
    defaults.passes, defaults.fogField, defaults.plumeScene, defaults.keepPage], ['auto', .22, 'hold', 8000, 'both', null, 'decay', false, false]);
  const empty = parseRoomOptions({ROOM_WORK_LIGHT: '', ROOM_WARMUP_MS: ' ', ROOM_QUALITY: '', ROOM_PASSES: '', ROOM_PLUME_SCENE: ''});
  assert.deepEqual([empty.workLight, empty.minimumWarmupMs, empty.quality, empty.passes, empty.plumeScene], [.22, 8000, 'auto', null, false]);
  assert.equal(parseRoomOptions({ROOM_WORK_LIGHT: '0'}).workLight, 0);
  for (const value of ['1', 'true', 'TRUE', 'yes']) assert.equal(parseRoomOptions({ROOM_PLUME_SCENE: value}).plumeScene, true);
  for (const value of ['0', 'false', 'no']) assert.equal(parseRoomOptions({ROOM_KEEP_PAGE: value}).keepPage, false);
  assert.throws(() => parseRoomOptions({ROOM_PLUME_SCENE: 'on'}), /ROOM_PLUME_SCENE must be 1, true, 0 or false/);
  assert.throws(() => parseRoomOptions({ROOM_WORK_LIGHT: '1.5'}), /between zero and one/);
});

test('Auto passes accept exactly the five bracketed sequences in the dense scene', () => {
  for (const [sequence, kind] of [['full,full,full', 'null'], ['full,simple-fixed-loop,full', 'loop'],
    ['full,full-no-timers,full', 'timer'], ['full,no-composite-fallback,full', 'composite'],
    ['full,present-cached-frame,skip-fog-raymarch,cached-shadows,full', 'attribution']]) {
    const options = parseRoomOptions({...controlled, ROOM_PASSES: sequence});
    assert.deepEqual([options.comparison, options.frozenAuto, options.fogField], [kind, true, 'hold']);
  }
  for (const sequence of ['full', 'full,full', 'full,cached-shadows,full', 'simple-fixed-loop,full,full'])
    assert.throws(() => parseRoomOptions({...controlled, ROOM_PASSES: sequence}), /requires exactly one of/);
  assert.throws(() => parseRoomOptions({ROOM_PASSES: 'full,full,full', ROOM_WARMUP_MS: '20000'}), /dense moving scene/);
  assert.throws(() => parseRoomOptions({ROOM_PASSES: 'full,full,full', ROOM_SCENE: 'dense'}), /≥20 s warmup/);
  assert.throws(() => parseRoomOptions({...controlled, ROOM_PASSES: 'full,bogus,full'}), /Unknown ROOM_PASSES/);
  // Fixed quality takes other pass probes, but not the Auto-only controls.
  assert.equal(parseRoomOptions({ROOM_QUALITY: 'mobile', ROOM_PASSES: 'full,cached-shadows,full'}).comparison, null);
  assert.throws(() => parseRoomOptions({ROOM_QUALITY: 'mobile', ROOM_PASSES: 'full,simple-fixed-loop,full'}), /require Auto/);
});

test('the fog field and tolerances are explicit options', () => {
  assert.equal(parseRoomOptions({...controlled, ROOM_PASSES: 'full,full,full', ROOM_FOG_FIELD: 'decay'}).fogField, 'decay');
  assert.throws(() => parseRoomOptions({ROOM_FOG_FIELD: 'hold'}), /requires the ROOM_PASSES diagnostic worker/);
  assert.throws(() => parseRoomOptions({ROOM_FOG_FIELD: 'frozen'}), /hold or decay/);
  const options = parseRoomOptions({ROOM_BRACKET_TOLERANCE: '.03', ROOM_STATIONARITY_TOLERANCE: '.04'});
  assert.deepEqual(options.bracketTolerance, {meanDensity: .01, roomHz: .03, workerFrameP50Ms: .03});
  assert.deepEqual(options.stationarity, {blocks: 10, tolerance: .04, floorMs: .25});
  assert.deepEqual(parseRoomOptions({}).bracketTolerance, {meanDensity: .01, roomHz: .02, workerFrameP50Ms: .02});
  assert.throws(() => parseRoomOptions({ROOM_BRACKET_TOLERANCE: '2'}), /fraction/);
  assert.throws(() => parseRoomOptions({ROOM_PLUME_SCENE: '1', ROOM_SCENE: 'fixed'}), /full moving renderer/);
});
