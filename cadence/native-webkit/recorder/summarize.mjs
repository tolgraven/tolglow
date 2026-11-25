import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {join} from 'node:path';

const [run, mode] = process.argv.slice(2);
const compact = mode === 'compact';
const read = name => readFile(join(run, name), 'utf8').then(JSON.parse);
const [metadata, receipt] = await Promise.all([read('metadata.json'), read('room.json')]);
assert.equal(metadata.stopped, true);
assert.equal(metadata.workerSHA256, '2af5a972e065ac97b739b07c84d5ad8688534d37bbd2f8a948b5d2cc96369332');
assert.ok(metadata.checks.every(check => check.status === 'unlocked'));
assert.equal(receipt.phase, 'finished');
assert.equal(receipt.bundleID, 'se.tolgraven.tolglow.studio');
assert.ok(!receipt.error);
assert.deepEqual(receipt.changed, []);
assert.equal(receipt.samples.length, 3);
if (!compact) {
  assert.equal(receipt.expanded, true);
  assert.equal(metadata.qualified, true);
}
const quantiles = values => {
  const sorted = values.filter(Number.isFinite).toSorted((a,b) => a-b);
  return sorted.length ? {p50: sorted[Math.ceil(sorted.length *.5)-1], p95: sorted[Math.ceil(sorted.length *.95)-1]} : null;
};
const summarize = sample => {
  assert.equal(sample.rendererThread, 'worker');
  assert.equal(sample.start.deviceRenderer, 'Apple GPU');
  assert.equal(sample.end.deviceRenderer, 'Apple GPU');
  assert.equal(sample.end.softwareRenderer, false);
  assert.deepEqual(sample.invalid, compact ? ['physical M4 worker route not established'] : []);
  assert.deepEqual(sample.start.camera, sample.end.camera);
  assert.deepEqual(sample.start.target, sample.end.target);
  assert.equal(sample.end.activeLights, 8);
  assert.equal(sample.end.targetRefreshHz, 60);
  assert.equal(sample.end.workLight, 0);
  for (const t of Object.values(sample.sampleBoundaryEpochMs))
    assert.ok(metadata.checks.some(check => Math.abs(Date.parse(check.checkedAt)-t) < 2500), 'Missing local lock sample');
  if (sample.plumes) assert.deepEqual(sample.plumes, {accepted:5,rejected:0});
  if (sample.workerTimings) assert.equal(sample.workerTimings.length, sample.presented.frames);
  const byStage = Object.fromEntries(['workerFrame','fence','bitmap','previousReady','rafDelivery','cpuSubmit'].map((key, i) =>
    [key, quantiles(sample.workerTimings?.map(row => row[i]) ?? sample.policySamples.map(row =>
      i === 0 ? row.workerFrameMs : i === 1 ? row.gpuFenceWaitMs : i === 5 ? row.cpuSubmitMs : null))]));
  return {name: sample.name, presented: sample.presented, display: sample.windowRaf, workerTimingsMilliseconds: byStage,
    raster:{canvas:sample.backing,scene:sample.sceneSizes}, quality: sample.qualities, plumes: sample.plumes};
};

console.log(JSON.stringify({mode, bundle: receipt.bundleID, gpu: 'Apple GPU', physicalDevice:'Apple M4 MacBook Air',
  packagedRevisions: metadata.packagedRevisions, workerSHA256: metadata.workerSHA256, osChecks:metadata.checks.length,
  recorderQualification: compact ? 'Revalidated the raw compact receipt against WebKit’s Apple GPU identifier on the inventoried host' : 'Qualified in the live recorder',
  rows: receipt.samples.map(summarize)}, null, 2));
