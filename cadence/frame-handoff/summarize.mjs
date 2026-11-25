import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';

const report = JSON.parse(await readFile(process.argv[2], 'utf8'));
assert.equal(report.qualified, true, 'OS, camera, output and comparison gates must qualify');
assert.equal(report.scenarios.length, 4);
assert.equal(report.diagnosticWorkerSha256, report.servedDiagnosticWorkerSha256);
assert.ok(report.desktopSession.samples.every(sample => sample.checks.length >= 15 && sample.checks.every(row => row.status === 'unlocked')));
const pick = (values, p) => values.toSorted((a,b) => a-b)[Math.ceil(p*values.length)-1];
const summarize = values => ({p50: pick(values,.5), p95: pick(values,.95)});
const table = report.scenarios.map(scenario => {
  const samples = scenario.queryFrames;
  assert.equal(samples.length, scenario.presented.frames);
  assert.ok(samples.every(s => Number.isFinite(s.fencePolls) && s.fencePolls >= 1 && Number.isFinite(s.bitmapTransferMs)));
  return {pass: scenario.pass, presentedHz: scenario.presented.hz, displayHz: scenario.windowRaf.hz,
    presentedFrames: scenario.presented.frames, gapsOver25ms: scenario.presented.over25ms,
    intervalsP95ms: scenario.presented.p95Ms, workerMs: summarize(samples.map(s => s.workerFrameMs)),
    fenceMs: summarize(samples.map(s => s.gpuFenceWaitMs)), bitmapMs: summarize(samples.map(s => s.bitmapTransferMs)),
    exportAndFenceMs: summarize(samples.map(s => s.bitmapTransferMs + s.gpuFenceWaitMs)),
    fencePolls: summarize(samples.map(s => s.fencePolls)), readyMs: summarize(samples.map(s => s.readyForFrameMs)),
    camera: scenario.start.camera, movingHeads: scenario.end.activeLights};
});
console.log(JSON.stringify({tolglow: report.tolglow, afterglow: report.afterglow,
  controls: report.scenarios[0].controls.before, raster: report.diagnostic.frozenPolicy, rows: table}, null, 2));
