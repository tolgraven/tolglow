import assert from 'node:assert/strict';
import {spawnSync} from 'node:child_process';
import {writeFileSync} from 'node:fs';
import {arch, cpus, platform} from 'node:os';
import {performance} from 'node:perf_hooks';
import {fileURLToPath} from 'node:url';
import {profiles, options, traces} from '../tolglow-visualization-precision-540/fixtures.mjs';
import {graphPath, historyViewHeight, maxSamples, quantizationBound} from '../tolglow-visualization-precision-540/trace-storage.mjs';
import {chartPath, RecordHistory, RingHistory, select} from './ring-history.mjs';

const push = options(profiles[0]);
const studio = options(profiles.at(-2));
const rounds = 3;
const cases = [
  ['fullRecords', 'full'],
  ['fullRing', 'full'],
  ['pushAuto', select(push).format],
  ['studioAuto', select(studio).format],
  ['fp16Ring', 'fp16'],
];
const draws = [16, 64, 16];
const capture = (timeMs, beat, value, source = 'a', samplePeriodMs = 100) =>
  ({timeMs, beat, value, source, samplePeriodMs});
function historyFor(name, format) {
  return name === 'fullRecords' ? new RecordHistory() : new RingHistory(format);
}

// 40 Hz snapshots carrying the eight v1 signal sequences. Three independent
// synchronous consumers request charts per arrival, including one that draws
// the whole retained window. Four 32.5 second sections exercise three resets.
function makeArrivals() {
  const events = [];
  const sections = [
    {offset: 1_000_000, beat: 6000, source: 'run-a'},
    {offset: 1_040_000, beat: 6080, source: 'run-b'},
    {offset: 993_000, beat: 6160, source: 'run-b'},
    {offset: 1_031_000, beat: 5900, source: 'run-b'},
  ];
  for (const [section, {offset, beat, source}] of sections.entries()) {
    for (let i = 0; i < 1300; i++) {
      const arrival = traces.map(({samples}, track) =>
        capture(offset + i * 25, beat + i / 20,
          samples[(i + 23 * track + 47 * section) % samples.length],
          source, i % 37 === 0 ? 150 : 100));
      events.push(arrival);
      if (i !== 0 && i % 32 === 0) events.push(arrival);
      if (i !== 0 && i % 64 === 0) {
        events.push(arrival.map(sample => ({...sample, value: Math.min(sample.value + 1e-7, 1)})));
      }
      if (i !== 0 && i % 257 === 0) events.push(arrival.map(sample => ({...sample, beat: NaN})));
    }
  }
  return events;
}

function percentile(values, fraction) {
  return values[Math.ceil(values.length * fraction) - 1];
}
function summarize(values) {
  values.sort((a, b) => a - b);
  return {
    points: values.length,
    p50Us: percentile(values, 0.50),
    p95Us: percentile(values, 0.95),
    p99Us: percentile(values, 0.99),
  };
}

function benchmark(name, format, arrivalEvents, trackTiming = false) {
  const histories = traces.map(() => historyFor(name, format));
  const durations = {admission: [], admitted: [], render: [], combined: [], saturatedCombined: []};
  const actions = {'invalid-clock': 0, reset: 0, replace: 0, append: 0, duplicate: 0, throttled: 0};
  let checksum = 0;
  let saturated = 0;
  for (const event of arrivalEvents) {
    const start = performance.now();
    let admitted = false;
    for (let track = 0; track < histories.length; track++) {
      const action = histories[track].observe(event[track]);
      actions[action]++;
      admitted ||= action === 'append' || action === 'replace' || action === 'reset';
    }
    const observed = performance.now();
    for (const window of draws) {
      for (const history of histories) {
        const path = chartPath(history, window);
        // Read the rendered string so the benchmark consumes the generated path.
        checksum += path.length + path.charCodeAt(path.length >>> 1);
      }
    }
    const end = performance.now();
    const full = histories[0].length === maxSamples;
    saturated += Number(full);
    if (trackTiming) {
      const admission = (observed - start) * 1000;
      durations.admission.push(admission);
      if (admitted) durations.admitted.push(admission);
      durations.render.push((end - observed) * 1000);
      const cycle = (end - start) * 1000;
      durations.combined.push(cycle);
      if (full) durations.saturatedCombined.push(cycle);
    }
  }
  assert.ok(Number.isFinite(checksum) && checksum > 0);
  return {
    actions,
    saturated,
    packedWrites: histories.reduce((count, history) => count + (history.packedWrites ?? 0), 0),
    payloadBytes: histories.reduce((count, history) => count + (history.payloadBytes ?? 0), 0) || null,
    durations: trackTiming && Object.fromEntries(Object.entries(durations).map(([key, values]) => [key, summarize(values)])),
  };
}

function points(path) {
  return Array.from(path.matchAll(/([ML])([^,]+),([^ML]+)/g), ([, pen, x, y]) => [pen, Number(x), Number(y)]);
}

function verify() {
  const tracks = traces.map(() => ({
    reference: new RecordHistory(),
    rings: ['full', 'unorm8', 'unorm16', 'fp16'].map(format => new RingHistory(format)),
  }));
  const maxDisplacement = {full: 0, unorm8: 0, unorm16: 0, fp16: 0};
  for (const [i, frame] of makeArrivals().entries()) {
    frame.forEach((event, track) => {
      const {reference, rings} = tracks[track];
      const action = reference.observe(event);
      for (const ring of rings) {
        const previousWrites = ring.packedWrites;
        assert.equal(ring.observe(event), action);
        const writeExpected = ring.format !== 'full' && ['append', 'replace', 'reset'].includes(action);
        assert.equal(ring.packedWrites - previousWrites, Number(writeExpected));
        assert.equal(ring.length, reference.length);
        assert.equal(ring.latestTime, reference.latestTime);
        assert.equal(ring.latestBeat, reference.latestBeat);
        assert.equal(ring.latestValue, reference.latestValue);
        assert.equal(ring.source, reference.source);
        assert.equal(ring.timeAt(0), reference.timeAt(0));
        assert.equal(ring.beatAt(0), reference.beatAt(0));
      }
      if (i % 41 === 0) verifyChart(reference, rings, maxDisplacement);
    });
  }

  const events = [capture(0, 0, 0.21), capture(100, 1, 0.5002), capture(200, 2, 0.5003)];
  const seed = () => {
    const ring = new RingHistory('unorm8');
    events.forEach(event => ring.observe(event));
    return ring;
  };
  const ring = seed();
  const original = ring.packedWrites;
  assert.equal(ring.observe(capture(201, 2, 0.5003)), 'duplicate');
  assert.equal(ring.observe(capture(201, 2, 0.5004)), 'replace');
  assert.equal(ring.packedWrites, original + 1);
  assert.equal(ring.latestValue, 0.5004);
  assert.equal(ring.observe(capture(NaN, 3, 0.99)), 'invalid-clock');
  assert.equal(ring.latestValue, 0.5004);
  assert.throws(() => ring.configure(studio, events), /retained source, clocks and packed values/);
  assert.equal(ring.format, 'unorm8');
  assert.equal(ring.length, 3);
  assert.equal(ring.configure(studio).transition, 'reset-to-exact-latest');
  assert.equal(ring.format, 'unorm16');
  assert.equal(ring.latestValue, 0.5004);
  assert.equal(ring.latestTime, 201);
  assert.equal(ring.length, 1);
  assert.equal(ring.configure({...studio, mode: 'full'}).transition, 'reset-to-exact-latest');
  assert.equal(ring.valueAt(0), 0.5004);
  const replayed = seed();
  assert.equal(replayed.configure(studio, events).transition, 'exact-replay');
  assert.equal(replayed.configure({...studio, mode: 'full'}, events).transition, 'exact-replay');
  assert.deepEqual(events.map(({timeMs,beat,value,source}) => ({timeMs,beat,value,source})),
    events.map((_, i) => replayed.recordAt(i)));
  assert.equal(replayed.configure(push).transition, 'exact-full-history');
  assert.equal(replayed.observe(capture(300, 3, null)), 'append');
  assert.equal(replayed.format, 'full');
  assert.equal(replayed.length, 2);
  assert.equal(replayed.valueAt(0), 0.5003);
  assert.ok(Number.isNaN(replayed.valueAt(1)));
  assert.equal(replayed.configure(push).reason, 'retained-sample-outside-qualified-domain');
  assert.equal(replayed.observe(capture(400, 4, 1.1)), 'append');
  assert.equal(replayed.valueAt(2), 1.1);
  return maxDisplacement;
}

function verifyChart(reference, rings, maxDisplacement) {
  const beats = reference.records.map(sample => sample.beat);
  const values = reference.records.map(sample => sample.value);
  for (const window of [0.5, 1, 2, 4, 8, ...draws.slice(0, 2)]) {
    const expected = graphPath(values, 'full', beats, window);
    assert.equal(chartPath(reference, window), expected);
    assert.equal(chartPath(rings[0], window), expected);
    const source = points(expected);
    for (const ring of rings.slice(1)) {
      const rendered = points(chartPath(ring, window));
      const height = ring.format === 'unorm8' ? 40 : 210;
      assert.equal(rendered.length, source.length);
      for (const [j, [pen, x, y]] of rendered.entries()) {
        assert.equal(pen, source[j][0]);
        assert.equal(x, source[j][1]);
        const diff = Math.abs(y - source[j][2]) * height / historyViewHeight;
        assert.ok(diff <= quantizationBound(ring.format, height) + 1e-12);
        maxDisplacement[ring.format] = Math.max(maxDisplacement[ring.format], diff);
      }
    }
  }
}

const [, , worker, name, format] = process.argv;
if (worker === '--worker') {
  const events = makeArrivals();
  benchmark(name, format, events);
  console.log(JSON.stringify(benchmark(name, format, events, true)));
} else {
  const verifiedDisplacementPx = verify();
  const receipts = Object.fromEntries(cases.map(([key]) => [key, []]));
  for (let round = 0; round < rounds; round++) {
    for (let i = 0; i < cases.length; i++) {
      const [key, format] = cases[(i + round) % cases.length];
      const run = spawnSync(process.execPath, [fileURLToPath(import.meta.url), '--worker', key, format], {encoding: 'utf8'});
      assert.equal(run.status, 0, run.stderr);
      receipts[key].push(JSON.parse(run.stdout));
    }
  }
  const results = Object.fromEntries(Object.entries(receipts).map(([key, runs]) => {
    const {actions, saturated, packedWrites, payloadBytes} = runs[0];
    assert.deepEqual(actions, receipts.fullRecords[0].actions);
    assert.equal(saturated, receipts.fullRecords[0].saturated);
    assert.ok(saturated > 100 && actions.duplicate > 0 && actions.replace > 0 &&
      actions['invalid-clock'] > 0 && actions.reset === 32);
    const conversionsExpected = ['fullRecords', 'fullRing'].includes(key) ? 0 : actions.append + actions.replace + actions.reset;
    assert.equal(packedWrites, conversionsExpected);
    return [key, {actions, saturated, packedWrites, payloadBytes,
      medianRunP95Us: Object.fromEntries(Object.keys(runs[0].durations).map(metric => [
        metric, runs.map(r => r.durations[metric].p95Us).sort((a, b) => a - b)[1],
      ])),
      runs: runs.map(run => run.durations),
    }];
  }));
  const report = {
    environment: {platform: platform(), architecture: arch(), cpu: cpus()[0]?.model, node: process.version},
    config: {rounds, tracks: traces.length, arrivals: makeArrivals().length, drawsPerArrival: draws, maxSamples},
    verification: {observedChartDisplacementPx: verifiedDisplacementPx, push: select(push), studio2x: select(studio)},
    results,
  };
  writeFileSync(new URL('replay-report.json', import.meta.url), JSON.stringify(report, null, 2) + '\n');
  console.log('Verified exact clocks and captions, graph error, replay transitions and one write per admitted sample. Wrote replay-report.json.');
}
