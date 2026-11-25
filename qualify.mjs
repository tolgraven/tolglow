import assert from 'node:assert/strict';
import {cpus, platform, arch} from 'node:os';
import {performance} from 'node:perf_hooks';
import {writeFileSync} from 'node:fs';
import {formats, buildTrace, encodeInto, graphPath, graphPoints, historyViewHeight, pixelBudget, quantizationBound} from './trace-storage.mjs';
import {options, profiles, schema, traces, zooms} from './fixtures.mjs';

const formatNames = Object.keys(formats);
const {samples} = traces[1];
const referenceOptions = options(profiles[0]);

assert.equal(buildTrace(samples).format, 'full');
assert.equal(buildTrace(samples, {...referenceOptions, mode: 'unknown'}).reason, 'unknown-mode');
assert.equal(buildTrace(samples, {...referenceOptions, rasterHeight: 0}).reason, 'unknown-raster-height');
assert.equal(buildTrace(samples, {...referenceOptions, rasterHeight: 100000}).reason, 'raster-budget-exceeded');
assert.equal(buildTrace(samples, {...referenceOptions, rasterHeight: 147}).format, 'unorm8');
assert.equal(buildTrace(samples, {...referenceOptions, rasterHeight: 148}).format, 'unorm16');
for (const usage of ['sparkline', 'spatial', 'parameter-label', 'control', 'wire']) {
  assert.equal(buildTrace(samples, {...referenceOptions, usage}).format, 'full');
}
for (const shape of [
  {type: 'number'}, {...schema, bounds: [-1, 1]}, {...schema, bounds: [0.50001, 0.50004]},
  {...schema, unit: 'deg'}, {...schema, units: 'deg'}, {...schema, semanticType: 'phase'}, {...schema, dmxBits: 16},
  {...schema, componentSchemas: [{...schema, dmxBits: 16}]},
  {...schema, type: 'vector'}, {...schema, type: 'range'}, {...schema, type: 'integer'},
  {...schema, type: 'boolean'}, {...schema, type: 'enum'}, {...schema, choices: [0, 1]},
]) {
  const fallback = buildTrace(samples, {...referenceOptions, schema: shape});
  assert.equal(fallback.format, 'full');
  assert.equal(fallback.values, samples);
}
for (const input of [[], new Array(3), [...samples, 0], [0, null, 1], [NaN], [-Infinity], [1.01], [-0.01], [[0, 1]]]) {
  assert.equal(buildTrace(input, referenceOptions).reason, 'unqualified-samples');
  assert.equal(buildTrace(input, referenceOptions).values, input);
}
const precise = 0.5001;
const exact = Object.freeze([0, precise]);
const packed = buildTrace(exact, referenceOptions);
assert.ok(packed.values instanceof Uint8Array);
assert.equal(packed.latestValue, precise);
assert.notEqual(precise.toPrecision(3), (packed.values[1] / 255).toPrecision(3));
const wide = buildTrace(exact, options(profiles.at(-2)));
assert.ok(wide.values instanceof Uint16Array);
assert.equal(wide.latestValue, precise);
const full = buildTrace(exact, options(profiles.at(-2), 'full'));
assert.equal(full.values, exact);
assert.equal(full.latestValue, precise);
const dmx16Adjacent = Object.freeze([32767 / 65535, 32768 / 65535]);
const dmx16 = buildTrace(dmx16Adjacent, {...referenceOptions, schema: {...schema, dmxBits: 16}});
assert.equal(dmx16.values, dmx16Adjacent);
assert.deepEqual(Array.from(dmx16.values, value => value.toFixed(5)), ['0.49999', '0.50001']);
assert.equal(graphPath([0, null, 1], 'full', [0, 1, 2], 2), 'M0,41M100,3');
assert.equal(graphPath([0, 1], 'full', [0, 2], 1), 'M0,22L100,3');
assert.equal(graphPath([null, 1], 'full', [0, 2], 1), 'M100,3');
assert.equal(graphPath([1.2, 0.2], 'full', [0, 2], 1), 'M0,14.400000000000002L100,33.4');
const dmx8 = Array.from({length: 256}, (_, i) => i / 255);
const dmx8Trace = buildTrace(dmx8, {...referenceOptions, schema: {...schema, dmxBits: 8}});
for (let i = 0; i <= 255; i++) assert.equal(dmx8Trace.values[i] / 255, dmx8[i]);

const report = {
  platform: {os: platform(), arch: arch(), cpu: cpus()[0]?.model, node: process.version},
  pixelBudget,
  profiles: profiles.map(profile => {
    const rasterHeight = profile.height * profile.dpr;
    return {
      ...profile,
      rasterHeight,
      selected: buildTrace(samples, options(profile)).format,
      reasons: {
        auto: buildTrace(samples, options(profile)).reason,
        full: buildTrace(samples, options(profile, 'full')).reason,
      },
      theoreticalMaximumPx: Object.fromEntries(formatNames.map(format => [
        format, quantizationBound(format, rasterHeight),
      ])),
      observedMaximumPx: Object.fromEntries(formatNames.map(format => {
        let max = 0;
        for (const trace of traces) {
          const data = encodeInto(trace.samples, format, formats[format].allocate(trace.samples.length));
          for (const window of zooms) {
            const full = graphPoints(trace.samples, 'full', trace.beats, window);
            const candidate = graphPoints(data, format, trace.beats, window);
            assert.equal(candidate.length, full.length);
            full.forEach(([x, y], i) => {
              assert.equal(candidate[i][0], x);
              max = Math.max(max, Math.abs(y - candidate[i][1]) * rasterHeight / historyViewHeight);
            });
          }
        }
        assert.ok(max <= quantizationBound(format, rasterHeight) + 1e-12);
        return [format, max];
      })),
    };
  }),
};
assert.equal(report.profiles[0].selected, 'unorm8');
assert.equal(report.profiles.at(-2).selected, 'unorm16');
assert.equal(report.profiles.at(-1).selected, 'unorm16');
assert.ok(report.profiles.at(-1).observedMaximumPx.unorm8 > pixelBudget);
assert.ok(report.profiles.at(-1).observedMaximumPx.unorm16 < pixelBudget);

// Sweep candidate midpoints and a fine grid, including exact endpoints.
const positions = Array.from({length: 65536}, (_, i) => i / 65535);
positions.push(...Array.from({length: 255}, (_, i) => (i + 0.5) / 255));
positions.push(...Array.from({length: 1024}, (_, i) => 0.5 + (i + 0.5) / 2048));
report.scalarSweep = Object.fromEntries(formatNames.map(format => {
  const buffer = encodeInto(positions, format, formats[format].allocate(positions.length));
  let maximumError = 0;
  for (let i = 0; i < positions.length; i++) {
    const decoded = buffer[i] / (formats[format].scale ?? 1);
    maximumError = Math.max(maximumError, Math.abs(decoded - positions[i]));
  }
  assert.ok(maximumError <= formats[format].maxError + Number.EPSILON);
  return [format, {samples: positions.length, maximumError}];
}));

const repeats = 11;
const framesPerRound = 200;
const buffers = Object.fromEntries(formatNames.map(format => [
  format, traces.map(trace => encodeInto(trace.samples, format, formats[format].allocate(trace.samples.length))),
]));
const trackOptions = {
  full: options(profiles[0], 'full'),
  push: options(profiles[0]),
  studio2x: options(profiles.at(-2)),
};
let checksum = 0;
function batch(operation, format) {
  let count = 0;
  for (let frame = 0; frame < framesPerRound; frame++) {
    traces.forEach((trace, index) => {
      const window = zooms[(frame + index) % zooms.length];
      if (operation === 'pack') {
        const out = encodeInto(trace.samples, format, buffers[format][index]);
        count += out[frame];
      } else if (operation === 'path') {
        count += graphPath(buffers[format][index], format, trace.beats, window).length;
      } else {
        const out = buildTrace(trace.samples, trackOptions[format]);
        count += graphPath(out.values, out.format, trace.beats, window).length;
      }
    });
  }
  checksum += count;
}

function benchmark(operation, names) {
  const timings = Object.fromEntries(names.map(name => [name, []]));
  for (let round = -3; round < repeats; round++) {
    for (let i = 0; i < names.length; i++) {
      const name = names[(i + round + repeats) % names.length];
      const start = performance.now();
      batch(operation, name);
      if (round >= 0) timings[name].push((performance.now() - start) * 1000 / framesPerRound);
    }
  }
  return Object.fromEntries(names.map(name => {
    const sorted = timings[name].toSorted((a, b) => a - b);
    return [name, {
      medianUsPerEightTraces: sorted[Math.floor(repeats / 2)],
      p90UsPerEightTraces: sorted[Math.ceil(repeats * 0.9) - 1],
      maxUsPerEightTraces: sorted.at(-1),
    }];
  }));
}

report.payloadBytesForEightFullTraces = Object.fromEntries(formatNames.map(format => {
  const values = buffers[format].reduce((sum, buffer) => sum + buffer.byteLength, 0);
  const beatsAndTimeMs = 2 * traces.reduce((sum, trace) => sum + trace.beats.byteLength, 0);
  const exactLatest = 8 * traces.length;
  return [format, {values, beatsAndTimeMs, exactLatest, total: values + beatsAndTimeMs + exactLatest}];
}));
report.timing = {
  repeats,
  framesPerRound,
  packIntoPreallocated: benchmark('pack', formatNames),
  pathFromStoredValues: benchmark('path', formatNames),
  selectionPackAndPath: benchmark('select', Object.keys(trackOptions)),
};
assert.ok(Number.isFinite(checksum) && checksum > 0);
writeFileSync(new URL('qualification.json', import.meta.url), JSON.stringify(report, null, 2) + '\n');
console.log('Verified format fallback, graph path parity, scalar error bounds and footprint. Wrote qualification.json.');
