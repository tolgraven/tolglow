function calibration(axis) {
  if (Array.isArray(axis)) return [axis[0] / 255, axis[1] / (180 * 255)];
  const width = axis['max-deg'] - axis['min-deg'];
  return [-axis['min-deg'] / width, 1 / width];
}

function pack(fixtures, axes, universeCount) {
  const bytes = new Uint8Array(512 * universeCount);
  for (let i = 0; i < fixtures.length; ++i) {
    const f = fixtures[i];
    const start = (f.universe - 4) * 512 + f.address - 1;
    for (let j = 0; j < 2; ++j) {
      const spec = j === 0 ? f.channels.pan : f.channels.tilt;
      let v = axes[i * 4 + j] * 255;
      const pivot = spec['inverted-from'];
      if (pivot != null && v >= pivot) v = 255 - (v - pivot);
      const coarse = Math.trunc(v);
      bytes[start + spec.offset - 1] = coarse;
      bytes[start + spec['fine-offset'] - 1] = Math.round((v - coarse) * 255);
    }
  }
  return bytes;
}

function distribution(samples) {
  const sorted = [...samples].sort((a,b) => a-b);
  function at(p) { return sorted[Math.ceil(sorted.length*p)-1]; }
  return { p50: at(.5), p95: at(.95), p99: at(.99), max: sorted.at(-1) };
}

function compare(measured, fixtures, frame, sample, axes, actual) {
  const expected = Object.values(frame.universes).flat();
  let frameDifferences = 0;
  for (let i = 0; i < expected.length; ++i) {
    if (actual[i] !== expected[i]) ++frameDifferences;
    measured.cpuDigest = Math.imul(measured.cpuDigest ^ expected[i], 16777619) >>> 0;
    measured.shaderDigest = Math.imul(measured.shaderDigest ^ actual[i], 16777619) >>> 0;
  }
  measured.byteMismatches += frameDifferences;
  if (!frameDifferences) ++measured.exactFrames;
  for (let i = 0; i < fixtures.length; ++i) {
    const f = fixtures[i];
    if (!Number.isFinite(axes[i*4]) || !Number.isFinite(axes[i*4+1]) || axes[i*4+3] !== 1)
      throw new Error(`Invalid aim for head ${i} frame ${sample}`);
    if (!frame.axes[i][2]) ++measured.unreachableHeadFrames;
    const [panOffset, panScale] = calibration(f.calibration.pan);
    if (Math.abs(frame.axes[i][0] - (panOffset + panScale * frame.axes[i][3])) > 359 * Math.abs(panScale)) {
      ++measured.wrappedPanHeadFrames;
    }
    if (Boolean(axes[i*4+2]) !== frame.axes[i][2]) ++measured.reachabilityMismatches;
    for (let j = 0; j < 2; ++j) {
      const err = Math.abs(axes[i*4+j] - frame.axes[i][j]);
      if (err > measured.maxAxisError) {
        measured.maxAxisError = err;
        measured.maxErrorSample = { frame: sample, head: i, axis: j === 0 ? 'pan' : 'tilt',
          cpu: frame.axes[i][j], shader: axes[i*4+j] };
      }
      const channel = j === 0 ? f.channels.pan : f.channels.tilt;
      const base = (f.universe - 4)*512 + f.address - 1;
      const c = base + channel.offset - 1, k = base + channel['fine-offset'] - 1;
      if (expected[c] !== actual[c]) ++measured.coarseMismatches;
      if (expected[k] !== actual[k]) ++measured.fineMismatches;
      const coarseDelta = expected[c] - actual[c], fineDelta = expected[k] - actual[k];
      const delta = coarseDelta * 255 + fineDelta;
      measured.maxWordError = Math.max(measured.maxWordError, Math.abs(coarseDelta * 256 + fineDelta));
      measured.maxPackedStepError = Math.max(measured.maxPackedStepError, Math.abs(delta));
    }
  }
}

export async function measure({ fixtures, frames }, vertexSource, createMovingHeadSolver) {
  const solver = createMovingHeadSolver(fixtures, vertexSource, document.createElement('canvas'));
  try {
    const n = fixtures.length;
    let previousAxes;
    const timing = [];
    const measured = { renderer: solver.renderer, webgl: solver.webgl, heads: n, frames: frames.length,
      warmup: 8, cpuDigest: 2166136261, shaderDigest: 2166136261, universes: Object.keys(frames[0].universes).length, byteMismatches: 0,
      coarseMismatches: 0, fineMismatches: 0, maxAxisError: 0, maxPackedStepError: 0, maxWordError: 0,
      reachabilityMismatches: 0, unreachableHeadFrames: 0, wrappedPanHeadFrames: 0, exactFrames: 0, comparisons: n * frames.length * 2,
      framesOver25ms: 0, maxErrorSample: null };
    for (let sample = -measured.warmup; sample < frames.length; ++sample) {
      const frame = frames[sample < 0 ? 0 : sample];
      const started = performance.now();
      const { axes, timing: solveTime } = await solver.solve(frame.aims, sample > 0 ? previousAxes : undefined);
      previousAxes = axes;
      const solved = performance.now();
      const packed = pack(fixtures, axes, measured.universes);
      const ended = performance.now();
      if (sample < 0) continue;
      timing.push({ ...solveTime, packMs: ended - solved, totalMs: ended - started });
      if (ended - started > 25) ++measured.framesOver25ms;
      compare(measured, fixtures, frame, sample, axes, packed);
    }
    measured.timingMs = Object.fromEntries(Object.keys(timing[0]).map(key =>
      [key, distribution(timing.map(frame => frame[key]))]));
    // A tolerance gate makes catastrophic angle, wrap, layout, and range regressions fail.
    // Exact byte differences remain visible; feed physical DMX only under a separate exact gate.
    measured.passed = measured.maxAxisError < 1e-5 && measured.maxPackedStepError <= 1 &&
      measured.reachabilityMismatches === 0 &&
      measured.byteMismatches === measured.coarseMismatches + measured.fineMismatches;
    return measured;
  } finally { solver.dispose(); }
}
