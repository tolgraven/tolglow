import test from 'node:test';
import assert from 'node:assert/strict';
import { performance } from 'node:perf_hooks';
import { FogField, FOG_QUALITY, intersectRoom, intersectCone, integrateHomogeneous } from '../../../js/preview/fog-field.js';
import { VolumetricFog } from '../../../js/preview/fog.js';

const near = (a, b, tolerance = 1e-6) => assert.ok(Math.abs(a - b) < tolerance, `${a} != ${b}`);

test('packed plume bounds include boundary cells and reset with authoritative haze and tier copies', () => {
  // Packing is a CPU operation; this test never asks for a graphics context.
  const fog = new VolumetricFog(null, {quality:'phone'});
  const bounds = () => [fog.uniforms.detailMin.value.toArray(), fog.uniforms.detailMax.value.toArray()];
  const nearBounds = expected => bounds().flat().forEach((value,i)=>near(value,expected.flat()[i]));
  const empty = () => assert.deepEqual(bounds(), [[1,1,1],[-1,-1,-1]]);
  try {
    fog.field.clear(.15); fog.upload(); empty();
    fog.field.data[0] = .5; fog.upload();
    nearBounds([[-6,0,-5],[-4.8,5/6,-4]]);
    const [nx,ny,nz] = fog.field.grid;
    fog.field.clear(.15); fog.field.data[nx*ny*nz-1] = .5; fog.upload();
    nearBounds([[4.8,25/6,4],[6,5,5]]);
    // A tier resample can move nonzero cells; upload derives new coordinates
    // from the destination grid instead of retaining the previous box.
    const source = new FogField({grid:[12,8,12],room:[12,5,10],initialHaze:.15,retainedHaze:.15});
    source.data[6+4*12+6*12*8] = 2;
    fog.field.copyFrom(source); fog.upload();
    assert.ok(bounds()[0][0] < .5 && bounds()[1][0] > .5);
    assert.ok(bounds()[0][2] < 5/12 && bounds()[1][2] > 5/12);
    for(const [time,density] of [[0,.4],[100,.01],[200,0]]) {
      fog.setAtmosphere('host', {'sample-time-ms':time,epoch:1,'mixed-density':density,sources:[]});
      fog.update(0); empty();
    }
    fog.setAtmosphere('host',null); fog.field.clear(.15); fog.upload(); empty();
    assert.equal(fog.stats().raySteps,24);
    assert.equal(fog.stats().maximumRaySteps,72);
  } finally {fog.dispose();}
});

test('analytic positive cone intervals retain narrow shafts and reject the backward cone', () => {
  const hit = intersectCone([-1, 0, -2], [1, 0, 0], [0, 0, 0], [0, 0, -1], .01, [0, 10]);
  near(hit[0], 1 - 2 * Math.tan(.01)); near(hit[1], 1 + 2 * Math.tan(.01));
  assert.equal(intersectCone([-1, 0, 2], [1, 0, 0], [0, 0, 0], [0, 0, -1], .1, [0, 10]), null);
  assert.deepEqual(intersectCone([0, 0, 2], [0, 0, -1], [0, 0, 0], [0, 0, -1], .1, [0, 10], 6), [2, 8]);
  assert.deepEqual(intersectCone([0, 0, -2], [0, 0, 1], [0, 0, 0], [0, 0, -1], .1, [0, 10]), [0, 2]);
  // Independent point-membership oracle, including near-parallel directions.
  let seed = 37;
  const random = () => { seed = (Math.imul(seed, 1664525) + 1013904223) >>> 0; return seed / 4294967296; };
  for (let n = 0; n < 200; n++) {
    const origin = [random() * 6 - 3, random() * 6 - 3, random() * 6 - 3];
    const raw = [random() * 2 - 1, random() * 2 - 1, random() * 2 - 1], length = Math.hypot(...raw);
    const direction = raw.map(v => v / length), angle = .02 + random() * .7;
    const interval = intersectCone(origin, direction, [0, 0, 0], [0, 0, -1], angle, [0, 8], 6);
    for (let sample = 0; sample < 80; sample++) {
      const distance = (sample + .37) / 10, p = origin.map((v, i) => v + direction[i] * distance);
      const inside = -p[2] >= Math.cos(angle) * Math.hypot(...p) && Math.hypot(...p) <= 6;
      assert.equal(Boolean(interval && distance >= interval[0] && distance <= interval[1]), inside);
    }
  }
});

test('finite aperture covers the optical lens and excludes the volume behind its plane', () => {
  const angle = .1, radius = .12, apexDistance = radius / Math.tan(angle), apex = [0, 0, apexDistance];
  const lens = intersectCone([1, 0, 0], [-1, 0, 0], apex, [0, 0, -1], angle, [0, 2], 0, apexDistance);
  near(lens[0], 1 - radius); near(lens[1], 1 + radius);
  assert.equal(intersectCone([1, 0, .01], [-1, 0, 0], apex, [0, 0, -1], angle, [0, 2], 0, apexDistance), null);
  const axial = intersectCone([0, 0, 2], [0, 0, -1], apex, [0, 0, -1], angle, [0, 10], 0, apexDistance);
  near(axial[0], 2); near(axial[1], 10);
});
function moments(field) {
  const [nx, ny, nz] = field.grid, [sx, sy, sz] = field.spacing;
  let mass = 0, y = 0, support = 0;
  for (let z = 0; z < nz; z++) for (let j = 0; j < ny; j++) for (let x = 0; x < nx; x++) {
    const density = field.data[x + j * nx + z * nx * ny];
    mass += density; y += (j + .5) * sy * density;
    if (density > .001) support++;
  }
  return { y: mass ? y / mass : 0, support, mass: mass * sx * sy * sz };
}

test('empty atmosphere stays empty without a source', () => {
  const field = new FogField({ grid: [12, 8, 12], initialHaze: 0 });
  for (let n = 0; n < 100; n++) field.advance(.05);
  assert.ok(field.data.every(v => v === 0));
  assert.ok(field.temperature.every(v => v === 0));
  assert.deepEqual(integrateHomogeneous(0, 12, 100), { transmittance: 1, inscatter: 0 });
});

test('uniform haze has exact ventilation decay without invented structures', () => {
  const field = new FogField({ grid: [12, 8, 12], initialHaze: .2 });
  for (let n = 0; n < 20; n++) field.advance(.05);
  const expected = .2 * Math.exp(-1 / 100);
  for (const v of field.data) near(v, expected);
});

test('manual room haze remains stable while emitted plume excess disperses', () => {
  const field = new FogField({grid: [12, 8, 12], initialHaze: .15, retainedHaze: .15});
  for (let n = 0; n < 200; n++) field.advance(.05);
  for (const density of field.data) near(density, .15);
  field.burst({position: [-2, .2, -1], direction: [1, .15, 0], duration: 1, strength: 2});
  for (let n = 0; n < 20; n++) field.advance(.05);
  const fresh = field.stats();
  assert.ok(fresh.maximumDensity > .5);
  for (let n = 0; n < 140; n++) field.advance(.05);
  const dispersed = field.stats();
  assert.equal(dispersed.activeSources, 0);
  assert.ok(dispersed.maximumDensity < fresh.maximumDensity);
  assert.ok(dispersed.meanDensity > .15);
  assert.ok(field.data.every(density => density >= .15));
  const resized = new FogField({grid: [16, 10, 16], initialHaze: 0});
  resized.copyFrom(field);
  assert.equal(resized.retainedHaze, .15);
  assert.equal(resized.time, field.time);
  near(resized.stats().massEstimate, dispersed.massEstimate, .15);
});

test('fresh hot plume is local, rises, diffuses and cools after emission ends', () => {
  const field = new FogField({ grid: [24, 16, 24], room: [8, 4, 8], initialHaze: 0 });
  field.burst({ position: [-2, .2, -1], direction: [1, .15, 0], duration: 1, strength: 1.5 });
  for (let n = 0; n < 20; n++) field.advance(.05);
  const fresh = field.stats(), early = moments(field);
  assert.ok(fresh.maximumDensity > fresh.meanDensity * 10, 'A burst is concentrated, not uniform room haze');
  assert.ok(fresh.maximumDensity > 1, 'Fresh fog begins optically dense');
  assert.equal(fresh.activeSources, 0);
  for (let n = 0; n < 140; n++) field.advance(.05);
  const dispersed = field.stats(), late = moments(field);
  assert.ok(late.y > early.y + .2, 'Warm emitted fog rises before cooling');
  assert.ok(late.support > early.support * 2, 'Fog spreads into a larger room volume');
  assert.ok(dispersed.maximumDensity < fresh.maximumDensity, 'The initial dense shape disperses');
  assert.ok(dispersed.heat < fresh.heat, 'Heat decays after the machine stops');
  assert.ok(dispersed.meanDensity > 0, 'Dispersed haze remains after the original plume');
  assert.ok(field.data.every(v => Number.isFinite(v) && v >= 0 && v <= 6));
});

test('fixed timesteps retain deterministic history and bound long-frame catchup', () => {
  const options = { grid: [12, 8, 12], initialHaze: 0 };
  const a = new FogField(options), b = new FogField(options);
  a.burst(); b.burst();
  for (let n = 0; n < 20; n++) { a.advance(.1); b.advance(.025); b.advance(.025); b.advance(.025); b.advance(.025); }
  assert.deepEqual(a.data, b.data);
  const before = a.stats();
  assert.equal(a.advance(3600), 2);
  assert.equal(a.stats().steps - before.steps, 2);
  near(a.stats().time - before.time, .1);
  near(a.stats().droppedSeconds, 3599.9);
  assert.throws(() => a.advance(Infinity));
  assert.throws(() => a.advance(-1));
});

test('quality changes resample owned density and preserve evolving source history', () => {
  const source = new FogField({grid: [12, 8, 12], initialHaze: .07});
  source.burst({duration: 4});
  for (let n = 0; n < 20; n++) source.advance(.05);
  const target = new FogField({grid: [24, 16, 24], initialHaze: 0});
  target.copyFrom(source);
  assert.equal(target.time, source.time);
  assert.equal(target.steps, source.steps);
  assert.deepEqual(target.sources, source.sources);
  assert.notEqual(target.sources[0], source.sources[0]);
  near(target.stats().massEstimate, source.stats().massEstimate, .01);
  const original = source.data.slice(), sourceRemaining = source.sources[0].remaining;
  target.advance(.05);
  assert.deepEqual(source.data, original);
  assert.equal(source.sources[0].remaining, sourceRemaining);
  assert.ok(target.sources[0].remaining < sourceRemaining);
});

test('machine count and burst duration are bounded, room changes reset owned history', () => {
  const field = new FogField({ grid: [12, 8, 12], initialHaze: 0 });
  for (let n = 0; n < 10; n++) field.burst({ position: [n * .1, .2, 0], duration: 100 });
  assert.equal(field.sources.length, 4);
  assert.ok(field.sources.every(s => s.remaining === 8));
  field.advance(.1);
  const original = field.data.slice();
  assert.equal(field.setRoom([12, 5, 10]), false);
  assert.deepEqual(field.data, original);
  assert.equal(field.setRoom([20, 8, 15]), true);
  assert.ok(field.data.every(v => v === 0));
  assert.equal(field.sources.length, 0);
  assert.throws(() => field.setRoom([0, 1, 1]));
  assert.throws(() => field.burst({ direction: [0, 0, 0] }));
});

test('room intersection and extinction bound integration to opaque surfaces', () => {
  assert.deepEqual(intersectRoom([0, 2, 8], [0, 0, -1], [10, 5, 10]), [3, 13]);
  assert.deepEqual(intersectRoom([0, 2, 0], [0, 0, -1], [10, 5, 10]), [0, 5]);
  assert.equal(intersectRoom([8, 2, 0], [0, 0, -1], [10, 5, 10]), null);
  const roomInterval = intersectRoom([0, 2, 8], [0, 0, -1], [10, 5, 10]);
  const opaqueSurfaceDistance = 5;
  const integral = integrateHomogeneous(.2, Math.min(roomInterval[1], opaqueSurfaceDistance) - roomInterval[0], 2);
  near(integral.transmittance, Math.exp(-.4));
  const full = integrateHomogeneous(.2, 10, 2), half = integrateHomogeneous(.2, 5, 2);
  near(full.transmittance, half.transmittance * half.transmittance);
  near(full.inscatter, half.inscatter + half.transmittance * half.inscatter);
  assert.ok(integral.inscatter < full.inscatter);
});

test('tier field work is bounded; record local CPU timings without device claims', t => {
  const results = [];
  for (const quality of ['phone', 'mobile', 'desktop', 'high']) {
    const field = new FogField({ grid: FOG_QUALITY[quality].grid }); field.burst();
    for (let n = 0; n < 3; n++) field.advance(.05);
    const start = performance.now();
    for (let n = 0; n < 12; n++) assert.equal(field.advance(.05), 1);
    results.push({ quality, cells: field.data.length, meanStepMs: (performance.now() - start) / 12 });
  }
  t.diagnostic(JSON.stringify({ environment: 'Node CPU only; excludes GPU, shadows and compositing', results }));
});
