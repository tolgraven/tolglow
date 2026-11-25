// Pure projection of committed renderer output. No effect clocks or interpolation.
export const clamp = (v, lo, hi) => Math.max(lo, Math.min(hi, v));
const radians = d => d * Math.PI / 180;
const vector = v => Array.isArray(v) && v.length === 3 && v.every(Number.isFinite);
export function roomDimensions(room) {
  return [room?.width, room?.height, room?.depth].map((value, i) =>
    Number.isFinite(value) && value > 0 ? value : [12, 5, 10][i]);
}
export function normalize(v) {
  const n = Math.hypot(...v);
  return n > 1e-9 ? v.map(x => x / n) : [0, -1, 0];
}
export function servoValue(output, axis) {
  const level = output[axis] ?? output.channels?.[axis];
  return Number.isFinite(level) && level >= 0 && level <= 1 ? level : undefined;
}
export function axisDegrees(fixture, output, axis) {
  const degrees = axis + '-deg';
  if (Number.isFinite(output.pose?.[degrees])) return output.pose[degrees];
  const servo = servoValue(output, axis);
  if (servo === undefined && Number.isFinite(output[degrees])) return output[degrees];
  const value = servo ?? 0;
  const calibration = fixture.calibration?.[axis];
  if (Array.isArray(calibration) && calibration.length === 2 && calibration[1] !== 0)
    return (value * 255 - calibration[0]) / calibration[1] * 180;
  const lo = calibration?.['min-deg'] ?? (axis === 'pan' ? -180 : -90);
  const hi = calibration?.['max-deg'] ?? (axis === 'pan' ? 180 : 90);
  return lo + value * (hi - lo);
}
export function rotateFixture(v, degrees = [0, 0, 0]) {
  let [x, y, z] = v;
  for (let axis = 0; axis < 3; axis++) {
    const angle = radians(Number(degrees[axis]) || 0), c = Math.cos(angle), s = Math.sin(angle);
    if (axis === 0) [y, z] = [y * c - z * s, y * s + z * c];
    if (axis === 1) [x, z] = [x * c + z * s, -x * s + z * c];
    if (axis === 2) [x, y] = [x * c - y * s, x * s + y * c];
  }
  return [x, y, z];
}
export function fixtureDirection(fixture, output = {}) {
  if (vector(output.pose?.direction)) return output.pose.direction;
  const axesKnown = (servoValue(output, 'pan') !== undefined && servoValue(output, 'tilt') !== undefined) ||
    (Number.isFinite(output.pose?.['pan-deg']) && Number.isFinite(output.pose?.['tilt-deg']));
  if (!axesKnown) {
    if (vector(output.direction) && Math.hypot(...output.direction) > 1e-9) return normalize(output.direction);
    if (vector(output.aim) && vector(fixture.position)) return normalize(output.aim.map((v, i) => v - fixture.position[i]));
  }
  const pan = radians(axisDegrees(fixture, output, 'pan'));
  const tilt = radians(axisDegrees(fixture, output, 'tilt'));
  // CPU resolve-geometry convention: elevation0=+Z, elevation-90=down.
  return normalize(rotateFixture([Math.sin(pan) * Math.cos(tilt), Math.sin(tilt), Math.cos(pan) * Math.cos(tilt)], fixture['rotation-deg']));
}
export function emittedColor(output = {}) {
  return vector(output['emitted-color']) ? output['emitted-color'].map(v => clamp(v, 0, 8)) : [0, 0, 0];
}
export function beamAngle(fixture, output = {}) {
  const range = fixture['zoom-range-deg'];
  const zoom = clamp(Number(output.zoom ?? output.channels?.zoom) || 0, 0, 1);
  const valid = value => Number.isFinite(value) && value > 0 && value < 180;
  const full = Array.isArray(range) && range.length === 2 && range.every(valid)
    ? range[0] + zoom * (range[1] - range[0])
    : valid(fixture['beam-angle']) ? fixture['beam-angle'] : 12 + zoom * 34;
  return radians(full / 2);
}
export function structuralKey(rig) {
  const fixtures = rig?.fixtures || [];
  return JSON.stringify({
    room: rig?.room, surfaces: rig?.surfaces,
    reflections: fixtures.filter(f => f['pseudo-fixture']).length,
    fixtures: fixtures.map(f => ({
      id: f.id, type: f['fixture-type'], aperture: f['lens-radius-m'],
      diffuser: f['diffuser-radius-m'] && [f['diffuser-radius-m'], f['pixel-layout-id'], f['pixel-index'], f.position]
    })),
    trusses: (rig?.trusses || []).map(t => ({id: t.id, length: t.length}))
  });
}
