// Lighting time belongs to the show; arrival and display times share the local
// monotonic clock. Hold the latest committed result when the pair is incomplete.
import {axisDegrees, servoValue} from './math.js';
import {showIdentity} from './presentation.js';

const EMPTY = Object.freeze({});
const rgb = value => Array.isArray(value) && value.length === 3 && value.every(Number.isFinite);
const mix = (a, b, t) => a + (b - a) * t;

function between(previous, current, fixture, amount) {
  if (!previous || !current || current['cut?'] || previous.strobe > 0 || current.strobe > 0 ||
      previous.channels?.strobe > 0 || current.channels?.strobe > 0 ||
      previous.shutter !== current.shutter || previous.channels?.shutter !== current.channels?.shutter) return current;
  const result = {...current};
  for (const field of ['intensity', 'dimmer']) {
    if (Number.isFinite(previous[field]) && Number.isFinite(current[field]))
      result[field] = mix(previous[field], current[field], amount);
  }
  const a = previous['emitted-color'], b = current['emitted-color'];
  if (rgb(a) && rgb(b)) result['emitted-color'] = b.map((v, i) => mix(a[i], v, amount));

  // Use the actuator travel selected by the backend. Spinning through the
  // pan end-stop is a visible discontinuity, even if the beam directions meet.
  const axes = ['pan', 'tilt'];
  if (axes.every(axis => servoValue(previous, axis) !== undefined && servoValue(current, axis) !== undefined &&
      Math.abs(axisDegrees(fixture, current, axis) - axisDegrees(fixture, previous, axis)) < 180)) {
    for (const axis of axes) {
      result[axis] = mix(servoValue(previous, axis), servoValue(current, axis), amount);
      result[axis + '-deg'] = mix(axisDegrees(fixture, previous, axis), axisDegrees(fixture, current, axis), amount);
    }
    // The committed pose is the latest endpoint; use the sampled mechanical
    // angles to position the yoke and beam between those endpoints.
    result.pose = {'pan-deg': result['pan-deg'], 'tilt-deg': result['tilt-deg']};
    delete result.direction;
    delete result.aim;
  }
  return result;
}

export class LightingSamples {
  constructor() {this.mode = 'hold';}
  setMode(mode) {
    if (!['hold', 'interpolate'].includes(mode)) throw new RangeError('Unknown preview lighting mode');
    if (this.mode !== mode) {this.mode = mode; this.previous = null;}
  }
  accept(snapshot, rigKey, receivedAt) {
    const last = this.latest;
    const period = snapshot['time-ms'] - last?.['time-ms'];
    const continuous = this.mode === 'interpolate' && last && this.rigKey === rigKey &&
      showIdentity(snapshot) === showIdentity(last) && snapshot.revision === last.revision &&
      !snapshot['blackout?'] && !last['blackout?'] && snapshot.master !== 0 && last.master !== 0 &&
      !snapshot['cut?'] && snapshot['frame-number'] === last['frame-number'] + 1 &&
      period > 0 && period <= 50 && receivedAt >= this.receivedAt && receivedAt - this.receivedAt < 75;
    this.latest = snapshot; this.previous = continuous ? last : null;
    this.rigKey = rigKey; this.period = period; this.receivedAt = receivedAt;
  }
  values(fixtures, now) {
    const current = this.latest?.output?.values || EMPTY;
    if (!this.previous) return current;
    const amount = Math.max(0, (now - this.receivedAt) / this.period);
    if (amount >= 1) {this.previous = null; return current;}
    const previous = this.previous.output?.values || {}, values = Object.create(null);
    for (const {id, fixture} of fixtures) values[id] = between(previous[id], current[id], fixture, amount);
    return values;
  }
}
