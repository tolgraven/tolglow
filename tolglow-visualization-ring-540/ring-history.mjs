import {buildTrace, formats, historyVerticalSpan, maxSamples} from '../tolglow-visualization-precision-540/trace-storage.mjs';

function normalized(value) {
  return Number.isFinite(value) && value >= 0 && value <= 1;
}

function admission(history, sample) {
  if (!Number.isFinite(sample.timeMs) || !Number.isFinite(sample.beat)) return 'invalid-clock';
  if (!history.length || sample.source !== history.source ||
      sample.timeMs < history.latestTime || sample.beat < history.latestBeat) return 'reset';
  if (sample.beat === history.latestBeat) {
    return sample.value === history.latestValue ? 'duplicate' : 'replace';
  }
  if (sample.timeMs - history.latestTime < Math.max(100, sample.samplePeriodMs || 0)) return 'throttled';
  return 'append';
}

// JS reference for the current full-value observation records and cap rebuild.
export class RecordHistory {
  records = [];
  get length() { return this.records.length; }
  get source() { return this.records.at(-1)?.source; }
  get latestTime() { return this.records.at(-1)?.timeMs; }
  get latestBeat() { return this.records.at(-1)?.beat; }
  get latestValue() { return this.records.at(-1)?.value; }
  timeAt(index) { return this.records[index].timeMs; }
  beatAt(index) { return this.records[index].beat; }
  valueAt(index) { return this.records[index].value; }

  observe(sample) {
    const action = admission(this, sample);
    if (action !== 'reset' && action !== 'replace' && action !== 'append') return action;
    const {timeMs, beat, value, source} = sample;
    const entry = {timeMs, beat, value, source};
    if (action === 'reset') this.records = [entry];
    if (action === 'replace') this.records = this.records.with(-1, entry);
    if (action === 'append') {
      const next = this.records.slice(this.records.length === maxSamples ? 1 : 0);
      next.push(entry);
      this.records = next;
    }
    return action;
  }
}

export function select(options) {
  const {format, reason} = buildTrace([0], options);
  return {format, reason};
}

export class RingHistory {
  times = new Float64Array(maxSamples);
  beats = new Float64Array(maxSamples);
  start = 0;
  length = 0;
  source;
  latestValue;
  packedWrites = 0;
  reason = 'explicit-format';

  constructor(format) {
    this.setFormat(format);
  }
  setFormat(format) {
    this.format = format;
    this.scale = formats[format].scale;
    this.values = formats[format].allocate(maxSamples);
  }
  index(i) { return (this.start + i) & (maxSamples - 1); }
  get latestTime() { return this.timeAt(this.length - 1); }
  get latestBeat() { return this.beatAt(this.length - 1); }
  timeAt(i) { return this.times[this.index(i)]; }
  beatAt(i) { return this.beats[this.index(i)]; }
  valueAt(i) {
    const stored = this.values[this.index(i)];
    return this.scale ? stored / this.scale : stored;
  }
  get payloadBytes() {
    return this.times.byteLength + this.beats.byteLength + this.values.byteLength + 8;
  }

  write(sample, replace = false) {
    if (!replace && this.length === maxSamples) this.start = this.index(1);
    else if (!replace) this.length++;
    const index = this.index(this.length - 1);
    this.times[index] = sample.timeMs;
    this.beats[index] = sample.beat;
    this.values[index] = this.scale ? Math.round(sample.value * this.scale) : sample.value ?? NaN;
    if (this.format !== 'full') this.packedWrites++;
    this.latestValue = sample.value;
    this.source = sample.source;
  }

  observe(sample) {
    const action = admission(this, sample);
    if (action !== 'reset' && action !== 'replace' && action !== 'append') return action;
    if (action === 'reset') this.clear();
    if (this.format !== 'full' && !normalized(sample.value)) {
      // The existing exact latest observation survives a domain-driven widening.
      this.switchFormat('full', 'observed-sample-outside-qualified-domain');
    }
    this.write(sample, action === 'replace');
    return action;
  }

  clear() {
    this.start = 0;
    this.length = 0;
    this.latestValue = undefined;
    this.source = undefined;
  }

  recordAt(i) {
    const value = i === this.length - 1 ? this.latestValue : this.valueAt(i);
    return {
      timeMs: this.timeAt(i),
      beat: this.beatAt(i),
      source: this.source,
      value: Number.isNaN(value) ? null : value,
    };
  }

  configure(options, exactReplay) {
    const {format, reason} = select(options);
    return this.switchFormat(format, reason, exactReplay);
  }

  switchFormat(format, reason, exactReplay) {
    if (format === this.format) {
      this.reason = reason;
      return {format, reason, transition: 'unchanged'};
    }
    let entries;
    let transition;
    if (this.format === 'full') {
      entries = Array.from({length: this.length}, (_, i) => this.recordAt(i));
      transition = 'exact-full-history';
    } else if (exactReplay !== undefined) {
      this.validateReplay(exactReplay);
      entries = exactReplay;
      transition = 'exact-replay';
    } else {
      entries = this.length ? [this.recordAt(this.length - 1)] : [];
      transition = 'reset-to-exact-latest';
    }
    if (format !== 'full' && entries.some(entry => !normalized(entry.value))) {
      this.reason = 'retained-sample-outside-qualified-domain';
      return {format: this.format, reason: this.reason, transition: 'unchanged'};
    }
    this.setFormat(format);
    this.clear();
    this.reason = reason;
    for (const entry of entries) this.write(entry);
    return {format, reason, transition};
  }

  validateReplay(entries) {
    if (entries.length !== this.length) throw new RangeError('Replay must match the retained observation count');
    for (let i = 0; i < entries.length; i++) {
      const entry = entries[i];
      if (entry.source !== this.source || entry.beat !== this.beatAt(i) || entry.timeMs !== this.timeAt(i) ||
          !normalized(entry.value) ||
          (i === entries.length - 1 && entry.value !== this.latestValue) ||
          (this.scale && Math.round(entry.value * this.scale) !== this.values[this.index(i)]) ||
          (this.format === 'fp16' && Math.f16round(entry.value) !== this.valueAt(i))) {
        throw new RangeError('Replay must match the retained source, clocks and packed values');
      }
    }
  }
}

// Use the same path construction in every benchmark case. Binary-search the
// retained beat sequence, then visit only the visible interval and edge pair.
export function chartPath(history, window) {
  if (!history.length) return '';
  const start = history.latestBeat - window;
  let first = 0;
  let end = history.length;
  while (first < end) {
    const middle = (first + end) >>> 1;
    if (history.beatAt(middle) < start) first = middle + 1;
    else end = middle;
  }
  let path = '';
  let pen = 'M';
  if (first > 0 && first < history.length && start < history.beatAt(first)) {
    const a = history.valueAt(first - 1);
    const b = history.valueAt(first);
    if (Number.isFinite(a) && Number.isFinite(b)) {
      const lo = history.beatAt(first - 1);
      const fraction = (start - lo) / (history.beatAt(first) - lo);
      path = 'M0,' + plotY(a + fraction * (b - a));
      pen = 'L';
    }
  }
  for (let i = first; i < history.length; i++) {
    const value = history.valueAt(i);
    if (!Number.isFinite(value)) {
      pen = 'M';
      continue;
    }
    path += pen + 100 * ((history.beatAt(i) - start) / window) + ',' + plotY(value);
    pen = 'L';
  }
  return path;
}

function plotY(value) {
  return 41 - historyVerticalSpan * Math.max(0, Math.min(1, value));
}
