// Advance the intended cadence, not callback jitter. Skip missed deadlines
// without a catch-up burst; early display callbacks retain the same phase.
export function nextDisplayDeadline(deadline, time, interval) {
  return deadline == null ? time + interval
    : deadline + Math.max(1, Math.floor((time - deadline) / interval) + 1) * interval;
}

// One outstanding wakeup per renderer. Use the display's timestamp on both
// Window and dedicated Worker, with timer pacing where worker RAF is absent.
export class DisplayClock {
  constructor({now = () => performance.now(), raf = globalThis.requestAnimationFrame?.bind(globalThis),
    cancelRaf = globalThis.cancelAnimationFrame?.bind(globalThis), timer = globalThis.setTimeout.bind(globalThis), cancelTimer = globalThis.clearTimeout.bind(globalThis)} = {}) {
    Object.assign(this, {now, raf, cancelRaf, timer, cancelTimer}); this.pending = false;
  }
  cancel() {
    if (this.rafId != null) this.cancelRaf?.(this.rafId);
    if (this.timerId != null) this.cancelTimer(this.timerId);
    this.rafId = this.timerId = null; this.pending = false;
  }
  at(deadline, draw, tolerance = .5) {
    this.cancel(); this.pending = true;
    const tick = timestamp => {
      this.rafId = this.timerId = null;
      if (timestamp + tolerance < deadline) {schedule(); return;}
      this.pending = false; draw(timestamp);
    };
    const schedule = () => {
      const wait = Math.max(0, deadline - this.now());
      if (this.raf && wait <= 18) {
        try {this.rafId = this.raf(tick); return;}
        catch (e) {if (e.name !== 'NotSupportedError') throw e; this.raf = null;}
      }
      if (this.raf) this.timerId = this.timer(schedule, wait - 16);
      else this.timerId = this.timer(() => tick(this.now()), Math.max(1, wait));
    };
    schedule();
  }
}

// Sample the display independently of a renderer which may deliberately draw
// once every several refreshes. Short foreground bursts also observe a window
// moved between displays without leaving a second RAF loop running continuously.
export class DisplayRefresh {
  constructor(changed, host = globalThis) {
    this.changed = changed;
    this.host = host;
    this.active = false;
    this.tick = this.tick.bind(this);
  }
  setActive(active) {
    if (active === this.active || !this.host.requestAnimationFrame) return;
    this.active = active;
    if (active) this.start();
    else {
      this.host.cancelAnimationFrame(this.raf);
      this.host.clearTimeout(this.timer);
    }
  }
  start() {
    this.gaps = [];
    this.samples = 0;
    this.previous = null;
    this.raf = this.host.requestAnimationFrame(this.tick);
  }
  tick(time) {
    if (!this.active) return;
    this.samples++;
    const gap = this.previous === null ? 0 : time - this.previous;
    this.previous = time;
    if (gap >= 3 && gap <= 120) this.gaps.push(gap);
    if (this.gaps.length < 16 && this.samples < 40) {
      this.raf = this.host.requestAnimationFrame(this.tick);
      return;
    }
    // Normalize missed refreshes using the faster quartile, then take the
    // median. Individual callback timestamps jitter; their fastest quartile
    // alone can mislabel a 60 Hz panel as 62 Hz.
    if (this.gaps.length >= 8) {
      this.gaps.sort((a, b) => a - b);
      const period = this.gaps[Math.floor(this.gaps.length / 4)];
      const normalized = this.gaps.map(gap => gap / Math.max(1, Math.round(gap / period))).sort((a, b) => a - b);
      const rate = Math.round(1000 / normalized[Math.floor(normalized.length / 2)]);
      if (this.hz == null || Math.abs(this.hz - rate) >= 2) {
        this.hz = rate;
        this.changed(rate);
      }
    }
    if (!this.active) return;
    this.timer = this.host.setTimeout(() => this.start(), 30000);
  }
}
