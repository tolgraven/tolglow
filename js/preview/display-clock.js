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
  at(deadline, draw) {
    this.cancel(); this.pending = true;
    const tick = timestamp => {
      this.rafId = this.timerId = null;
      if (timestamp + .5 < deadline) {schedule(); return;}
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
