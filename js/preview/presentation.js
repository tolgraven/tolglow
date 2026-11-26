// Transferred frames carry show identity, not merely a render revision. An old
// show must never flash back onto the canvas after a restore/restart.
export const showIdentity = snapshot => JSON.stringify([snapshot?.['runtime-id'] ?? null, snapshot?.['show-id'] ?? null, snapshot?.epoch ?? null]);
export function presentFrame(message, identity, lastFrame, present) {
  try {
    if (message.frameId <= lastFrame || showIdentity({'runtime-id': message.runtimeId, 'show-id': message.showId, epoch: message.epoch}) !== identity) return false;
    present(message.bitmap); return true;
  } finally {message.bitmap.close();}
}

// Page-side readers of a canvas the worker presents. The browser commits a
// worker canvas at the display's next frame, usually after the report for it
// has reached the page, so the animation frame that follows a report still
// finds the previous image. Reports are therefore read one animation frame
// later, in order, when the page's copy normally holds exactly that image. A
// page starved of animation frames can find the one after it instead.
export class CommittedFrames {
  constructor(read, host = globalThis, limit = 240) {
    this.read = read; this.host = host; this.limit = limit;
    this.reported = []; this.committed = []; this.frame = null;
  }
  add(report) {
    if (this.reported.push(report) > this.limit) this.reported.shift();
    this.frame ??= this.host.requestAnimationFrame(() => this.step());
  }
  step() {
    const ready = this.committed; this.committed = this.reported; this.reported = [];
    this.frame = this.committed.length ? this.host.requestAnimationFrame(() => this.step()) : null;
    if (ready.length) this.read(ready);
  }
  clear() {
    if (this.frame !== null) this.host.cancelAnimationFrame(this.frame);
    this.frame = null; this.reported = []; this.committed = [];
  }
}
