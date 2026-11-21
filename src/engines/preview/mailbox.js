// One state message may be in flight. Intermediate snapshots are deliberately
// replaced, rather than accumulating a render-time backlog on the UI thread.
export class LatestStateChannel {
  constructor(send) {this.send = send; this.sequence = 0; this.inFlight = null; this.pending = null; this.ready = false; this.closed = false; this.coalesced = 0;}
  offer(state) {if (this.closed) return; if (this.pending) this.coalesced++; this.pending = state; this.flush();}
  open() {this.ready = true; this.flush();}
  flush() {
    if (!this.closed && this.ready && !this.inFlight && this.pending) {
      const message = {type: 'state', id: ++this.sequence, state: this.pending};
      this.pending = null; this.inFlight = message.id; this.send(message);
    }
  }
  acknowledge(id) {if (id !== this.inFlight) return false; this.inFlight = null; this.flush(); return true;}
  close() {this.closed = true; this.pending = null; this.inFlight = null;}
}
