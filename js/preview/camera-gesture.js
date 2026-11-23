// Sample both contacts together on the display clock. Browsers dispatch the
// two pointer moves separately even when the fingers travel in parallel.
function geometry(points) {
  const [a, b] = points.values();
  return {x: (a.x + b.x) / 2, y: (a.y + b.y) / 2, span: Math.hypot(b.x - a.x, b.y - a.y)};
}

export class CameraGesture {
  points = new Map();
  pair = null;
  kind = null;

  add(id, x, y) {
    this.points.set(id, {x, y});
    this.rebase();
  }

  remove(id) {
    this.points.delete(id);
    this.rebase();
  }

  rebase() {
    this.pair = this.points.size === 2 ? geometry(this.points) : null;
    this.kind = null;
  }

  sample() {
    if (!this.pair) return null;
    const next = geometry(this.points);
    const x = next.x - this.pair.x, y = next.y - this.pair.y;
    const spread = Math.abs(next.span - this.pair.span) / 2;
    const travel = Math.hypot(x, y);
    if (!this.kind) {
      if (Math.max(travel, spread) < 4) return null;
      this.kind = spread > travel ? 'pinch' : 'walk';
    }
    const zoom = Math.log(Math.max(1, next.span) / Math.max(1, this.pair.span));
    this.pair = next;
    return {kind: this.kind, x, y, zoom};
  }
}
