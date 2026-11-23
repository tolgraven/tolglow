// The document can be visible in a background window. Keep each signal
// independent so a scroll or focus event cannot reopen another closed gate.
export class PreviewActivity {
  constructor(canvas, changed, doc = document, win = window) {
    this.focused = doc.hasFocus(); this.visibleDocument = !doc.hidden;
    this.inViewport = false; this.pageActive = true; this.active = false;
    this.abort = new AbortController();
    const signal = this.abort.signal;
    const refresh = () => {
      const active = this.inViewport && this.visibleDocument && this.focused && this.pageActive;
      if (active === this.active) return;
      this.active = active;
      // The Studio leases server-side fields on exactly the same gates as the GPU.
      canvas.dispatchEvent?.(new CustomEvent('tolglow-preview-activity', {detail: active}));
      changed(active);
    };
    const focus = value => {this.focused = value; refresh();};
    win.addEventListener('focus', () => focus(true), {signal});
    win.addEventListener('blur', () => focus(false), {signal});
    doc.addEventListener('visibilitychange', () => {this.visibleDocument = !doc.hidden; refresh();}, {signal});
    win.addEventListener('pagehide', () => {this.pageActive = false; refresh();}, {signal});
    win.addEventListener('pageshow', () => {
      this.pageActive = true; this.visibleDocument = !doc.hidden; this.focused = doc.hasFocus(); refresh();
    }, {signal});
    this.observer = new win.IntersectionObserver(entries => {
      const entry = entries.at(-1);
      if (!entry || signal.aborted) return;
      this.inViewport = entry.isIntersecting && entry.intersectionRatio > 0;
      refresh();
    }, {threshold: [0, Number.EPSILON]});
    this.observer.observe(canvas);
  }
  dispose() {this.abort.abort(); this.observer.disconnect(); this.active = false;}
}
