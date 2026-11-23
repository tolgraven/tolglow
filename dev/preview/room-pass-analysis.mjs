// Browser-side collection uses completed, accepted bitmaps from the real proxy.
export function beginPassSample(mode) {
  const engine = document.querySelector('#preview').tolglowPreview;
  if (!engine.worker || !engine.bitmapPresenter) throw new Error('Pass attribution requires a worker and bitmaprenderer.');
  const presenter = engine.presenter, original = presenter.transferFromImageBitmap, values = new Map();
  let submitted;
  const add = (name, ms) => {
    if (!Number.isFinite(ms)) return;
    if (!values.has(name)) values.set(name, []);
    values.get(name).push(ms);
  };
  presenter.transferFromImageBitmap = function(bitmap) {
    const start = performance.now(), result = original.call(this, bitmap);
    submitted = performance.now() - start;
    return result;
  };
  const collect = ({data}) => {
    const t = data?.telemetry, p = t?.passProbe;
    if (!p || p.mode !== mode || t !== engine.telemetry) return;
    for (const [phase, ms] of Object.entries(p.submitMs)) add(phase+'SubmitMs', ms);
    for (const [phase, ms] of Object.entries(p.gpuMs)) add(phase+'GpuMs', ms);
    add('workerFrameMs', t.workerFrameMs); add('gpuFenceWaitMs', t.gpuCompletionWaitMs);
    add('bitmapExportMs', t.bitmapTransferMs); add('domSubmitMs', submitted);
    add('workerPostThroughDomMs', performance.timeOrigin + performance.now() - p.sentAt);
  };
  engine.worker.addEventListener('message', collect);
  window.finishRoomPassSample = () => {
    presenter.transferFromImageBitmap = original;
    engine.worker.removeEventListener('message', collect);
    delete window.finishRoomPassSample;
    return Object.fromEntries([...values].map(([key, xs]) => {
      xs.sort((a,b) => a-b);
      const at = p => xs[Math.ceil(xs.length * p)-1];
      return [key, {samples:xs.length, p50:at(.5), p95:at(.95), max:xs.at(-1)}];
    }));
  };
}
