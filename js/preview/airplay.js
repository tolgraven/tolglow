import {
  BufferTarget, canEncodeVideo, CanvasSource, HlsOutputFormat,
  MpegTsOutputFormat, Output, PathedTarget, Quality
} from 'mediabunny';

// Keep the visible renderer's aspect and resolution. Use an encoder the device
// actually has, stepping down before acquiring a single canvas media track.
async function encoderSize(canvas) {
  for (const [maxWidth, maxHeight] of [[3840, 2160], [1920, 1080], [1280, 720], [640, 360]]) {
    const scale = Math.min(1, maxWidth / canvas.width, maxHeight / canvas.height);
    const width = Math.max(2, 2 * Math.floor(canvas.width * scale / 2));
    const height = Math.max(2, 2 * Math.floor(canvas.height * scale / 2));
    const quality = new Quality({bitrate: Math.max(400000, width * height * 2)});
    if (await canEncodeVideo('avc', {width, height, quality, frameRate: 30, latencyMode: 'realtime'})) {
      return {width, height, quality};
    }
  }
  throw new Error('This device has no H.264 encoder for sharing the preview.');
}

// Capture after the scene's final film pass (or before the worker bitmap is
// transferred). The staging surface retains the last *presented* frame while
// the renderer sleeps. WebGL may clear its own drawing buffer between frames.
export async function startVideo(canvas, publish, ready, failed, signal) {
  const {width, height, quality} = await encoderSize(canvas);
  signal.throwIfAborted();
  const staging = document.createElement('canvas');
  staging.width = width;
  staging.height = height;
  const context = staging.getContext('2d', {alpha: false});
  const source = new CanvasSource(staging, {
    codec: 'avc', quality, keyFrameInterval: 1, latencyMode: 'realtime'
  });
  let announced = false;
  const target = new PathedTarget('master.m3u8', ({path}) => new BufferTarget({
    onFinalize: async bytes => {
      if (signal.aborted) return;
      await publish(path, bytes, signal);
      if (!announced && !signal.aborted && path === 'master.m3u8') {
        announced = true;
        ready(width, height);
      }
    }
  }));
  const output = new Output({target, format: new HlsOutputFormat({
    live: true, segmentFormat: new MpegTsOutputFormat(),
    targetDuration: 1, maxLiveSegmentCount: 6
  })});
  output.addVideoTrack(source, {frameRate: 30});
  const started = performance.now();
  let lastCopy = -Infinity;
  let busy = false;
  let clock;
  const capture = pixels => {
    const now = performance.now();
    if (now - lastCopy < 1000 / 30) return;
    try {
      context.drawImage(pixels, 0, 0, width, height);
      lastCopy = now;
    } catch (error) {
      if (!signal.aborted) failed(error.message);
    }
  };
  const stop = () => {
    clearInterval(clock);
    if (canvas.tolglowCaptureVideo === capture) delete canvas.tolglowCaptureVideo;
    output.cancel().catch(() => {});
  };
  const sample = async () => {
    if (busy || signal.aborted || lastCopy === -Infinity) return;
    busy = true;
    try { await source.add((performance.now() - started) / 1000, 1 / 30); }
    catch (error) { if (!signal.aborted) failed(error.message); }
    finally { busy = false; }
  };
  signal.addEventListener('abort', stop, {once: true});
  try {
    await output.start();
    signal.throwIfAborted();
    canvas.tolglowCaptureVideo = capture;
    // Bitmap and 2D canvases retain their last picture. The next actual scene
    // presentation immediately replaces this initial sample on a WebGL canvas.
    capture(canvas);
    clock = setInterval(sample, 1000 / 30);
  } catch (error) { stop(); throw error; }
}

// Start the LAN listener only after the mounted scene has presented its first
// frame. A failed or disposed renderer must leave no empty receiver grant.
export async function waitForPreview(canvas, signal) {
  const engine = canvas?.tolglowPreview;
  if (!engine) throw new Error('The 3D preview is still loading.');
  if (engine.ready) return;
  signal.throwIfAborted();
  let cancel;
  let timer;
  try {
    const stopped = new Promise((_, reject) => {
      cancel = () => reject(new DOMException('Capture cancelled', 'AbortError'));
      signal.addEventListener('abort', cancel, {once: true});
      timer = setTimeout(() => reject(new Error('The 3D preview has not produced a frame.')), 90000);
    });
    if (!await Promise.race([engine.whenReady, stopped])) throw new Error('The 3D preview could not render.');
  } finally {
    clearTimeout(timer);
    signal.removeEventListener('abort', cancel);
  }
}
