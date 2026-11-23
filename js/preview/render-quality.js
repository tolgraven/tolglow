// Budgets are in physical pixels. Only the last pass and editor overlay run
// at display resolution when the lighting scene needs a smaller raster.
const UHD = 3840 * 2160;
export const TIERS = {
  low: {dpr: .75, maxPixels: 180000, shadows: 1, shadowSize: 256, fps: 10, fixtures: 32, fog: 'phone'},
  mobile: {dpr: 2, maxPixels: 1500000, shadows: 2, shadowSize: 512, fps: 120, fixtures: 32},
  desktop: {dpr: 2, maxPixels: 3200000, shadows: 4, shadowSize: 1024, fps: 120, fixtures: 64},
  high: {dpr: 4, maxPixels: UHD, shadows: 4, shadowSize: 2048, fps: 120, fixtures: 128},
};

// Auto may use low's spatial budget on a hardware GPU while still presenting
// faster than 10 Hz. Manual low and software rendering retain their explicit cap.
export function presentationHz(tier, choice, software, displayHz) {
  const limit = tier === 'low' && choice === 'auto' && !software ? TIERS.mobile.fps : TIERS[tier].fps;
  return Math.min(limit, displayHz || limit);
}

export function rasterSizes(width, height, dpr, tier, scale, maxDimension) {
  const native = Math.min(dpr || 1, Math.sqrt(UHD / (width * height)), maxDimension / Math.max(width, height));
  const internal = Math.min(native, tier.dpr, Math.sqrt(tier.maxPixels / (width * height))) * scale;
  const size = ratio => [width, height].map(n => Math.max(1, Math.round(n * ratio)));
  return {scene: size(internal), display: size(native)};
}

export function supportedSamples(gl) {
  // Depth is resolved into the texture used by the fog and glass passes.
  const counts = format => Array.from(gl.getInternalformatParameter(gl.RENDERBUFFER, format, gl.SAMPLES));
  const depth = counts(gl.DEPTH_COMPONENT24);
  return Math.max(0, ...counts(gl.RGBA16F).filter(n => n <= 4 && depth.includes(n)));
}

export function sceneSamples(quality, width, height, samples) {
  // Bound multisample colour + depth memory at large resolutions. The image
  // AA pass still resolves silhouettes in the fully composited picture.
  return quality === 'low' || width * height * samples > 16000000 ? 0 : samples;
}

export function adjacentQuality(quality, direction) {
  const tiers = Object.keys(TIERS);
  return tiers[Math.max(0, Math.min(tiers.length - 1, tiers.indexOf(quality) + direction))];
}

export function hdrDisplayAvailable() {
  const api = globalThis.WebGL2RenderingContext?.prototype;
  return Boolean(globalThis.matchMedia?.('(dynamic-range: high)').matches &&
    typeof api?.drawingBufferStorage === 'function' && typeof api?.drawingBufferToneMapping === 'function');
}

export function configureDisplayRange(gl, width, height, requestHDR) {
  if (!requestHDR || !gl.getExtension('EXT_color_buffer_float')) return 'sdr';
  try {
    gl.drawingBufferStorage(gl.RGBA16F, width, height);
    if (gl.drawingBufferFormat === gl.RGBA16F && gl.drawingBufferToneMapping({mode: 'extended'})?.mode === 'extended')
      return 'extended';
  } catch { /* Restore the ordinary framebuffer when the surface rejects HDR. */ }
  if (gl.drawingBufferFormat === gl.RGBA16F) {
    gl.drawingBufferStorage(gl.RGBA8, width, height);
    try { gl.drawingBufferToneMapping({mode: 'standard'}); } catch { /* SDR storage is active. */ }
  }
  return 'sdr';
}

/** One outstanding asynchronous GPU timer. Never stall the UI for a result. */
export class GpuTimer {
  constructor(gl) {
    this.gl = gl;
    this.extension = gl.getExtension('EXT_disjoint_timer_query_webgl2');
  }
  sample() {
    const gl = this.gl;
    if (!this.extension) {
      const elapsed = this.elapsed;
      this.elapsed = null;
      return elapsed;
    }
    if (!this.query) return null;
    const disjoint = gl.getParameter(this.extension.GPU_DISJOINT_EXT);
    if (!disjoint && !gl.getQueryParameter(this.query, gl.QUERY_RESULT_AVAILABLE)) return null;
    const ms = disjoint ? null : gl.getQueryParameter(this.query, gl.QUERY_RESULT) / 1e6;
    this.dispose();
    return ms;
  }
  begin() {
    if (!this.extension) {
      if (this.fence) return false;
      this.started = performance.now();
      return true;
    }
    if (this.query) return false;
    this.query = this.gl.createQuery();
    if (!this.query) return false;
    this.gl.beginQuery(this.extension.TIME_ELAPSED_EXT, this.query);
    return true;
  }
  end() {
    const gl = this.gl;
    if (this.extension) { gl.endQuery(this.extension.TIME_ELAPSED_EXT); return; }
    this.fence = gl.fenceSync(gl.SYNC_GPU_COMMANDS_COMPLETE, 0);
    if (!this.fence) return;
    gl.flush();
    const poll = () => {
      const status = gl.clientWaitSync(this.fence, 0, 0);
      const elapsed = performance.now() - this.started;
      if (status === gl.TIMEOUT_EXPIRED && elapsed < 30000) {
        this.timeout = setTimeout(poll, 2);
        return;
      }
      this.elapsed = status === gl.CONDITION_SATISFIED || status === gl.ALREADY_SIGNALED ? elapsed : null;
      gl.deleteSync(this.fence); this.fence = null;
    };
    this.timeout = setTimeout(poll, 0);
  }
  dispose() {
    if (this.query) this.gl.deleteQuery(this.query);
    if (this.fence) this.gl.deleteSync(this.fence);
    clearTimeout(this.timeout);
    this.query = this.fence = this.elapsed = null;
  }
}
