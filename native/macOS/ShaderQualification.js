// Opt-in diagnostics for the existing Studio renderer. No shaders are replaced.
(() => {
  const tag = 'tolglow-shader-qualification';
  const report = event => window.webkit.messageHandlers.shaderQualification.postMessage(event);
  function observe(send, scope) {
    const contexts = new WeakMap();
    let programs = 0, contextCount = 0, limitReported = false;
    let active = true;
    setTimeout(() => { active = false; }, 60000);
    function context(gl) {
      let state = contexts.get(gl);
      if (!state) {
        const debug = gl.getExtension('WEBGL_debug_renderer_info');
        state = {id: scope + ':' + (++contextCount), drawn: false,
          shaders: gl.getExtension('WEBGL_debug_shaders')};
        contexts.set(gl, state);
        send({type: 'context', scope, context: state.id,
          renderer: gl.getParameter(debug ? debug.UNMASKED_RENDERER_WEBGL : gl.RENDERER),
          vendor: gl.getParameter(debug ? debug.UNMASKED_VENDOR_WEBGL : gl.VENDOR),
          version: gl.getParameter(gl.VERSION), translatedSourceAvailable: !!state.shaders});
      }
      return state;
    }
    for (const name of ['WebGLRenderingContext', 'WebGL2RenderingContext']) {
      const prototype = globalThis[name]?.prototype;
      if (!prototype) continue;
      const link = prototype.linkProgram;
      prototype.linkProgram = function(program) {
        const result = link.call(this, program);
        if (!active || (scope === 'window' && !this.canvas?.tolglowPreview)) return result;
        if (programs >= 128) {
          if (!limitReported) send({type: 'limit', scope, reason: 'program count'});
          limitReported = true;
          return result;
        }
        const state = context(this);
        const shaders = (this.getAttachedShaders(program) || []).map(shader => {
          const source = this.getShaderSource(shader) || '';
          const translated = state.shaders?.getTranslatedShaderSource(shader) || '';
          return {stage: this.getShaderParameter(shader, this.SHADER_TYPE) === this.VERTEX_SHADER ? 'vertex' : 'fragment',
            compiled: this.getShaderParameter(shader, this.COMPILE_STATUS),
            source: source.slice(0, 1048576), translated: translated.slice(0, 1048576),
            truncated: source.length > 1048576 || translated.length > 1048576,
            sourceLength: source.length, translatedLength: translated.length,
            log: this.getShaderInfoLog(shader) || ''};
        });
        send({type: 'program', scope, context: state.id, shaders,
          linked: this.getProgramParameter(program, this.LINK_STATUS), log: this.getProgramInfoLog(program) || ''});
        programs++;
        return result;
      };
      for (const name of ['drawArrays', 'drawElements', 'drawArraysInstanced', 'drawElementsInstanced']) {
        const draw = prototype[name];
        if (!draw) continue;
        prototype[name] = function(...args) {
          const result = draw.apply(this, args);
          if (active && (scope !== 'window' || this.canvas?.tolglowPreview)) {
            const state = context(this);
            if (!state.drawn) {
              state.drawn = true;
              send({type: 'draw', scope, context: state.id, contextLost: this.isContextLost()});
            }
          }
          return result;
        };
      }
    }
  }
  observe(report, 'window');
  // The shipped preview is a module worker. Import its original URL so its own
  // import.meta.url and relative imports retain their normal meaning.
  const NativeWorker = window.Worker;
  if (NativeWorker) window.Worker = class extends NativeWorker {
    constructor(url, options) {
      const resolved = new URL(url, location.href);
      if (options?.name !== 'tolglow-room-preview' || options.type !== 'module' || resolved.origin !== location.origin) {
        super(url, options); return;
      }
      const bootstrap = `
        const pending = [];
        const hold = event => {
          if (pending.length >= 16) throw new Error('Qualification worker startup queue exceeded');
          pending.push(event); event.stopImmediatePropagation();
        };
        self.addEventListener('message', hold);
        (${observe.toString()})(event => postMessage({tag: ${JSON.stringify(tag)}, event}), 'tolglow-room-preview');
        await import(${JSON.stringify(resolved.href)});
        self.removeEventListener('message', hold);
        for (const event of pending) self.dispatchEvent(new MessageEvent('message', {data: event.data, ports: event.ports}));
      `;
      const blob = URL.createObjectURL(new Blob([bootstrap], {type: 'text/javascript'}));
      try { super(blob, options); } catch (error) { URL.revokeObjectURL(blob); throw error; }
      const release = () => URL.revokeObjectURL(blob);
      this.addEventListener('message', release, {once: true});
      this.addEventListener('error', release, {once: true});
      this.addEventListener('message', event => {
        if (event.data?.tag === tag) { event.stopImmediatePropagation(); report(event.data.event); }
      });
    }
  };
  window.__tolglowShaderQualificationSnapshot = () => JSON.parse(JSON.stringify({
    url: location.href,
    previews: [...document.querySelectorAll('canvas')].filter(canvas => canvas.tolglowPreview).map(canvas => {
      const preview = canvas.tolglowPreview;
      const engine = preview.fallback ?? preview;
      const worker = preview.worker && !preview.fallback;
      return {ready: !!preview.ready, presented: worker ? !!preview.presented : engine.draws > 0,
        width: canvas.width, height: canvas.height,
        route: worker ? 'worker' : 'main',
        telemetry: preview.getTelemetry?.() ?? preview.telemetry ?? {}};
    })
  }));
})();
