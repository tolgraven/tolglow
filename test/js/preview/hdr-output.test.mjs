import test from 'node:test';
import assert from 'node:assert/strict';
import {existsSync, readFileSync} from 'node:fs';
import {chromium} from 'playwright-core';

test('direct HDR presentation preserves linear highlights through film on a negotiated float canvas', {timeout: 120000}, async t => {
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!existsSync(executablePath)) return t.skip('Set CHROMIUM_PATH to exercise WebGL');
  const browser = await chromium.launch({executablePath, args: ['--use-angle=swiftshader', '--enable-unsafe-swiftshader',
    '--enable-blink-features=WebGLDrawingBufferStorage,WebGLToneMapping']});
  try {
    const page = await browser.newPage();
    const asset = readFileSync(new URL('../../../resources/studio/preview.js', import.meta.url));
    await page.route('http://localhost/venue', route => route.fulfill({contentType: 'text/html', body: '<canvas style="width:32px;height:16px"></canvas>'}));
    await page.route('http://localhost/preview.js', route => route.fulfill({contentType: 'text/javascript', body: asset}));
    await page.goto('http://localhost/venue');
    const result = await page.evaluate(async () => {
      // Emulate the display capability only. Storage, compositor mode, shaders
      // and the floating-point pixel read are the browser's actual implementation.
      const media = matchMedia.bind(window);
      window.matchMedia = query => query === '(dynamic-range: high)' ? {matches: true} : media(query);
      const {SceneRenderer} = await import('/preview.js');
      const engine = new SceneRenderer(document.querySelector('canvas'), null, {autoStart: false});
      try {
        engine.visible = true;
        if (!engine.renderer) engine.initialize();
        if (!engine.hdrRequested) return {supported: false};
        engine.pause();
        engine.scene.remove(engine.content);
        engine.scene.background.setRGB(12, 4, 1);
        engine.setFog(0); engine.setFilm({bloom: 0, grain: 0});
        engine.draw(0); engine.renderer.setRenderTarget(null);
        const gl = engine.renderer.getContext(), type = gl.getParameter(gl.IMPLEMENTATION_COLOR_READ_TYPE);
        const half = type === gl.HALF_FLOAT, pixel = half ? new Uint16Array(4) : new Float32Array(4);
        gl.readPixels(12, 8, 1, 1, gl.RGBA, type, pixel);
        return {supported: true, range: engine.displayRange, hdr: gl.drawingBufferFormat === gl.RGBA16F,
          aboveWhite: pixel[0] > (half ? 0x3c00 : 1), error: gl.getError()};
      } finally { engine.dispose(); }
    });
    if (!result.supported) return t.skip('Chromium does not expose floating-point WebGL display storage');
    assert.deepEqual(result, {supported: true, range: 'extended', hdr: true, aboveWhite: true, error: 0});
  } finally { await browser.close(); }
});
