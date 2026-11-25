// Matched master vs #631 Room stills and timing. Same bundle except fog-field.js.
// Run from worktree root: CHROMIUM_PATH=... node .evidence-631/capture.mjs
import {createServer} from 'node:http';
import {writeFile, mkdir} from 'node:fs/promises';
import {resolve} from 'node:path';
import {build} from 'esbuild';
import {chromium} from 'playwright-core';

const out = resolve('.evidence-631/out'); await mkdir(out, {recursive: true});
const masterField = resolve('.evidence-631/fog-field-master.mjs');
const redirect = {name: 'master-fog-field', setup(b) {
  b.onResolve({filter: /fog-field\.js$/}, () => ({path: masterField}));
}};
const bundle = async plugins => (await build({entryPoints: ['js/preview/index.js'], format: 'esm', bundle: true,
  platform: 'browser', write: false, plugins})).outputFiles[0].text;
const code = {branch: await bundle([]), master: await bundle([redirect])};
const W = 1280, H = 720;
const server = createServer((req, res) => {
  const m = req.url.match(/^\/(master|branch)\/room\.js/);
  if (m) { res.setHeader('Content-Type', 'text/javascript'); return res.end(code[m[1]]); }
  const p = req.url.match(/^\/(master|branch)\/?$/);
  if (p) { res.setHeader('Content-Type', 'text/html');
    return res.end(`<!doctype html><body style="margin:0;background:#000"><canvas id="preview" style="display:block;width:${W}px;height:${H}px"></canvas>
<script type="module">import {SceneRenderer} from '/${p[1]}/room.js'; window.SceneRenderer = SceneRenderer; window.loaded = true;</script></body>`); }
  res.writeHead(404); res.end();
});
await new Promise(ok => server.listen(0, '127.0.0.1', ok));
const origin = 'http://127.0.0.1:' + server.address().port;

// Dense crossing rig: eight heads on a truss converging through the room centre,
// plus a pair separated in depth. Fixed rotations; no effect state advanced.
const fixtures = [];
for (let i = 0; i < 8; i++) {
  const x = -4.2 + i * 1.2;
  fixtures.push({id: 'h' + i, position: [x, 4.4, -1.5], 'rotation-deg': [18, 0, Math.atan2(x, 4.2) * 180 / Math.PI * 0.9]});
}
fixtures.push({id: 'd0', position: [-3, 4.4, 2.5], 'rotation-deg': [-10, 0, -25]});
fixtures.push({id: 'd1', position: [3, 4.4, 2.5], 'rotation-deg': [-10, 0, 25]});
const colors = [[1, .35, .05], [.2, .5, 1], [1, 1, 1], [.9, .1, .6]];
const snapshot = {'show-id': 'fog-axis-631', epoch: 1, revision: 1, 'frame-number': 1,
  rig: {room: {width: 12, height: 5, depth: 10}, fixtures},
  output: {values: Object.fromEntries(fixtures.map((f, i) => [f.id, {'emitted-color': colors[i % colors.length]}]))}};

const executablePath = process.env.CHROMIUM_PATH;
const browser = await chromium.launch({executablePath, headless: true,
  args: ['--use-angle=metal', '--enable-gpu', '--ignore-gpu-blocklist']});
const report = {viewport: [W, H], snapshot, runs: {}};
try {
  for (const quality of ['desktop', 'mobile', 'high']) {
    for (const variant of ['master', 'branch', 'master', 'branch']) {
      const page = await browser.newPage({viewport: {width: W, height: H}, deviceScaleFactor: 1}), errors = [];
      page.on('pageerror', e => errors.push(e.message));
      page.on('console', m => { if (m.type() === 'error') errors.push(m.text()); });
      await page.goto(`${origin}/${variant}`);
      await page.waitForFunction(() => window.loaded);
      const result = await page.evaluate(async ({snapshot, quality}) => {
        const canvas = document.querySelector('#preview');
        const engine = window.engine = new SceneRenderer(canvas, null);
        for (let i = 0; i < 50 && !engine.visible; i++) await new Promise(r => setTimeout(r, 50));
        if (!engine.visible) engine.changeActivity(true);
        engine.pause(); engine.setQuality(quality); engine.update(snapshot, true); engine.setWorkLight(.08);
        if (engine.controls) engine.controls.enableDamping = false;
        engine.setCamera('audience');
        engine.draw(0);
        const ready = await Promise.race([engine.whenReady, new Promise(r => setTimeout(() => r('timeout'), 20000))]);
        if (ready !== true) throw new Error('engine not ready: ' + ready + ' visible=' + engine.visible);
        const gl = engine.renderer.getContext(), debug = gl.getExtension('WEBGL_debug_renderer_info');
        const field = engine.fog.field;
        field.clear(.07);
        field.burst({position: [-1.5, .35, 1], direction: [.7, .3, -.2], duration: 1.5, strength: 2.5});
        field.burst();
        // Solver timing on this field and tier, 60 fixed 50 ms steps (3 s of fog).
        const stepMs = [];
        for (let i = 0; i < 60; i++) { const t = performance.now(); field.step(.05); stepMs.push(performance.now() - t); }
        engine.fog.upload();
        engine.setFog(.75);
        const px = new Uint8Array(4);
        const drawMs = [];
        for (let i = 0; i < 40; i++) {
          const t = performance.now(); engine.draw(0); gl.readPixels(0, 0, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, px);
          drawMs.push(performance.now() - t);
        }
        // Hash the density field so stills can be tied to identical state.
        let h = 2166136261; const bytes = new Uint8Array(field.data.buffer);
        for (let i = 0; i < bytes.length; i++) { h ^= bytes[i]; h = Math.imul(h, 16777619); }
        const med = xs => [...xs].sort((a, b) => a - b)[xs.length >> 1];
        return {gpu: debug && gl.getParameter(debug.UNMASKED_RENDERER_WEBGL), quality: engine.getTelemetry().quality,
          grid: field.grid, densityHash: (h >>> 0).toString(16), stepMsMedian: med(stepMs), stepMsMean: stepMs.reduce((a, b) => a + b) / stepMs.length,
          drawPlusReadbackMsMedian: med(drawMs.slice(5))};
      }, {snapshot, quality});
      const key = `${quality}-${variant}`;
      if (!report.runs[key]) {
        await page.locator('#preview').screenshot({path: `${out}/${quality}-${variant}.png`});
        report.runs[key] = [];
      }
      report.runs[key].push({...result, errors});
      console.log(key, JSON.stringify(result), errors.length ? errors : '');
      await page.close();
    }
  }
} finally { await browser.close(); server.close(); }
await writeFile(`${out}/capture.json`, JSON.stringify(report, null, 2));
