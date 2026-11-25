import assert from 'node:assert/strict';
import {readFileSync, writeFileSync} from 'node:fs';
import {createRequire} from 'node:module';
import {profiles} from './fixtures.mjs';

const require = createRequire(import.meta.url);
const {chromium} = require(process.env.PLAYWRIGHT_MODULE ?? 'playwright-core');
const origin = 'http://precision.test';
const modules = new Map(['trace-storage.mjs', 'fixtures.mjs'].map(file => [
  '/' + file, readFileSync(new URL(file, import.meta.url)),
]));
const html = `<!doctype html><meta charset="utf-8"><title>Parameter history precision</title>
<style>
  body {margin:0; background:#111712; color:#e2e9dc; font:12px system-ui;}
  main {display:grid; gap:8px; padding:8px; width:max-content;}
  h1 {font-size:16px; margin:0;}
  section {display:grid; grid-template-columns:repeat(4,var(--chart-width)); gap:8px;}
  figure {margin:0; padding:0;}
  figcaption {height:18px; white-space:nowrap; font-size:10px; line-height:18px; overflow:hidden; text-overflow:ellipsis;}
  svg {display:block; background:#141e19;}
  line {stroke:#899d8e; stroke-width:.6; opacity:.26;}
</style><main><h1></h1><section></section></main>`;
const browser = await chromium.launch({
  executablePath: process.env.CHROMIUM_PATH ?? '/usr/bin/chromium',
  args: ['--no-sandbox', '--disable-dev-shm-usage'],
});
const result = {chromium: browser.version(), profiles: []};

try {
  for (const profile of profiles) {
    const page = await browser.newPage({
      viewport: {width: 4 * profile.width + 44, height: 8 * (profile.height + 28) + 44},
      deviceScaleFactor: profile.dpr,
    });
    await page.route(origin + '/**', async route => {
      const path = new URL(route.request().url()).pathname;
      if (modules.has(path)) {
        await route.fulfill({body: modules.get(path), contentType: 'text/javascript'});
      } else if (path === '/') {
        await route.fulfill({body: html, contentType: 'text/html'});
      } else {
        await route.abort();
      }
    });
    await page.goto(origin);
    const scene = await page.evaluate(async profile => {
      const {formats, buildTrace, encodeInto, graphPath} = await import('/trace-storage.mjs');
      const {options, traces} = await import('/fixtures.mjs');
      const svgNS = 'http://www.w3.org/2000/svg';
      const candidates = ['full', 'unorm8', 'unorm16', 'fp16'];
      const selected = buildTrace(traces[0].samples, options(profile)).format;
      document.querySelector('h1').textContent = `${profile.name} · ${profile.width} × ${profile.height} CSS px · Auto uses ${selected.toUpperCase()}`;
      const section = document.querySelector('section');
      section.style.setProperty('--chart-width', `${profile.width}px`);
      const rects = traces.map((trace, index) => {
        const window = index % 2 === 0 ? 16 : 4;
        const referencePath = graphPath(trace.samples, 'full', trace.beats, window);
        return {
          label: trace.name,
          window,
          candidates: Object.fromEntries(candidates.map(format => {
            const encoded = encodeInto(trace.samples, format, formats[format].allocate(trace.samples.length));
            const figure = document.createElement('figure');
            section.append(figure);
            const label = document.createElement('figcaption');
            label.textContent = `${format.toUpperCase()} · ${trace.name}`;
            figure.append(label);
            const svg = document.createElementNS(svgNS, 'svg');
            figure.append(svg);
            svg.setAttribute('width', profile.width);
            svg.setAttribute('height', profile.height);
            svg.setAttribute('viewBox', '0 0 100 44');
            svg.setAttribute('preserveAspectRatio', 'none');
            for (const x of [0, 25, 50, 75, 100]) {
              const line = document.createElementNS(svgNS, 'line');
              for (const [key, value] of Object.entries({x1:x, x2:x, y1:0, y2:44})) line.setAttribute(key, value);
              svg.append(line);
            }
            const path = document.createElementNS(svgNS, 'path');
            const d = graphPath(encoded, format, trace.beats, window);
            path.dataset.referencePath = referencePath;
            const attrs = {d, stroke:'#e8a083', fill:'none', 'stroke-width':1.6, 'vector-effect':'non-scaling-stroke'};
            for (const [key, value] of Object.entries(attrs)) path.setAttribute(key, value);
            svg.append(path);
            const {x, y, width, height} = svg.getBoundingClientRect();
            return [format, {x, y, width, height, pathCount: d.split('M').length - 1, exactPath: d === referencePath}];
          })),
        };
      });
      return {rects, selected, dpr: devicePixelRatio};
    }, profile);
    assert.equal(scene.dpr, profile.dpr);
    for (const row of scene.rects) {
      for (const rect of Object.values(row.candidates)) {
        assert.equal(rect.width, profile.width);
        assert.equal(rect.height, profile.height);
        assert.equal(rect.pathCount, 1);
        assert.ok(rect.x + rect.width <= page.viewportSize().width);
        assert.ok(rect.y + rect.height <= page.viewportSize().height);
      }
    }
    assert.ok(scene.rects[0].candidates.unorm8.exactPath && scene.rects[3].candidates.unorm8.exactPath);
    const screenshot = await page.screenshot({scale: 'device', animations: 'disabled'});
    // Chromium coverage can depend on absolute raster position. Render the
    // full path at each candidate's original rectangle for a paired comparison.
    await page.evaluate(() => {
      for (const path of document.querySelectorAll('svg path')) path.setAttribute('d', path.dataset.referencePath);
    });
    const fullScreenshot = await page.screenshot({scale: 'device', animations: 'disabled'});
    const comparisons = await page.evaluate(async ({png, fullPng, scene}) => {
      async function decode(source) {
        const image = new Image();
        image.src = 'data:image/png;base64,' + source;
        await image.decode();
        const cv = document.createElement('canvas');
        cv.width = image.naturalWidth;
        cv.height = image.naturalHeight;
        const ctx = cv.getContext('2d', {willReadFrequently:true});
        ctx.drawImage(image, 0, 0);
        return ctx;
      }
      const [candidateContext, referenceContext] = await Promise.all([decode(png), decode(fullPng)]);
      const pixels = (ctx, {x, y, width, height}) =>
        ctx.getImageData(x * scene.dpr, y * scene.dpr, width * scene.dpr, height * scene.dpr).data;
      return scene.rects.map(({label, window, candidates}) => {
        const formats = Object.fromEntries(Object.entries(candidates).filter(([name]) => name !== 'full').map(([name, rect]) => {
          const candidate = pixels(candidateContext, rect);
          const reference = pixels(referenceContext, rect);
          let changed = 0;
          let max = 0;
          let sum = 0;
          for (let offset = 0; offset < reference.length; offset += 4) {
            const error = Math.max(
              Math.abs(reference[offset] - candidate[offset]),
              Math.abs(reference[offset + 1] - candidate[offset + 1]),
              Math.abs(reference[offset + 2] - candidate[offset + 2]),
            );
            if (error) changed++;
            max = Math.max(max, error);
            sum += error;
          }
          return [name, {changed, pixels: reference.length / 4, maxChannel: max, meanMaxChannel: 4 * sum / reference.length}];
        }));
        return {label, window, formats};
      });
    }, {png: screenshot.toString('base64'), fullPng: fullScreenshot.toString('base64'), scene});
    assert.equal(comparisons[0].formats.unorm8.changed, 0, 'An exact DMX8 ramp must render byte-identically');
    assert.equal(comparisons[3].formats.unorm8.changed, 0, 'Exact DMX8 steps must render byte-identically');
    result.profiles.push({...profile, selected: scene.selected, comparisons});
    if (profile.name === 'Push' || profile.name === 'Studio detail 2×') {
      const file = profile.name === 'Push' ? 'push-raster.png' : 'studio-2x-raster.png';
      writeFileSync(new URL(file, import.meta.url), screenshot);
    }
    await page.close();
  }
} finally {
  await browser.close();
}
writeFileSync(new URL('raster-report.json', import.meta.url), JSON.stringify(result, null, 2) + '\n');
console.log('Verified Chromium SVG raster profiles and exact DMX8 plots. Wrote raster-report.json.');
