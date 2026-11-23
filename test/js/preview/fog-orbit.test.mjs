import test from 'node:test';
import assert from 'node:assert/strict';
import { existsSync } from 'node:fs';
import { mkdir, writeFile } from 'node:fs/promises';
import { build } from 'esbuild';
import { chromium } from 'playwright-core';

test('orbiting a single illuminated volume keeps optical path and phase separate', async (t) => {
  const endpoint = process.env.CDP_ENDPOINT;
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!endpoint && !existsSync(executablePath)) return t.skip('Set CDP_ENDPOINT or CHROMIUM_PATH to run actual WebGL2');
  const bundle = await build({
    entryPoints: [new URL('./fog-orbit-fixture.mjs', import.meta.url).pathname],
    bundle: true,
    format: 'iife',
    write: false,
  });
  const browser = endpoint ? await chromium.connectOverCDP(endpoint) : await chromium.launch({
    executablePath,
    args: ['--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'],
  });
  let page;
  const reuseTitle = process.env.CDP_PAGE_TITLE;
  try {
    if (reuseTitle) {
      if (!endpoint) throw new Error('CDP_PAGE_TITLE requires CDP_ENDPOINT');
      for (const candidate of browser.contexts()[0].pages()) if (await candidate.title() === reuseTitle) page = candidate;
      if (!page) throw new Error('The owned diagnostic page is missing');
      await page.evaluate(() => {window.crossing?.dispose(); document.body.innerHTML='';});
    } else page = endpoint ? await browser.contexts()[0].newPage() : await browser.newPage();
    const output = process.env.FOG_ORBIT_OUTPUT, errors = [];
    page.on('pageerror', error => errors.push(error.message));
    page.on('console', message => { if (message.type() === 'error') errors.push(message.text()); });
    if (!reuseTitle) await page.bringToFront();
    await page.addScriptTag({ content: bundle.outputFiles[0].text });
    if (output) await mkdir(output, { recursive: true });
    const measurements = [];
    for (const [width, height] of [
      [323, 183],
      [31, 19],
    ])
      for (const offset of [0, 0.21]) {
        const test = await page.evaluate((args) => window.measureFogOrbit(args), { width, height, offset });
        for (const sample of test.samples) {
          if (output && sample.picture)
            await writeFile(
              output + '/angle-' + sample.angle + '.png',
              Buffer.from(sample.picture, 'base64'),
            );
          delete sample.picture;
          assert.ok(
            sample.raw > 0 && sample.historical > 0 && sample.fixed > 0,
            'Analytically intersected fog disappeared: ' + JSON.stringify(sample),
          );
          const nominal = Math.exp(-test.density * test.extinction * sample.roomPath);
          assert.ok(
            Math.abs(sample.transmittance - nominal) < 0.003,
            'Camera extinction must follow the integrated room distance',
          );
          assert.ok(
            Math.abs(sample.raw - sample.composited) < 0.004 * Math.max(1, sample.raw),
            'Flat background and aligned center ray',
          );
          const angle = (sample.angle * Math.PI) / 180,
            g = 0.38;
          const projection = 4 / Math.hypot(4, offset);
          const expected = (1 - g * g) / (1 + g * g - 2 * g * projection * Math.cos(angle)) ** 1.5;
          const observed = sample.fixedHistorical / sample.fixed;
          assert.ok(
            Math.abs(observed / expected - 1) < 0.035,
            'Carrier-independent phase at ' + sample.angle + ' degrees: ' + observed + ' / ' + expected,
          );
        }
        const fixed = test.samples.map((row) => row.fixed);
        assert.ok(
          Math.max(...fixed) / Math.min(...fixed) < 1.02,
          'Matched mass and eye distance must retain essentially identical bulk haze across the orbit',
        );
        const side = test.samples.find((row) => row.angle === 90);
        assert.equal(side.repeated, side.raw, 'A stationary homogeneous field has no temporal jitter');
        measurements.push(test);
      }
    const silhouette = await page.evaluate(() => window.measureFogSilhouette());
    const subpixel = await page.evaluate(() => window.measureSubpixelShaft());
    assert.ok(silhouette.native > 0);
    assert.ok(
      Math.abs(silhouette.half - silhouette.native) < silhouette.native * 0.02,
      'A thin foreground surface must retain the scattering between it and the camera',
    );
    assert.ok(subpixel.values.some((row) => row.native > 0));
    t.diagnostic(JSON.stringify({ silhouette, subpixel }));
    if (output) {
      await writeFile(output + '/orbit.json', JSON.stringify(measurements, null, 2));
      await writeFile(output + '/edges.json', JSON.stringify({ silhouette, subpixel }, null, 2));
    }
    assert.deepEqual(errors, []);
  } finally {
    await page?.evaluate(() => window.closeFogOrbit?.()).catch(() => {});
    if (!reuseTitle) await page?.close();
    // CDP close disconnects this transport, preserving the shared browser and
    // default context. Only the page created above belongs to this test.
    await browser.close();
  }
});
