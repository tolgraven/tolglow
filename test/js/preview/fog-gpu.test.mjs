import test from 'node:test';
import assert from 'node:assert/strict';
import {existsSync} from 'node:fs';
import {createServer} from 'node:http';
import {build} from 'esbuild';
import {chromium} from 'playwright-core';
import {fogTransmission, fogAbsorptance} from '../../../js/preview/fog-optics.generated.js';

test('actual WebGL2 fog preserves empty surfaces, opaque depth, extinction and PCF depth shadows', async t => {
  const endpoint = process.env.CDP_ENDPOINT;
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!endpoint && !existsSync(executablePath)) return t.skip('Set CDP_ENDPOINT or CHROMIUM_PATH to run actual GPU-context assertions');
  const bundle = await build({entryPoints: [new URL('./fog-gpu-fixture.mjs', import.meta.url).pathname],
    bundle: true, format: 'esm', platform: 'browser', write: false});
  const server = createServer((request, response) => {
    response.setHeader('Content-Type', request.url === '/fixture.js' ? 'text/javascript' : 'text/html');
    response.end(request.url === '/fixture.js' ? bundle.outputFiles[0].text : '<script type="module" src="/fixture.js"></script>');
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  let browser, page;
  const reuseTitle = process.env.CDP_PAGE_TITLE;
  try {
    browser = endpoint ? await chromium.connectOverCDP(endpoint) : await chromium.launch({executablePath, headless: true,
      args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
    if (reuseTitle) {
      if (!endpoint) throw new Error('CDP_PAGE_TITLE requires CDP_ENDPOINT');
      for (const candidate of browser.contexts()[0].pages()) if (await candidate.title() === reuseTitle) page = candidate;
      if (!page) throw new Error('The owned diagnostic page is missing');
      await page.evaluate(() => window.crossing?.dispose());
    } else page = endpoint ? await browser.contexts()[0].newPage() : await browser.newPage();
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    page.on('console', message => { if (message.type() === 'error') errors.push(message.text()); });
    await page.goto(`http://127.0.0.1:${server.address().port}/`);
    if (reuseTitle) await page.evaluate(title => {document.title=title;}, reuseTitle);
    await page.waitForFunction(() => typeof window.fogTest === 'function');
    const result = await page.evaluate(() => window.fogTest());
    t.diagnostic(JSON.stringify(result));
    assert.deepEqual(errors, []);
    assert.ok(Math.abs(result.manualHazeDensity - .15) < .00001);
    assert.ok(Math.abs(result.manualHazeTransmission - fogTransmission(1.25, .15 * .7, 8)) < .002);
    assert.equal(result.culledLightDifference, 0);
    assert.equal(result.zeroAmountDifference, 0);
    assert.equal(result.emptyFieldDifference, 0);
    assert.equal(result.behindOpaqueDifference, 0);
    assert.ok(result.extinctionRatio > 0 && result.extinctionRatio < .5);
    assert.equal(result.noInventedRadiance, true);
    assert.ok(result.unshadowedRadiance > 1);
    assert.equal(result.comparisonDepthTexture, true);
    assert.equal(result.clearShadowDifference, 0);
    assert.equal(result.clearShadowRatio, 1);
    assert.ok(result.shadowRatio > 0 && result.shadowRatio < .9);
    assert.equal(result.shadowSlots, 1);
    assert.equal(result.blackoutRadiance, 0);
    assert.ok(result.ambientRadiance > 1);
    assert.equal(result.emptyAmbientRadiance, 0);
    assert.equal(result.narrowShaftCoverage, 1);
    assert.ok(result.narrowShaftMin > 0);
    assert.equal(result.finiteNonnegative, true);
    assert.equal(result.behindLensRadiance, 0);
    assert.ok(result.apertureCoverage.every(value => value > 0));
    assert.equal(result.manualDuringShow,false);
    assert.equal(result.dryHostDifference,0);
    assert.ok(result.jetDarkest < .85 && result.jetClearest > .98, JSON.stringify(result));
    assert.ok(result.roomTransmission < .9 && result.roomTransmission > 0);
    // The central camera ray crosses eight metres of uniform 10% haze.
    // Half-float rendering/texture sampling is less precise than the CPU material.
    assert.ok(Math.abs(result.roomTransmission - fogTransmission(1.25, .1, 8)) < .002);
    assert.ok(Math.abs(result.calibratedTransmission - fogTransmission(2.5, .1, 8)) < .002);
    assert.ok(Math.abs(result.calibratedTransmission-result.roomTransmission**2)<.001);
    assert.ok(Math.abs(result.calibratedMass-25.6)<.00001);
    assert.ok(result.opticalCore > result.halo && result.halo > 0);
    assert.equal(result.haloGreen,0);
    assert.ok(Math.abs(result.brighterHalo/result.halo - 2) < .02);
    assert.equal(result.haloInsideCore, 0);
    assert.ok(result.haloOutsideCore > 0);
    assert.ok(result.haloAtEdge < result.haloOutsideCore * .001);
    assert.equal(result.dryHalo,0);
    assert.equal(result.glError, 0);
  } finally {
    if (!reuseTitle) await page?.close();
    // For connectOverCDP, Playwright closes only its transport here. The shared
    // browser and default context remain open; only this test's page is closed.
    await browser?.close();
    await new Promise(resolve => server.close(resolve));
  }
});

test('the emitted material preserves transparent boundaries and trace attenuation', () => {
  for (const args of [[0, .3, 4], [1, -.3, 4], [1, .3, -4]]) {
    assert.equal(fogTransmission(...args), 1);
    assert.equal(Math.abs(fogAbsorptance(...args)), 0);
  }
  assert.equal(fogAbsorptance(1, 1e-19, 1), 1e-19);
  assert.equal(fogTransmission(1, .3, 10000), 0);
});
