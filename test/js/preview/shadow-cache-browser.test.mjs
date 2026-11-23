import test from 'node:test';
import assert from 'node:assert/strict';
import {existsSync} from 'node:fs';
import {mkdir, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {createServer} from 'node:http';
import {build} from 'esbuild';
import {chromium} from 'playwright-core';

test('actual room GPU shadows reuse fixed maps and refresh moving dark casters without changing pixels', {timeout: 120000}, async t => {
  const endpoint = process.env.CDP_ENDPOINT;
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!endpoint && !existsSync(executablePath)) return t.skip('Set CDP_ENDPOINT or CHROMIUM_PATH for actual WebGL checks');
  const bundle = await build({entryPoints: [new URL('./shadow-cache-browser-fixture.mjs', import.meta.url).pathname],
    bundle: true, format: 'esm', platform: 'browser', write: false});
  const server = createServer((request, response) => {
    response.setHeader('Content-Type', request.url === '/fixture.js' ? 'text/javascript' : 'text/html');
    response.end(request.url === '/fixture.js' ? bundle.outputFiles[0].text :
      '<!doctype html><style>body{margin:0;background:#05070a}canvas{display:block;width:960px;height:540px}</style><canvas></canvas><script type="module" src="/fixture.js"></script>');
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  let browser, page;
  const reuseTitle = process.env.CDP_PAGE_TITLE;
  const directory = process.env.SHADOW_MEDIA_DIR, captures = [];
  try {
    browser = endpoint ? await chromium.connectOverCDP(endpoint) : await chromium.launch({executablePath, headless: true,
      args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
    if (reuseTitle) {
      if (!endpoint) throw new Error('CDP_PAGE_TITLE requires CDP_ENDPOINT');
      for (const candidate of browser.contexts()[0].pages()) if (await candidate.title() === reuseTitle) page = candidate;
      if (!page) throw new Error('The owned diagnostic page is missing');
      await page.evaluate(() => window.crossing?.dispose());
    } else page = endpoint ? await browser.contexts()[0].newPage() : await browser.newPage();
    await page.setViewportSize({width: 980, height: 600});
    if (!reuseTitle) await page.bringToFront();
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    page.on('console', message => {if (message.type() === 'error') errors.push(message.text());});
    await page.goto(`http://127.0.0.1:${server.address().port}/`);
    if (reuseTitle) await page.evaluate(title => {document.title=title;}, reuseTitle);
    await page.waitForFunction(() => typeof window.shadowCacheTest === 'function');
    const result = await page.evaluate(fullSizeCapture => window.shadowCacheTest({fullSizeCapture}), Boolean(directory));
    t.diagnostic(JSON.stringify(result));
    assert.deepEqual(errors, []);
    assert.equal(result.glError, 0);
    assert.equal(result.first.shadowMaps, 4);
    assert.ok(result.first.shadowDraws > 0);
    assert.equal(result.identical.shadowDraws, 0);
    assert.deepEqual(result.identical.difference, {maximum: 0, changed: 0});
    assert.equal(result.color.shadowDraws, 0); assert.ok(result.color.difference.changed > 100);
    assert.equal(result.camera.shadowDraws, 0); assert.ok(result.camera.difference.changed > 100);
    assert.equal(result.darkStill.shadowDraws, 0);
    assert.equal(result.darkMoved.shadowMaps, 4); assert.ok(result.darkMoved.shadowDraws > 0);
    assert.equal(result.cached.shadowDraws, 0);
    assert.equal(result.forced.shadowMaps, 4); assert.ok(result.forced.shadowDraws > 0);
    assert.deepEqual(result.forced.difference, {maximum: 0, changed: 0});
    assert.equal(new Set(result.initialMaps).size, 4);
    assert.ok(result.initialMaps.every(id => typeof id === 'string' && id.length > 0));
    assert.deepEqual(result.finalMaps, result.initialMaps);
    assert.ok(result.fogBrightened > 100); assert.ok(result.fogDimmed > 100);
    assert.deepEqual(result.directCopyDifference, {maximum: 0, changed: 0});
    assert.deepEqual(result.directRepeatDifference, {maximum: 0, changed: 0});
    assert.equal(result.directInputEnabled, false);
    assert.equal(result.glass.inputEnabled, true); assert.equal(result.glass.targetIsComposite, true);
    assert.ok(result.glass.changed.changed > 100);
    assert.deepEqual(result.glass.referenceDifference, {maximum: 0, changed: 0});
    const compact = result.telemetry.softwareRenderer && !directory;
    assert.deepEqual(result.telemetry.pixels, compact ? [480,270] : [960,540]);
    if (directory) await mkdir(directory, {recursive: true});
    const captureScene = async (name, prefix = '') => {
      const {data, telemetry, capture} = await page.evaluate(name => window.captureShadowCacheScene(name), name);
      const png = Buffer.from(data.slice(data.indexOf(',') + 1), 'base64');
      assert.deepEqual([...png.subarray(0, 8)], [137, 80, 78, 71, 13, 10, 26, 10]);
      assert.deepEqual([png.readUInt32BE(16), png.readUInt32BE(20)], telemetry.displayPixels);
      assert.deepEqual(capture.pixels, telemetry.displayPixels);
      assert.deepEqual(capture.viewport, [0, 0, ...telemetry.displayPixels]);
      assert.equal(capture.opaqueEdges, true);
      assert.ok(capture.brightPixels > 1000, `${prefix}${name} contains the illuminated scene`);
      if (directory) await writeFile(join(directory, prefix + name + '.png'), png);
      captures.push({name: prefix + name, telemetry, capture});
    };
    const names = ['clear-surfaces', 'crossing-shafts', 'fresh-plume', 'dispersed-plume'];
    if (directory) for (const name of names) await captureScene(name);
    if (!compact) await page.setViewportSize({width: 1580, height: 730});
    const floor = await page.evaluate(() => window.setShadowCaptureAutoFloor());
    assert.deepEqual(floor.pixels, floor.softwareRenderer ? [317,142] : [914,410]);
    assert.deepEqual(floor.displayPixels, compact ? [1404,630] : [2806,1260]);
    assert.deepEqual(floor.fog.targetSize, floor.softwareRenderer ? [104,46] : [301,135]);
    for (const name of directory ? names : ['fresh-plume']) await captureScene(name, 'auto-');
    assert.deepEqual(errors, []);
    if (directory) {
      const {data, ...motion} = await page.evaluate(() => window.recordShadowMotion());
      await writeFile(join(directory, 'auto-motion.webm'), Buffer.from(data.slice(data.indexOf(',') + 1), 'base64'));
      await writeFile(join(directory, 'evidence.json'), JSON.stringify({scope: 'Controlled browser scene; no host or device output', result, captures, motion, errors}, null, 2));
    }
  } finally {
    await page?.evaluate(() => window.disposeShadowCacheTest?.()).catch(() => {});
    if (!reuseTitle) await page?.close();
    // Connected-CDP close disconnects Playwright's transport, leaving the
    // shared Chrome browser and default context running.
    await browser?.close(); await new Promise(resolve => server.close(resolve));
  }
});
