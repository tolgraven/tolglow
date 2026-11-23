import assert from 'node:assert/strict';
import {existsSync} from 'node:fs';
import {mkdir, mkdtemp, readFile, writeFile} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import test from 'node:test';
import {build} from 'esbuild';
import {chromium} from 'playwright-core';

function maxDifference(a, b) {
  return Math.max(...a.map((value, i) => Math.abs(value - b[i])));
}

test('Room preview keeps beam crossings additive below the bloom threshold', async (t) => {
  const endpoint = process.env.CDP_ENDPOINT;
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!endpoint && !existsSync(executablePath)) return t.skip('Set CDP_ENDPOINT or CHROMIUM_PATH to run actual WebGL2');
  const bundle = await build({
    entryPoints: [new URL('../../../dev/preview/beam-crossing-fixture.mjs', import.meta.url).pathname],
    bundle: true,
    format: 'iife',
    write: false,
  });
  const hardware = Boolean(endpoint) || process.env.BEAM_CROSSING_HARDWARE === '1';
  const browser = endpoint ? await chromium.connectOverCDP(endpoint) : await chromium.launch({
    executablePath,
    headless: !hardware,
    args: hardware ? [] : ['--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader', '--disable-gpu-shader-disk-cache'],
  });
  const retainPage = process.env.BEAM_CROSSING_KEEP_PAGE === '1';
  let page, directory;
  const all = [];
  try {
    const reuseTitle = process.env.CDP_PAGE_TITLE;
    if (reuseTitle) {
      if (!endpoint) throw new Error('CDP_PAGE_TITLE requires CDP_ENDPOINT');
      for (const candidate of browser.contexts()[0].pages()) if (await candidate.title() === reuseTitle) page = candidate;
      if (!page) throw new Error('The owned diagnostic page is missing');
      await page.evaluate(() => window.crossing?.dispose());
    } else page = endpoint ? await browser.contexts()[0].newPage() : await browser.newPage({deviceScaleFactor: 1});
    await page.setViewportSize({width: 720, height: 430});
    await page.setContent('<title>Beam intersection HDR trace</title><body style="margin:0;background:#05070a"><canvas style="display:block;width:420px;height:248px"></canvas>');
    await page.addScriptTag({content: bundle.outputFiles[0].text});
    const keepEvidence = process.env.BEAM_CROSSING_OUTPUT;
    if (keepEvidence) await mkdir(keepEvidence, {recursive: true});
    directory = keepEvidence && await mkdtemp(join(keepEvidence, 'beam-crossing-'));
    if (endpoint) {
      const session = await page.context().newCDPSession(page);
      const {targetInfo} = await session.send('Target.getTargetInfo'); await session.detach();
      t.diagnostic(JSON.stringify({targetId: targetInfo.targetId, retained: retainPage, directory}));
    }
    for (const [separation, quality, width, height] of [[0, 'mobile', 420, 248], [2, 'high', 660, 390]]) {
      const config = await page.evaluate(options => window.crossing.configure(options),
        {width, height, quality, separation});
      if (hardware) assert.doesNotMatch(config.renderer, /swiftshader|llvmpipe|softpipe|software rasterizer/i);
      const results = [];
      for (const [mask, bloom] of [[0, 0], [1, 0], [2, 0], [3, 0], [3, .28]]) {
        const row = await page.evaluate(settings => window.crossing.draw(settings),
          {mask, bloom, picture: Boolean(directory), diagnose: true});
        if (row.picture) {
          await writeFile(join(directory, quality + '-' + mask + '-' + bloom + '.png'),
            Buffer.from(row.picture.split(',')[1], 'base64'));
          delete row.picture;
        }
        results.push(row);
      }
      const [black, a, b, both, bloom] = results;
      const diagnostic = [];
      all.push({config, separation, results, diagnostic});
      for (const gain of [2, 4, 8, 16]) for (const bloom of [0, .28]) {
        const {picture, ...row} = await page.evaluate(settings => window.crossing.draw(settings),
          {mask: 3, gain, bloom, diagnose: true, picture: Boolean(directory)});
        diagnostic.push(row);
        if (picture) await writeFile(join(directory, `${quality}-gain-${gain}-bloom-${bloom}.png`),
          Buffer.from(picture.split(',')[1], 'base64'));
      }
      if (directory) await writeFile(join(directory, 'evidence.json'), JSON.stringify(all, null, 2));
      for (const row of [...results, ...diagnostic]) {
        assert.equal(row.glError, 0);
        for (const [pass, values] of Object.entries(row.extrema)) {
          assert.equal(values.nonfinite, 0, `${pass} must not contain NaN or Infinity at gain ${row.settings.gain}`);
          assert.equal(values.negative, 0, `${pass} must not contain negative radiance at gain ${row.settings.gain}`);
        }
      }
      for (const pass of ['volume', 'linear']) {
        const pixels = both[pass].crossing;
        const sum = a[pass].crossing.map((v, i) => v + b[pass].crossing[i] - black[pass].crossing[i]);
        assert.ok(pixels.every(v => v > .003), 'The overlap should visibly contain both colours');
        assert.ok(maxDifference(pixels, sum) < .001, pass + ' changed the sum of the two independent beams');
      }
      assert.ok(maxDifference(both.linear.crossing, bloom.linear.crossing) < .0002);
      assert.deepEqual(bloom.bloomSource.crossing, [0, 0, 0], 'Crossing pixels must not seed bloom');
      assert.ok(maxDifference(bloom.preBloom.crossing, bloom.postBloom.crossing) < .0015);
      assert.ok(maxDifference(both.screen.crossing, bloom.screen.crossing) < 2 / 255);
      assert.ok(maxDifference(both.screen.lens, bloom.screen.lens) > 8 / 255, 'The bright lens must retain glare');
      assert.deepEqual(black.screen.crossing, [0, 0, 0]);
      if (directory) {
        for (let frame = 0; frame < 11; frame++) {
          const yaw = (frame - 5) * .095;
          const picture = await page.evaluate(yaw => window.crossing.draw({yaw, picture: true, measure: false}), yaw);
          await writeFile(join(directory, quality + '-orbit-' + String(frame).padStart(2, '0') + '.png'),
            Buffer.from(picture.split(',')[1], 'base64'));
        }
      }
      const timings = directory && await page.evaluate(() =>
        Array.from({length: 8}, () => window.crossing.draw({measure: false})));
      all.at(-1).timings = timings;
    }
    if (directory && process.env.BEAM_CROSSING_ROOM_SNAPSHOT) {
      const snapshot = JSON.parse(await readFile(process.env.BEAM_CROSSING_ROOM_SNAPSHOT, 'utf8'));
      await page.setViewportSize({width: 1280, height: 760});
      const config = await page.evaluate(roomRig => window.crossing.configure({
        roomRig, width: 1280, height: 760, quality: 'desktop',
      }), snapshot.snapshot?.rig || snapshot.rig);
      const results = [];
      for (const [mask, bloom, grain] of [[0, 0, 0], [0x55, 0, 0], [0xaa, 0, 0], [-1, 0, 0], [-1, .28, 0], [-1, .28, .0015]]) {
        const {picture, ...row} = await page.evaluate(settings => window.crossing.draw(settings),
          {mask, bloom, grain, picture: true, diagnose: true});
        await writeFile(join(directory, 'room-' + mask + '-' + bloom + '-' + grain + '.png'),
          Buffer.from(picture.split(',')[1], 'base64'));
        results.push(row);
      }
      const timings = await page.evaluate(() => Array.from({length: 8}, () => window.crossing.draw({measure: false})));
      all.push({config, results, timings});
    }
    for (const auto of [true, false]) {
      const quadrature = await page.evaluate(auto => window.crossing.measureQuadrature({auto}), auto);
      all.push({quadrature}); t.diagnostic(JSON.stringify({quadrature}));
      assert.equal(quadrature.finiteNonnegative, true);
      assert.ok(quadrature.adaptive.count > 1000);
      assert.ok(quadrature.adaptive.p95relative < .05, 'Dense crossings must converge without a coarse bright knot');
      assert.ok(quadrature.adaptive.rmse < quadrature.coarse.rmse / 3);
      assert.ok(quadrature.adaptive.maximumOver < .04);
      assert.equal(quadrature.counts.haze.minimum, 24);
      assert.equal(quadrature.counts.haze.maximum, 24);
      assert.equal(quadrature.counts.mobileHaze.minimum, 32);
      assert.equal(quadrature.counts.mobileHaze.maximum, 32);
      assert.equal(quadrature.counts.full.minimum, 72);
      assert.equal(quadrature.counts.full.maximum, 72);
      assert.equal(quadrature.counts.mobileFull.minimum, 64);
      assert.equal(quadrature.counts.mobileFull.maximum, 64);
      assert.equal(quadrature.counts.desktopHaze.minimum, 48);
      assert.equal(quadrature.counts.desktopHaze.maximum, 48);
      assert.equal(quadrature.counts.desktopFull.minimum, 72);
      assert.equal(quadrature.counts.desktopFull.maximum, 72);
      assert.equal(quadrature.counts.highHaze.minimum, 72);
      assert.equal(quadrature.counts.highHaze.maximum, 72);
      assert.equal(quadrature.counts.highFull.minimum, 72);
      assert.equal(quadrature.counts.highFull.maximum, 72);
      assert.ok(quadrature.counts.plume.minimum === 24 && quadrature.counts.plume.maximum > 24 && quadrature.counts.plume.maximum <= 72);
      assert.ok(quadrature.counts.desktopPlume.minimum === 48 && quadrature.counts.desktopPlume.maximum > 48 && quadrature.counts.desktopPlume.maximum <= 72);
    }
    if (directory) await writeFile(join(directory, 'evidence.json'), JSON.stringify(all, null, 2));
  } finally {
    if (directory) await writeFile(join(directory, 'evidence.json'), JSON.stringify(all, null, 2));
    if (!retainPage) {
      await page?.evaluate(() => window.crossing?.dispose()).catch(() => {});
      await page?.close();
    }
    // With CDP, disconnect this transport; keep the shared browser running.
    // An explicitly retained diagnostic page stays paused for inspection.
    await browser.close();
  }
});
