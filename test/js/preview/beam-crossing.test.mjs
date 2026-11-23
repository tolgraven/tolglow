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
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!existsSync(executablePath)) return t.skip('Set CHROMIUM_PATH to run actual WebGL2');
  const bundle = await build({
    entryPoints: [new URL('../../../dev/preview/beam-crossing-fixture.mjs', import.meta.url).pathname],
    bundle: true,
    format: 'iife',
    write: false,
  });
  const hardware = process.env.BEAM_CROSSING_HARDWARE === '1';
  const browser = await chromium.launch({
    executablePath,
    headless: !hardware,
    args: hardware ? [] : ['--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader', '--disable-gpu-shader-disk-cache'],
  });
  try {
    const page = await browser.newPage({viewport: {width: 720, height: 430}, deviceScaleFactor: 1});
    await page.setContent('<body style="margin:0"><canvas style="display:block;width:420px;height:248px"></canvas>');
    await page.addScriptTag({content: bundle.outputFiles[0].text});
    const keepEvidence = process.env.BEAM_CROSSING_OUTPUT;
    if (keepEvidence) await mkdir(keepEvidence, {recursive: true});
    const directory = keepEvidence && await mkdtemp(join(keepEvidence, 'beam-crossing-'));
    const all = [];
    for (const [separation, quality, width, height] of [[0, 'mobile', 420, 248], [2, 'high', 660, 390]]) {
      const config = await page.evaluate(options => window.crossing.configure(options),
        {width, height, quality, separation});
      if (hardware) assert.doesNotMatch(config.renderer, /swiftshader|llvmpipe|softpipe|software rasterizer/i);
      const results = [];
      for (const [mask, bloom] of [[0, 0], [1, 0], [2, 0], [3, 0], [3, .28]]) {
        const row = await page.evaluate(settings => window.crossing.draw(settings),
          {mask, bloom, picture: Boolean(directory)});
        if (row.picture) {
          await writeFile(join(directory, quality + '-' + mask + '-' + bloom + '.png'),
            Buffer.from(row.picture.split(',')[1], 'base64'));
          delete row.picture;
        }
        results.push(row);
      }
      const [black, a, b, both, bloom] = results;
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
      all.push({config, separation, results, timings});
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
          {mask, bloom, grain, picture: true});
        await writeFile(join(directory, 'room-' + mask + '-' + bloom + '-' + grain + '.png'),
          Buffer.from(picture.split(',')[1], 'base64'));
        results.push(row);
      }
      const timings = await page.evaluate(() => Array.from({length: 8}, () => window.crossing.draw({measure: false})));
      all.push({config, results, timings});
    }
    if (directory) await writeFile(join(directory, 'evidence.json'), JSON.stringify(all, null, 2));
    await page.evaluate(() => window.crossing.dispose());
  } finally {
    await browser.close();
  }
});
