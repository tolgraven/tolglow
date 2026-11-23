import assert from 'node:assert/strict';
import {existsSync} from 'node:fs';
import test from 'node:test';
import {build} from 'esbuild';
import {chromium} from 'playwright-core';

test('the opt-in bloom pass saves a draw while retaining the Room pixels', {timeout: 240_000}, async t => {
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!existsSync(executablePath)) return t.skip('Set CHROMIUM_PATH to run the Room WebGL comparison.');
  const bundle = await build({entryPoints: [new URL('./bloom-display-fixture.mjs', import.meta.url).pathname],
    platform: 'browser', format: 'esm', bundle: true, write: false});
  const browser = await chromium.launch({executablePath, headless: true,
    args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
  try {
    const page = await browser.newPage();
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    page.on('console', message => {if (message.type() === 'error') errors.push(message.text());});
    await page.route('http://bloom-display.test/', route => route.fulfill({contentType: 'text/html', body: '<title>Room bloom comparison</title>'}));
    await page.route('http://bloom-display.test/fixture.js', route => route.fulfill({contentType: 'text/javascript', body: bundle.outputFiles[0].text}));
    await page.goto('http://bloom-display.test/');
    const result = await page.evaluate(async () => (await import('/fixture.js')).compareBloomPaths());
    t.diagnostic(JSON.stringify(result));
    assert.deepEqual(errors, []);
    assert.notDeepEqual(result.scene, result.display);
    assert.ok(result.bloomContribution.changed > 100, 'bloom must visibly affect the reference');
    assert.ok(result.glassContribution.changed > 100, 'glass must visibly affect the reference');
    for (const {name, calls, differences, repeat} of result.cases) {
      const disabled = name === 'bloom disabled';
      assert.equal(calls[0] - calls[1], disabled ? 0 : 1, name);
      assert.equal(repeat.changed, 0, `${name} baseline is deterministic`);
      if (name !== 'extended transform with glass') {
        assert.equal(differences.changed, 0, `${name} fused frame matches baseline`);
      } else {
        assert.ok(differences.maximum <= 1 && differences.mean <= 0.001,
          `${name} stays within one RGB8 level and at most 0.1% changed channels`);
      }
    }
  } finally {
    await browser.close();
  }
});
