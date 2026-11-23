import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {existsSync} from 'node:fs';
import test from 'node:test';
import {fileURLToPath} from 'node:url';
import {build} from 'esbuild';
import {chromium} from 'playwright-core';
import {bloomOperands, height, width} from './bloom-storage-fixture.mjs';

test('compare four bloom storage paths over identical binary32 operands', {timeout: 60_000}, async t => {
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  assert.ok(existsSync(executablePath), 'Set CHROMIUM_PATH to an installed Chrome or Chromium executable');
  const {scene, bloom} = bloomOperands();
  const payload = Buffer.alloc((scene.length + bloom.length) * Float32Array.BYTES_PER_ELEMENT);
  let bytes = 0;
  for (const image of [scene, bloom]) {
    for (const value of image) bytes = payload.writeFloatLE(value, bytes);
  }
  const operandsSha256 = createHash('sha256').update(payload).digest('hex');
  assert.equal(operandsSha256, 'da7fa09169909a1683f98806b0bef3408fa987d38c0fbc18a0d8ed78136c3aac');

  const bundle = await build({entryPoints: [fileURLToPath(new URL('./bloom-storage-fixture.mjs', import.meta.url))],
    platform: 'browser', format: 'esm', bundle: true, write: false});
  const headless = process.env.BLOOM_STORAGE_HEADED !== '1';
  const requestSwiftShader = process.env.BLOOM_STORAGE_SWIFTSHADER === '1';
  const args = requestSwiftShader ? ['--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'] : [];
  const browser = await chromium.launch({executablePath, headless, args});
  try {
    const page = await browser.newPage();
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    page.on('console', message => {if (message.type() === 'error') errors.push(message.text());});
    await page.route('http://bloom-storage.test/', route => route.fulfill({contentType: 'text/html', body: '<title>Room bloom storage</title>'}));
    await page.route('http://bloom-storage.test/fixture.js', route => route.fulfill({contentType: 'text/javascript', body: bundle.outputFiles[0].text}));
    await page.goto('http://bloom-storage.test/');
    const result = await page.evaluate(async inputs => (await import('/fixture.js')).compareBloomStorage(inputs),
      {scene: Array.from(scene), bloom: Array.from(bloom)});
    t.diagnostic(JSON.stringify({operandsSha256, raster: [width, height], chrome: browser.version(),
      headless, requestSwiftShader, ...result}));
    assert.deepEqual(errors, []);
    assert.equal(result.components, scene.length);
    assert.equal(result.controls.additiveBlend, true, 'Three used FUNC_ADD, ONE, ONE for RGBA');
    assert.equal(result.controls.initialStorage.changed, 0, 'Blend destination held the exact scene operands');
    assert.equal(result.controls.copySource.changed, 0, 'CopyShader supplied the exact bloom operands');
    assert.equal(result.controls.gpuSum.changed, 0, 'Shader sum matched the independent binary32 CPU sum');
    assert.ok(result.controls.cpuPolicies.changed > 0, 'Corpus distinguishes midpoint rounding and truncation');
    assert.equal(result.comparisons['towardZero/cpuTowardZero'].changed, 0, 'Explicit truncation matches the CPU oracle');
  } finally {
    await browser.close();
  }
});
