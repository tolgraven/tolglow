// Exercise the actual re-frame page and owned graphics lifecycle. No show
// commands, hardware or fabricated application UI. Viewports are emulation.
import {chromium} from 'playwright-core';
import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
const origin = process.argv[2] || 'http://127.0.0.1:8500';
const directory = process.argv[3] || '/tmp/studio-qa/preview-app';
await mkdir(directory, {recursive: true});
const browser = await chromium.launch({executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium', headless: true,
  args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
const results = [];
try {
  for (const [name, width, height, touch] of [['desktop', 1440, 1000, false], ['ipad', 820, 1180, true], ['phone', 390, 844, true]]) {
    const context = await browser.newContext({viewport: {width, height}, hasTouch: touch, deviceScaleFactor: 1});
    const page = await context.newPage(), errors = [];
    page.setDefaultTimeout(60000);
    page.on('pageerror', error => errors.push(error.message));
    page.on('console', message => {if (message.type() === 'error') errors.push(message.text());});
    console.log(`${name}: opening studio`);
    await page.goto(origin, {waitUntil: 'domcontentloaded'});
    await page.waitForFunction(() => document.querySelector('#connection-label')?.textContent === 'Studio connected');
    console.log(`${name}: connected`);
    await page.locator('[data-view="perform"]').click();
    if (name === 'phone') await page.getByRole('button', {name: 'Room', exact: true}).click();
    const panel = page.locator('#preview-panel');
    await panel.scrollIntoViewIfNeeded();
    await page.waitForFunction(() => document.querySelector('#preview')?.tolglowPreview?.ready);
    console.log(`${name}: real renderer ready`);
    const automatic = await page.evaluate(async () => {
      const engine = document.querySelector('#preview').tolglowPreview;
      engine.pause(); return engine.getTelemetry();
    });
    if (automatic.softwareRenderer) {
      assert.equal(automatic.quality, 'low');
      assert.ok(automatic.pixels[0] * automatic.pixels[1] <= 181000);
    }
    await page.getByLabel('Preview quality', {exact: true}).selectOption(touch ? 'mobile' : 'desktop');
    await page.getByLabel('Preview camera', {exact: true}).selectOption('overview');
    await page.waitForFunction(() => document.querySelector('#preview').tolglowPreview.cameraName === 'overview');
    await page.getByRole('button', {name: 'Reset preview camera', exact: true}).click();
    await page.waitForFunction(() => document.querySelector('#preview').tolglowPreview.cameraName === 'audience');
    for (const [label, value] of [['Preview work light', '0.35'], ['Preview fog density', '0.65']]) {
      await page.getByLabel(label, {exact: true}).evaluate((element, value) => {
        Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value').set.call(element, value);
        element.dispatchEvent(new Event('input', {bubbles: true}));
      }, value);
    }
    await page.waitForFunction(() => {
      const engine = document.querySelector('#preview').tolglowPreview;
      return engine.workLight === .35 && engine.fogAmount === .65;
    });
    await page.getByRole('button', {name: '+ Plume', exact: true}).click();
    await page.getByRole('button', {name: 'Expand', exact: true}).click();
    await page.waitForFunction(() => {
      const panel = document.querySelector('#preview-panel');
      const rect = panel?.querySelector('#preview')?.getBoundingClientRect();
      if (!panel?.classList.contains('expanded') || !rect?.height) return false;
      const visibleHeight = Math.min(rect.bottom, innerHeight) - Math.max(rect.top, 0);
      return visibleHeight >= Math.min(rect.height, innerHeight) - 1;
    });
    const telemetry = await page.evaluate(async () => {
      const engine = document.querySelector('#preview').tolglowPreview;
      engine.pause(); await engine.draw(0); return engine.getTelemetry();
    });
    assert.equal(telemetry.failed, null);
    assert.equal(telemetry.quality, touch ? 'mobile' : 'desktop');
    assert.ok(telemetry.fogAmount === .65 && telemetry.workLight === .35);
    const controls = await panel.locator('button,select,input').evaluateAll(nodes => nodes.map(node => {
      const r = node.getBoundingClientRect(); return {name: node.getAttribute('aria-label') || node.textContent, width: r.width, height: r.height};
    }));
    assert.ok(controls.every(control => control.height >= 44), `${name} preview controls are below touch height`);
    await page.screenshot({path: `${directory}/${name}-expanded.png`, fullPage: true, timeout: 90000});
    console.log(`${name}: expanded screenshot`);
    if (name === 'desktop') {
      await page.getByRole('button', {name: 'Fullscreen preview', exact: true}).click();
      await page.waitForFunction(() => document.fullscreenElement?.id === 'preview-panel');
      await page.evaluate(async () => {const engine = document.querySelector('#preview').tolglowPreview; engine.resize(); await engine.draw(0);});
      await page.screenshot({path: `${directory}/desktop-fullscreen.png`, timeout: 90000});
      await page.evaluate(() => document.exitFullscreen());
    }
    // The live engine and all scene resources must be removed on route change.
    const lifecycle = await page.evaluate(async () => {
      const engine = document.querySelector('#preview').tolglowPreview;
      engine.dispose(); return {disposed: engine.disposed, canvasOwnerRemoved: !document.querySelector('#preview').tolglowPreview};
    });
    assert.ok(lifecycle.disposed && lifecycle.canvasOwnerRemoved);
    assert.deepEqual(errors, []);
    results.push({name, viewport: {width, height}, automatic, telemetry, controls, lifecycle, errors});
    await context.close();
  }
  await writeFile(`${directory}/evidence.json`, JSON.stringify({scope: 'Actual Chromium application controls; viewport emulation, not device/Safari performance', results}, null, 2));
  console.log(JSON.stringify({directory, passed: results.map(result => result.name)}));
} finally {await browser.close();}
