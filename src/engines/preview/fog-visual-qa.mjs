// Read-only host snapshot; all scene/field manipulation remains in this browser.
// Run: node src/engines/preview/fog-visual-qa.mjs [http://127.0.0.1:8500] [output-dir]
import {chromium} from 'playwright-core';
import {mkdir, writeFile} from 'node:fs/promises';
import {resolve} from 'node:path';

const origin = process.argv[2] || 'http://127.0.0.1:8500';
const directory = resolve(process.argv[3] || '/tmp/studio-qa/fog-volume');
await mkdir(directory, {recursive: true});
const browser = await chromium.launch({executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium', headless: true,
  args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
try {
  const page = await browser.newPage({viewport: {width: 1280, height: 800}, deviceScaleFactor: 1}), errors = [];
  page.on('pageerror', error => errors.push(error.message));
  page.on('console', message => { if (message.type() === 'error') errors.push(message.text()); });
  await page.route('**/fog-visual-qa', route => route.fulfill({contentType: 'text/html', body: `<!doctype html>
    <body style="margin:0;background:#080a0c;color:white"><canvas style="display:block;width:1280px;height:760px"></canvas>
    <div id="status"></div><script type="module">import {SceneRenderer as RoomPreview} from '/studio/preview.js?v=${Date.now()}';
    window.engine=new RoomPreview(document.querySelector('canvas'),document.querySelector('#status'));</script></body>`}));
  await page.goto(`${origin}/fog-visual-qa`);
  await page.waitForFunction(() => window.engine?.fog, {timeout: 30000});
  const snapshot = await (await page.request.get(`${origin}/api/state`)).json();
  if (!snapshot.rig?.fixtures) throw new Error('QA requires a real rig snapshot');
  snapshot.output ||= {}; snapshot.output.values = Object.fromEntries(snapshot.rig.fixtures.map(f => [f.id, {'emitted-color': [0, 0, 0]}]));
  await page.evaluate(snapshot => {
    engine.pause(); engine.setQuality('desktop'); engine.update(snapshot); engine.setWorkLight(.35);
    engine.controls.enableDamping = false; engine.setCamera('audience'); engine.fog.field.clear(.07); engine.fog.upload();
  }, snapshot);
  const images = [];
  async function capture(name, amount) {
    const telemetry = await page.evaluate(amount => {
      engine.pause(); engine.setFog(amount); engine.draw(0);
      const gl = engine.renderer.getContext(), debug = gl.getExtension('WEBGL_debug_renderer_info');
      return {...engine.getTelemetry(), gpuRenderer: debug && gl.getParameter(debug.UNMASKED_RENDERER_WEBGL)};
    }, amount);
    await page.screenshot({path: `${directory}/${name}.png`, timeout: 90000});
    images.push({name, telemetry});
  }
  await capture('01-clear-worklight', 0);
  await capture('02-room-haze-worklight', .75);
  await page.evaluate(() => {
    engine.fog.burst({position: [-1.5, .35, 1], direction: [.7, .3, -.2], duration: 1.5, strength: 2.5});
    for (let i = 0; i < 40; i++) engine.fog.field.advance(.05);
    engine.fog.upload();
  });
  await capture('03-fresh-plume-worklight', .75);
  await page.evaluate(() => { for (let i = 0; i < 200; i++) engine.fog.field.advance(.05); engine.fog.upload(); });
  await capture('04-dispersed-plume-worklight', .75);
  await writeFile(`${directory}/evidence.json`, JSON.stringify({scope: 'Actual software WebGL2 screenshots; no mobile performance claim',
    inputs: {snapshot, worklight: .35, quality: 'desktop', initialHaze: .07, freshSeconds: 2, dispersedSeconds: 12,
      source: {position: [-1.5, .35, 1], direction: [.7, .3, -.2], duration: 1.5, strength: 2.5}}, images, errors}, null, 2));
  console.log(JSON.stringify({directory, images: images.map(({name, telemetry}) => ({name, field: telemetry.fog})), errors}));
  await page.evaluate(() => engine.dispose());
  if (errors.length) throw new Error(errors.join('\n'));
} finally { await browser.close(); }
