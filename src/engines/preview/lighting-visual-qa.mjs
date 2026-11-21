// Controlled optics/atmosphere scenes using the actual host rig. Only browser
// projections are replaced; no lighting command or fixture mutation is sent.
import {chromium} from 'playwright-core';
import {mkdir, writeFile} from 'node:fs/promises';
const origin = process.argv[2] || 'http://127.0.0.1:8500';
const directory = process.argv[3] || '/tmp/studio-qa/preview-optics';
await mkdir(directory, {recursive: true});
const browser = await chromium.launch({executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium', headless: true,
  args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
try {
  const page = await browser.newPage({viewport: {width: 1280, height: 800}, deviceScaleFactor: 1}), errors = [];
  page.on('pageerror', error => errors.push(error.message));
  page.on('console', message => {if (message.type() === 'error') errors.push(message.text());});
  await page.route('**/preview-optics-qa', route => route.fulfill({contentType: 'text/html', body: `<!doctype html>
    <body style="margin:0;background:#080a0c;color:white"><canvas style="display:block;width:1280px;height:760px"></canvas>
    <div id="status"></div><script type="module">import {RoomPreview} from '/studio/preview.js?v=${Date.now()}';
    window.engine=new RoomPreview(document.querySelector('canvas'),document.querySelector('#status'));</script></body>`}));
  await page.goto(`${origin}/preview-optics-qa`);
  await page.waitForFunction(() => window.engine?.fog, {timeout: 30000});
  const snapshot = await (await page.request.get(`${origin}/api/state`)).json();
  await page.evaluate(snapshot => {
    cancelAnimationFrame(engine.frame); engine.setQuality('desktop'); engine.update(snapshot);
    engine.controls.enableDamping = false; engine.setCamera('audience');
    engine.fog.field.clear(.07); engine.fog.upload(); window.snapshot = snapshot;
  }, snapshot);
  const captures = [];
  async function capture(name, mode, fog, worklight) {
    const telemetry = await page.evaluate(({mode, fog, worklight}) => {
      cancelAnimationFrame(engine.frame);
      const state = structuredClone(window.snapshot); state.output ||= {}; state.output.values = {};
      state.rig.fixtures.forEach((fixture, i) => {
        const active = mode === 'crossing' || (mode === 'single' && i === 3);
        const rgb = active ? (i % 2 ? [.21, .48, .67] : [.78, .22, .045]) : [0, 0, 0];
        state.output.values[fixture.id] = {'emitted-color': rgb, direction: [(i - 3.5) * -.22, -1, .45], zoom: mode === 'single' ? .02 : .16, focus: .2};
      });
      engine.setWorkLight(worklight); engine.setFog(fog); engine.update(state); engine.draw(0);
      return engine.getTelemetry();
    }, {mode, fog, worklight});
    await page.screenshot({path: `${directory}/${name}.png`, timeout: 90000});
    captures.push({name, mode, fog, worklight, telemetry}); console.log(name);
  }
  await capture('01-blackout-dark-room', 'off', .65, 0);
  await capture('02-single-no-fog', 'single', 0, .12);
  await capture('03-single-finite-aperture', 'single', .65, .12);
  await capture('04-crossing-beams-room-haze', 'crossing', .65, .12);
  await page.evaluate(() => {
    engine.fog.burst({position: [-1.5, .35, 1], direction: [.7, .3, -.2], duration: 1.5, strength: 5});
    for (let i = 0; i < 40; i++) engine.fog.field.advance(.05);
    engine.fog.upload();
  });
  await capture('05-crossing-fresh-fog', 'crossing', .65, .12);
  await page.evaluate(() => {for (let i = 0; i < 200; i++) engine.fog.field.advance(.05); engine.fog.upload();});
  await capture('06-crossing-dispersed-fog', 'crossing', .65, .12);
  await writeFile(`${directory}/evidence.json`, JSON.stringify({scope: 'Actual Three/WebGL2 controlled browser projection; no show or hardware changes', snapshot, captures, errors}, null, 2));
  await page.evaluate(() => engine.dispose());
  if (errors.length) throw new Error(errors.join('\n'));
} finally {await browser.close();}
