// Render a showcase capture through the production Room renderer.
//
//   lein run -m tolglow.qa.showcase-capture target/showcase 16 26
//   node dev/preview/showcase-capture.mjs target/showcase --video --stills 16.5,20.02
//
// Reads rig.json, frames.ndjson and meta.json from the directory, loads the
// released resources/studio/preview.js bundle in headless Chrome and draws one
// Room frame per committed show frame. Frames are replayed in order with the
// show's own frame period as the fog and film time step, so a capture renders
// the same way every time. Nothing here starts a show or opens a device.
//
// Options:
//   --video [name]     encode every frame to <dir>/<name>.mp4 (default room.mp4)
//   --stills a,b,c     save <dir>/still-<bar>.png for these zero-based bars
//   --size WxH         raster size (default 1920x1080)
//   --drift metres     slow sideways camera travel across the clip (default 0)
//   --work-light n     preview work light, 0 to 1 (default 0.04)
//   --software         render with SwiftShader instead of the machine's GPU
//
// CHROMIUM_PATH selects the browser. The GPU that drew the frames is recorded
// in <dir>/render.json. A headless run is not a foreground display capture.
import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {createHash} from 'node:crypto';
import {createReadStream} from 'node:fs';
import {mkdir, readFile, writeFile} from 'node:fs/promises';
import {join, resolve} from 'node:path';
import {createInterface} from 'node:readline';
import {chromium} from 'playwright-core';

const args = process.argv.slice(2);
const dir = resolve(args[0] || 'target/showcase');
const option = (name, fallback) => {
  const index = args.indexOf(name);
  if (index < 0) return undefined;
  const value = args[index + 1];
  return value && !value.startsWith('--') ? value : fallback;
};
const videoName = option('--video', 'room');
const stillBars = (option('--stills', '') || '').split(',').filter(Boolean).map(Number);
const [width, height] = (option('--size', '1920x1080') || '1920x1080').split('x').map(Number);
const drift = Number(option('--drift', '0') || 0);
const workLight = Number(option('--work-light', '0.04') ?? 0.04);
const software = args.includes('--software');
assert.ok(Number.isInteger(width) && Number.isInteger(height) && width >= 320 && height >= 180, 'Give --size as WxH');
assert.ok(stillBars.every(Number.isFinite), 'Give --stills as comma-separated bars');
assert.ok(videoName || stillBars.length, 'Ask for --video, --stills or both');

const meta = JSON.parse(await readFile(join(dir, 'meta.json'), 'utf8'));
const rig = JSON.parse(await readFile(join(dir, 'rig.json'), 'utf8'));
const bundle = await readFile('resources/studio/preview.js');
const beatsPerBar = meta.meter['beats-per-bar'];
const dt = meta['frame-ms'] / 1000;
const fps = 1000 / meta['frame-ms'];

const profile = resolve('target/showcase-chrome-profile');
await mkdir(profile, {recursive: true});
const context = await chromium.launchPersistentContext(profile, {
  headless: true, viewport: {width, height}, deviceScaleFactor: 1,
  executablePath: process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  args: software
    ? ['--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']
    : ['--use-angle=metal', '--enable-gpu', '--ignore-gpu-blocklist'],
});
const errors = [];
let encoder, encoded;
try {
  const page = context.pages()[0] || await context.newPage();
  page.on('pageerror', error => errors.push(error.message));
  page.on('console', message => { if (message.type() === 'error') errors.push(message.text()); });
  await page.route('http://preview.test/scene', r => r.fulfill({contentType: 'text/html', body: ''}));
  await page.route('http://preview.test/preview.js', r => r.fulfill({contentType: 'text/javascript', body: bundle}));
  await page.goto('http://preview.test/scene');
  const renderer = await page.evaluate(async ({rig, workLight}) => {
    const {SceneRenderer} = await import('/preview.js');
    document.documentElement.style.margin = document.body.style.margin = '0';
    document.documentElement.style.background = '#000';
    const canvas = document.createElement('canvas');
    canvas.style.cssText = 'display:block;width:100vw;height:100vh';
    document.body.append(canvas);
    const engine = new SceneRenderer(canvas, document.createElement('output'));
    window.room = engine; window.rig = rig;
    await engine.whenReady;
    if (!engine.ready) throw Error(engine.error || 'WebGL2 scene unavailable');
    engine.pause();
    engine.setQuality('high');
    engine.setWorkLight(workLight);
    engine.controls.enableDamping = false;
    engine.setCamera('audience');
    window.home = {position: engine.camera.position.toArray(), target: engine.controls.target.toArray()};
    const t = engine.getTelemetry();
    return {renderer: t.renderer, gpu: t.deviceRenderer, software: t.softwareRenderer, userAgent: navigator.userAgent};
  }, {rig, workLight});
  console.log('Room renderer:', renderer.gpu || renderer.renderer);
  assert.equal(Boolean(renderer.software), software, 'The Room is not drawing on the requested renderer');

  if (videoName) {
    encoder = spawn('ffmpeg', ['-y', '-loglevel', 'error', '-f', 'image2pipe', '-framerate', String(fps), '-c:v', 'png', '-i', '-',
      '-c:v', 'libx264', '-preset', 'slow', '-crf', '16', '-pix_fmt', 'yuv420p', '-movflags', '+faststart',
      join(dir, videoName + '.mp4')], {stdio: ['pipe', 'inherit', 'inherit']});
    encoded = new Promise((ok, fail) => {
      encoder.on('error', fail);
      encoder.on('close', code => code === 0 ? ok() : fail(new Error('ffmpeg exited with ' + code)));
    });
  }
  const pendingStills = stillBars.map(bar => ({bar, done: false}));
  const lines = createInterface({input: createReadStream(join(dir, 'frames.ndjson')), crlfDelay: Infinity});
  const digest = createHash('sha256');
  const samples = [];
  let index = 0, last;
  for await (const line of lines) {
    if (!line) continue;
    const frame = JSON.parse(line);
    const bar = frame.beat / beatsPerBar;
    const wanted = pendingStills.filter(still => !still.done && bar >= still.bar);
    const {png, sample} = await page.evaluate(({frame, progress, drift, dt, encode}) => {
      const engine = window.room;
      frame.rig = window.rig;
      if (drift) {
        const offset = drift * (progress - .5);
        engine.camera.position.fromArray(window.home.position); engine.camera.position.x += offset;
        engine.controls.target.fromArray(window.home.target); engine.controls.update();
      }
      engine.update(frame);
      engine.clock += dt;
      engine.draw(dt);
      engine.renderer.getContext().finish();
      // Read in the draw task, before Chrome clears the unpreserved buffer.
      const png = encode ? engine.canvas.toDataURL('image/png').split(',')[1] : null;
      const t = engine.getTelemetry();
      return {png, sample: {lights: t.activeLights, scene: t.pixels, display: t.displayPixels, tier: t.quality,
        fog: t.fog?.atmosphere, frameNumber: t.frameNumber}};
    }, {frame, progress: meta.frames > 1 ? index / (meta.frames - 1) : 0, drift, dt, encode: Boolean(encoder) || wanted.length > 0});
    if (png) {
      const image = Buffer.from(png, 'base64');
      digest.update(image);
      if (encoder && !encoder.stdin.write(image)) await new Promise(ok => encoder.stdin.once('drain', ok));
      for (const still of wanted) {
        still.done = true;
        const name = 'still-bar-' + String(still.bar).replace('.', '_') + '.png';
        await writeFile(join(dir, name), image);
        samples.push({still: name, bar, beat: frame.beat, running: frame.running, ...sample});
        console.log(name, 'bar', bar.toFixed(2), sample.lights, 'lights');
      }
    }
    last = {bar, ...sample};
    index++;
    if (index % 100 === 0) console.log('frame', index, 'of', meta.frames, 'bar', bar.toFixed(2));
  }
  if (encoder) { encoder.stdin.end(); await encoded; }
  assert.equal(index, meta.frames, 'Every captured frame must be drawn');
  assert.deepEqual(last.scene, [width, height], 'The scene must be drawn at the requested size');
  assert.deepEqual(errors, []);
  const report = {browser: context.browser()?.version() ?? renderer.userAgent, ...renderer, size: [width, height], fps,
    frames: index, drift, workLight, bundleSha256: createHash('sha256').update(bundle).digest('hex'),
    imagesSha256: digest.digest('hex'), video: videoName ? videoName + '.mp4' : null, stills: samples, capture: meta, last};
  await writeFile(join(dir, 'render.json'), JSON.stringify(report, null, 2) + '\n');
  await page.evaluate(() => window.room.dispose());
} finally {
  if (encoder && encoder.exitCode == null) encoder.stdin.destroy();
  await context.close();
}
