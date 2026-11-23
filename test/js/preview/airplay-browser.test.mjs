import test from 'node:test';
import assert from 'node:assert/strict';
import {existsSync, readFileSync} from 'node:fs';
import {createServer} from 'node:http';
import {mkdtemp, rm, writeFile} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {execFile} from 'node:child_process';
import {promisify} from 'node:util';
import {chromium} from 'playwright-core';

const run = promisify(execFile);

test('the HLS receiver decodes changing colors from the actual canvas capture', {timeout: 75000}, async t => {
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!existsSync(executablePath)) return t.skip('Provide Chromium for canvas capture');
  try { await run('ffmpeg', ['-version']); }
  catch { return t.skip('Provide FFmpeg for receiver decoding'); }

  const files = new Map();
  const module = readFileSync(new URL('../../../resources/studio/airplay.js', import.meta.url));
  const server = createServer(async (request, response) => {
    const file = request.url.slice(1);
    if (request.method === 'PUT') {
      const chunks = [];
      for await (const chunk of request) chunks.push(chunk);
      files.set(file, Buffer.concat(chunks));
      response.writeHead(204).end();
    } else if (file === 'airplay.js') {
      response.writeHead(200, {'Content-Type': 'text/javascript'}).end(module);
    } else if (files.has(file)) {
      response.writeHead(200, {'Content-Type': file.endsWith('.ts') ? 'video/mp2t' : 'application/vnd.apple.mpegurl'}).end(files.get(file));
    } else if (!file) {
      response.writeHead(200, {'Content-Type': 'text/html'}).end('<canvas width="320" height="180"></canvas>');
    } else response.writeHead(404).end();
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  const origin = 'http://127.0.0.1:' + server.address().port;
  let browser;
  try {
    browser = await chromium.launch({executablePath, args: ['--no-sandbox', '--autoplay-policy=no-user-gesture-required', '--use-gl=angle',
      '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
    const page = await browser.newPage();
    await page.goto(origin);
    const result = await page.evaluate(async () => {
      const {startVideo} = await import('/airplay.js');
      const canvas = document.querySelector('canvas');
      const gl = canvas.getContext('webgl2', {preserveDrawingBuffer: false});
      if (!gl) return 'WebGL2 context unavailable';
      const abort = new AbortController();
      let draws = 0;
      const paint = green => {
        gl.clearColor(green ? 0 : 1, green ? 1 : 0, 0, 1);
        gl.clear(gl.COLOR_BUFFER_BIT);
        canvas.tolglowCaptureVideo?.(canvas);
      };
      // A paused renderer has no retained drawing buffer. Its first requested
      // draw must populate HLS even before the regular animation clock starts.
      canvas.tolglowPreview = {draw() {draws++; paint(false);}};
      window.stopPreview = () => abort.abort();
      window.showGreen = () => paint(true);
      try {
        await new Promise((resolve, reject) => {
          startVideo(canvas, (path, bytes) => fetch('/' + path, {method: 'PUT', body: bytes}),
            resolve, reject, abort.signal).catch(reject);
        });
        if (draws !== 1) return 'No requested presentation';
        return 'ready';
      } catch (error) {
        return String(error);
      }
    });
    if (/no H\.264 encoder/.test(result)) {
      if (process.env.REQUIRE_H264) assert.fail(result);
      return t.skip(result);
    }
    assert.equal(result, 'ready');
    const directory = await mkdtemp(join(tmpdir(), 'tolglow-hls-'));
    t.after(() => rm(directory, {recursive: true, force: true}));
    const master = await (await fetch(origin + '/master.m3u8')).text();
    const variant = master.split('\n').find(line => line.endsWith('.m3u8'));
    assert.ok(variant);
    const receipt = async () => {
      const manifest = await (await fetch(origin + '/' + variant)).text();
      const file = manifest.split('\n').filter(line => line.endsWith('.ts')).at(-1);
      return {file, number: Number(file.match(/-(\d+)\.ts$/)[1])};
    };
    const decode = async ({file}) => {
      const segment = new Uint8Array(await (await fetch(origin + '/' + file)).arrayBuffer());
      const target = join(directory, file);
      await writeFile(target, segment);
      const {stdout} = await run('ffmpeg', ['-hide_banner', '-loglevel', 'error', '-threads', '1',
        '-i', target, '-vf', 'scale=1:1', '-frames:v', '1', '-c:v', 'rawvideo', '-threads', '1',
        '-pix_fmt', 'rgb24', '-f', 'rawvideo', 'pipe:1'], {encoding: 'buffer', timeout: 10000});
      return [...stdout];
    };
    const red = await receipt();
    const [r, g] = await decode(red);
    assert.ok(r > 180 && g < 70, 'The first requested presentation did not reach the receiver');
    await page.evaluate(() => window.showGreen());
    const before = await receipt();
    let last = before;
    let rgb = [r, g];
    const until = performance.now() + 22000;
    do {
      await new Promise(resolve => setTimeout(resolve, 150));
      const next = await receipt();
      if (next.number > last.number) {
        last = next;
        rgb = await decode(last);
      }
    } while (!(rgb[1] > 180 && rgb[0] < 70) && performance.now() < until);
    assert.ok(rgb[1] > 180 && rgb[0] < 70, `Expected green, received ${rgb} in ${last.file}`);
    await page.evaluate(() => window.stopPreview());
  } catch (error) {
    t.diagnostic('Receiver playlist: ' + files.get('playlist-1.m3u8')?.toString());
    throw error;
  } finally {
    await browser?.close();
    server.closeAllConnections();
    await new Promise(resolve => server.close(resolve));
  }
});
