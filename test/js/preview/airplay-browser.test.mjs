import test from 'node:test';
import assert from 'node:assert/strict';
import {existsSync, readFileSync} from 'node:fs';
import {createServer} from 'node:http';
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
    browser = await chromium.launch({executablePath, args: ['--no-sandbox', '--autoplay-policy=no-user-gesture-required']});
    const page = await browser.newPage();
    await page.goto(origin);
    const result = await page.evaluate(async () => {
      const {startVideo} = await import('/airplay.js');
      const canvas = document.querySelector('canvas');
      const ctx = canvas.getContext('2d');
      const start = performance.now();
      const abort = new AbortController();
      const paint = () => {
        const t = performance.now() - start;
        ctx.fillStyle = Math.floor(t / 700) % 2 ? 'lime' : 'red';
        ctx.fillRect(0, 0, 320, 180);
        ctx.fillStyle = 'white';
        ctx.fillRect(Math.floor(t / 20) % 320, 0, 1, 180);
        canvas.tolglowCaptureVideo?.(canvas);
      };
      paint();
      const clock = setInterval(paint, 32);
      window.stopPreview = () => {abort.abort(); clearInterval(clock);};
      try {
        await new Promise((resolve, reject) => {
          startVideo(canvas, (path, bytes) => fetch('/' + path, {method: 'PUT', body: bytes}),
            resolve, reject, abort.signal).catch(reject);
        });
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
    const {stdout} = await run('ffmpeg', ['-hide_banner', '-loglevel', 'error', '-threads', '1', '-analyzeduration', '0', '-probesize', '75000', '-live_start_index', '0',
      '-i', origin + '/master.m3u8', '-t', '2.5', '-vf', 'scale=1:1', '-c:v', 'rawvideo', '-threads', '1',
      '-pix_fmt', 'rgb24', '-f', 'rawvideo', 'pipe:1'], {encoding: 'buffer', timeout: 45000});
    assert.ok(stdout.length > 30);
    const colors = [];
    for (let i = 0; i + 2 < stdout.length; i += 3) colors.push([...stdout.subarray(i, i + 3)]);
    assert.ok(colors.some(([r, g]) => r > 180 && g < 70), 'No decoded red frame');
    assert.ok(colors.some(([r, g]) => g > 180 && r < 70), 'No decoded green frame');
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
