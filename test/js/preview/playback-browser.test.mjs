import test from 'node:test';
import assert from 'node:assert/strict';
import {existsSync, readFileSync} from 'node:fs';
import {createServer} from 'node:http';
import {chromium} from 'playwright-core';

test('Chromium captures live pixels and opens the system PiP window on supported desktops', {timeout: 30000}, async t => {
  const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!existsSync(executablePath)) return t.skip('Provide CHROMIUM_PATH for media integration');
  const script = readFileSync(new URL('../../../js/preview/playback.js', import.meta.url));
  const server = createServer((req, res) => {
    if (req.url === '/playback.js') { res.setHeader('Content-Type', 'text/javascript'); res.end(script); }
    else {
      res.setHeader('Content-Type', 'text/html');
      res.end('<!doctype html><canvas width="384" height="216"></canvas><video muted playsinline></video><button>Open</button>');
    }
  });
  await new Promise(resolve => server.listen(0, '127.0.0.1', resolve));
  let browser;
  try {
    browser = await chromium.launch({executablePath, headless: !process.env.DISPLAY, args: ['--no-sandbox']});
    const page = await browser.newPage();
    await page.goto(`http://127.0.0.1:${server.address().port}`);
    const capable = await page.evaluate(async () => {
      const {CanvasPlayback, canPresentCanvas} = await import('/playback.js');
      const canvas = document.querySelector('canvas'), video = document.querySelector('video');
      const supported = canPresentCanvas(canvas, video);
      const ctx = canvas.getContext('2d');
      window.ink = value => {ctx.fillStyle = value; ctx.fillRect(0, 0, canvas.width, canvas.height);};
      window.media = new CanvasPlayback(canvas, video, state => document.title = state, message => document.title = message);
      window.visible = [];
      canvas.addEventListener('tolglow-preview-external', e => visible.push(e.detail));
      document.querySelector('button').onclick = () => media.present();
      media.prepare();
      requestAnimationFrame(() => ink('red'));
      return supported;
    });
    await page.waitForFunction(() => document.title === 'ready');
    if (capable) {
      await page.click('button');
      await page.waitForFunction(() => document.title === 'active');
    } else {
      t.diagnostic('The browser has no system PiP window. Checking its captured video track.');
    }
    const sample = await page.evaluate(async () => {
      const video = document.pictureInPictureElement || document.querySelector('video');
      const next = new Promise(resolve => video.requestVideoFrameCallback(resolve));
      ink('lime');
      await next;
      const sample = document.createElement('canvas').getContext('2d');
      sample.drawImage(video, 0, 0, 1, 1);
      const [r, g, b] = sample.getImageData(0, 0, 1, 1).data;
      const track = video.srcObject.getVideoTracks()[0];
      media.close();
      return {r, g, b, width: video.videoWidth, ended: track.readyState, visible};
    });
    assert.ok(sample.g > 200 && sample.r < 40 && sample.b < 40, JSON.stringify(sample));
    assert.equal(sample.ended, 'ended');
    assert.deepEqual(sample.visible, capable ? [true, false] : []);
  } finally {
    await browser?.close();
    await new Promise(resolve => server.close(resolve));
  }
});
