import test from 'node:test';
import assert from 'node:assert/strict';
import {createServer} from 'node:http';
import {existsSync} from 'node:fs';
import {build} from 'esbuild';
import {chromium} from 'playwright-core';
import {beginPassSample} from '../../../dev/preview/room-pass-analysis.mjs';

test('private Room diagnostics keep real worker presentations through each pass isolation', {timeout:120000}, async t => {
  const binary = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if (!existsSync(binary)) return t.skip('A local Chromium executable is required.');
  const out = await build({entryPoints:{room:'js/preview/index.js', probe:'dev/preview/room-pass-worker.mjs'},
    format:'esm', bundle:true, platform:'browser', outdir:'/virtual', write:false});
  const assets = new Map(out.outputFiles.map(f => [f.path.split('/').at(-1), Buffer.from(f.contents)]));
  const server = createServer((req, res) => {
    if (req.url === '/studio/room.js') {res.setHeader('Content-Type','text/javascript'); res.end(assets.get('room.js'));}
    else if (req.url === '/studio/preview-worker.js') {res.setHeader('Content-Type','application/javascript'); res.end(assets.get('probe.js'));}
    else if (req.url === '/') {res.setHeader('Content-Type','text/html');res.end('<canvas id="preview" width="220" height="128" style="width:220px;height:128px"></canvas>');}
    else {res.writeHead(404);res.end();}
  });
  await new Promise(ok => server.listen(0,'127.0.0.1',ok));
  const browser = await chromium.launch({executablePath:binary,headless:true,args:['--no-sandbox','--use-angle=swiftshader','--enable-unsafe-swiftshader']});
  try {
    const page = await browser.newPage(), shaderErrors = [];
    page.on('console', message => {if (/shader error|program not valid/i.test(message.text())) shaderErrors.push(message.text());});
    const origin = 'http://127.0.0.1:'+server.address().port;
    await page.goto(origin);
    await page.evaluate(async () => {
      const {RoomPreview} = await import('/studio/room.js'), canvas=document.querySelector('#preview');
      const engine=canvas.tolglowPreview=new RoomPreview(canvas);
      engine.setQuality('mobile');engine.setFog(.35);engine.setWorkLight(.15);
      engine.update({'show-id':'test',epoch:1,revision:1,'frame-number':1,
        rig:{room:{width:12,height:5,depth:10},fixtures:[{id:'spot',position:[-1,3,0]}]},
        output:{values:{spot:{'emitted-color':[.9,.4,.02]}}}});
      await engine.whenReady;
    });
    const peer=page.workers().find(w => w.url().endsWith('/studio/preview-worker.js'));
    assert.ok(peer, 'private worker stayed active');
    await page.evaluate(() => document.querySelector('#preview').tolglowPreview.setQuality('desktop'));
    await page.waitForFunction(() => document.querySelector('#preview').tolglowPreview.getTelemetry().quality === 'desktop');
    // The loop and composite-fallback probes rewrite the production fog shader source.
    for (const mode of ['full','cached-shadows','skip-fog-raymarch','present-cached-frame','simple-fixed-loop','no-composite-fallback','full']) {
      await peer.evaluate(choice => self.setRoomPassMode(choice),mode);
      await page.waitForFunction(choice => {
        const p=document.querySelector('#preview').tolglowPreview.getTelemetry().passProbe;
        return p?.mode === choice && (choice !== 'present-cached-frame' || p.submitMs.replay != null);
      },mode);
      await page.evaluate(beginPassSample,mode);
      await page.evaluate(async () => {
        const engine=document.querySelector('#preview').tolglowPreview;
        await new Promise(ok => engine.worker.addEventListener('message',function next(e) {
          if (e.data.type === 'frame') {engine.worker.removeEventListener('message',next);ok();}
        }));
      });
      const {timings:observations,raymarched}=await page.evaluate(() => window.finishRoomPassSample());
      if (['full','cached-shadows','simple-fixed-loop','no-composite-fallback'].includes(mode)) assert.ok(raymarched > 0);
      else assert.equal(raymarched,0);
      assert.ok(observations.domSubmitMs.samples > 0,mode);
      assert.ok(observations[mode==='present-cached-frame'?'replaySubmitMs':'fogSubmitMs'].samples > 0,mode);
      assert.ok(observations.gpuFenceWaitMs.p50 >= 0);
      if(process.env.ROOM_TEST_MEDIA) await page.locator('#preview').screenshot({path:process.env.ROOM_TEST_MEDIA+'/'+mode+'.png'});
    }
    assert.deepEqual(shaderErrors, []);
  } finally {await browser.close(); await new Promise(ok => server.close(ok));}
});
