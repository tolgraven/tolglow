import test from 'node:test';
import assert from 'node:assert/strict';
import {existsSync} from 'node:fs';
import {createServer} from 'node:http';
import {build} from 'esbuild';
import {chromium} from 'playwright-core';

test('actual glass shader refracts room stripes, preserves a CSG hole and rejects foreground occlusion', {timeout:60000}, async t=>{
  const executablePath=process.env.CHROMIUM_PATH || '/usr/bin/chromium';
  if(!existsSync(executablePath)) return t.skip('Set CHROMIUM_PATH to run the WebGL2 probe');
  const fixture=await build({entryPoints:[new URL('./glass-fixture.mjs',import.meta.url).pathname],bundle:true,
    format:'esm',platform:'browser',write:false});
  const server=createServer((req,res)=>{
    const script=req.url==='/fixture.js';
    res.setHeader('Content-Type',script?'text/javascript':'text/html');
    res.end(script?fixture.outputFiles[0].text:'<canvas></canvas>');
  });
  await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));
  let browser;
  try{
    browser=await chromium.launch({executablePath,args:['--no-sandbox','--use-angle=swiftshader','--enable-unsafe-swiftshader']});
    const page=await browser.newPage(),errors=[];
    page.on('pageerror',e=>errors.push(e.message));
    page.on('console',message=>{if(message.type()==='error')errors.push(message.text());});
    await page.goto('http://127.0.0.1:'+server.address().port);
    const stats=await page.evaluate(async()=>{const {probe}=await import('/fixture.js');return probe(document.querySelector('canvas'));});
    assert.deepEqual(errors,[]);
    assert.equal(stats.hole,0);assert.equal(stats.foreground,0);assert.equal(stats.outside,0);
    assert.ok(stats.altered>1500,JSON.stringify(stats));assert.ok(stats.shiftedStripes>200,JSON.stringify(stats));
  }finally{await browser?.close();await new Promise(resolve=>server.close(resolve));}
});
