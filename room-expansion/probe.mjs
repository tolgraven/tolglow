import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {createHash} from 'node:crypto';
import {mkdtemp, readFile, writeFile} from 'node:fs/promises';
import {join, resolve} from 'node:path';
import {setTimeout as delay} from 'node:timers/promises';
import {chromium} from 'playwright-core';

const [app, output, studioPath, resources] = process.argv.slice(2);
const landing = await mkdtemp('/private/tmp/tolglow-room-reveal-');
const java = join(app,'Contents/Resources/runtime/bin/java');
const artifact = join(app,'Contents/Resources/studio.jar');
const classpath = [studioPath, resources, artifact].filter(Boolean).map(item=>resolve(item)).join(':');
const file = join(landing,'port');
await writeFile(join(landing,'show.clj'),
 `(ns show)
(def config {:show {:credential-path "credentials.edn" :workspace-options nil}
 :init {:components [:show] :modules [:web-server]}})`);
const server = spawn(java,['-Djava.awt.headless=true','-cp',classpath,'clojure.main','-m','tolglow.desktop',landing,file],
  {cwd:landing,stdio:['pipe','inherit','inherit']});
let browser, controller, targetId;
try {
  let port;
  for(let i=0;i<180;i++){
    if(server.exitCode!==null) throw Error('Loopback show exited');
    port=(await readFile(file,'utf8').catch(()=> '')).trim();
    if(/^\d+$/.test(port)) break;
    await delay(500);
  }
  assert.match(port,/^\d+$/);
  const origin='http://127.0.0.1:'+port;
  const served=await fetch(origin+'/studio/js/main.js');
  assert.ok(served.ok);
  const mainSHA256=createHash('sha256').update(Buffer.from(await served.arrayBuffer())).digest('hex');
  if(studioPath) {
    const expected=createHash('sha256').update(await readFile(join(studioPath,'studio/js/main.js'))).digest('hex');
    assert.equal(mainSHA256,expected);
  }
  browser=await chromium.connectOverCDP('http://127.0.0.1:9223');
  const context=browser.contexts()[0];
  controller=await browser.newBrowserCDPSession();
  const created=context.waitForEvent('page');
  ({targetId}=await controller.send('Target.createTarget',{url:'about:blank',background:true}));
  const page=await created;
  const cdp=await context.newCDPSession(page);
  assert.equal((await cdp.send('Target.getTargetInfo')).targetInfo.targetId,targetId);
  const snapshots=[];
  for(const [width,height] of [[1024,660],[1180,730],[1440,900]]) {
    await cdp.send('Emulation.setDeviceMetricsOverride',{width,height,deviceScaleFactor:1,mobile:false});
    await page.goto(origin);
    await page.evaluate(()=>{document.title='Tolglow Room expand layout test';});
    const expand=page.getByRole('button',{name:'Expand',exact:true});
    await expand.scrollIntoViewIfNeeded();
    const geometry = () => {
      const c=document.querySelector('#preview'), rect=c.getBoundingClientRect();
      const intersect=Math.max(0,Math.min(rect.right,innerWidth)-Math.max(rect.left,0))*
        Math.max(0,Math.min(rect.bottom,innerHeight)-Math.max(rect.top,0));
      return {top:rect.top,bottom:rect.bottom,area:intersect,canvas:[rect.width,rect.height],viewport:[innerWidth,innerHeight],
        scrollY,expanded:document.querySelector('#preview-panel').classList.contains('expanded'),
        layout:getComputedStyle(document.querySelector('#view-perform .performance-layout')).gridTemplateAreas};
    };
    const before=await page.evaluate(geometry);
    await expand.click();
    await page.waitForFunction(()=>document.querySelector('#preview-panel')?.classList.contains('expanded'),null,{polling:100});
    await page.waitForTimeout(500);
    const after=await page.evaluate(geometry);
    await page.getByRole('button',{name:'Compact',exact:true}).evaluate(button=>button.click());
    await page.waitForFunction(()=>!document.querySelector('#preview-panel')?.classList.contains('expanded'),null,{polling:100});
    await page.waitForTimeout(500);
    const compacted=await page.evaluate(geometry);
    snapshots.push({view:[width,height],before,after,compacted});
  }
  await writeFile(output,JSON.stringify({studio:mainSHA256,customStudio:Boolean(studioPath),snapshots},null,2)+'\n');
  if (studioPath) for (const {view,after,compacted} of snapshots) {
    for (const sample of [after,compacted]) {
      const viewportHeight = sample.viewport[1];
      const visibleHeight = Math.min(sample.bottom,viewportHeight) - Math.max(sample.top,0);
      assert.ok(visibleHeight >= Math.min(sample.canvas[1], viewportHeight)-1, JSON.stringify({view,sample}));
    }
  }
} catch (error) {
  console.error(error);
  process.exitCode=2;
} finally {
  if(targetId) await controller.send('Target.closeTarget',{targetId}).catch(()=>{});
  server.stdin.end();
  for(let i=0;i<100&&server.exitCode===null&&server.signalCode===null;i++) await delay(100);
  if(server.exitCode===null&&server.signalCode===null) server.kill('SIGTERM');
  process.exit(process.exitCode??0);
}
