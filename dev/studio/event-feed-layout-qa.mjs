// Run against the disposable preview host via lein run -m tolglow.qa.studio-browser event-feed-layout.
import assert from 'node:assert/strict';
import {mkdir,writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin,output]=process.argv.slice(2);
await mkdir(output,{recursive:true});
const browser=await chromium.launch({executablePath:process.env.CHROMIUM_PATH || '/usr/bin/chromium'});
const metrics={};

async function emitCommands() {
  const state=await (await fetch(origin+'/api/state',{headers:{accept:'application/json'}})).json();
  assert.equal(state.status.output.type,'preview');
  for (const id of ['breathe','spot','sweep','flash']) {
    const response=await fetch(origin+'/api/command',{
      method:'POST',headers:{accept:'application/json','content-type':'application/json'},
      body:JSON.stringify({op:'cue/launch','cue-id':id,'command-id':crypto.randomUUID(),
        'runtime-id':state['runtime-id'],'show-id':state['show-id'],epoch:state.epoch,
        source:{adapter:'web','session-id':'event-feed-layout-qa'}})
    });
    assert.ok(response.ok);
    assert.notEqual((await response.json()).status,'rejected');
  }
}

async function measure(page) {
  return page.evaluate(()=>{
    const rect=e=>e.getBoundingClientRect();
    const list=rect(document.querySelector('.debug-event-list'));
    const sidebar=rect(document.querySelector('.sidebar'));
    const bottom=Math.min(innerHeight,list.bottom,sidebar.top>0 ? sidebar.top : innerHeight);
    const top=Math.max(list.top,rect(document.querySelector('.debug-table-head')).bottom);
    const rows=Array.from(document.querySelectorAll('.debug-row'),rect);
    return {visible:rows.filter(r=>r.top>=top-.5 && r.bottom<=bottom+.5).length,
      firstEvent:Math.round(rows[0].top),rowHeight:Math.round(rows[0].height),
      overflow:document.documentElement.scrollWidth-innerWidth};
  });
}

async function verifyFilters(page,name) {
  const toggle=page.getByRole('button',{name:/^Filters/});
  const type=page.getByRole('combobox',{name:'Event type'});
  await toggle.focus();
  await toggle.press('Enter');
  await type.selectOption('clock/beat');
  const names=page.locator('.debug-row strong');
  await page.waitForFunction(()=>Array.from(document.querySelectorAll('.debug-row strong')).every(e=>e.textContent==='clock/beat'));
  assert.ok(await names.count()>0);
  await toggle.click();
  await type.waitFor({state:'detached'});
  assert.equal(await toggle.getAttribute('aria-expanded'),'false');
  assert.match(await toggle.innerText(),/1/);
  await toggle.click();
  assert.equal(await type.inputValue(),'clock/beat');
  await page.screenshot({path:join(output,name+'-filters.png')});
  const clear=page.getByRole('button',{name:'Clear filters'});
  await clear.click();
  await clear.waitFor({state:'detached'});
  await page.waitForFunction(()=>document.activeElement?.id==='debug-filter-type');
  assert.equal(await type.inputValue(),'');
  await toggle.click();
  const rowId=await page.locator('.debug-row').first().getAttribute('id');
  await page.locator('.debug-row').first().press('Enter');
  await page.waitForFunction(()=>document.activeElement?.id==='debug-inspector-title');
  await page.screenshot({path:join(output,name+'-detail.png')});
  await page.getByRole('button',{name:'Close detail'}).click();
  await page.waitForFunction(id=>document.activeElement?.id===id,rowId);
  const resume=page.getByRole('button',{name:/^Resume/});
  await resume.click();
  await page.getByRole('button',{name:'Pause feed'}).waitFor();
  await page.getByRole('button',{name:'DMX output'}).click();
  const universes=page.getByRole('combobox',{name:'Output universe'});
  await universes.selectOption({index:1});
  await page.locator('.debug-channel').first().waitFor();
  assert.equal(await page.locator('.debug-channel').count(),64);
  await page.getByRole('combobox',{name:'Channel bank'}).selectOption('7');
  await page.locator('.debug-channel[aria-label^="Channel 512,"]').waitFor();
}

async function verifySize(name,width,height) {
  const page=await browser.newPage({viewport:{width,height},hasTouch:name!=='desktop'});
  const errors=[];
  page.on('pageerror',error=>errors.push(error.message));
  try {
    await page.goto(origin+'/debug');
    await page.locator('.debug-row').first().waitFor();
    await emitCommands();
    await page.waitForFunction(()=>document.querySelectorAll('.debug-row').length>=20);
    await page.getByRole('button',{name:'Pause feed'}).click();
    await page.getByRole('button',{name:/^Resume/}).waitFor();
    const result=metrics[name]=await measure(page);
    await page.screenshot({path:join(output,name+'.png')});
    assert.equal(result.overflow,0);
    assert.ok(result.rowHeight>=44);
    assert.ok(result.visible>=9,name+' visible event rows');
    await verifyFilters(page,name);
    assert.deepEqual(errors,[]);
  } finally {await page.close();}
}

try {
  for (const d of [['phone',390,844],['ipad',1024,768],['desktop',1440,900]]) await verifySize(...d);
  await writeFile(join(output,'metrics.json'),JSON.stringify(metrics,null,2)+'\n');
  console.log(JSON.stringify(metrics));
} finally {await browser.close();}
