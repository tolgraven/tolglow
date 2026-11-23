// A disposable Studio is required because the browser creates and saves a macro.
import assert from 'node:assert/strict';
import {mkdir,writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin,directory] = process.argv.slice(2);
const state = await (await fetch(origin+'/api/state',{headers:{Accept:'application/json'}})).json();
assert.equal(state.status.output.type,'preview','Use a disposable preview-only show');
await mkdir(directory,{recursive:true});
const browser = await chromium.launch({executablePath:process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args:['--disable-dev-shm-usage','--use-gl=angle','--use-angle=swiftshader','--enable-unsafe-swiftshader']});
const results = {};
async function measure(page,selector) {
  return page.locator(selector).evaluateAll(nodes=>nodes.map(n=>{
    const {top,width,height}=n.getBoundingClientRect(); return [top,width,height].map(Math.round);
  }));
}
try {
  for (const [label,width,height,touch] of [['phone',390,844,true],['ipad',1024,768,true],['desktop',1440,900,false]]) {
    const page=await browser.newPage({viewport:{width,height},hasTouch:touch});
    const errors=[];page.on('pageerror',error=>errors.push(error.message));
    await page.goto(origin+'/compose');
    await page.locator('#connection-dot.connected').waitFor({state:'attached'});
    await page.locator('#slot-cue option[value=orbital]').waitFor({state:'attached'});
    await page.locator('#new-composition').click();
    await page.waitForFunction(()=>document.querySelector('#composition-name').value==='New composition');
    const name='Evening drift · '+label;
    await page.locator('#composition-name').fill(name);
    await page.locator('#composition-id').fill('layout-'+label);
    const slots=page.locator('.composition-slot');
    for(const [index,id] of ['ember','sweep','breathe','orbital'].entries()) {
      await page.locator('#slot-cue').selectOption(id);
      await page.locator('#add-slot').click();
      await slots.nth(index).waitFor();
    }
    assert.equal(await slots.count(),4);
    await page.getByLabel('Step 1 duration value').fill('2');
    await page.getByLabel('Step 1 duration unit').selectOption('bars');
    await page.evaluate(()=>scrollTo(0,0));
    await page.screenshot({path:join(directory,label+'-compose.png')});
    results[label]={steps:await measure(page,'.composition-slot')};
    await page.locator('#composition-mode').selectOption('macro');
    await slots.locator('.duration-control').first().waitFor({state:'detached'});
    await slots.nth(1).getByRole('button',{name:'Move earlier: step 2'}).click();
    await slots.first().locator('.slot-title').filter({hasText:'Slow sweep'}).waitFor();
    const reply=page.waitForResponse(r=>new URL(r.url()).pathname==='/api/composition' && r.request().method()==='POST');
    await page.locator('#save-composition').click();
    assert.equal((await reply).ok(),true,'The host accepts the composition');
    const saved=page.locator('[data-composition-id="layout-'+label+'"]');
    await saved.getByRole('button',{name:'Open'}).click();
    assert.equal(await page.locator('#composition-mode').inputValue(),'macro');
    await page.locator('#composition-mode').selectOption('sequence');
    assert.equal(await page.getByLabel('Step 2 duration value').inputValue(),'2');
    assert.equal(await page.getByLabel('Step 2 duration unit').inputValue(),'bars');
    await page.locator('a[data-view=rig]').click();
    const rig=page.locator('.rig-editor');
    const tools=rig.getByRole('group',{name:'3D editor tool'});
    await tools.getByRole('button',{name:'Explore',exact:true}).click();
    await rig.locator('.rig-path-form').waitFor({state:'detached'});
    const canvas=rig.locator('canvas');
    await canvas.scrollIntoViewIfNeeded();
    results[label].explore=await measure(page,'.rig-editor canvas');
    await tools.getByRole('button',{name:'Draw path',exact:true}).click();
    await rig.getByLabel('Duration',{exact:true}).waitFor();
    results[label].draw=await measure(page,'.rig-editor canvas');
    await tools.getByRole('button',{name:'Explore',exact:true}).click();
    await rig.locator('.rig-path-form').waitFor({state:'detached'});
    assert.deepEqual(errors,[]);
    await page.close();
  }
  await writeFile(join(directory,'metrics.json'),JSON.stringify(results,null,2)+'\n');
  console.log(JSON.stringify(results));
} finally { await browser.close(); }
