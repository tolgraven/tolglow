// Use lein run -m tolglow.qa.studio-browser registry-editor-layout for a disposable show.
import assert from 'node:assert/strict';
import {mkdir,writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin,output]=process.argv.slice(2);
await mkdir(output,{recursive:true});
const browser=await chromium.launch({executablePath:process.env.CHROMIUM_PATH || '/usr/bin/chromium'});
const measurements={};

async function geometry(page) {
  return page.evaluate(() => {
    const rect=selector=>{
      const {x,y,width,height}=document.querySelector(selector).getBoundingClientRect();
      return [x,y,width,height].map(Math.round);
    };
    return {search:rect('#definition-search'),entry:rect('.definition-item'),modes:rect('#editor-modes button'),
      source:rect('#definition-source'),publish:rect('#publish-definition'),engine:rect('#editor-engine'),
      overflow:document.documentElement.scrollWidth-innerWidth};
  });
}
async function open(name,width,height) {
  const page=await browser.newPage({viewport:{width,height},hasTouch:name!=='desktop'});
  await page.goto(origin+'/library');
  await page.locator('.definition-item').first().waitFor();
  return page;
}
async function checkSize(name,width,height) {
  const page=await open(name,width,height);
  try {
    const m=measurements[name]=await geometry(page);
    assert.equal(m.overflow,0);
    assert.ok(m.publish[1]+m.publish[3]<height, name+' publication is visible');
    assert.ok(m.source[1]<height*.66, name+' code is visible');
    for(const field of ['search','entry','modes','publish','engine']) assert.ok(m[field][3]>=44,name+' '+field+' touch height');
    await page.screenshot({path:join(output,name+'.png')});
    await page.locator('#definition-search').fill('not-a-definition-here');
    await page.getByText('No matching definitions.',{exact:true}).waitFor();
    await page.locator('#definition-search').fill('Ember');
    await page.locator('.definition-item').first().click();
    await page.locator('.definition-item[aria-current="true"]').waitFor();
    assert.match(await page.locator('#definition-source').inputValue(),/:ember/);
    await page.screenshot({path:join(output,name+'-selected.png')});
  } finally {await page.close();}
}
async function editAndPublish() {
  const page=await open('ipad',1024,768);
  const errors=[];
  page.on('pageerror',error=>errors.push(error.message));
  try {
    await page.locator('.material-heading').click();
    await page.locator('.material-edit').waitFor();
    await page.locator('.material-heading').click();
    await page.getByRole('button',{name:'Visual builder',exact:true}).click();
    await page.locator('#builder-id').fill('registry-layout-check');
    await page.locator('#builder-name').fill('Registry layout check');
    await page.screenshot({path:join(output,'ipad-builder.png')});
    await page.locator('#publish-definition').click();
    await page.locator('#definition-status').filter({hasText:'Published'}).waitFor();
    const response=await fetch(origin+'/api/definition?kind=effects&id=registry-layout-check',{headers:{Accept:'application/json'}});
    assert.ok(response.ok);assert.match((await response.json()).source,/:registry-layout-check/);
    await page.getByRole('button',{name:'EDN source',exact:true}).click();
    await page.locator('#new-definition').click();
    // Read the new source once it has rendered, not the previous definition's text.
    await page.locator('#definition-status').filter({hasText:/^New definition$/}).waitFor();
    assert.equal(await page.locator('#auto-publish').isChecked(),false);
    const source=(await page.locator('#definition-source').inputValue()).replace(':after-hours',':source-layout-check');
    await page.locator('#definition-source').fill('{:id');
    await page.locator('#publish-definition').click();
    await page.locator('#validation-result.error').waitFor();
    await page.locator('#definition-source').fill(source);
    await page.waitForFunction(() => !document.querySelector('#validation-result').textContent.trim());
    await page.locator('#editor-engine').selectOption('neovim');
    await page.locator('.nvim-screen').waitFor();
    await page.getByText(/Neovim is running in WebAssembly with Bruvbox/).first().waitFor({timeout:90000});
    await page.screenshot({path:join(output,'ipad-neovim.png')});
    await page.locator('#publish-definition').click();
    await page.locator('#definition-status').filter({hasText:'Published'}).waitFor();
    await page.locator('#editor-engine').selectOption('textarea');
    await page.locator('#definition-source').waitFor();
    assert.match(await page.locator('#definition-source').inputValue(),/:source-layout-check/);
    assert.deepEqual(errors,[]);
  } finally {await page.close();}
}
try {
 for(const dimensions of [['phone',390,844],['ipad',1024,768],['desktop',1440,900]]) await checkSize(...dimensions);
 await editAndPublish();
 await writeFile(join(output,'metrics.json'),JSON.stringify(measurements,null,2));
 console.log(JSON.stringify(measurements));
} finally {await browser.close();}
