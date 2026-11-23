// Run via lein run -m tolglow.qa.workspace-layout with a built Studio bundle.
import assert from 'node:assert/strict';
import {mkdir,writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin,out]=process.argv.slice(2);
await mkdir(out,{recursive:true});
const browser=await chromium.launch({executablePath:process.env.CHROMIUM_PATH || '/usr/bin/chromium'});
const measurements={};

async function measure(page) {
  return page.evaluate(()=>{
    const rect=selector=>{
      const {x,y,width,height}=document.querySelector(selector).getBoundingClientRect();
      return [x,y,width,height].map(Math.round);
    };
    return {source:rect('#workspace-source'),save:rect('.workspace-save-bar button'),
      nav:rect('.workspace-panes'),repository:rect('.workspace-repositories button'),
      overflow:document.documentElement.scrollWidth-innerWidth};
  });
}

async function screenshot(page,name) {
  await page.evaluate(()=>window.scrollTo(0,0));
  await page.screenshot({path:join(out,name+'.png')});
}

async function exerciseEditor(page) {
  const pane=name=>page.getByRole('navigation',{name:'Development workspace views'}).getByRole('button',{name,exact:true});
  const engine=page.getByRole('combobox',{name:'Application file editor engine'});
  const source=page.locator('#workspace-source');
  const original=await source.inputValue();
  const modified=original+'\n(def brightness 0.5)\n';
  await source.fill(modified);
  await engine.selectOption('neovim');
  await page.getByText(/Neovim is ready/).first().waitFor({timeout:90000});
  await screenshot(page,'ipad-neovim');
  await pane('Coding agent').click();
  const prompt=page.locator('#application-agent-prompt');
  await prompt.fill('Review the fixture definition.');
  await screenshot(page,'ipad-agent');
  await pane('Files').click();
  await page.getByText(/Neovim is ready/).first().waitFor({timeout:90000});
  await engine.selectOption('text');
  assert.equal((await source.inputValue()).trimEnd(),modified.trimEnd());
  await page.getByRole('button',{name:'Save file',exact:true}).click();
  await page.waitForFunction(()=>!document.querySelector('.workspace-file-heading')?.textContent.includes('Unsaved edits'));
  await pane('Diff').click();
  await page.locator('.workspace-diff-text').filter({hasText:'+\u0028def brightness 0.5)'}).first().waitFor();
  await screenshot(page,'ipad-diff');
  await pane('Coding agent').click();
  assert.equal(await prompt.inputValue(),'Review the fixture definition.');
  await pane('Checks & preview').click();
  await page.getByRole('heading',{name:'Checks and review readiness'}).waitFor();
  await pane('Terminal').click();
  await page.locator('.workspace-terminal-slot').waitFor();
  await pane('Files').click();
  await page.getByRole('group',{name:'Paired repositories'}).getByRole('button',{name:/Afterglow/i}).click();
  await page.locator('.workspace-file[title="src/core.clj"]').click();
  assert.equal(await source.inputValue(),original);
}

// Hold the provider response in the browser to exercise the pane lifetime without starting an external agent.
async function delayedStart(page) {
  let observed,release;
  const sent=new Promise(resolve=>{observed=resolve;});
  const allowed=new Promise(resolve=>{release=resolve;});
  let starts=0,accepted=false;
  const task={id:'local-ack-fixture',name:'Held command',status:'completed'};
  await page.route('**/api/application-agents**',async route=>{
    const op=new URL(route.request().url()).pathname.split('/').pop();
    let body;
    if (op==='start') {
      starts++;observed();await allowed;accepted=true;body=task;
    } else if (op==='events') {
      body={'task-id':task.id,status:'completed',cursor:0,events:[]};
    } else if (op==='status') {
      body=task;
    } else {
      body={'runner-ready?':true,providers:[{id:'codex','configured?':true}],tasks:accepted?[task]:[]};
    }
    await route.fulfill({json:body});
  });
  const pane=name=>page.getByRole('navigation',{name:'Development workspace views'}).getByRole('button',{name,exact:true});
  const start=page.locator('#application-agent-start');
  const prompt=page.locator('#application-agent-prompt');
  await pane('Coding agent').click();
  await prompt.fill('First request');
  await start.click();
  await sent;
  await pane('Files').click();
  await pane('Coding agent').click();
  assert.equal(await start.isDisabled(),true);
  await prompt.fill('Keep this new draft');
  await pane('Files').click();
  const receipt=page.waitForResponse(r=>r.url().endsWith('/api/application-agents/start'));
  release();
  await receipt;
  await pane('Coding agent').click();
  await page.locator('.application-agent-task[aria-pressed="true"]').filter({hasText:'Held command'}).waitFor();
  assert.equal(await prompt.inputValue(),'Keep this new draft');
  assert.equal(starts,1);
}

async function verifySize(name,width,height) {
  const page=await browser.newPage({viewport:{width,height},hasTouch:name!=='desktop'});
  const errors=[];
  page.on('pageerror',error=>errors.push(error.message));
  try {
    await page.goto(origin+'/develop');
    const create=page.getByRole('button',{name:'Create workspace',exact:true});
    if (name==='phone') {
      await create.waitFor();
      await screenshot(page,'phone-create');
      await page.getByLabel(/Workspace ID/).fill('studio-layout');
      await create.click();
    }
    await page.waitForFunction(()=>document.querySelector('.workspace-selection select')?.value==='studio-layout');
    assert.equal(await create.isVisible(),false);
    await page.locator('.workspace-file[title="src/core.clj"]').click();
    await page.locator('#workspace-source').waitFor();
    await screenshot(page,name);
    const m=measurements[name]=await measure(page);
    assert.equal(m.overflow,0);
    assert.ok(m.source[1]<height*.65,name+' source starts in first viewport');
    assert.ok(m.save[1]+m.save[3]<height,name+' save is accessible with the buffer');
    assert.ok(m.repository[3]>=44 && m.save[3]>=44);
    if (name==='phone') {
      const browse=page.getByRole('button',{name:'Browse files'});
      await browse.tap();
      await page.locator('.workspace-file[title="src/core.clj"]').waitFor();
      await page.getByRole('button',{name:'Hide file browser'}).tap();
      await page.getByRole('button',{name:'New workspace'}).tap();
      await create.waitFor();
      await page.getByRole('button',{name:'New workspace'}).tap();
    }
    if (name==='ipad') await exerciseEditor(page);
    if (name==='desktop') await delayedStart(page);
    assert.deepEqual(errors,[]);
  } finally {await page.close();}
}

try {
  for (const d of [['phone',390,844],['ipad',1024,768],['desktop',1440,900]]) await verifySize(...d);
  await writeFile(join(out,'metrics.json'),JSON.stringify(measurements,null,2)+'\n');
  console.log(JSON.stringify(measurements));
} finally {await browser.close();}
