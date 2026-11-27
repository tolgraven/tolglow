// Test a release build served by tolglow.qa.performance-layout.
import assert from 'node:assert/strict';
import {mkdir,writeFile} from 'node:fs/promises';
import {checkCardAlignment} from './live-card-alignment.mjs';
import {checkCardGrid, checkDetachedRepack} from './card-grid-checks.mjs';
import {join} from 'node:path';
import {chromium, devices, webkit} from 'playwright-core';

const [origin,directory] = process.argv.slice(2);
await mkdir(directory,{recursive:true});
const webkitRun = process.env.STUDIO_TEST_BROWSER === 'webkit';
const browser = webkitRun
  ? await webkit.launch()
  : await chromium.launch({executablePath:process.env.CHROMIUM_PATH || '/usr/bin/chromium',
      args:['--disable-dev-shm-usage','--use-angle=swiftshader','--enable-unsafe-swiftshader']});
const profiles = [
  ['phone', 390, 844, true, 'iPhone 15'],
  ['ipad', 1024, 768, true, 'iPad Pro 11 landscape'],
  ['desktop', 1440, 900, false, 'Desktop Safari'],
  // A wide display gives the macro board six tracks (#795).
  ['wide', 2560, 1440, false, 'Desktop Safari'],
];
const measurements = {};

async function positionBoard(page) {
  await page.locator('.live-board').evaluate(board => {
    const tabs = document.querySelector('.perform-section-tabs'), top = document.querySelector('.topbar');
    const inset = top.getBoundingClientRect().height + (tabs.offsetWidth ? tabs.getBoundingClientRect().height + 10 : 6);
    scrollTo(0, Math.max(0, board.getBoundingClientRect().top + scrollY - inset));
  });
}

async function measureCards(page) {
  return page.locator('[data-live-instance]').evaluateAll(cards => {
    const rect = node => {
      const {x,y,width,height}=node.getBoundingClientRect();
      return [x,y,width,height].map(Math.round);
    };
    return {
      horizontalOverflow:document.documentElement.scrollWidth-innerWidth,
      topbar:rect(document.querySelector('.topbar')),
      cards:cards.map(card=>({
        name:card.querySelector('h3').textContent,
        box:rect(card),
        controls:card.querySelectorAll('.control-strip>.param-row').length,
        scope:card.querySelector('.strip-monitor svg') && rect(card.querySelector('.strip-monitor svg')),
      })),
    };
  });
}

function sourceChecks(page,touch) {
  const card = page.locator('article', {has:page.getByRole('heading',{name:'Orbital wash'})});
  const activate = button => touch ? button.tap() : button.click();
  const source = title => card.getByRole('region', {name:title+' source'});
  async function open(title, bound) {
    await activate(card.getByRole('button',{name:(bound?'Edit':'Attach')+' source for '+title,exact:true}));
    return source(title);
  }
  async function close(panel) { await activate(panel.getByRole('button',{name:'Close source editor'})); }
  async function attach(title, configure) {
    const panel = await open(title,false);
    await configure(panel);
    await activate(panel.getByRole('button',{name:'Attach LFO',exact:true}));
    await panel.getByRole('button',{name:'Detach LFO',exact:true}).waitFor();
    return panel;
  }
  async function detach(title) {
    const panel=await open(title,true);
    await activate(panel.getByRole('button',{name:'Detach LFO',exact:true}));
    await panel.getByRole('button',{name:'Attach LFO',exact:true}).waitFor();
    await close(panel);
  }
  return {card,activate,open,close,attach,detach};
}

async function verifySourceFocus(page,touch,profile) {
  const {card,activate,close}=sourceChecks(page,touch);
  const trigger=card.getByRole('button',{name:'Attach source for Intensity',exact:true});
  const panel=card.getByRole('region',{name:'Intensity source',exact:true});
  const heading=panel.getByRole('heading',{name:'Intensity source',exact:true});
  const focused=page.locator(':focus');
  await trigger.focus();
  await trigger.press('Enter');
  await heading.and(focused).waitFor({timeout:5000});
  const panelId=await panel.getAttribute('id');
  assert.ok(panelId,'The source editor has a targetable id');
  assert.equal(await trigger.getAttribute('aria-controls'),panelId);
  assert.equal(await trigger.getAttribute('aria-expanded'),'true');
  if(!touch) await page.screenshot({path:join(directory,profile+'-source-focus.png')});
  if(!touch && !webkitRun) {
    await page.keyboard.press('Tab');
    await panel.getByRole('button',{name:'Close source editor'}).and(focused).waitFor({timeout:5000});
  }
  await page.keyboard.press('Escape');
  await panel.waitFor({state:'detached'});
  await trigger.and(focused).waitFor({timeout:5000});
  assert.equal(await trigger.getAttribute('aria-expanded'),'false');
  await activate(trigger);
  await heading.and(focused).waitFor({timeout:5000});
  if(touch) await page.screenshot({path:join(directory,profile+'-source-focus.png')});
  await close(panel);
  await panel.waitFor({state:'detached'});
  await trigger.and(focused).waitFor({timeout:5000});

  await card.getByRole('slider',{name:'Intensity rotary',exact:true}).focus();
  await trigger.evaluate(button => {button.click(); button.click();});
  await trigger.and(focused).waitFor({timeout:5000});
  assert.equal(await trigger.getAttribute('aria-expanded'),'false');
  await panel.waitFor({state:'detached'});

  await activate(trigger);
  await heading.and(focused).waitFor({timeout:5000});
  const nextTrigger=card.getByRole('button',{name:'Attach source for Orbit aim',exact:true});
  const nextPanel=card.getByRole('region',{name:'Orbit aim source',exact:true});
  await card.evaluate(article => {
    article.querySelector('button[aria-label="Attach source for Orbit aim"]').click();
    article.querySelector('button[aria-label="Close source editor"]').click();
  });
  await nextPanel.getByRole('heading').and(focused).waitFor({timeout:5000});
  await panel.waitFor({state:'detached'});
  assert.equal(await nextTrigger.getAttribute('aria-controls'),await nextPanel.getAttribute('id'));
  await close(nextPanel);
  await nextTrigger.and(focused).waitFor({timeout:5000});
}

async function verifyControls(page,touch,profile) {
  const sweep=page.locator('article',{has:page.getByRole('heading',{name:'Slow sweep',exact:true})});
  const fader=sweep.getByRole('slider',{name:'width',exact:true});
  assert.equal(await fader.isVisible(),true);
  assert.equal(await fader.getAttribute('aria-orientation'),'vertical');
  assert.deepEqual(await sweep.locator('.parameter-fader .fader-bound').allTextContents(),['0.45','0']);
  const {card,activate,open,close,attach,detach}=sourceChecks(page,touch);
  const opacity=card.getByRole('slider',{name:'Effect opacity'});
  const initialOpacity=await opacity.inputValue();
  const strip=card.getByRole('group',{name:'Parameter strip'});
  assert.equal(await strip.locator('.param-row').count(),8);
  assert.equal(await strip.locator('.param-source-trigger').count(),8);
  const original = await strip.getByRole('slider',{name:'Intensity rotary'}).getAttribute('aria-valuenow');
  const intensity=await attach('Intensity', async panel => {
    await panel.getByLabel('Waveform source').selectOption({label:'Studio test sine'});
    await panel.getByLabel('Source period',{exact:true}).fill('3/2');
    await panel.getByLabel('From',{exact:true}).fill('0.15');
    await panel.getByLabel('To',{exact:true}).fill('0.9');
    if(profile==='ipad') await page.screenshot({path:join(directory,'ipad-attach.png')});
  });
  await close(intensity);
  const scope=card.locator('.strip-monitor');
  await scope.scrollIntoViewIfNeeded();
  await page.waitForFunction(() => {
    const path=document.querySelector('.strip-monitor .parameter-history path')?.getAttribute('d') || '';
    const span=Array.from(path.matchAll(/[ML]([\d.-]+),([\d.-]+)/g), m=>m.slice(1).map(Number));
    return new Set(span.map(point=>point[1].toFixed(1))).size>=3;
  });
  assert.equal(await page.locator('.strip-monitor').count(),1);
  assert.equal(await scope.locator('strong').textContent(),'Intensity');
  if(profile==='phone') await card.evaluate(node=>scrollBy(0,node.getBoundingClientRect().top-170));
  else await positionBoard(page);
  await page.screenshot({path:join(directory,profile+'-bound.png')});
  if(profile==='ipad') {
    // The scope samples only while visible; large pad slots (#795) make the strip taller.
    await scope.scrollIntoViewIfNeeded();
    for(let i=0;i<8;i++) {
      const previous=await scope.locator('svg path').first().getAttribute('d');
      await page.waitForFunction(d=>document.querySelector('.strip-monitor svg path')?.getAttribute('d')!==d,previous);
      await page.screenshot({path:join(directory,'ipad-lfo-'+i+'.png')});
    }
  }
  const edit=await open('Intensity',true);
  const period=edit.getByLabel('Source period',{exact:true});
  await period.fill('1/4');
  const current=await scope.locator('svg path').first().getAttribute('d');
  await page.waitForFunction(previous=>document.querySelector('.strip-monitor svg path')?.getAttribute('d')!==previous,current);
  assert.equal(await period.inputValue(),'1/4','A real pushed sample preserves the edited draft');
  await activate(edit.getByRole('button',{name:'Update LFO',exact:true}));
  await page.waitForFunction(()=>document.querySelector('input[aria-label="Source period"]').value==='0.25');
  await activate(edit.getByRole('button',{name:'Use base value',exact:true}));
  await edit.getByRole('button',{name:'Resume LFO',exact:true}).waitFor();
  await scope.waitFor({state:'detached'});
  await card.getByRole('button',{name:'Edit source for Intensity'}).locator('.transport-symbol.pause').waitFor();
  await activate(edit.getByRole('button',{name:'Resume LFO',exact:true}));
  await edit.getByRole('button',{name:'Use base value',exact:true}).waitFor();
  await scope.waitFor({state:'attached'});
  await close(edit);
  const vector=await attach('Orbit aim', async panel => {
    await panel.getByLabel('From X',{exact:true}).fill('-0.85');
    await panel.getByLabel('To X',{exact:true}).fill('0.42');
  });
  await close(vector);
  const picker=scope.getByLabel('Monitored parameter');
  assert.deepEqual((await picker.locator('option').allTextContents()).sort(),['Intensity','Orbit aim']);
  await picker.selectOption({label:'Orbit aim'});
  await scope.scrollIntoViewIfNeeded();
  await scope.locator('.parameter-space').waitFor();
  await activate(card.getByRole('button',{name:'Expand Orbital wash controls'}));
  const aim=card.locator('[data-param=aim]');
  // A driven parameter's plot sits below the Detail grid (#795), so a card never grows.
  const plot=card.locator('[data-plot=aim]');
  await activate(aim.getByRole('button',{name:'Edit Orbit aim components'}));
  const dialog=aim.getByRole('dialog');
  const axis=dialog.getByRole('spinbutton',{name:'Orbit aim X value'});
  await axis.fill(Number(await axis.inputValue())>0 ? '-0.42' : '0.42');
  await activate(dialog.getByRole('button',{name:'Done'}));
  await plot.getByRole('button',{name:'History'}).waitFor({state:'detached'});
  const overridden=await open('Orbit aim',true);
  await activate(overridden.getByRole('button',{name:'Resume LFO',exact:true}));
  await overridden.getByRole('button',{name:'Use base value',exact:true}).waitFor();
  await close(overridden);
  await activate(plot.getByRole('button',{name:'History'}));
  await plot.getByLabel('Parameter history beat window').selectOption('8');
  // The scope samples only while visible.
  await plot.locator('.parameter-visualizer').scrollIntoViewIfNeeded();
  await plot.getByRole('img',{name:'Last 8 beats'}).waitFor();
  await activate(plot.getByRole('button',{name:'3D'}));
  assert.equal(await card.locator('[data-plot=glow]').count(),0);
  await activate(card.getByRole('button',{name:'Collapse Orbital wash controls'}));
  await detach('Orbit aim');
  await detach('Intensity');
  assert.equal(await card.locator('.strip-monitor').count(),0);
  assert.equal(await opacity.inputValue(),initialOpacity,'Editing a source leaves the cue opacity untouched');
  assert.equal(await strip.getByRole('slider',{name:'Intensity rotary'}).getAttribute('aria-valuenow'), original);
  const base=strip.getByRole('slider',{name:'Intensity rotary'});
  await base.focus();
  await base.press(Number(original)>=0.95?'ArrowLeft':'ArrowRight');
  await page.waitForFunction(([id,old])=>document.querySelector('[data-live-instance="'+id+'"] [aria-label="Intensity rotary"]').getAttribute('aria-valuenow')!==old,
    [await card.getAttribute('data-live-instance'),original]);
  await activate(card.getByRole('button',{name:'Pause',exact:true}));
  await activate(card.getByRole('button',{name:'Resume',exact:true}));
  await card.getByRole('button',{name:'Pause',exact:true}).waitFor();
}

async function verifyShellActions(page,profile,touch) {
  const activate=button=>touch ? button.tap() : button.click();
  const pause=page.getByRole('button',{name:'Pause transport',exact:true});
  await pause.scrollIntoViewIfNeeded();
  await pause.locator('.transport-symbol.pause').waitFor();
  await activate(pause);
  const play=page.getByRole('button',{name:'Play transport',exact:true});
  await play.locator('.transport-symbol.play').waitFor();
  await page.screenshot({path:join(directory,profile+'-transport-paused.png')});
  await play.focus();
  await play.press('Enter');
  await pause.locator('.transport-symbol.pause').waitFor();
  const files=page.locator(profile==='phone'?'.mobile-file-actions':'.sidebar-bottom');
  const upload=files.getByLabel('Import show',{exact:true});
  const download=files.getByRole('button',{name:'Export show',exact:true});
  await upload.scrollIntoViewIfNeeded();
  await upload.focus();
  const selected=page.waitForEvent('filechooser');
  if(touch) await upload.tap(); else await upload.press('Enter');
  await (await selected).setFiles([]);
  await page.screenshot({path:join(directory,profile+'-file-actions.png')});
  const saving=page.waitForEvent('download');
  await activate(download);
  assert.equal((await saving).suggestedFilename(),'tolglow-show.transit');
}

async function verifyCueActions(page, name) {
  const cues = page.getByRole('group', {name:'Performance sections'}).getByRole('button',{name:'Cues',exact:true});
  if (await cues.isVisible()) await cues.tap();
  const cell = page.locator('#cue-grid [data-cue-id="ember"]').first();
  await cell.scrollIntoViewIfNeeded();
  const bounds = await cell.boundingBox();
  // Playwright's touchscreen supports taps. Use a real held pointer here, so
  // WebKit owns capture and the application's gesture timer fires normally.
  await page.mouse.move(bounds.x + bounds.width/2, bounds.y + bounds.height/2);
  await page.mouse.down();
  const actions = page.getByRole('dialog', {name:/Ember/i});
  try {
    await actions.waitFor();
  } finally {
    await page.mouse.up();
  }
  const copy=actions.getByRole('button',{name:/Copy cue/});
  assert.equal(await copy.isEnabled(),true);
  assert.equal(await actions.getByRole('button',{name:/Paste here/}).isEnabled(),false);
  await page.screenshot({path:join(directory,name+'-actions.png')});
  await actions.getByRole('button',{name:'Close actions'}).tap();
  await cell.focus();
  await cell.press('Shift+F10');
  await actions.waitFor();
  await page.keyboard.press('Escape');
  await actions.waitFor({state:'hidden'});
}

// Every control in a compact strip slot stays inside that slot, slots do not
// overlap, and the board's header actions do not overlap each other (#655),
// so no press lands on a neighbour's control.
async function checkStripContainment(page,name) {
  const problems=await page.evaluate(()=>{
    const out=[];
    const box=e=>e.getBoundingClientRect();
    const sized=r=>r.width>0&&r.height>0;
    const inside=(r,o)=>r.left>=o.left-1&&r.right<=o.right+1&&r.top>=o.top-1&&r.bottom<=o.bottom+1;
    const overlap=(a,b)=>Math.min(a.right,b.right)-Math.max(a.left,b.left)>1&&Math.min(a.bottom,b.bottom)-Math.max(a.top,b.top)>1;
    for(const strip of document.querySelectorAll('.control-strip')) {
      const rows=[...strip.children].filter(e=>e.matches('.param-row')&&sized(box(e)));
      rows.forEach((row,i)=>{
        const r=box(row);
        for(const e of row.querySelectorAll('[role=slider],button,input,select,.compound-control,.axis-control,.fader-track,.fader-thumb')) {
          const b=box(e);
          if(sized(b)&&!inside(b,r)) out.push((row.dataset.param||row.dataset.group)+' '+(e.getAttribute('aria-label')||e.className)+' leaves its slot');
        }
        rows.slice(i+1).forEach(other=>{ if(overlap(r,box(other))) out.push((row.dataset.param||row.dataset.group)+' overlaps '+(other.dataset.param||other.dataset.group)); });
      });
    }
    for(const actions of document.querySelectorAll('.live-board-actions')) {
      const items=[...actions.children].filter(e=>sized(box(e)));
      // A squeezed group can paint its buttons over the next item while the
      // group boxes themselves do not overlap, so compare every part.
      const parts=a=>[a,...a.querySelectorAll('.button,label,select,summary,span')].filter(e=>sized(box(e)));
      items.forEach(a=>items.forEach(b=>{
        if(a!==b) for(const e of parts(a)) if(overlap(box(e),box(b))) out.push('header actions overlap: '+e.textContent.trim()+' / '+b.textContent.trim());
      }));
    }
    return out;
  });
  assert.deepEqual(problems,[],'Strip containment at '+name);
}

try {
  for(const [name,width,height,touch,device] of profiles) {
    const descriptor = webkitRun ? devices[device] : {};
    const page=await browser.newPage({...descriptor,viewport:{width,height},hasTouch:touch,deviceScaleFactor:1});
    const errors=[];
    page.setDefaultTimeout(60000);
    page.on('pageerror',error=>errors.push(error.message));
    await page.goto(origin + '/perform');
    await page.locator('[data-live-instance]').first().waitFor({state:'attached',timeout:60000});
    const live=page.getByRole('button',{name:/^Live ·/});
    if(await live.isVisible()) await (touch ? live.tap() : live.click());
    await positionBoard(page);
    // The desktop run leaves the Wave selector running with a fixture-driven sample.
    const waveSelector=page.locator('article',{has:page.getByRole('heading',{name:'Wave selector',exact:true})});
    assert.equal(await page.locator('.strip-monitor,.parameter-visualizer').count()
                 -await waveSelector.locator('.strip-monitor,.parameter-visualizer').count(),0,'Static inputs have no scopes');
    await page.screenshot({path:join(directory,name+'.png')});
    measurements[name]=await measureCards(page);
    assert.equal(measurements[name].horizontalOverflow,0,'Document overflow at '+name);
    await checkCardAlignment(page, name, directory, measurements[name]);
    await checkStripContainment(page, name);
    await checkCardGrid(page, name, directory, measurements[name]);
    await positionBoard(page);
    // The wide run checks layout only; the source and shell flows ran at desktop.
    if(process.env.STUDIO_CARD_LAYOUT_ONLY==='1' || name==='wide') {
      if(name==='desktop') measurements[name].detached=await checkDetachedRepack(page,directory);
      await page.close();
      continue;
    }
    if(process.env.STUDIO_SHELL_ONLY==='1') {
      await verifyShellActions(page,name,touch);
      assert.deepEqual(errors,[]);
      await page.close();
      continue;
    }
    try {
      await verifySourceFocus(page,touch,name);
      await verifyControls(page,touch,name);
    } catch (error) {
      await page.screenshot({path:join(directory,name+'-failed.png')});
      console.error(name, await page.locator('.strip-monitor,.performance-error').allTextContents());
      throw error;
    }
    if(name==='desktop') {
      const cue=page.locator('#cue-grid [data-cue-id="selector"]').first();
      await cue.click();
      const selector=page.locator('article',{has:page.getByRole('heading',{name:'Wave selector',exact:true})});
      const curve=selector.getByLabel('Live waveform shape').locator('svg polyline');
      await curve.scrollIntoViewIfNeeded();
      await curve.waitFor();
      assert.ok((await curve.getAttribute('points')).split(' ').length>16,'Authored waveform shape remains visible');
      assert.equal(await selector.locator('[data-plot=wave]').count(),0);
      const scope=selector.locator('.strip-monitor');
      await scope.scrollIntoViewIfNeeded();
      await scope.getByText('Fixture · Beam 6').waitFor();
      const level=scope.locator('.history-reading>span').first();
      await level.waitFor({state:'attached'});
      assert.ok(Math.abs(Number(await level.textContent())-0.665)<0.01,'Real fixture X drives the first sample');
      await selector.getByRole('button',{name:'Edit source for Lamp sample'}).click();
      await selector.getByLabel('Sample fixture').selectOption({label:'head-8'});
      await selector.getByRole('button',{name:'Close source editor'}).click();
      await scope.getByText('Fixture · Beam 8').waitFor();
      await page.waitForFunction(id=>Math.abs(Number(document.querySelector('[data-live-instance="'+id+'"] .history-reading>span')?.textContent)-0.885)<0.01,
        await selector.getAttribute('data-live-instance'));
      await page.screenshot({path:join(directory,'desktop-wave-selector.png')});
    }
    if (webkitRun && touch) await verifyCueActions(page,name);
    await verifyShellActions(page,name,touch);
    assert.deepEqual(errors,[]);
    await page.close();
  }
  await writeFile(join(directory,'metrics.json'),JSON.stringify(measurements,null,2)+'\n');
  console.log(JSON.stringify(measurements,null,2));
} finally {
  await browser.close();
}
