// Test a release build served by tolglow.qa.performance-layout.
import assert from 'node:assert/strict';
import {mkdir,writeFile} from 'node:fs/promises';
import {checkCardAlignment} from './live-card-alignment.mjs';
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

async function verifyControls(page,touch,profile) {
  const sweep=page.locator('article',{has:page.getByRole('heading',{name:'Slow sweep',exact:true})});
  const fader=sweep.getByRole('slider',{name:'width',exact:true});
  assert.equal(await fader.isVisible(),true);
  assert.deepEqual(await sweep.locator('.strip-fader-bounds>span').allTextContents(),['0','0.45']);
  const {card,activate,open,close,attach,detach}=sourceChecks(page,touch);
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
  await activate(aim.getByRole('button',{name:'Edit Orbit aim components'}));
  const dialog=aim.getByRole('dialog');
  const axis=dialog.getByRole('spinbutton',{name:'Orbit aim X value'});
  await axis.fill(Number(await axis.inputValue())>0 ? '-0.42' : '0.42');
  await activate(dialog.getByRole('button',{name:'Done'}));
  await aim.getByRole('button',{name:'History'}).waitFor({state:'detached'});
  const overridden=await open('Orbit aim',true);
  await activate(overridden.getByRole('button',{name:'Resume LFO',exact:true}));
  await overridden.getByRole('button',{name:'Use base value',exact:true}).waitFor();
  await close(overridden);
  await activate(aim.getByRole('button',{name:'History'}));
  await aim.getByLabel('Parameter history beat window').selectOption('8');
  await aim.getByRole('img',{name:'Last 8 beats'}).waitFor();
  await activate(aim.getByRole('button',{name:'3D'}));
  assert.equal(await card.locator('[data-param=glow] .parameter-visualizer').count(),0);
  await activate(card.getByRole('button',{name:'Collapse Orbital wash controls'}));
  await detach('Orbit aim');
  await detach('Intensity');
  assert.equal(await card.locator('.strip-monitor').count(),0);
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
    assert.equal(await page.locator('.strip-monitor,.parameter-visualizer').count(),0,'Static inputs have no scopes');
    await page.screenshot({path:join(directory,name+'.png')});
    measurements[name]=await measureCards(page);
    assert.equal(measurements[name].horizontalOverflow,0,'Document overflow at '+name);
    await checkCardAlignment(page, name, directory, measurements[name]);
    if(process.env.STUDIO_CARD_LAYOUT_ONLY==='1') {
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
      assert.equal(await selector.locator('[data-param=wave] .parameter-visualizer').count(),0);
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
