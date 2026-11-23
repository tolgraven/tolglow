// Test a release build served by tolglow.qa.performance-layout.
import assert from 'node:assert/strict';
import {mkdir,writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin,directory] = process.argv.slice(2);
await mkdir(directory,{recursive:true});
const browser = await chromium.launch({executablePath:process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args:['--disable-dev-shm-usage','--use-angle=swiftshader','--enable-unsafe-swiftshader']});
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

async function verifyControls(page,touch) {
  const card = page.locator('article', {has:page.getByRole('heading',{name:'Orbital wash'})});
  const activate = async button => touch ? button.tap() : button.click();
  const strip = card.getByRole('group',{name:'Parameter strip'});
  assert.equal(await strip.locator('.param-row').count(),8);
  const ids = await strip.locator('[data-param]').evaluateAll(nodes=>nodes.map(node=>node.dataset.param));
  assert.equal(new Set(ids).size,8);
  const picker=card.getByLabel('Monitored parameter');
  await picker.scrollIntoViewIfNeeded();
  await picker.selectOption('aim');
  await card.locator('.strip-monitor .parameter-space').waitFor();
  const input=strip.getByRole('slider',{name:'Intensity rotary'});
  await input.focus();
  const previous=Number(await input.getAttribute('aria-valuenow'));
  await input.press(previous>=0.95?'ArrowLeft':'ArrowRight');
  await page.waitForFunction(([id,old])=>Number(document.querySelector(
    '[data-live-instance="'+id+'"] [aria-label="Intensity rotary"]').getAttribute('aria-valuenow'))!==old,
    [await card.getAttribute('data-live-instance'),previous]);
  await picker.selectOption('dimmer');
  await card.locator('.strip-monitor .parameter-history svg path').first().waitFor({state:'attached'});
  await activate(card.getByRole('button',{name:'Pause',exact:true}));
  await card.getByRole('button',{name:'Resume',exact:true}).waitFor();
  await activate(card.getByRole('button',{name:'Resume',exact:true}));
  await activate(card.getByRole('button',{name:'Expand Orbital wash controls'}));
  const aim=card.locator('[data-param=aim]');
  await activate(aim.getByRole('button',{name:'Edit Orbit aim components'}));
  const dialog=aim.getByRole('dialog');
  await dialog.getByRole('spinbutton',{name:'Orbit aim X value'}).fill('0.42');
  await activate(dialog.getByRole('button',{name:'Done'}));
  await aim.locator('.parameter-space figcaption').filter({hasText:'0.42'}).waitFor();
  await activate(aim.getByRole('button',{name:'History'}));
  await aim.getByLabel('Parameter history beat window').selectOption('8');
  await aim.getByRole('img',{name:'Last 8 beats'}).waitFor();
  await activate(aim.getByRole('button',{name:'3D'}));
  await activate(card.getByRole('button',{name:'Collapse Orbital wash controls'}));
  await picker.selectOption('aim');
}

try {
  for(const [name,width,height,touch] of [['phone',390,844,true],['ipad',1024,768,true],['desktop',1440,900,false]]) {
    const page=await browser.newPage({viewport:{width,height},hasTouch:touch,deviceScaleFactor:1});
    const errors=[];
    page.on('pageerror',error=>errors.push(error.message));
    await page.goto(origin + '/perform');
    await page.locator('[data-live-instance]').first().waitFor({state:'attached',timeout:60000});
    const live=page.getByRole('button',{name:/^Live ·/});
    if(await live.isVisible()) await live.click();
    await positionBoard(page);
    await page.locator('.strip-monitor:visible .parameter-history svg').first().waitFor();
    await page.waitForFunction(()=>document.querySelector('.strip-monitor:has(.parameter-history) path')?.getAttribute('d')?.includes('L'));
    await page.screenshot({path:join(directory,name+'.png')});
    measurements[name]=await measureCards(page);
    assert.equal(measurements[name].horizontalOverflow,0,'Document overflow at '+name);
    await verifyControls(page,touch);
    assert.deepEqual(errors,[]);
    await page.close();
  }
  await writeFile(join(directory,'metrics.json'),JSON.stringify(measurements,null,2)+'\n');
  console.log(JSON.stringify(measurements,null,2));
} finally {
  await browser.close();
}
