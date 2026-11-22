// Run against a disposable Tolglow preview show with patched fixture groups.
import assert from 'node:assert/strict';
import {chromium} from 'playwright-core';

const origin = process.argv[2];
const state = () => fetch(origin + '/api/state', {headers:{accept:'application/json'}}).then(r => r.json());
const initial = await state();
assert.equal(initial.status.output.type, 'preview');
const browser = await chromium.launch({executablePath:process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args:['--no-sandbox','--use-gl=angle','--use-angle=swiftshader','--enable-unsafe-swiftshader']});
try {
  const page = await browser.newPage({viewport:{width:1024,height:768},hasTouch:true});
  const errors = []; page.on('pageerror', e => errors.push(e.message));
  await page.goto(origin + '/rig');
  const panel = page.getByRole('region',{name:'Spatial controls'}), id = 'qa-orbit-' + Date.now();
  await panel.getByLabel('New volume ID').fill(id);
  const write = async (control) => {
    const [response] = await Promise.all([page.waitForResponse(r => r.url().endsWith('/api/settings') && r.request().method()==='POST'), control.click()]);
    assert.ok(response.ok(), await response.text());
  };
  await write(panel.getByRole('button',{name:'Add volume'}));
  await page.waitForFunction(id => [...document.querySelectorAll('.spatial-volume-list button[aria-pressed=true]')].some(el => el.textContent===id), id);
  const radius = panel.getByRole('spinbutton',{name:'Radius · m',exact:true});
  await radius.fill('0.82'); await radius.press('Enter');
  await panel.getByLabel('Modulate radius').click();
  await panel.getByRole('combobox', {name:'Radius modulation source'}).waitFor();
  await write(panel.getByRole('button',{name:'Apply volume'}));
  await page.waitForFunction(async ({id}) => {
    const s = await fetch('/api/state',{headers:{accept:'application/json'}}).then(r => r.json());
    return s.settings.spatial.volumes[id].radius === .82;
  }, {id});
  const source = (await state()).settings.spatial.volumes[id]['radius-source'];
  assert.deepEqual(source.range,[.41,.82]);
  const filter = panel.getByRole('combobox',{name:'Limit contact to'});
  await filter.click(); await filter.fill('qa-floor');
  await panel.getByRole('option',{name:/qa-floor/i}).first().click();
  await write(panel.getByRole('button',{name:'Bind action'}));
  const config = (await state()).settings.spatial;
  assert.ok(Object.values(config.bindings).some(b => b.selector?.['group-id']==='qa-floor' && b.action?.preset==='aim'));
  await write(panel.getByRole('button',{name:'Throw ball',exact:true}));
  assert.ok((await state()).settings.spatial.volumes[id]['throw-id']);
  await page.setViewportSize({width:390,height:844});
  await panel.getByRole('button',{name:'Apply volume'}).scrollIntoViewIfNeeded();
  const box = await panel.boundingBox(); assert.ok(box.width <= 390, JSON.stringify(box));
  await write(panel.getByRole('button',{name:'Remove volume'}));
  assert.equal((await state()).settings.spatial.volumes[id],undefined);
  const push = await browser.newPage({viewport:{width:960,height:160}});
  await push.goto(origin+'/push-display');
  await push.waitForFunction(()=>globalThis.tolglowPushDisplay?.ready);
  await push.evaluate(()=>globalThis.tolglowPushDisplay.update({'target-picker':{'open?':true,page:1,pages:1,rows:[{kind:'volume',label:'Orbit'}]}}));
  assert.equal(await push.getByRole('region',{name:'Cue launch targets'}).getByText('Orbit').count(),1);
  assert.deepEqual(errors,[]);
  console.log('Spatial editor, typed search, live settings writes, phone layout and Push webview passed.');
} finally { await browser.close(); }
