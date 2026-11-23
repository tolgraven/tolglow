// Run against a disposable preview-only show with numeric [:variables :weather :density].
// Optional EVIDENCE_DIR stores the two operator views and a sequence of frames from the live plot.
import assert from 'node:assert/strict';
import {mkdir} from 'node:fs/promises';
import {chromium} from 'playwright-core';

const origin = process.argv[2];
assert.ok(origin, 'Supply the Ring origin.');
const show = await (await fetch(origin+'/api/state', {headers:{accept:'application/json'}})).json();
assert.equal(show.status.output.type, 'preview');
const browser = await chromium.launch({executablePath:process.env.CHROMIUM_PATH || '/usr/bin/chromium'});
const evidence = process.env.EVIDENCE_DIR;
if (evidence) await mkdir(evidence, {recursive:true});

async function operator(viewport) {
  const page = await browser.newPage({viewport,hasTouch:true});
  const errors = [], leases = [];
  page.on('pageerror', error => errors.push(error.message));
  page.on('websocket', socket => socket.on('framesent', ({payload}) => {
    const frame = String(payload);
    if (frame.includes('queries')) leases.push(frame.includes('runtime/block-trace'));
  }));
  await page.goto(origin+'/compose');
  const panel = page.getByRole('region', {name:'Show variable chains'});
  await panel.getByRole('button',{name:'Edit variable chains'}).tap();
  await panel.getByLabel('Find live variables').fill('density');
  await panel.getByRole('button',{name:'Search sources'}).tap();
  await panel.getByLabel('Live sources').selectOption({label:'weather / density'});
  await panel.locator('.variable-block-editor').scrollIntoViewIfNeeded();
  return {page, panel, errors, leases};
}

async function change({page, panel}, name) {
  const receipt = page.waitForResponse(r => r.url().endsWith('/api/variable-block') && r.request().method()==='POST');
  await panel.getByRole('button',{name}).tap();
  assert.equal((await receipt).status(),200);
}

async function capture(page, file) {
  if (evidence) await page.screenshot({path:evidence+'/'+file});
}

try {
  const first = await operator({width:1024,height:820});
  const second = await operator({width:390,height:844});
  const {page, panel, leases} = first;
  assert.ok(!leases.includes(true),'A new binding should have no trace sample demand');
  await panel.getByLabel('New block').selectOption({label:'Through water'});
  await panel.getByRole('button',{name:'Add stage'}).tap();
  const wet = panel.getByRole('spinbutton',{name:'Wet amount',exact:true});
  await wet.fill('0.55');
  await wet.blur();
  if (evidence) await panel.evaluate(el => el.scrollIntoView({block:'center'}));
  await capture(page,'editor.png');
  await change(first, 'Save variable chain');
  await panel.getByText('Variable chain saved.').waitFor();
  await second.panel.getByRole('button',{name:'Reload binding'}).waitFor();
  assert.ok(await second.panel.getByRole('button',{name:'Save variable chain'}).isDisabled());
  await second.panel.getByRole('button',{name:'Reload binding'}).tap();
  assert.equal(await second.panel.getByRole('spinbutton',{name:'Wet amount',exact:true}).inputValue(),'0.55');
  const fits = await second.panel.evaluate(el => el.getBoundingClientRect().right <= window.innerWidth + 1);
  assert.ok(fits,'Phone controls stay within the viewport');
  const desktop = await operator({width:1440,height:900});
  assert.equal(await desktop.panel.getByRole('spinbutton',{name:'Wet amount',exact:true}).inputValue(),'0.55');
  await desktop.page.close();
  const plot = panel.getByRole('region',{name:'Block signal trace'});
  await panel.locator('.block-trace-viewport').scrollIntoViewIfNeeded();
  await plot.waitFor();
  await page.waitForFunction(() => document.querySelector('.block-trace-result polyline')?.getAttribute('points').split(' ').length>15);
  assert.ok(leases.includes(true),'Viewing the plot requests the server sampler');
  await page.waitForFunction(() => document.querySelector('.block-trace-result polyline')?.getAttribute('points').split(' ').length>45);
  await capture(page,'trace.png');
  if (evidence) {
    for (let i=0; i<32; i++) {
      await capture(page,'live-'+String(i).padStart(2,'0')+'.png');
      await new Promise(resolve => setTimeout(resolve,65));
    }
  }
  await panel.getByRole('button',{name:'Close editor'}).tap();
  await page.waitForFunction(() => !document.querySelector('.variable-block-editor'));
  await new Promise(resolve => setTimeout(resolve,300));
  assert.equal(leases.at(-1),false,'Closing the inspector releases its remote sampler');
  await change(second,'Remove chain');
  await second.panel.getByText('Variable chain removed.').waitFor();
  assert.deepEqual([...first.errors,...second.errors,...desktop.errors],[]);
} finally { await browser.close(); }
