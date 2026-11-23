// Run through the disposable Leiningen host. Each logical radio uses normal sensor admission.
import assert from 'node:assert/strict';
import { mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { chromium } from 'playwright-core';

const [origin, output] = process.argv.slice(2);
const browser = await chromium.launch({ executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium' });
const devices = [['ipad',1024,768], ['phone',390,844], ['desktop',1440,900]];
const mapping = '{:version 1 :timeout-ms 1700 :channels [{:id :audience-count :input :audience-count} {:id :dance-effort :input :dance-effort} {:id :pulse-count :input :pulse-count} {:id :pulse-bpm :input :pulse-bpm}]}';

async function post(route, body) {
  const response = await fetch(origin + route, {
    method: 'POST', headers: { 'Content-Type': 'application/json', Accept: 'application/json' },
    body: JSON.stringify(body)
  });
  const result = await response.json();
  assert.ok(response.ok && result.status !== 'rejected' && result['accepted?'] !== false,
            'Show accepts ' + route + ': ' + (result.message || result.status || result.reason || ''));
  return result;
}

async function gateway(id, label, people, effort, wearers, pulse) {
  const pair = await post('/api/sensors/pair', { 'device-id': id, label });
  await post('/api/sensors/configure', { 'device-id': id, source: mapping });
  let serial = 0;
  let running = true;
  let failure;
  async function sample() {
    await post('/api/sensors/sample', {
      version: 1, 'device-id': id, 'session-id': 'gateway-' + id, 'sample-id': serial++,
      'time-ms': Date.now(), 'pairing-token': pair['pairing-token'],
      values: { 'audience-count': people, 'dance-effort': effort, 'pulse-count': wearers, 'pulse-bpm': pulse },
      gestures: []
    });
  }
  async function stream() {
    try {
      while (running) {
        await sample();
        await new Promise(resolve => setTimeout(resolve, 200));
      }
    } catch (error) { failure = error; }
  }
  const task = stream();
  return async () => { running = false; await task; if (failure) throw failure; };
}

await mkdir(join(output, 'frames'), { recursive: true });
const stops = [];
try {
  const state = await (await fetch(origin + '/api/state')).json();
  assert.equal(state.status.output.type, 'preview');
  const pages = new Map();
  const errors = [];
  for (const [name,width,height] of devices) {
    const page = await browser.newPage({ viewport: { width,height }, hasTouch: name !== 'desktop' });
    page.on('pageerror',error => errors.push(error.message));
    await page.goto(origin + '/connections');
    await page.getByText(/Enroll a gateway with/).waitFor();
    pages.set(name,page);
  }
  const ipad = pages.get('ipad');
  let frame = 0;
  async function capture() {
    await ipad.screenshot({ path: join(output,'frames',String(++frame).padStart(3,'0')+'.png') });
  }
  await capture();
  await ipad.screenshot({ path: join(output,'empty.png') });
  stops.push(await gateway('front','Dance floor',5,0.6,2,110));
  stops.push(await gateway('balcony','Balcony',3,0.2,1,80));
  const card = ipad.locator('#audience-zones');
  await card.locator('[data-zone="balcony"][data-status="ready"]').waitFor();
  await capture();
  for (const label of ['Dance floor','Balcony']) {
    await card.getByRole('checkbox',{ name:'Include '+label+' in audience mix' }).check();
    await card.getByRole('button',{ name:'Arm '+label,exact:true }).click();
    await card.getByRole('button',{ name:'Disarm '+label,exact:true }).waitFor();
    await capture();
  }
  for (const [name,page] of pages) {
    const meter = page.getByRole('meter',{name:'Crowd dance effort'});
    await page.getByText('100 BPM',{exact:true}).waitFor();
    assert.equal(await meter.getAttribute('value'),'45');
    assert.equal(await page.locator('#audience-zones dd').first().innerText(),'8');
    assert.equal(await page.locator('#audience-zones').evaluate(el => el.scrollWidth <= el.clientWidth),true);
    await page.screenshot({ path: join(output,name+'.png') });
  }
  await card.getByRole('checkbox',{name:'Include Dance floor in audience mix'}).uncheck();
  await card.getByText('80 BPM',{exact:true}).waitFor();
  await capture();
  await stops[1]();
  await card.locator('[data-zone="balcony"][data-status="expired"]').waitFor();
  await card.getByText('0 / 0',{exact:true}).waitFor();
  await capture();
  await ipad.screenshot({ path: join(output,'expired.png') });
  await ipad.reload();
  await ipad.locator('#audience-zones [data-zone="balcony"][data-status="expired"]').waitFor();
  assert.equal(await ipad.getByRole('checkbox',{name:'Include Balcony in audience mix'}).isChecked(),true);
  assert.equal(await ipad.getByRole('checkbox',{name:'Include Dance floor in audience mix'}).isChecked(),false);
  assert.deepEqual(errors,[]);
  console.log('Three viewport sizes received eight dancers, 45% weighted effort, and 100 BPM from three consenting wearers. Zone exclusion, dropout and reload passed.');
} finally {
  try { await Promise.all(stops.map(stop => stop())); }
  finally { await browser.close(); }
}
