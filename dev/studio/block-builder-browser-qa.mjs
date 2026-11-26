// Run against a disposable, preview-only Ring application with the block catalog.
import assert from 'node:assert/strict';
import {chromium} from 'playwright-core';

const origin = process.argv[2];
assert.ok(origin,'Supply the Ring origin.');
const show = await (await fetch(origin+'/api/state',{headers:{accept:'application/json'}})).json();
assert.equal(show.status.output.type,'preview');
const browser = await chromium.launch({executablePath:process.env.CHROMIUM_PATH || '/usr/bin/chromium'});
try {
  const page = await browser.newPage({viewport:{width:1024,height:768},hasTouch:true});
  const errors = [];
  page.on('pageerror',error => errors.push(error.message));
  await page.goto(origin+'/library');
  const source = page.getByLabel('Declarative EDN definition');
  const id = 'water-glass-'+Date.now();
  const before = `; authored source\n{:id :${id} :version 1 :params {}\n :outputs {:color [0.8 0.7 0.4] :dimmer 0.65}} ; preserve this\n`;
  await source.fill(before);
  const panel = page.locator('.block-editor:visible');
  await panel.getByRole('button',{name:'Assign blocks'}).tap();
  await panel.getByLabel('Block binding').selectOption({label:'outputs / color'});
  async function add(name) {
    await panel.getByLabel('New block').selectOption({label:name});
    await panel.getByRole('button',{name:'Add stage'}).tap();
  }
  await add('Through water');
  await add('Color gel');
  await panel.getByRole('button',{name:'Move Color gel earlier'}).tap();
  // Address the water stage only once the reorder has rendered.
  const stage = panel.locator('.block-stage').nth(1).filter({hasText:'2 · Through water'});
  await stage.getByLabel('Coordinates').selectOption({label:'Surface UV'});
  const wet = stage.getByRole('spinbutton',{name:'Wet amount',exact:true});
  // The tap on Write blurs the field, which commits the amount in the same task as the
  // tap itself (as on an iPad). Both must reach the written stages.
  await wet.fill('0.4');
  await panel.getByRole('button',{name:'Write stages to EDN'}).tap();
  await page.waitForFunction(() => document.querySelector('#definition-source').value.includes(':blocks'));
  const changed = await source.inputValue();
  assert.ok(changed.startsWith(before.slice(0,before.indexOf('}} ; preserve this')+1)));
  assert.ok(changed.endsWith('} ; preserve this\n'));
  assert.ok(changed.includes(':coordinates :surface-uv'));
  assert.ok(changed.includes(':amount 0.4'));
  assert.ok(changed.indexOf(':optics/gel')<changed.indexOf(':optics/through-water'));
  assert.match(changed,/\{:block :optics\/through-water,?\s+:coordinates :surface-uv,?\s+:amount 0\.4\}/);
  const ack = page.waitForResponse(response => response.url().endsWith('/api/definition') && response.request().method()==='POST');
  await page.getByRole('button',{name:'Validate & publish'}).tap();
  assert.equal((await ack).status(),200);
  assert.deepEqual(errors,[]);
} finally {await browser.close();}
