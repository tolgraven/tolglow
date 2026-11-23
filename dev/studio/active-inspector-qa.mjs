// Run against a disposable Ring preview show with at least one moving head.
import assert from 'node:assert/strict';
import {chromium} from 'playwright-core';

const origin = process.argv[2] || 'http://127.0.0.1:8773';
const read = async (path, options = {}) => {
  const response = await fetch(origin + path, {...options, headers: {accept: 'application/json', 'content-type': 'application/json'}});
  const body = await response.json();
  assert.ok(response.ok && body.status !== 'rejected', JSON.stringify(body));
  return body;
};
const show = await read('/api/state');
assert.equal(show.status.output.type, 'preview');
const id = 'inspector-qa-' + Date.now();
const post = (path, body) => read(path, {method:'POST', body:JSON.stringify({...body, 'expected-epoch':show.epoch, 'runtime-id':show['runtime-id']})});
const definition = (kind, source) => post('/api/definition', {kind, source});
const command = (op, body) => post('/api/command', {op, epoch:show.epoch, 'show-id':show['show-id'],
  'command-id':crypto.randomUUID(), source:{adapter:'web', 'session-id':id}, ...body});
const scalar = '{:type :number :bounds [0 1] :default 0.5}';
await definition('effects', `{:id :${id} :version 1 :params {:x ${scalar} :y ${scalar} :z ${scalar} :aim-vector {:type :vector :size 3 :bounds [-4 4] :default [0 1 0]}}
  :outputs {:aim {:op :vec3 :args [{:op :param :key :x} {:op :param :key :y} {:op :param :key :z}]} :dimmer 0.1}}`);
await definition('cues', `{:id :${id} :name "QA geometry" :version 1 :effect-id :${id} :reload-policy :preserve-state}`);
const launched = await command('cue/launch', {'cue-id':id});
const browser = await chromium.launch({executablePath:process.env.CHROMIUM_PATH || '/usr/bin/chromium', args:['--no-sandbox','--use-gl=angle','--use-angle=swiftshader','--enable-unsafe-swiftshader']});
try {
  const page = await browser.newPage({viewport:{width:820,height:980},hasTouch:true});
  const errors = [];
  page.on('pageerror', err => errors.push(err.message));
  await page.goto(origin);
  const live = page.getByRole('button', {name:/^Live ·/});
  if (await live.isVisible()) await live.tap();
  const card = page.locator('[data-live-instance="' + launched['instance-id'] + '"]');
  await card.getByRole('button',{name:'Expand QA geometry controls'}).tap();
  const source = card.getByRole('button',{name:'Attach source for aim',exact:true});
  await source.tap();
  const editor = card.getByRole('region',{name:/^[xyz] source$/});
  await editor.getByRole('heading').and(page.locator(':focus')).waitFor();
  const axis = editor.getByRole('combobox',{name:'Source axis'});
  const editorId = await editor.getAttribute('id');
  await axis.focus();
  await axis.selectOption('y');
  await editor.getByRole('heading',{name:'y source',exact:true}).waitFor();
  await axis.and(page.locator(':focus')).waitFor();
  assert.equal(await editor.getAttribute('id'),editorId);
  assert.equal(await source.getAttribute('aria-controls'),editorId);
  await page.keyboard.press('Escape');
  await source.and(page.locator(':focus')).waitFor();
  await card.getByRole('button',{name:'Edit aim components', exact:true}).tap();
  const pop = page.getByRole('dialog',{name:'aim components',exact:true});
  await pop.getByRole('slider',{name:'aim X · x rotary'}).press('End');
  await pop.getByRole('button',{name:'Done'}).tap();
  await card.getByRole('button',{name:/Edit aim.vector components/i}).tap();
  const vec = page.getByRole('dialog',{name:/aim.vector components/i});
  await vec.getByRole('slider',{name:/Y rotary$/}).press('End');
  await vec.getByRole('button',{name:'Done'}).tap();
  const patch = card.locator('.cue-patch-editor').filter({has:page.locator('summary',{hasText:'Patch dimmer'})});
  await patch.locator('summary').tap();
  await patch.getByRole('combobox',{name:'Source parameter'}).selectOption({label:':x'});
  await patch.getByRole('spinbutton',{name:'Scale'}).fill('0.5');
  const [resp] = await Promise.all([page.waitForResponse(r => r.url().endsWith('/api/definition') && r.request().method()==='POST'),patch.getByRole('button',{name:'Publish connection'}).tap()]);
  assert.ok(resp.ok(), await resp.text());
  await card.getByText('v2 · preserve state',{exact:true}).waitFor();
  const instance = (await read('/api/state')).instances.find(i=>i.id===launched['instance-id']);
  assert.equal(instance.params.x,1);
  assert.equal(instance.params['aim-vector'][1],4);
  assert.equal(instance.definition.outputs.dimmer.op,'multiply');
  await definition('effects', `{:id :${id} :version 3 :params {} :outputs {:dimmer {:op :divide :args [1 0]}}}`);
  await card.locator('.cue-fault summary').first().waitFor();
  assert.ok(await card.evaluate(el => el.classList.contains('has-fault')));
  await card.locator('.cue-fault summary').first().tap();
  await card.locator('.cue-fault pre').first().waitFor();
  assert.deepEqual(errors,[]);
  console.log('Verified XYZ grouping, vector overlays, versioned patch hot reload and failure diagnostics.');
} finally {
  await browser.close();
  await command('instance/remove',{'instance-id':launched['instance-id']});
}
