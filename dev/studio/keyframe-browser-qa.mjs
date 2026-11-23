// Run against a disposable preview show. The first argument is its Ring origin.
// An optional second argument receives screenshots of the real editor gesture.
import assert from 'node:assert/strict';
import {mkdir} from 'node:fs/promises';
import {chromium} from 'playwright-core';

const [origin, captures] = process.argv.slice(2);
assert.ok(origin, 'Specify the disposable preview origin');
if (captures) await mkdir(captures, {recursive:true});
async function request(path, body) {
  const response = await fetch(origin + path, {method: body ? 'POST' : 'GET',
    headers: {accept:'application/json','content-type':'application/json'},
    body: body && JSON.stringify(body)});
  const data = await response.json();
  assert.ok(response.ok && data.status !== 'rejected', JSON.stringify(data));
  return data;
}
const initial = await request('/api/state');
assert.equal(initial.status.output.type, 'preview');
const id = 'keyframes-qa-' + Date.now();
const scope = {'expected-epoch':initial.epoch, 'runtime-id':initial['runtime-id']};
const publish = (kind, source) => request('/api/definition', {...scope, kind, source});
await publish('effects', `{:id :${id} :name "Beat motion" :version 1
  :keyframes {:interval {:beats 1/3} :curve :spring}
  :outputs {:dimmer {:op :fract :args [{:op :multiply :args [0.25 {:op :beat}]}]}}}`);
await publish('cues', `{:id :${id} :name "Beat motion" :effect-id :${id} :version 1 :reload-policy :preserve-state}`);
const launch = await request('/api/command', {...scope, op:'cue/launch', 'cue-id':id,
  epoch:initial.epoch, 'show-id':initial['show-id'],'command-id':crypto.randomUUID(),source:{adapter:'web','session-id':id}});
const browser = await chromium.launch({executablePath:process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args:['--no-sandbox','--use-gl=angle','--use-angle=swiftshader','--enable-unsafe-swiftshader']});
try {
  const page = await browser.newPage({viewport:{width:834,height:1010},hasTouch:true});
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(origin);
  const live = page.getByRole('button',{name:/^Live ·/});
  if (await live.isVisible()) await live.tap();
  const card = page.locator('[data-live-instance="' + launch['instance-id'] + '"]');
  const open = async () => {
    const summary = card.locator('summary').filter({hasText:'Keyframe timing'});
    if (!await summary.isVisible()) await card.getByRole('button',{name:/Expand .* controls/}).tap();
    if (!await card.getByLabel('Keyframe interval',{exact:true}).isVisible()) await summary.tap();
  };
  await open();
  const interval = card.getByLabel('Keyframe interval',{exact:true});
  assert.equal(await interval.inputValue(),'1/3');
  await card.getByRole('spinbutton',{name:'Spring damping'}).fill('0.4');
  await card.getByLabel('Tween curve',{exact:true}).selectOption('cubic-bezier');
  const editor = card.locator('.keyframe-editor');
  const graph = editor.locator('svg');
  await graph.scrollIntoViewIfNeeded();
  const handle = await graph.locator('circle.hit').first().boundingBox();
  const bounds = await graph.boundingBox();
  const start = {x:handle.x + handle.width / 2,y:handle.y + handle.height / 2};
  const end = {x:start.x + bounds.width * .31,y:start.y - bounds.height * .38};
  const capture = async i => {
    if (captures) await editor.screenshot({path:captures + '/' + String(i).padStart(2,'0') + '.png'});
  };
  // Chromium's touch sequence dispatches actual pointer capture and move events.
  const cdp = await page.context().newCDPSession(page);
  const touch = (type, points) => cdp.send('Input.dispatchTouchEvent',{type,touchPoints:points});
  await capture(0);
  await touch('touchStart',[start]);
  for (let i=1;i<=10;i++) {
    await touch('touchMove',[{x:start.x+(end.x-start.x)*i/10,y:start.y+(end.y-start.y)*i/10}]);
    await capture(i);
  }
  await touch('touchEnd',[]);
  const x = Number(await editor.getByLabel('First X',{exact:true}).inputValue());
  const y = Number(await editor.getByLabel('First Y',{exact:true}).inputValue());
  assert.ok(x > .40 && y > .40, 'Touch drag must change both control point coordinates');
  await interval.fill('1/4');
  await interval.press('Tab');
  const published = page.waitForResponse(r=>r.url().endsWith('/api/definition') && r.request().method()==='POST');
  await card.getByRole('button',{name:'Publish keyframe settings'}).tap();
  const response = await published;
  assert.ok(response.ok(),await response.text());
  await page.waitForFunction(async ({id}) => {
    const state = await (await fetch('/api/state',{headers:{accept:'application/json'}})).json();
    return state.instances.find(x=>x.id===id)?.['effect-version']===2;
  },{id:launch['instance-id']});
  await open();
  const updated = (await request('/api/state')).instances.find(x=>x.id===launch['instance-id']);
  assert.equal(updated.definition.keyframes.curve.type,'cubic-bezier');
  assert.ok(Math.abs(updated.definition.keyframes.curve.points[0]-x)<1e-8);
  assert.equal(await interval.inputValue(),'1/4');
  await card.locator('.keyframe-sample dl').waitFor();
  assert.match(await card.locator('.keyframe-sample').innerText(),/origin/i);
  assert.match(await card.locator('.keyframe-sample').innerText(),/target/i);
  assert.match(await card.locator('.keyframe-sample').innerText(),/result/i);
  if (captures) await card.locator('details[open]').filter({has:page.locator('.keyframe-editor')}).last()
    .screenshot({path:captures+'/published.png'});
  assert.deepEqual(errors,[]);
  console.log('Touch drag, exact beat fractions, spring edit, published v2 and live native values verified.');
} finally {await browser.close();}
