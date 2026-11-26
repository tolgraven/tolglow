// Run through tolglow.qa.auto-show-browser (#217). Phase "repl" checks that a
// REPL section plan reached the open auto panel; "studio" restarts auto mode
// from Studio with varied parameters, keeps the plan and sees a refusal;
// "stopped" checks that a REPL stop reached the page; "pending" sees a REPL
// change waiting on its bar and cancels it from Studio; "line" starts auto on
// the next bar from Studio (#767). docs/auto-show.md.
import assert from 'node:assert/strict';
import {mkdir, readFile, writeFile} from 'node:fs/promises';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, directory, phase] = process.argv.slice(2);
await mkdir(directory, {recursive: true});

async function state(page) {
  const response = await page.request.get(origin + '/api/state', {headers: {accept: 'application/json'}});
  assert.ok(response.ok());
  return response.json();
}

async function eventually(check, what) {
  for (let attempt = 0; attempt < 100; attempt++) {
    const value = await check();
    if (value) return value;
    await new Promise(done => setTimeout(done, 100));
  }
  throw new Error(`timed out waiting for ${what}`);
}

async function openPanel(page) {
  await page.goto(origin + '/perform');
  const tools = page.getByRole('button', {name: /^Tools/});
  if (await tools.count() && await tools.first().isVisible()) await tools.first().click();
  const panel = page.locator('details.auto-panel').filter({has: page.locator('#auto-start')});
  await panel.waitFor();
  if (!(await panel.evaluate(el => el.open))) await panel.locator('summary').click();
  return panel;
}

async function replPhase(page) {
  const panel = await openPanel(page);
  await eventually(async () => (await panel.locator('#auto-status').textContent()).includes('on'), 'auto on');
  const keep = panel.locator('#auto-keep-plan');
  assert.ok(await keep.isChecked(), 'a REPL plan is kept by default');
  const label = await keep.locator('xpath=..').textContent();
  assert.match(label, /red, purple, quiet, strobe/);
  const reason = await eventually(async () => {
    const text = await panel.locator('#auto-reason').textContent().catch(() => '');
    return /^Section (red|purple|quiet|strobe)/.test(text) && text;
  }, 'section summary');
  await panel.screenshot({path: join(directory, 'auto-panel-repl-plan.png')});
  return {label, reason};
}

async function studioPhase(page) {
  const panel = await openPanel(page);
  // The REPL plan varies parameters, so Vary starts ticked.
  const vary = panel.getByLabel('Vary marked parameters within their ranges');
  await eventually(() => vary.isChecked(), 'vary follows the saved config');
  const started = page.waitForResponse(r => r.request().method() === 'POST' && new URL(r.url()).pathname === '/api/auto');
  await panel.getByRole('button', {name: 'Start auto', exact: true}).click();
  const reply = await started;
  assert.equal(reply.status(), 200, await reply.text());
  // Studio posts transit: check the two fields this change adds.
  const sent = reply.request().postData();
  assert.match(sent, /"~:keep-plan\?",true/);
  assert.match(sent, /"~:randomize",\["\^ ","~:params","marked"\]/);
  const auto = await eventually(async () => {
    const a = (await state(page)).auto;
    return a['enabled?'] && a.config.sections?.length === 4 && a.config.randomize?.palette && a;
  }, 'committed plan with palette');
  // A refusal from Studio: more cues per step than the planner allows.
  const count = panel.locator('#auto-count');
  await count.fill('70');
  await count.blur();
  const refused = page.waitForResponse(r => r.request().method() === 'POST' && new URL(r.url()).pathname === '/api/auto');
  await panel.getByRole('button', {name: 'Start auto', exact: true}).click();
  const refusal = await refused;
  assert.notEqual(refusal.status(), 200);
  const message = await eventually(async () => {
    const text = await page.locator('body').textContent();
    const shown = 'Invalid auto config: Count must be an integer from 1 to 64';
    return text.includes(shown) && shown;
  }, 'refusal feedback');
  const after = (await state(page)).auto;
  assert.equal(after.config.count, auto.config.count, 'the committed config is unchanged');
  await count.fill('1');
  await panel.screenshot({path: join(directory, 'auto-panel-studio-start.png')});
  return {sections: auto.config.sections.map(s => s.id), randomize: auto.config.randomize.params, refusal: message};
}

async function stoppedPhase(page) {
  const panel = await openPanel(page);
  await eventually(async () => (await panel.locator('#auto-status').textContent()).includes('off'), 'auto off');
  assert.ok(await panel.locator('#auto-keep-plan').isVisible(), 'the stopped show still offers its plan');
  return {status: await panel.locator('#auto-status').textContent()};
}

async function pendingPhase(page) {
  const panel = await openPanel(page);
  const bar = (await readFile(join(directory, 'pending-bar.txt'), 'utf8')).trim();
  const item = panel.locator('#auto-pending li');
  await eventually(async () => (await item.count()) === 1 && (await item.first().textContent()).includes(`Drop at bar ${bar}`),
                   'the REPL change listed with its bar');
  await panel.screenshot({path: join(directory, 'auto-panel-pending.png')});
  const text = (await item.first().textContent()).replace('Cancel', '').trim();
  const cancelled = page.waitForResponse(r => r.request().method() === 'POST' && new URL(r.url()).pathname === '/api/command');
  await item.first().getByRole('button', {name: 'Cancel'}).click();
  const reply = await cancelled;
  assert.equal(reply.status(), 200, await reply.text());
  await eventually(async () => (await panel.locator('#auto-pending').count()) === 0, 'the list empties');
  assert.equal((await state(page)).auto.pending, undefined, 'nothing pending in the projection');
  return {listed: text, cancelled: true};
}

async function linePhase(page) {
  const panel = await openPanel(page);
  const applies = panel.locator('#auto-applies');
  assert.ok(await applies.locator('option[value="next-section"]').isDisabled(), 'no section line while auto is off');
  await applies.selectOption('next-bar');
  const started = page.waitForResponse(r => r.request().method() === 'POST' && new URL(r.url()).pathname === '/api/auto');
  await panel.getByRole('button', {name: 'Start auto', exact: true}).click();
  const reply = await started;
  assert.equal(reply.status(), 200, await reply.text());
  const sent = reply.request().postData();
  assert.match(sent, /"~:at","next-bar"/);
  const body = await reply.text();
  assert.match(body, /scheduled/);
  await eventually(async () => (await panel.locator('#auto-status').textContent()).includes('on'), 'auto on after the line');
  await eventually(() => applies.locator('option[value="next-section"]').isEnabled(), 'a playing section offers its line');
  await panel.screenshot({path: join(directory, 'auto-panel-line.png')});
  return {sent: 'next-bar', status: await panel.locator('#auto-status').textContent()};
}

const context = await chromium.launchPersistentContext(resolve('target/auto-show-chrome'), {
  executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args: ['--headless=new', '--use-angle=metal', '--disable-dev-shm-usage'],
  viewport: {width: 1440, height: 1400},
});
try {
  const page = await context.newPage();
  page.setDefaultTimeout(10000);
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  const run = {repl: replPhase, studio: studioPhase, stopped: stoppedPhase, pending: pendingPhase, line: linePhase}[phase];
  const report = await run(page).catch(async error => {
    await page.screenshot({path: join(directory, `auto-${phase}-failure.png`), fullPage: true});
    throw error;
  });
  assert.deepEqual(errors, []);
  await writeFile(join(directory, `auto-${phase}.json`), JSON.stringify(report, null, 2));
  console.log(`auto show ${phase} phase passed`, JSON.stringify(report));
} finally {
  await context.close();
}
