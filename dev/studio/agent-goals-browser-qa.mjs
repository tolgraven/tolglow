// Run through tolglow.qa.agent-goals-browser, which starts the optimized
// Studio on the preview-only demo show and makes its REPL edits while this
// page stays open. Goals panel (#857): docs/repl-studio-parity.md.
// Headless Chrome (--headless=new, real GPU through ANGLE Metal). Not
// foreground-display or iPad Safari evidence.
import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, directory] = process.argv.slice(2);
const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
await mkdir(directory, {recursive: true});
const log = [];
const note = (...parts) => { const line = parts.join(' '); log.push(line); console.log(line); };

async function state(page) {
  const response = await page.request.get(origin + '/api/state', {headers: {accept: 'application/json'}});
  assert.ok(response.ok());
  return response.json();
}

async function until(page, predicate, what) {
  for (let attempt = 0; attempt < 200; attempt++) {
    const show = await state(page);
    if (predicate(show)) return show;
    await new Promise(r => setTimeout(r, 100));
  }
  throw new Error('Timed out waiting for ' + what);
}

const goalsOf = show => show['agent-goals'].active;

const context = await chromium.launchPersistentContext(resolve(directory, 'chrome-profile'), {
  executablePath, headless: true, viewport: {width: 1440, height: 1000},
  args: ['--headless=new', '--use-angle=metal', '--disable-dev-shm-usage', '--hide-scrollbars'],
});
const errors = [];
try {
  const page = context.pages()[0] || await context.newPage();
  page.on('console', m => { if (m.type() === 'error') errors.push(m.text()); });
  page.on('pageerror', e => errors.push(String(e)));
  await page.goto(origin + '/perform');
  const live = page.getByRole('button', {name: /^Live/});
  if (await live.isVisible()) await live.click();
  const panel = page.getByRole('region', {name: 'Agent goals'});
  await panel.waitFor();
  await panel.scrollIntoViewIfNeeded();

  // REPL -> Studio: the goals the REPL put are listed in order.
  await panel.locator('li.agent-goal').nth(1).waitFor();
  assert.deepEqual(await panel.locator('li.agent-goal').evaluateAll(cards => cards.map(c => c.dataset.scope)), ['night', 'set']);
  assert.match(await panel.locator('li.agent-goal').first().innerText(), /Warm and patient/);
  assert.equal(await panel.getByRole('img', {name: 'Palette, 3 keyframes'}).count(), 1);
  assert.equal(await panel.getByRole('img', {name: 'Energy curve, 4 points'}).count(), 1);
  note('panel shows the REPL goals:', 'night, set');

  // Studio -> REPL: add a section goal from the panel.
  await panel.getByRole('button', {name: 'Add goal', exact: true}).click();
  const editor = panel.getByRole('form', {name: 'New goal'});
  await editor.getByLabel('Scope').selectOption('section');
  await editor.getByLabel('Goal text').fill('Lift the second chorus; hold the first one back.');
  await editor.getByLabel('From position').fill('16');
  await editor.getByLabel('Until position').fill('24');
  await editor.getByRole('button', {name: 'Add goal'}).click();
  let show = await until(page, s => goalsOf(s).length === 3, 'the Studio goal');
  const added = goalsOf(show)[2];
  assert.deepEqual([added.scope, added.by, added.from, added.until], ['section', 'studio', {bar: 16}, {bar: 24}]);
  await panel.locator('li.agent-goal').nth(2).waitFor();
  assert.match(await panel.locator('li.agent-goal').nth(2).innerText(), /bar 16 → bar 24/);
  note('panel added', added.id, 'as', JSON.stringify(added.from), '→', JSON.stringify(added.until));

  // Move it to the top: two Move up presses.
  for (const position of [3, 2]) {
    await panel.getByRole('button', {name: `Move up Goal ${position}, Section`}).click();
    await until(page, s => goalsOf(s).find(g => g.id === added.id).order === position - 1, 'the move');
  }
  await page.waitForFunction(() => document.querySelector('li.agent-goal')?.dataset.scope === 'section');
  note('moved', added.id, 'to order 1');

  // Client-side refusal: over-long text.
  await panel.getByRole('button', {name: 'Add goal', exact: true}).click();
  await panel.getByLabel('Goal text').fill('x'.repeat(601));
  assert.match(await panel.locator('#agent-goal-count').innerText(), /601 \/ 600/);
  await panel.getByRole('form', {name: 'New goal'}).getByRole('button', {name: 'Add goal'}).click();
  assert.match(await panel.getByRole('alert').innerText(), /at most 600 characters; this one has 601/);
  await panel.getByRole('button', {name: 'Cancel'}).click();
  note('over-long text refused before sending');

  // REPL -> Studio: the harness edits the Studio goal's text from the REPL.
  const first = panel.locator('li.agent-goal').first();
  await page.waitForFunction(() => /edited from the REPL/.test(document.querySelector('li.agent-goal')?.innerText || ''),
                             null, {timeout: 20000});
  note('REPL edit reached the open panel');

  // Owner refusal: open the editor, change the goal from another client, save.
  await first.getByRole('button', {name: /^Edit Goal 1/}).click();
  show = await state(page);
  const current = goalsOf(show)[0];
  const other = await page.request.post(origin + '/api/agent-goal', {
    headers: {'content-type': 'application/json', accept: 'application/json'},
    data: {op: 'edit', id: current.id, 'expected-revision': current.revision,
           'runtime-id': show['runtime-id'], 'expected-epoch': show.epoch,
           changes: {text: 'Changed by another Studio window.'}, source: {adapter: 'web', 'session-id': 'other-window'}}});
  assert.ok(other.ok(), await other.text());
  await panel.getByRole('form', {name: 'Edit goal'}).getByLabel('Goal text').fill('My stale edit');
  await panel.getByRole('button', {name: 'Save goal'}).click();
  await panel.getByRole('alert').waitFor();
  assert.match(await panel.getByRole('alert').innerText(), /changed since it was shown/);
  show = await state(page);
  assert.equal(goalsOf(show)[0].text, 'Changed by another Studio window.');
  note('stale Studio edit refused:', await panel.getByRole('alert').innerText());
  await panel.getByRole('button', {name: 'Cancel'}).click();
  await page.waitForFunction(() => /another Studio window/.test(document.querySelector('li.agent-goal')?.innerText || ''));

  // Layout: every goal card is the same wide slot.
  const boxes = await panel.locator('li.agent-goal').evaluateAll(cards => cards.map(c => {
    const r = c.getBoundingClientRect(); return [Math.round(r.width), Math.round(r.height)];
  }));
  assert.ok(boxes.every(([w, h]) => w === boxes[0][0] && h === boxes[0][1]), JSON.stringify(boxes));
  note('desktop card sizes', JSON.stringify(boxes));
  await panel.screenshot({path: join(directory, 'goals-panel-desktop.png')});

  await page.setViewportSize({width: 390, height: 900});
  await panel.scrollIntoViewIfNeeded();
  const overflow = await page.evaluate(() => document.documentElement.scrollWidth - innerWidth);
  assert.ok(overflow <= 0, 'phone horizontal overflow ' + overflow);
  await panel.screenshot({path: join(directory, 'goals-panel-phone.png')});
  note('phone width: no horizontal overflow');

  assert.deepEqual(errors, [], 'console errors');
  note('console errors: 0');
  await writeFile(join(directory, 'agent-goals-qa.txt'), log.join('\n') + '\n');
} finally {
  await context.close();
}
