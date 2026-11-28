// Run through tolglow.qa.rehearsal-browser (#217, #670). Phase "plan" reads a
// REPL-saved auto plan in Plan the wandering, edits and saves it from Studio,
// and sees the show refuse an unknown cue. Phase "rehearsal" arms a club
// rehearsal (replacing the show), reads the moods and the seeded replay's
// review, starts it, stops it and reads the run. Phase "repl-armed" checks
// that an arm from the REPL reaches the open panel. docs/rehearsal.md.
import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, directory, phase] = process.argv.slice(2);
await mkdir(directory, {recursive: true});

async function eventually(check, what, attempts = 150) {
  for (let attempt = 0; attempt < attempts; attempt++) {
    const value = await check();
    if (value) return value;
    await new Promise(done => setTimeout(done, 100));
  }
  throw new Error(`timed out waiting for ${what}`);
}

async function openPanel(page, marker) {
  await page.goto(origin + '/perform');
  const tools = page.getByRole('button', {name: /^Tools/});
  if (await tools.count() && await tools.first().isVisible()) await tools.first().click();
  const panel = page.locator('details.auto-panel').filter({has: page.locator(marker)});
  await panel.waitFor({state: 'attached'});
  if (!(await panel.evaluate(el => el.open))) await panel.locator('summary').click();
  return panel;
}

async function planPhase(page) {
  const panel = await page.goto(origin + '/perform').then(async () => {
    const tools = page.getByRole('button', {name: /^Tools/});
    if (await tools.count() && await tools.first().isVisible()) await tools.first().click();
    const p = page.locator('details.auto-plan-panel');
    await p.waitFor();
    await p.locator('summary').click();
    return p;
  });
  // REPL -> Studio: the saved plan's two sections and palette are listed.
  await eventually(async () => (await panel.locator('#auto-plan-status').textContent()).startsWith('2 sections · 3 colours'), 'REPL plan');
  assert.equal(await panel.locator('li.auto-plan-section').count(), 2);
  assert.equal(await panel.locator('li[data-section-id="hush"]').count(), 1);
  // Studio -> REPL: add a section, set the plan's lit limit, save.
  await panel.locator('#auto-plan-add-section').click();
  await panel.locator('#auto-plan-section-2-id').fill('glow');
  await panel.getByLabel('Section 3 cues').fill('head-glow, sweep');
  await panel.getByLabel('Section 3 length', {exact: true}).fill('1');
  await panel.locator('#auto-plan-max-lit').fill('5');
  const saved = page.waitForResponse(r => r.request().method() === 'POST' && new URL(r.url()).pathname === '/api/auto/plan');
  await panel.locator('#auto-plan-save').click();
  const reply = await saved;
  assert.equal(reply.status(), 200, await reply.text());
  await eventually(async () => (await panel.locator('#auto-plan-saved').textContent().catch(() => '')).includes('Plan saved'), 'saved notice');
  await eventually(async () => (await panel.locator('#auto-plan-status').textContent()).startsWith('3 sections'), 'projection shows three sections');
  await panel.screenshot({path: join(directory, 'auto-plan-saved.png')});
  // A refusal: the show has no cue called ghost.
  await panel.getByLabel('Section 3 cues').fill('ghost');
  const refused = page.waitForResponse(r => r.request().method() === 'POST' && new URL(r.url()).pathname === '/api/auto/plan');
  await panel.locator('#auto-plan-save').click();
  const refusal = await refused;
  assert.equal(refusal.status(), 422);
  const message = await eventually(async () => panel.locator('#auto-plan-error').textContent().catch(() => ''), 'refusal shown');
  assert.match(message, /No saved cue :ghost/);
  await panel.screenshot({path: join(directory, 'auto-plan-refused.png')});
  return {message};
}

async function rehearsalPhase(page) {
  const panel = await openPanel(page, '#rehearsal-summary');
  await panel.locator('#rehearsal-passage').selectOption('club');
  await panel.locator('#rehearsal-seed').fill('670');
  await panel.locator('#rehearsal-outage').selectOption('silent');
  await panel.locator('#rehearsal-outage-from').fill('2');
  // Arming without the confirmation is refused by the show.
  const refused = page.waitForResponse(r => new URL(r.url()).pathname === '/api/rehearsal/arm');
  await panel.locator('#rehearsal-arm').click();
  assert.equal((await refused).status(), 422);
  assert.match(await eventually(() => panel.locator('#rehearsal-error').textContent().catch(() => ''), 'refusal'), /replaces the open show/);
  // A controlled checkbox: Reagent re-renders it on the next animation frame.
  await panel.locator('#rehearsal-replace').click();
  await eventually(() => panel.locator('#rehearsal-replace').isChecked(), 'replace confirmed');
  const armed = page.waitForResponse(r => new URL(r.url()).pathname === '/api/rehearsal/arm');
  await panel.locator('#rehearsal-arm').click();
  assert.equal((await armed).status(), 200);
  // Arming answers at once and goes on in the background (:arming, then :armed).
  await eventually(async () => (await panel.locator('#rehearsal-status').textContent()).startsWith('Armed · club · seed 670 · bar 0'), 'armed', 400);
  const moods = await panel.locator('ul.rehearsal-moods li').allTextContents();
  assert.equal(moods.length, 4);
  assert.match(moods[3], /quiet from bar 40 — spot, sweep/);
  await eventually(async () => (await panel.locator('#rehearsal-flags').count()) > 0, 'seeded replay review', 300);
  const flags = await panel.locator('#rehearsal-flags').textContent();
  assert.match(flags, /did not answer for return and the host gave up/);
  const outcomes = await panel.locator('table[aria-label="Agent changes"] tbody tr td:nth-child(3)').allTextContents();
  assert.deepEqual(outcomes, ['applied', 'applied', 'agent timed out', 'agent timed out']);
  assert.equal(await panel.locator('table[aria-label="Phrase by phrase"] tbody tr').count(), 12);
  await panel.screenshot({path: join(directory, 'rehearsal-armed-review.png')});
  await panel.locator('#rehearsal-start').click();
  await eventually(async () => /^Playing · .* bar ([2-9]|\d\d)/.test(await panel.locator('#rehearsal-status').textContent()), 'playing past bar 2');
  await panel.screenshot({path: join(directory, 'rehearsal-playing.png')});
  const stopped = page.waitForResponse(r => new URL(r.url()).pathname === '/api/rehearsal/stop');
  await panel.locator('#rehearsal-stop').click();
  assert.equal((await stopped).status(), 200);
  const status = await eventually(async () => {
    const text = await panel.locator('#rehearsal-status').textContent();
    return text.startsWith('Stopped') && text;
  }, 'stopped');
  assert.match(status, /\(transport stopped\)/);
  const replies = await panel.locator('table[aria-label="Agent replies in this run"] tbody tr').allTextContents();
  assert.ok(replies.length >= 1 && /configure_auto accepted/.test(replies[0]), replies.join(' | '));
  await panel.screenshot({path: join(directory, 'rehearsal-stopped.png')});
  return {status, moods, outcomes, replies};
}

async function replArmedPhase(page) {
  const panel = await openPanel(page, '#rehearsal-summary');
  const status = await eventually(async () => {
    const text = await panel.locator('#rehearsal-status').textContent();
    return text.startsWith('Armed · concert take b · seed 7') && text;
  }, 'REPL arm reaches the panel');
  // The show arming replaced comes back from its named checkpoint.
  const restore = panel.locator('#rehearsal-restore');
  assert.match(await restore.textContent(), /Restore gig-2023 from before the rehearsal/);
  const restored = page.waitForResponse(r => new URL(r.url()).pathname === '/api/rehearsal/restore');
  await restore.click();
  assert.equal((await restored).status(), 200);
  const after = await eventually(async () => {
    const text = await panel.locator('#rehearsal-status').textContent();
    return text.startsWith('Nothing armed') && text;
  }, 'restored');
  await panel.screenshot({path: join(directory, 'rehearsal-restored.png')});
  return {status, after};
}

const context = await chromium.launchPersistentContext(resolve('target/rehearsal-chrome'), {
  executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args: ['--headless=new', '--use-angle=metal', '--disable-dev-shm-usage'],
  viewport: {width: 1440, height: 1600},
});
try {
  const page = await context.newPage();
  page.setDefaultTimeout(15000);
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  const run = {plan: planPhase, rehearsal: rehearsalPhase, 'repl-armed': replArmedPhase}[phase];
  const report = await run(page).catch(async error => {
    await page.screenshot({path: join(directory, `rehearsal-${phase}-failure.png`), fullPage: true});
    throw error;
  });
  assert.deepEqual(errors, []);
  await writeFile(join(directory, `rehearsal-${phase}.json`), JSON.stringify(report, null, 2));
  console.log(`rehearsal ${phase} phase passed`, JSON.stringify(report));
} finally {
  await context.close();
}
