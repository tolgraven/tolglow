// Check the show folder note and the checkpoint from before the update (#742)
// in the optimized Studio, against three Mac-style folder shows started by
// dev/tolglow/qa/show_folder.clj: A and B share an older checkpoint, C has its own.
// Usage: CHROMIUM_PATH=… node dev/studio/show-folder-qa.mjs <output-dir> <origin-a> <origin-b> <origin-c>
import assert from 'node:assert/strict';
import { mkdirSync } from 'node:fs';
import { join, resolve } from 'node:path';
import { chromium } from 'playwright-core';

const executablePath = process.env.CHROMIUM_PATH || (process.platform === 'darwin'
  ? '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome' : '/usr/bin/chromium');
const [outputArg, originA, originB, originC] = process.argv.slice(2);
assert.ok(originA && originB && originC, 'pass an output directory and three origins');
const output = resolve(outputArg);
mkdirSync(output, { recursive: true });

async function api(origin, path, body) {
  const response = await fetch(origin + path, body
    ? { method: 'POST', headers: { 'Content-Type': 'application/json', Accept: 'application/json' }, body: JSON.stringify(body) }
    : { headers: { Accept: 'application/json' } });
  return { status: response.status, data: await response.json() };
}
const state = async origin => (await api(origin, '/api/state')).data;

// Evaluate in the show's own REPL session, where `runtime` is that show's handle.
async function repl(origin, source) {
  const response = await fetch(origin + '/api/repl/eval', {
    method: 'POST', headers: { 'Content-Type': 'application/json', Accept: 'application/json' },
    body: JSON.stringify({ source, 'request-id': crypto.randomUUID(), 'session-id': 'main' }),
  });
  const body = await response.text();
  assert.ok(response.ok, body);
  const ending = body.split('\n').filter(line => line.startsWith('data: ')).map(line => JSON.parse(line.slice(6))).at(-1);
  assert.equal(ending?.status, 'completed', body);
  return ending.result;
}
const offered = origin => repl(origin, '(contains? (:available (tolglow.show/recovery-status runtime)) :legacy)');

async function eventually(check, message, ms = 5000) {
  const deadline = Date.now() + ms;
  for (;;) {
    if (await check()) return;
    if (Date.now() > deadline) assert.fail(message);
    await new Promise(done => setTimeout(done, 50));
  }
}

for (const origin of [originA, originB, originC]) {
  const view = await state(origin);
  assert.equal(view.status.output.type, 'preview', 'the QA shows must not own an output');
  assert.equal(view['show-folder']['new?'], true);
  assert.ok(view['show-folder']['legacy-checkpoint'], 'every folder is offered the older checkpoint');
  assert.equal(view['blackout?'], false, 'and none applied it at startup');
}

const context = await chromium.launchPersistentContext(join(output, 'chrome-profile'), {
  executablePath,
  headless: true,
  args: ['--headless=new', '--use-angle=metal'],
  viewport: { width: 1280, height: 900 },
  deviceScaleFactor: 2,
});
const problems = [];
try {
  const page = context.pages()[0] || await context.newPage();
  page.on('pageerror', error => problems.push(error.message));
  page.on('console', message => { if (message.type() === 'error') problems.push(message.text()); });

  // A: the first-open note explains the identity file and offers the older checkpoint.
  await page.goto(originA);
  const notice = page.getByRole('region', { name: 'Show folder' });
  const identity = notice.locator('[data-notice="identity"]');
  const legacy = notice.locator('[data-notice="legacy"]');
  await identity.getByText('This show folder now has its own recovery').waitFor();
  assert.match(await identity.innerText(), /\.tolglow-folder-id/);
  assert.match(await identity.innerText(), /Copies are detected and safe/);
  await legacy.getByText('A checkpoint from before the update is waiting').waitFor();
  assert.match(await legacy.innerText(), /Nothing has been applied/);
  await page.screenshot({ path: join(output, 'a-first-open.png') });

  await page.setViewportSize({ width: 390, height: 844 });
  await notice.scrollIntoViewIfNeeded();
  const phoneWidth = await page.evaluate(() => document.documentElement.scrollWidth);
  assert.ok(phoneWidth <= 390, `the note fits a phone without horizontal scroll, got ${phoneWidth}px`);
  await page.screenshot({ path: join(output, 'a-first-open-phone.png') });
  await page.setViewportSize({ width: 1280, height: 900 });

  await identity.getByRole('button', { name: 'OK' }).click();
  await identity.waitFor({ state: 'detached' });
  assert.equal(await legacy.isVisible(), true, 'closing the identity note keeps the offer');

  // Studio restore in A; the REPL reads the same outcome in A and B.
  await legacy.getByRole('button', { name: 'Review checkpoint' }).click();
  const dialog = page.getByRole('dialog', { name: 'Show checkpoints' });
  await dialog.locator('[data-checkpoint="legacy"]').waitFor();
  await dialog.getByRole('heading', { name: 'Show folder' }).waitFor();
  assert.match(await dialog.innerText(), /\.tolglow-folder-id/);
  await page.screenshot({ path: join(output, 'a-checkpoints.png') });
  await dialog.getByRole('button', { name: 'Restore Checkpoint from before the update' }).click();
  await dialog.getByText('into this show folder').waitFor();
  await dialog.getByRole('button', { name: 'Restore checkpoint' }).click();
  await dialog.getByText('Restored Checkpoint from before the update.').waitFor({ timeout: 15000 });
  assert.equal(await dialog.locator('[data-checkpoint="legacy"]').count(), 0, 'the dialog no longer lists it');
  await page.screenshot({ path: join(output, 'a-restored.png') });
  await legacy.waitFor({ state: 'detached' });
  assert.equal(await repl(originA, '(:blackout? (tolglow.show/snapshot runtime))'), 'true');
  assert.equal(await offered(originA), 'false');
  assert.equal(await offered(originB), 'false', 'B is no longer offered it');
  assert.equal((await state(originB))['blackout?'], false, 'and B was not changed');
  await eventually(async () => !(await state(originB))['show-folder']['legacy-checkpoint'],
    "B's open state stops offering it by its next autosave");

  // C: a REPL dismissal removes the open Studio's offer; a second dismissal is refused.
  await page.goto(originC);
  const legacyC = page.getByRole('region', { name: 'Show folder' }).locator('[data-notice="legacy"]');
  await legacyC.waitFor();
  assert.equal(await offered(originC), 'true', "C's REPL sees its own offer");
  assert.equal(await repl(originC, '(tolglow.show/dismiss-legacy-recovery! runtime)'), 'true');
  await legacyC.waitFor({ state: 'detached', timeout: 5000 });
  assert.equal((await state(originC))['blackout?'], false, 'dismissing applies nothing');
  await page.locator('#restore-show').click();
  const dialogC = page.getByRole('dialog', { name: 'Show checkpoints' });
  await dialogC.getByRole('heading', { name: 'Show folder' }).waitFor();
  await dialogC.getByText('Working autosave').waitFor();
  assert.equal(await dialogC.locator('[data-checkpoint="legacy"]').count(), 0);
  const { 'runtime-id': runtimeId, 'show-id': showId, epoch } = await state(originC);
  const refused = await api(originC, '/api/show/checkpoints/dismiss-legacy', { 'runtime-id': runtimeId, 'show-id': showId, epoch });
  assert.equal(refused.data.status, 'rejected');
  assert.match(refused.data.message, /no longer offered/);
  assert.equal(await offered(originC), 'false');

  assert.deepEqual(problems, [], 'no page errors');
  console.log('show folder checks passed');
} finally {
  await context.close();
}
