// Run through tolglow.qa.surface-link-browser. Phase "browser" pages a linked
// X-Touch session box from Studio and checks the outline; phase "repl" checks
// that a REPL bank move reached the open page (#705, #706).
import assert from 'node:assert/strict';
import {existsSync} from 'node:fs';
import {mkdir, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, directory, phase] = process.argv.slice(2);
await mkdir(directory, {recursive: true});

async function openMixer(page) {
  await page.goto(origin + '/perform');
  const mixer = page.getByRole('region', {name: 'Effect group mixer'});
  await mixer.waitFor();
  await mixer.locator('details').evaluate(element => { element.open = true; });
  const surfaces = mixer.getByRole('region', {name: 'Control surfaces'});
  await surfaces.waitFor();
  return {mixer, surfaces};
}

const range = surfaces => surfaces.locator('[data-surface="desk"] .surface-box-range');

async function browserPhase(page) {
  const {mixer, surfaces} = await openMixer(page);
  await page.waitForFunction(() => document.querySelector('[data-surface="desk"] .surface-box-range')?.textContent === 'Strips 1–16 of 20');
  const outlinedBefore = await mixer.locator('.effect-group-mixer-channel[data-on-surface="true"]').count();
  const badges = await mixer.locator('.surface-strip-badge').allTextContents();
  const card = page.locator('article.live-effect-card[data-on-surface="true"]');
  await card.first().waitFor();
  const cardBadge = await card.first().locator('.surface-strip-badge').textContent();
  await surfaces.screenshot({path: join(directory, 'surface-box-first-page.png')});
  await mixer.screenshot({path: join(directory, 'group-mixer-outline.png')});
  await surfaces.getByRole('button', {name: 'Next bank on desk'}).click();
  await page.waitForFunction(() => document.querySelector('[data-surface="desk"] .surface-box-range')?.textContent === 'Strips 17–20 of 20');
  const outlinedAfter = await mixer.locator('.effect-group-mixer-channel[data-on-surface="true"]').count();
  assert.equal(await surfaces.getByRole('button', {name: 'Next bank on desk'}).isDisabled(), true);
  await surfaces.screenshot({path: join(directory, 'surface-box-last-page.png')});
  // Studio -> unit: the encoder mode menu commits :control-surface/layout (#706 slice 2).
  await surfaces.getByRole('combobox', {name: 'Encoder mode on desk'}).selectOption('plug-in');
  await page.waitForFunction(() => document.querySelector('[data-surface="desk"] .surface-box-layout select')?.value === 'plug-in'
    && !document.querySelector('[data-surface="desk"] .surface-box-layout select')?.disabled);
  await surfaces.screenshot({path: join(directory, 'surface-box-encoder-mode.png')});
  return {outlinedBefore, outlinedAfter, badges, cardBadge, range: await range(surfaces).textContent(),
          encoderMode: await surfaces.locator('[data-surface="desk"] .surface-box-layout select').inputValue()};
}

async function replPhase(page) {
  const {surfaces} = await openMixer(page);
  await page.waitForFunction(() => document.querySelector('[data-surface="desk"] .surface-box-range')?.textContent === 'Strips 1–16 of 20');
  // Unit or REPL -> Studio: the REPL chose USER (knobs) with a selected strip.
  await page.waitForFunction(() => document.querySelector('[data-surface="desk"] .surface-box-layout select')?.value === 'user');
  return {range: await range(surfaces).textContent(),
          encoderMode: await surfaces.locator('[data-surface="desk"] .surface-box-layout select').inputValue(),
          layoutTitle: await surfaces.locator('[data-surface="desk"] .surface-box-layout').getAttribute('title')};
}

const context = await chromium.launchPersistentContext(join(directory, 'chrome-profile-' + phase), {
  executablePath: process.env.CHROMIUM_PATH
    || ['/usr/bin/chromium', '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'].find(existsSync),
  args: ['--headless=new', '--disable-dev-shm-usage', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'],
  viewport: {width: 1280, height: 1400},
});
try {
  const page = await context.newPage();
  page.setDefaultTimeout(10000);
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  const report = await (phase === 'repl' ? replPhase(page) : browserPhase(page)).catch(async error => {
    await page.screenshot({path: join(directory, `surface-${phase}-failure.png`), fullPage: true});
    throw error;
  });
  assert.deepEqual(errors, []);
  await writeFile(join(directory, `surface-${phase}.json`), JSON.stringify(report, null, 2));
  console.log(`surface ${phase} phase passed`, JSON.stringify(report));
} finally {
  await context.close();
}
