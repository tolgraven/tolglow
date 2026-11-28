// Run through tolglow.qa.push-cue-grid-browser (#545). Phase "browser" pages the
// virtual Push 3 grid's session box from Studio and reads the pad badges on the
// running cards. Phase "push" checks that a Push pad selection reached Studio,
// then turns on Follow selection and opens Detail on another launch.
import assert from 'node:assert/strict';
import {existsSync} from 'node:fs';
import {mkdir, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, directory, phase, selectedId, targetId] = process.argv.slice(2);
await mkdir(directory, {recursive: true});

async function openSurfaces(page) {
  await page.goto(origin + '/perform');
  const mixer = page.getByRole('region', {name: 'Effect group mixer'});
  await mixer.waitFor();
  await mixer.locator('details').evaluate(element => { element.open = true; });
  const surfaces = mixer.getByRole('region', {name: 'Control surfaces'});
  await surfaces.waitFor();
  return surfaces;
}

async function browserPhase(page) {
  const surfaces = await openSurfaces(page);
  await page.waitForFunction(() => document.querySelector('[data-surface="push-a"] .surface-box-range')?.textContent === 'Cues 1–64 of 70');
  const badges = await page.locator('article.live-effect-card .surface-strip-badge').allTextContents();
  assert.ok(badges.length >= 64, `pad badges on the first page: ${badges.length}`);
  // A Push 2 publishes the launch its encoders edit as the selection from the start.
  assert.ok(badges.every(text => /^push-a [1-8]·[1-8]$/.test(text) || text === 'Selected on push-a'), badges.slice(0, 4).join(', '));
  await surfaces.screenshot({path: join(directory, 'grid-session-box-page-1.png')});
  assert.equal(await surfaces.getByRole('button', {name: 'Previous page on push-a'}).isDisabled(), true);
  await surfaces.getByRole('button', {name: 'Next page on push-a'}).click();
  await page.waitForFunction(() => document.querySelector('[data-surface="push-a"] .surface-box-range')?.textContent === 'Cues 65–70 of 70');
  assert.equal(await surfaces.getByRole('button', {name: 'Next page on push-a'}).isDisabled(), true);
  const secondPage = await page.locator('article.live-effect-card .surface-strip-badge').allTextContents();
  await surfaces.screenshot({path: join(directory, 'grid-session-box-page-2.png')});
  await surfaces.getByRole('button', {name: 'First page on push-a'}).click();
  await page.waitForFunction(() => document.querySelector('[data-surface="push-a"] .surface-box-range')?.textContent === 'Cues 1–64 of 70');
  return {firstPageBadges: badges.length, secondPageBadges: secondPage, encoderMenu: await surfaces.getByRole('combobox', {name: 'Encoder mode on push-a'}).count()};
}

async function pushPhase(page) {
  const surfaces = await openSurfaces(page);
  const selectedCard = page.locator(`article.live-effect-card[data-live-instance="${selectedId}"]`);
  const badge = selectedCard.locator('[data-surface-selected="push-a"]');
  await badge.waitFor();
  const badgeText = await badge.textContent();
  assert.equal(badgeText, 'Selected on push-a');
  assert.equal(await page.locator('[data-surface-selected]').count(), 1);
  const boxText = await surfaces.locator('[data-surface="push-a"] .surface-box-layout').textContent();
  await selectedCard.screenshot({path: join(directory, 'card-selected-on-push.png')});
  // Studio -> Push: Follow selection, then Detail on another launch.
  const follow = surfaces.getByRole('checkbox');
  if (!(await follow.isChecked())) {
    // A controlled checkbox: it turns on when the committed setting comes back.
    await follow.click();
    await page.waitForFunction(() => document.querySelector('.surface-panel input[type="checkbox"]')?.checked === true);
  }
  const targetCard = page.locator(`article.live-effect-card[data-live-instance="${targetId}"]`);
  await targetCard.locator('button.live-effect-expand').click();
  await targetCard.locator('[data-surface-selected="push-a"]').waitFor();
  assert.equal(await selectedCard.locator('[data-surface-selected]').count(), 0);
  await surfaces.screenshot({path: join(directory, 'grid-session-box-followed.png')});
  return {badgeText, boxText, followedText: await surfaces.locator('[data-surface="push-a"] .surface-box-layout').textContent()};
}

const context = await chromium.launchPersistentContext(join(directory, 'chrome-profile-' + phase), {
  executablePath: process.env.CHROMIUM_PATH
    || ['/usr/bin/chromium', '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'].find(existsSync),
  args: ['--headless=new', '--disable-dev-shm-usage', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'],
  viewport: {width: 1280, height: 1400},
});
try {
  const page = await context.newPage();
  page.setDefaultTimeout(15000);
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  const report = await (phase === 'push' ? pushPhase(page) : browserPhase(page)).catch(async error => {
    await page.screenshot({path: join(directory, `push-grid-${phase}-failure.png`), fullPage: true});
    throw error;
  });
  assert.deepEqual(errors, []);
  await writeFile(join(directory, `push-grid-${phase}.json`), JSON.stringify(report, null, 2));
  console.log(`push cue grid ${phase} phase passed`, JSON.stringify(report));
} finally {
  await context.close();
}
