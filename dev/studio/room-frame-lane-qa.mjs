// Room frame lane (#783): the Room engine receives whole show frames from the
// owned :room lane, pauses while the Room is inactive, resumes with the newest
// frame, and the rig editor's Room receives the same lane.
//
// Usage: node dev/studio/room-frame-lane-qa.mjs <studio-origin> <output-dir>
// Run against a capture-only host with a running cue so frames advance.
// Headless Chrome; set CHROMIUM_PATH. This is not foreground or iPad evidence.
import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, directory = 'target/room-frame-lane'] = process.argv.slice(2);
if (!origin) throw new Error('Pass the Studio origin.');
await mkdir(directory, {recursive: true});
const context = await chromium.launchPersistentContext(join(directory, 'chrome'), {
  executablePath: process.env.CHROMIUM_PATH || chromium.executablePath(), headless: true,
  args: ['--headless=new', '--use-angle=metal'], viewport: {width: 1440, height: 900}});
const errors = [];
try {
  const page = context.pages()[0] || await context.newPage();
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(origin, {waitUntil: 'domcontentloaded'});
  await page.waitForFunction(() => document.querySelector('#connection-label')?.textContent === 'Studio connected', null, {timeout: 60000});

  // Record what the engine actually receives, at the engine boundary only.
  const watch = async canvas => {
    await page.waitForFunction(id => document.querySelector(id)?.tolglowPreview?.ready, canvas, {timeout: 90000});
    await page.evaluate(id => {
      const engine = document.querySelector(id).tolglowPreview, log = window.roomUpdates = [];
      const update = engine.update;
      engine.update = function(frame) {
        const values = frame.output?.values || {};
        log.push({at: performance.now(), frame: frame['frame-number'], revision: frame.revision,
          fixtures: Object.keys(values).length,
          colored: Object.values(values).filter(row => Array.isArray(row?.['emitted-color'])).length,
          visible: this.getTelemetry().visible});
        return update.call(this, frame);
      };
    }, canvas);
  };
  const sample = async ms => {
    const start = await page.evaluate(() => window.roomUpdates.length);
    await page.waitForTimeout(ms);
    return page.evaluate(n => window.roomUpdates.slice(n), start);
  };
  const increasing = updates => updates.every((u, i) => i === 0 || u.frame > updates[i - 1].frame);

  // Saved UI preferences may reopen another view; the Room lives on Perform.
  await page.locator('a[data-view=perform]').click();
  await page.locator('#preview').scrollIntoViewIfNeeded();
  await watch('#preview');
  const live = await sample(3000);
  const fixtures = await page.evaluate(() => document.querySelector('#fixture-count').textContent);
  assert.ok(live.length >= 20, 'The visible Room receives show frames: ' + live.length);
  assert.ok(increasing(live), 'Frames arrive whole and in order');
  assert.ok(live.at(-1).fixtures > 0 && live.at(-1).colored === live.at(-1).fixtures, 'Each frame carries fixture values');
  await page.locator('#preview-panel').screenshot({path: join(directory, 'room-live.png')});

  // Scroll the Room out of view: the engine and its frame lane pause together.
  await page.evaluate(() => window.scrollTo(0, document.documentElement.scrollHeight));
  await page.waitForFunction(() => !document.querySelector('#preview').tolglowPreview.getTelemetry().visible, null, {timeout: 10000});
  await page.waitForTimeout(300);
  const paused = await sample(2000);
  const lastBefore = await page.evaluate(() => window.roomUpdates.at(-1).frame);
  assert.equal(paused.length, 0, 'An offscreen Room receives no frames');

  await page.locator('#preview').scrollIntoViewIfNeeded();
  await page.waitForFunction(() => document.querySelector('#preview').tolglowPreview.getTelemetry().visible, null, {timeout: 10000});
  const resumed = await sample(2000);
  assert.ok(resumed.length >= 10, 'The Room resumes: ' + resumed.length);
  assert.ok(resumed[0].frame > lastBefore + 10, 'It resumes with the newest whole frame, not the backlog: ' + resumed[0].frame + ' after ' + lastBefore);
  assert.ok(increasing(resumed));

  // The rig editor's Room is a second owner of the same lane.
  await page.locator('a[data-view=rig]').click();
  await watch('#rig-preview');
  await page.locator('#rig-preview').scrollIntoViewIfNeeded();
  const rig = await sample(3000);
  assert.ok(rig.length >= 20, 'The rig editor Room receives show frames: ' + rig.length);
  assert.ok(increasing(rig) && rig.at(-1).fixtures > 0);
  await page.locator('.rig-editor-scene').screenshot({path: join(directory, 'rig-room-live.png')});
  assert.deepEqual(errors, []);

  const report = {fixtures, live: {updates: live.length, perSecond: +(live.length / 3).toFixed(1), fixturesPerFrame: live.at(-1).fixtures},
    paused: {updates: paused.length, lastFrameBefore: lastBefore},
    resumed: {updates: resumed.length, firstFrame: resumed[0].frame, skipped: resumed[0].frame - lastBefore - 1},
    rig: {updates: rig.length, fixturesPerFrame: rig.at(-1).fixtures}, pageErrors: errors.length,
    renderer: await page.evaluate(() => document.querySelector('#rig-preview').tolglowPreview.getTelemetry().deviceRenderer)};
  await writeFile(join(directory, 'report.json'), JSON.stringify(report, null, 2));
  console.log(report);
} finally { await context.close(); }
