// Exercise the optimized Studio video strip editor (#672) against the
// preview-only show from tolglow.qa-video-studio. Usage:
//   node dev/studio/video-strips-browser-qa.mjs http://127.0.0.1:4580 target/video-strips-qa
import assert from 'node:assert/strict';
import { mkdirSync } from 'node:fs';
import { chromium } from 'playwright-core';

const origin = process.argv[2] || 'http://127.0.0.1:4580';
const out = process.argv[3] || 'target/video-strips-qa';
mkdirSync(out, { recursive: true });

async function json(path) {
  const response = await fetch(origin + path, { headers: { Accept: 'application/json' } });
  assert.equal(response.status, 200, path);
  return response.json();
}
const settings = async () => (await json('/api/settings')).video;

const browser = await chromium.launchPersistentContext(`${out}/profile`, {
  executablePath: process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  headless: true,
  args: ['--headless=new', '--use-angle=metal'],
  viewport: { width: 1280, height: 1100 }, deviceScaleFactor: 2,
});
const report = {};
try {
  const page = await browser.newPage();
  const problems = [];
  page.on('pageerror', error => problems.push(error.message));
  if (process.env.QA_DEBUG) page.on('request', r => { if (r.method() === 'POST') console.error('POST', r.postData()?.slice(0, 240)); });
  await page.goto(origin + '/rig');
  // Keep the sticky top bar out of element screenshots.
  await page.addStyleTag({ content: '.topbar{position:static !important}' });
  const panel = page.getByRole('region', { name: 'Video strip mapping' });
  await panel.waitFor();
  await panel.scrollIntoViewIfNeeded();

  // Two sources, both live.
  const sources = panel.locator('.video-source');
  await page.waitForFunction(() => document.querySelectorAll('#video-strips .video-source').length === 2);
  const states = await sources.locator('.video-state').allTextContents();
  report.sourceStates = states;
  assert.ok(states.every(s => s.startsWith('live')), JSON.stringify(states));

  // Select the test grid; the latched frame and 128 sampled footprints draw.
  await sources.filter({ hasText: 'Test pattern' }).getByRole('button').click();
  await page.waitForFunction(() => {
    const img = document.querySelector('#video-strips img.video-frame');
    return img && img.complete && img.naturalWidth > 0;
  });
  await page.waitForFunction(() => document.querySelectorAll('#video-strips .video-strip polygon').length === 128);
  const status = await json('/api/video');
  const expected = status.mappings['video/test-grid'].strips[0].colors[0];
  const firstFill = await panel.locator('.video-strip').first().locator('polygon').first().getAttribute('fill');
  report.firstFootprint = { fill: firstFill, projected: expected };
  assert.equal(firstFill, expected);
  await panel.screenshot({ path: `${out}/video-strips-test-grid.png` });

  // Drag the end handle of tube-2 and confirm the committed points moved.
  const before = (await settings()).mappings['video/test-grid'].strips[1].points;
  // Select tube-2 with a click (no movement, so nothing is committed).
  const line = await panel.locator('.video-strip').nth(1).locator('.video-strip-line').boundingBox();
  await page.mouse.click(line.x + line.width / 2, line.y + line.height / 2);
  await page.waitForFunction(() => document.querySelectorAll('#video-strips .video-strip.selected .video-strip-handle').length === 2);
  const handle = panel.locator('.video-strip.selected .video-strip-handle').nth(1);
  const box = await handle.boundingBox();
  const posted = page.waitForResponse(r => r.request().method() === 'POST' && new URL(r.url()).pathname === '/api/video-mapping');
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  await page.mouse.down();
  await page.mouse.move(box.x + box.width / 2 + 40, box.y + box.height / 2 + 120, { steps: 8 });
  await page.mouse.up();
  assert.equal((await posted).status(), 200);
  await page.waitForFunction(async () => true);
  let after;
  for (let i = 0; i < 40; i++) {
    after = (await settings()).mappings['video/test-grid'].strips[1].points;
    if (JSON.stringify(after) !== JSON.stringify(before)) break;
    await new Promise(r => setTimeout(r, 50));
  }
  report.dragged = { before, after };
  assert.notDeepEqual(after, before);
  assert.deepEqual(after[0], before[0], 'the other endpoint stays put');
  assert.ok(after[1][1] > before[1][1], 'dragging down moves the end down');

  // Rotate, set sampling height and switch the filter from the editor.
  const editor = panel.locator('.video-strip-editor');
  await editor.getByRole('button', { name: '↻ 15°' }).click();
  const height = editor.getByRole('slider', { name: 'Sampling height', exact: true });
  await height.fill('0.2');
  for (let i = 0; i < 40; i++) {
    const strip = (await settings()).mappings['video/test-grid'].strips[1];
    if (Math.abs(strip.height - 0.2) < 1e-9) { report.edited = strip; break; }
    await new Promise(r => setTimeout(r, 50));
  }
  assert.ok(report.edited, 'height edit committed');
  await page.waitForTimeout(400);
  await panel.screenshot({ path: `${out}/video-strips-edited.png` });

  // Syphon: map the same tubes onto the second source.
  await sources.filter({ hasText: 'Syphon' }).getByRole('button').first().click().catch(() =>
    sources.nth(0).getByRole('button').click());
  const mapButton = panel.getByRole('button', { name: 'Map strips on this source' });
  if (await mapButton.waitFor({ timeout: 3000 }).then(() => true, () => false)) {
    await mapButton.click();
    const add = panel.getByRole('combobox', { name: 'Add a pixel layout as a strip' });
    await add.waitFor();
    await add.selectOption('tube-3');
    await page.waitForFunction(() => document.querySelectorAll('#video-strips .video-strip polygon').length === 32);
    await page.waitForFunction(() => {
      const img = document.querySelector('#video-strips img.video-frame');
      return img && img.complete && img.naturalWidth > 0;
    });
    await page.waitForTimeout(400);
    await panel.screenshot({ path: `${out}/video-strips-syphon.png` });
    report.syphonMapping = Object.keys((await settings()).mappings);
  }

  // The Room preview receives the video cue's output.
  await page.goto(origin + '/perform');
  await page.waitForTimeout(2500);
  await page.locator('#preview-panel').screenshot({ path: `${out}/room-video-cue.png` });
  report.problems = problems;
  assert.deepEqual(problems, []);
  console.log(JSON.stringify({ passed: true, ...report }, null, 1));
} finally {
  await browser.close();
}
