// Exercise video on 3D surfaces (#674) in the optimized Studio against the
// preview-only show from tolglow.qa-video-surfaces. Usage:
//   node dev/studio/video-surfaces-browser-qa.mjs http://127.0.0.1:18660 target/video-surfaces-qa
// Headless Chrome with the real GPU (--use-angle=metal); not iPad Safari.
import assert from 'node:assert/strict';
import { mkdirSync, writeFileSync } from 'node:fs';
import { chromium } from 'playwright-core';

const origin = process.argv[2] || 'http://127.0.0.1:18660';
const out = process.argv[3] || 'target/video-surfaces-qa';
mkdirSync(out, { recursive: true });

async function json(path) {
  const response = await fetch(origin + path, { headers: { Accept: 'application/json' } });
  assert.equal(response.status, 200, path);
  return response.json();
}
const video = () => json('/api/video');
const sleep = ms => new Promise(r => setTimeout(r, ms));

const browser = await chromium.launchPersistentContext(`${out}/profile`, {
  executablePath: process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  headless: true,
  args: ['--headless=new', '--use-angle=metal', '--enable-gpu'],
  viewport: { width: 1400, height: 1000 }, deviceScaleFactor: 2,
});
const report = {};

// Place the Room camera for a screenshot (inspection only; the scene itself
// comes from the show projection).
async function aim(page, position, target) {
  await page.evaluate(([position, target]) => {
    const preview = [...document.querySelectorAll('canvas')].map(c => c.tolglowPreview).find(Boolean);
    preview.navigation?.stop?.();
    preview.camera.position.set(...position); preview.controls.target.set(...target);
    preview.controls.update(); preview.sendState?.();
  }, [position, target]);
  await sleep(900);
}

async function roomShot(page, name) {
  const canvas = page.locator('#preview-panel canvas, .rig-editor-scene canvas').first();
  await canvas.screenshot({ path: `${out}/${name}.png` });
}

try {
  const page = await browser.newPage();
  const problems = [];
  page.on('pageerror', error => problems.push(error.message));
  page.on('console', m => { if (m.type() === 'error') problems.push(m.text()); });

  // The projection: three running surfaces with their contacts.
  const status = await video();
  const running = status.surfaces.running;
  report.running = running.map(v => ({ surface: v.surface, count: v.count }));
  const count = id => running.find(v => v.surface === id)?.count;
  assert.equal(count('sheet'), 4 * 16, 'the four nearest rows touch the sheet');
  assert.equal(count('globe'), 24, 'the whole ring touches the globe');
  assert.ok(count('ball') > 0);

  // Performance Room: surfaces, textures and contact markers.
  await page.goto(origin + '/perform');
  await page.waitForFunction(() => [...document.querySelectorAll('canvas')].some(c => c.tolglowPreview));
  await sleep(2500);
  await aim(page, [0.6, 2.1, 3.2], [0.4, 2.0, -3.2]);
  await roomShot(page, 'room-sheet-threshold-and-overlap');
  await aim(page, [4.0, 2.5, 1.2], [4.6, 2.0, -2.0]);
  await roomShot(page, 'room-globe-seam');

  // Rig & space: the surface editor lists contacts and edits the shape.
  await page.goto(origin + '/rig');
  await page.addStyleTag({ content: '.topbar{position:static !important}' });
  const panel = page.getByRole('region', { name: 'Video surfaces' });
  await panel.waitFor();
  await panel.getByRole('button', { name: /^sheet · plane/ }).click();
  const editor = panel.locator('.video-surface-editor');
  await editor.getByText('64 heads in contact').waitFor();
  await panel.scrollIntoViewIfNeeded();
  await panel.screenshot({ path: `${out}/editor-sheet.png` });

  // Per-strip source height: set row-1 to a quarter of the surface height.
  const rowHeight = editor.locator('li', { hasText: 'row-1' }).getByRole('spinbutton');
  await rowHeight.fill('0.25'); await rowHeight.press('Enter');
  for (let i = 0; i < 40 && !report.stripHeight; i++) {
    const s = (await json('/api/settings')).video.surfaces.sheet;
    if (s.strips?.['row-1']?.height === 0.25) report.stripHeight = s.strips['row-1'];
    await sleep(50);
  }
  assert.ok(report.stripHeight, 'strip height committed');

  // Turn the sheet into a moving wave from the editor.
  await editor.getByRole('combobox', { name: 'Shape' }).selectOption('wave');
  for (let i = 0; i < 60 && !report.wave; i++) {
    const s = (await json('/api/settings')).video.surfaces.sheet;
    if (s.shape === 'wave') report.wave = s.wave;
    await sleep(50);
  }
  assert.ok(report.wave, 'shape edit committed');
  const contactsAt = async () => (await video()).surfaces.running.find(v => v.surface === 'sheet');
  const first = await contactsAt();
  await page.goto(origin + '/perform');
  await page.waitForFunction(() => [...document.querySelectorAll('canvas')].some(c => c.tolglowPreview));
  await sleep(2000);
  await aim(page, [-4.2, 2.6, 1.0], [0.2, 2.0, -3.2]);
  await roomShot(page, 'room-wave-a');
  await sleep(1500);
  await roomShot(page, 'room-wave-b');
  const second = await contactsAt();
  report.wavePhase = [first.resolved.wave.phase, second.resolved.wave.phase];
  assert.notEqual(first.resolved.wave.phase, second.resolved.wave.phase, 'the wave travels with the beat');
  assert.notDeepEqual(first.contacts.map(c => c.id), second.contacts.map(c => c.id), 'different heads qualify as it moves');

  report.problems = problems;
  writeFileSync(`${out}/report.json`, JSON.stringify(report, null, 1));
  assert.deepEqual(problems, []);
  console.log(JSON.stringify({ passed: true, ...report }, null, 1));
} finally {
  await browser.close();
}
