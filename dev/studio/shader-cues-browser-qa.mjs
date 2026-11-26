// Exercise Studio's shader cues (#673) and the borderless projector page
// against the preview-only show from tolglow.qa-shader-studio, which must run
// in a +native-gpu JVM. Usage:
//   node dev/studio/shader-cues-browser-qa.mjs http://127.0.0.1:18640 target/shader-cues-qa
// Headless Chrome; not foreground-display, projector-hardware or iPad evidence.
import assert from 'node:assert/strict';
import { mkdirSync, writeFileSync } from 'node:fs';
import { chromium } from 'playwright-core';

const origin = process.argv[2] || 'http://127.0.0.1:18640';
const out = process.argv[3] || 'target/shader-cues-qa';
mkdirSync(out, { recursive: true });

async function json(path, body) {
  const response = await fetch(origin + path, {
    method: body ? 'POST' : 'GET',
    headers: { Accept: 'application/json', ...(body ? { 'Content-Type': 'application/json' } : {}) },
    body: body ? JSON.stringify(body) : undefined,
  });
  assert.ok(response.ok, `${path} ${response.status}`);
  return response.json();
}
const shaders = () => json('/api/video/shaders');
const sleep = ms => new Promise(r => setTimeout(r, ms));
async function until(fn, label, timeout = 8000) {
  const deadline = Date.now() + timeout;
  for (;;) {
    const value = await fn();
    if (value) return value;
    if (Date.now() > deadline) throw new Error(`Timed out: ${label}`);
    await sleep(50);
  }
}

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
  await page.goto(origin + '/rig');
  await page.addStyleTag({ content: '.topbar{position:static !important}' });
  const panel = page.getByRole('region', { name: 'Shader cues' });
  await panel.waitFor();
  await panel.scrollIntoViewIfNeeded();

  // Three shader cues; the two running ones show live frames.
  await page.waitForFunction(() => document.querySelectorAll('#shader-cues .shader-card').length === 3);
  const cards = panel.locator('.shader-card');
  report.cards = await cards.locator('.shader-card-top strong').allTextContents();
  await page.waitForFunction(() => [...document.querySelectorAll('#shader-cues img.shader-frame')]
    .filter(img => img.complete && img.naturalWidth > 0).length === 2);
  report.renderer = (await panel.locator('.panel-top .muted').textContent()).trim();
  assert.match(report.renderer, /GPU · Apple/);
  const plasma = cards.filter({ hasText: 'Shader plasma' }).filter({ hasNotText: 'strip' });
  report.plasmaStats = (await plasma.locator('p.small.muted').first().textContent()).trim();
  assert.match(report.plasmaStats, /640×360 at 30 fps/);
  await panel.screenshot({ path: `${out}/shader-cues-panel.png` });

  // Launch the 3D scene from its card.
  const scene = cards.filter({ hasText: 'Shader 3D scene' });
  await scene.getByRole('button', { name: 'Launch' }).click();
  await until(async () => (await shaders()).instances.some(i => i.feed === 'shader/shader-scene' && i.rendered > 5), 'scene renders');
  await page.waitForFunction(() => [...document.querySelectorAll('#shader-cues img.shader-frame')]
    .filter(img => img.complete && img.naturalWidth > 0).length === 3);
  await scene.getByRole('button', { name: 'Stop' }).waitFor();
  report.sceneStats = (await scene.locator('p.small.muted').first().textContent()).trim();
  assert.match(report.sceneStats, /25 fps/);

  // Enlarge it.
  await scene.getByRole('button', { name: 'Enlarge', exact: true }).click();
  const enlarged = page.getByRole('dialog', { name: /Enlarged/ });
  await enlarged.waitFor();
  await page.waitForFunction(() => {
    const img = document.querySelector('.shader-enlarged img.shader-frame-large');
    return img && img.complete && img.naturalWidth >= 640;
  });
  await page.screenshot({ path: `${out}/shader-cues-enlarged.png` });
  await enlarged.getByRole('button', { name: 'Close' }).click();
  await enlarged.waitFor({ state: 'detached' });

  // Edit: a broken version auditions with the driver log and is not saved.
  await scene.getByRole('button', { name: 'Edit GLSL' }).click();
  const source = scene.getByRole('textbox', { name: /GLSL for/ });
  const original = await source.inputValue();
  await source.fill(original.replace('fragColor = vec4(col * u_level, 1.0);', 'fragColor = vec4(oops * u_level, 1.0);'));
  await scene.getByRole('button', { name: 'Audition compile' }).click();
  const log = scene.locator('.shader-log');
  await log.waitFor();
  report.brokenLog = (await log.textContent()).trim();
  assert.match(report.brokenLog, /oops/);
  await scene.screenshot({ path: `${out}/shader-cues-audition-error.png` });

  // A working edit auditions, saves as version 2 and reaches the running feed.
  await source.fill(original.replace('fragColor = vec4(col * u_level, 1.0);', 'fragColor = vec4(col.bgr * u_level, 1.0);'));
  await scene.getByRole('button', { name: 'Audition compile' }).click();
  await scene.locator('.shader-ok').waitFor();
  const saved = page.waitForResponse(r => r.request().method() === 'POST' && new URL(r.url()).pathname === '/api/video/shader/save');
  await scene.getByRole('button', { name: 'Save version' }).click();
  assert.equal((await saved).status(), 200);
  await page.waitForFunction(() => [...document.querySelectorAll('#shader-cues .shader-card')]
    .some(c => c.textContent.includes('shader/shader-scene · v2')));
  await until(async () => (await shaders()).instances.some(i => i.feed === 'shader/shader-scene' && i.compiles >= 2 && !i.error),
              'running scene compiles version 2');
  report.savedVersion = 2;

  // Save a version that does not compile: the last working version keeps rendering.
  const v2 = await source.inputValue();
  assert.ok(v2.includes('col.bgr'), 'the editor still holds the saved text');
  await source.fill(v2.replace('col.bgr', 'oops'));
  await scene.getByRole('button', { name: 'Save version' }).click();
  const kept = await until(async () => (await shaders()).instances.find(i => i.feed === 'shader/shader-scene' && i['last-working?']),
                           'last working version kept');
  const before = kept.rendered;
  await sleep(500);
  const after = (await shaders()).instances.find(i => i.feed === 'shader/shader-scene');
  report.lastWorking = { error: kept.error?.code, renderedDuring500ms: after.rendered - before };
  assert.ok(after.rendered - before >= 8, JSON.stringify(report.lastWorking));
  await page.waitForFunction(() => document.querySelector('#shader-cues .shader-card .shader-log')?.textContent.includes('last working'));
  await scene.screenshot({ path: `${out}/shader-cues-last-working.png` });

  // Stop the scene; its cadence and feed go away.
  await scene.getByRole('button', { name: 'Stop' }).click();
  await until(async () => !(await shaders()).instances.some(i => i.feed === 'shader/shader-scene'), 'scene stopped');
  await scene.getByRole('button', { name: 'Launch' }).waitFor();
  report.problems = problems;
  assert.deepEqual(problems, []);

  // The projector page: only the feed, black around it, at the source rate.
  const projector = await browser.newPage({ viewport: { width: 1280, height: 720 } });
  const projectorProblems = [];
  projector.on('pageerror', e => projectorProblems.push(e.message));
  await projector.goto(`${origin}/studio/video-output.html?source=${encodeURIComponent(':shader/shader-plasma')}`);
  await projector.waitForFunction(() => window.tolglowVideoOutput?.frames > 3);
  const t0 = await projector.evaluate(() => ({ frames: window.tolglowVideoOutput.frames, at: performance.now() }));
  await sleep(4000);
  const t1 = await projector.evaluate(() => ({ ...window.tolglowVideoOutput, at: performance.now() }));
  const layout = await projector.evaluate(() => ({
    elements: [...document.body.querySelectorAll('*')].filter(e => e.tagName !== 'SCRIPT').map(e => e.tagName),
    background: getComputedStyle(document.body).backgroundColor,
    cursor: getComputedStyle(document.body).cursor,
    canvas: (() => { const r = document.querySelector('canvas').getBoundingClientRect(); return [r.width, r.height]; })(),
  }));
  const sortedDraw = [...t1.drawMs].sort((a, b) => a - b);
  report.projector = {
    fps: +((t1.frames - t0.frames) / ((t1.at - t0.at) / 1000)).toFixed(2),
    size: [t1.width, t1.height], empty: t1.empty, errors: t1.errors,
    drawMsP50: +sortedDraw[Math.floor(sortedDraw.length / 2)].toFixed(2),
    layout,
  };
  assert.deepEqual(layout.elements, ['CANVAS']);
  assert.equal(layout.background, 'rgb(0, 0, 0)');
  assert.equal(layout.cursor, 'none');
  assert.deepEqual([t1.width, t1.height], [640, 360]);
  assert.ok(report.projector.fps > 24, JSON.stringify(report.projector));
  await projector.screenshot({ path: `${out}/projector-output.png` });

  // Pixel-for-pixel: freeze the transport, then the 200 × 1 feed on the page
  // equals the latched frame byte for byte.
  await projector.goto(`${origin}/studio/video-output.html?source=${encodeURIComponent(':shader/shader-plasma-strip')}`);
  await projector.waitForFunction(() => window.tolglowVideoOutput?.frames > 2);
  await page.evaluate(async () => {
    // The same command Studio's transport button sends.
    const r = await fetch('/api/command', { method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ op: 'transport/set', transport: { 'playing?': false }, 'command-id': crypto.randomUUID(),
                             source: { adapter: 'web', 'session-id': 'qa' } }) });
    return r.status;
  });
  await sleep(600);
  const raw = new Uint8Array(await (await fetch(`${origin}/api/video/next?source=${encodeURIComponent(':shader/shader-plasma-strip')}&after=-1`)).arrayBuffer());
  await sleep(300);
  const shown = await projector.evaluate(() => {
    const c = document.querySelector('canvas');
    return { size: [c.width, c.height], rgba: [...c.getContext('2d').getImageData(0, 0, c.width, c.height).data] };
  });
  let mismatches = 0;
  for (let i = 0; i < 200; i++) {
    if (shown.rgba[4 * i] !== raw[4 * i + 2] || shown.rgba[4 * i + 1] !== raw[4 * i + 1] || shown.rgba[4 * i + 2] !== raw[4 * i]) mismatches++;
  }
  report.stripPage = { size: shown.size, mismatches, firstPixels: shown.rgba.slice(0, 12) };
  assert.deepEqual(shown.size, [200, 1]);
  assert.equal(mismatches, 0);
  await fetch(origin + '/api/command', { method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ op: 'transport/set', transport: { 'playing?': true }, 'command-id': crypto.randomUUID(),
                           source: { adapter: 'web', 'session-id': 'qa' } }) });

  // A projector follows blackout by default; ?master=ignore keeps the raw feed.
  const command = body => fetch(origin + '/api/command', { method: 'POST', headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ ...body, 'command-id': crypto.randomUUID(), source: { adapter: 'web', 'session-id': 'qa' } }) });
  const rawPage = await browser.newPage({ viewport: { width: 640, height: 360 } });
  await rawPage.goto(`${origin}/studio/video-output.html?source=${encodeURIComponent(':shader/shader-plasma')}&master=ignore`);
  await projector.goto(`${origin}/studio/video-output.html?source=${encodeURIComponent(':shader/shader-plasma')}`);
  await projector.waitForFunction(() => window.tolglowVideoOutput?.frames > 2);
  await rawPage.waitForFunction(() => window.tolglowVideoOutput?.frames > 2);
  const lit = async p => p.evaluate(() => {
    const c = document.querySelector('canvas'); const d = c.getContext('2d').getImageData(0, 0, c.width, c.height).data;
    let max = 0; for (let i = 0; i < d.length; i += 4) max = Math.max(max, d[i], d[i + 1], d[i + 2]); return max;
  });
  await command({ op: 'blackout/set', value: true });
  await projector.waitForFunction(() => window.tolglowVideoOutput.gain === '0.0');
  await sleep(200);
  report.blackout = { followingMax: await lit(projector), rawMax: await lit(rawPage) };
  assert.equal(report.blackout.followingMax, 0);
  assert.ok(report.blackout.rawMax > 100, JSON.stringify(report.blackout));
  await projector.screenshot({ path: `${out}/projector-blackout.png` });
  await command({ op: 'blackout/set', value: false });
  await projector.waitForFunction(() => window.tolglowVideoOutput.gain === '1.0');
  await sleep(200);
  report.blackout.restoredMax = await lit(projector);
  assert.ok(report.blackout.restoredMax > 100);
  report.projectorProblems = projectorProblems;
  assert.deepEqual(projectorProblems, []);
} finally {
  writeFileSync(`${out}/report.json`, JSON.stringify(report, null, 2));
  console.log(JSON.stringify(report, null, 2));
  await browser.close();
}
