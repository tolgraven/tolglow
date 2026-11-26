// Exercise the optimized Studio's track-feel picker against a capture-only show.
// Usage: CHROMIUM_PATH=… node dev/studio/personality-picker-qa.mjs <origin> <output-dir>
import assert from 'node:assert/strict';
import { mkdirSync, writeFileSync } from 'node:fs';
import { createRequire } from 'node:module';
import { join, resolve } from 'node:path';
import { chromium } from 'playwright-core';

const { createWorker } = createRequire(import.meta.url)('../../resources/devices/push-display/worker.cjs');
const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';

const origin = process.argv[2] || 'http://127.0.0.1:4180';
const output = resolve(process.argv[3] || 'target/personality-picker');
mkdirSync(output, { recursive: true });

async function api(path, body) {
  const response = await fetch(origin + path, body
    ? { method: 'POST', headers: { 'Content-Type': 'application/json', Accept: 'application/json' }, body: JSON.stringify(body) }
    : { headers: { Accept: 'application/json' } });
  return { status: response.status, data: await response.json() };
}
const feel = async () => (await api('/api/state')).data.personalities;
// Evaluate in the application's own REPL session, where `runtime` is this show's handle.
async function repl(source) {
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

const initial = (await api('/api/state')).data;
assert.equal(initial.status.output.type, 'preview', 'the QA show must not own an output');
assert.equal(initial.personalities.active, null);
assert.ok(initial.personalities.library.length >= 6);

const context = await chromium.launchPersistentContext(join(output, 'chrome-profile'), {
  executablePath,
  headless: true,
  args: ['--headless=new', '--use-angle=metal'],
  viewport: { width: 1280, height: 900 },
  deviceScaleFactor: 2,
});
try {
  const page = context.pages()[0] || await context.newPage();
  const problems = [];
  page.on('pageerror', error => problems.push(error.message));
  page.on('console', message => { if (message.type() === 'error') problems.push(message.text()); });
  await page.goto(origin);
  const live = page.getByRole('button', { name: /^Live ·/ });
  if (await live.isVisible()) await live.click();
  const panel = page.getByRole('region', { name: 'Track feel', exact: true });
  await panel.waitFor();
  await panel.getByText('No track feel').waitFor();
  const choices = panel.getByRole('group', { name: 'Personality' });

  // Closed, the panel is one row; the summary opens it from the keyboard.
  const closed = await panel.boundingBox();
  assert.ok(closed.height <= 96, `the closed panel stays compact, got ${closed.height}px`);
  assert.equal(await choices.isVisible(), false);
  await panel.locator('summary').focus();
  await page.keyboard.press('Enter');
  await choices.waitFor();
  assert.equal(await choices.getByRole('button').count(), initial.personalities.library.length);

  // Every icon is the SVG the show projected, decoded by the browser as an image.
  const icons = await choices.locator('img.personality-icon').evaluateAll(async images => {
    await Promise.all(images.map(image => image.decode()));
    return images.map(image => ({ natural: image.naturalWidth, shown: image.getBoundingClientRect().width, src: image.src }));
  });
  assert.equal(icons.length, initial.personalities.library.length);
  for (const [index, icon] of icons.entries()) {
    assert.equal(icon.natural, 96, 'the projected SVG decodes at its intrinsic size');
    assert.equal(icon.shown, 40);
    assert.equal(decodeURIComponent(icon.src.split(',')[1]), initial.personalities.library[index].icon);
  }

  async function operate(action) {
    const [reply] = await Promise.all([
      page.waitForResponse(r => r.request().method() === 'POST' && new URL(r.url()).pathname === '/api/personality'),
      action(),
    ]);
    assert.equal(reply.status(), 200, await reply.text());
    return feel();
  }

  // Select from Studio; the committed projection and the header follow.
  const squircle = choices.getByRole('button', { name: 'Apple-hardware-level corner obsession' });
  let state = await operate(() => squircle.click());
  assert.equal(state.active.id, 'squircle');
  await page.waitForFunction(() => document.querySelector('.personality-current strong')?.textContent === 'Apple-hardware-level corner obsession');
  assert.equal(await squircle.getAttribute('aria-pressed'), 'true');
  assert.equal(await choices.locator('[aria-pressed="true"]').count(), 1);
  const shown = await panel.locator('.personality-current img.personality-icon').evaluate(async image => {
    await image.decode();
    return { src: image.src, width: image.getBoundingClientRect().width };
  });
  assert.equal(shown.width, 48);
  assert.equal(decodeURIComponent(shown.src.split(',')[1]), state.active.icon);
  assert.equal(await panel.locator('.personality-swatches i').count(), state.active.palette.length);
  await page.screenshot({ path: join(output, 'picker-desktop.png') });
  await panel.locator('summary').click();
  await choices.waitFor({ state: 'hidden' });
  await panel.screenshot({ path: join(output, 'picker-closed.png') });
  await panel.locator('summary').click();
  await choices.waitFor();

  // The running REPL reads the selection Studio made.
  assert.equal(await repl('(get-in (tolglow.show/snapshot runtime) [:personalities :active :id])'), ':squircle');

  // A selection made at the REPL reaches the open Studio view. The console pretty-prints data (#717).
  const accepted = await repl('(select-keys @(tolglow.show/submit! runtime :personality {:op :select :id :zigzag-shaky :seed 5}) [:status :active])');
  assert.equal(accepted, '{:status :accepted :active {:mix [{:id :zigzag-shaky :version 1 :weight 1.0}] :seed 5}}');
  await page.waitForFunction(() => document.querySelector('.personality-current strong')?.textContent === 'Zig to the zag but shaky');
  await choices.getByRole('button', { name: 'Zig to the zag but shaky', pressed: true }).waitFor();
  await panel.getByText('seed 5', { exact: false }).waitFor();

  // Blend from Studio with the keyboard-reachable controls.
  await panel.getByRole('combobox', { name: 'Personality to blend in' }).selectOption({ label: 'Still water' });
  const share = panel.getByRole('slider', { name: 'Share of the blended personality' });
  await share.focus();
  await share.press('ArrowRight');
  const wanted = Number(await share.inputValue());
  assert.equal(wanted, 35);
  state = await operate(() => panel.getByRole('button', { name: 'Blend', exact: true }).click());
  assert.deepEqual(state.active.mix.map(part => [part.id, part.weight]), [['zigzag-shaky', 0.65], ['still-water', 0.35]]);
  assert.equal(state.active.seed, 5, 'a blend keeps the seed');
  await page.waitForFunction(() => /65% Zig to the zag but shaky \+ 35% Still water/.test(document.querySelector('.personality-current strong')?.textContent || ''));
  assert.equal(await choices.locator('[aria-pressed="true"]').count(), 0, 'a blend is not one library entry');

  // Reshuffle changes only the seed.
  const before = state.active;
  state = await operate(() => panel.getByRole('button', { name: 'Reshuffle shapes' }).click());
  assert.deepEqual(state.active.mix, before.mix);
  assert.notEqual(state.active.seed, before.seed);
  await panel.getByText(`seed ${state.active.seed}`, { exact: false }).waitFor();

  // A refusal reads the same at the REPL and over HTTP, and the committed feel stays.
  const refusedAtRepl = await repl('(select-keys @(tolglow.show/submit! runtime :personality {:op :select :id :no-such-feel}) [:status :message :details])');
  assert.equal(refusedAtRepl, '{:status :rejected :message "Unknown personality" :details {:id :no-such-feel}}');
  const refused = await api('/api/personality', { op: 'select', id: 'no-such-feel' });
  assert.equal(refused.status, 422);
  assert.deepEqual([refused.data.message, refused.data.details], ['Unknown personality', { id: 'no-such-feel' }]);
  assert.deepEqual((await feel()).active.mix, state.active.mix);

  // Studio shows the owner's refusal of its own request. The request is altered in transit to name a missing ID.
  await page.route('**/api/personality', route => {
    const sent = route.request().postData();
    assert.ok(sent.includes('~:squares'), sent);
    return route.continue({ postData: sent.replace('~:squares', '~:no-such-feel') });
  });
  const [rejection] = await Promise.all([
    page.waitForResponse(r => r.request().method() === 'POST' && new URL(r.url()).pathname === '/api/personality'),
    choices.getByRole('button', { name: 'Squares and rectangles' }).click(),
  ]);
  assert.equal(rejection.status(), 422);
  await panel.getByRole('alert').filter({ hasText: 'Unknown personality' }).waitFor();
  await page.unroute('**/api/personality');
  assert.deepEqual((await feel()).active.mix, state.active.mix);
  assert.equal(await choices.locator('[aria-pressed="true"]').count(), 0, 'the view still shows the committed blend');
  const expectedProblems = problems.splice(0).filter(text => !/422/.test(text));
  assert.deepEqual(expectedProblems, [], 'only the provoked 422 may reach the console');

  // Phone width: the choices stay reachable with 44 px targets and no horizontal overflow.
  await page.setViewportSize({ width: 390, height: 844 });
  const liveNarrow = page.getByRole('button', { name: /^Live ·/ });
  if (await liveNarrow.isVisible()) await liveNarrow.click();
  await panel.scrollIntoViewIfNeeded();
  const layout = await panel.evaluate(element => {
    const box = element.getBoundingClientRect();
    const targets = [...element.querySelectorAll('button, select, input')].map(control => control.getBoundingClientRect());
    return { left: box.left, right: box.right, viewport: document.documentElement.clientWidth,
             scroll: document.documentElement.scrollWidth,
             smallest: Math.min(...targets.map(target => target.height)),
             outside: targets.filter(target => target.left < box.left - 1 || target.right > box.right + 1).length };
  });
  assert.ok(layout.left >= 0 && layout.right <= layout.viewport + 1, JSON.stringify(layout));
  assert.ok(layout.scroll <= layout.viewport + 1, JSON.stringify(layout));
  assert.equal(layout.outside, 0);
  assert.ok(layout.smallest >= 44, `controls are at least 44 px tall, got ${layout.smallest}`);
  await panel.screenshot({ path: join(output, 'picker-phone.png') });

  // Clear from Studio.
  const blended = state.active;
  state = await operate(() => panel.getByRole('button', { name: 'Clear feel' }).click());
  assert.equal(state.active, null);
  await panel.getByText('No track feel').waitFor();
  assert.deepEqual(problems, []);
  console.log('Personality picker checks passed against', origin);

  // The device display page rasterizes the same SVG through the packaged 960 x 160 capture worker.
  let display;
  const worker = await createWorker({ url: origin + '/push-display', executablePath, startupTimeoutMs: 30000, captureTimeoutMs: 8000 },
    async options => {
      display = await chromium.launchPersistentContext(join(output, 'push-profile'), {
        executablePath: options.executablePath, headless: true, args: ['--headless=new', '--use-angle=metal', ...options.args],
        viewport: { width: 960, height: 160 }, deviceScaleFactor: 1, reducedMotion: 'reduce', colorScheme: 'dark',
      });
      return { close: () => display.close(), newContext: async () => display };
    });
  try {
    const view = { title: 'Tolglow', page: 'main', 'display-mode': 'parameters', bpm: 120, master: 1, status: 'ready' };
    const plain = await worker.capture(view);
    const device = display.pages().find(tab => tab.url().endsWith('/push-display'));
    const errors = [];
    device.on('pageerror', error => errors.push(error.message));
    assert.equal(await device.locator('.push-personality').count(), 0, 'no feel, no change to the display');
    const icon = 'data:image/svg+xml;base64,' + Buffer.from(blended.icon, 'utf8').toString('base64');
    const result = await worker.capture({ ...view, personality: { id: 'blend', name: blended.name, icon } });
    const drawn = await device.locator('.push-personality img').evaluate(async image => {
      await image.decode();
      const box = image.getBoundingClientRect();
      return { natural: image.naturalWidth, x: Math.round(box.x), y: Math.round(box.y), width: box.width, height: box.height, alt: image.alt };
    });
    assert.deepEqual([drawn.natural, drawn.width, drawn.height, drawn.alt], [96, 20, 20, blended.name]);
    const colours = await device.evaluate(async ({ png, box }) => {
      const image = new Image();
      image.src = 'data:image/png;base64,' + png;
      await image.decode();
      const canvas = new OffscreenCanvas(image.naturalWidth, image.naturalHeight);
      const pen = canvas.getContext('2d');
      pen.drawImage(image, 0, 0);
      const pixels = pen.getImageData(box.x, box.y, box.width, box.height).data;
      const seen = new Set();
      let vivid = 0;
      for (let i = 0; i < pixels.length; i += 4) {
        seen.add((pixels[i] << 16) | (pixels[i + 1] << 8) | pixels[i + 2]);
        if (Math.max(pixels[i], pixels[i + 1], pixels[i + 2]) > 160) vivid += 1;
      }
      return { size: [image.naturalWidth, image.naturalHeight], distinct: seen.size, vivid };
    }, { png: result.png, box: drawn });
    assert.deepEqual(colours.size, [960, 160]);
    assert.ok(colours.distinct > 12 && colours.vivid > 20, `the captured frame holds the drawn icon: ${JSON.stringify(colours)}`);
    assert.notEqual(result.png, plain.png);
    writeFileSync(join(output, 'push-display.png'), Buffer.from(result.png, 'base64'));
    assert.deepEqual(errors, []);
    console.log('The 960 x 160 display worker rasterized the active icon at', drawn.x, drawn.y);
  } finally {
    await worker.close();
  }
} finally {
  await context.close();
}
