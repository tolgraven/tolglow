// Capture the shared parameter widgets from a show started by tolglow.qa.parameter-widgets.
// Push frames come from the recorded real projection through the shipped capture worker.
// Studio rows come from the release build. PARAMETER_WIDGETS_BASELINE=1 skips the
// widget assertions so an earlier build can be captured for comparison.
import assert from 'node:assert/strict';
import {mkdir, readFile, writeFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const require = createRequire(import.meta.url);
const {createWorker} = require('../../resources/devices/push-display/worker.cjs');
const [origin, id, selectorId, directory] = process.argv.slice(2);
assert.ok(origin && id && selectorId && directory, 'Pass the show URL, both instance IDs and a capture directory');
const executablePath = process.env.CHROMIUM_PATH || process.env.CHROMIUM || '/usr/bin/chromium';
const baseline = process.env.PARAMETER_WIDGETS_BASELINE === '1';
const check = async assertion => { if (!baseline) await assertion(); };
const units = ['sixteenth', 'beat', 'bar', 'phrase'];
const periods = [
  {name: 'quarter-beat', period: '1/4', unit: 'beats', window: '1', minor: 'sixteenth', major: 'beat'},
  {name: 'one-beat', period: '1', unit: 'beats', window: '2', minor: 'sixteenth', major: 'beat'},
  {name: 'one-bar', period: '1', unit: 'bars', window: '4', minor: 'beat', major: 'bar'},
  {name: 'four-bars', period: '4', unit: 'bars', window: '16', minor: 'bar', major: 'phrase'},
  {name: 'one-phrase', period: '1', unit: 'phrases', window: '16', minor: 'bar', major: 'phrase'},
];

async function capturePush() {
  const recording = await readFile(join(directory, 'requests.ndjson'), 'utf8');
  const frames = recording.trim().split('\n').map(line => JSON.parse(line));
  let browser;
  const worker = await createWorker({url: origin + '/push-display', executablePath,
    startupTimeoutMs: 30000, captureTimeoutMs: 8000}, async options => (browser = await chromium.launch(options)));
  const page = browser.contexts()[0].pages()[0];
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  try {
    for (const {id: frame, view} of frames) {
      const label = frame.split(':')[1];
      const result = await worker.capture(view);
      await writeFile(join(directory, 'push-' + label + '.png'), Buffer.from(result.png, 'base64'));
      if (label === 'live-auto') await check(async () => {
        assert.equal(await page.locator('.parameter-space').count(), 1, 'only the position vector is spatial');
        // The real Push layout gives the colour one hue/saturation/brightness encoder.
        assert.equal(await page.locator('.parameter-color').count(), 1, 'the tint encoder shows the colour widget');
        assert.ok(await page.locator('.shape-glyph').count() >= 3, 'the shape enum draws registered glyphs');
      });
      if (label === 'live-touch-tint') await check(async () => {
        assert.equal(await page.locator('.push-compound-area .parameter-color').count(), 1);
        assert.equal(await page.locator('.push-compound-area .parameter-space').count(), 0);
      });
    }
    await check(async () => {
      assert.ok(await page.locator('.parameter-history .musical-grid line').count() > 0, 'history carries a musical grid');
    });
    assert.deepEqual(errors, []);
    console.log('Captured ' + frames.length + ' projected show views through the 960 × 160 Push worker.');
  } finally { await worker.close(); }
}

async function sourcePeriod(page, card, {period, unit}) {
  await card.getByRole('button', {name: 'Edit source for Level', exact: true}).click();
  const panel = card.getByRole('region', {name: 'Level source'});
  await panel.getByLabel('Source period', {exact: true}).fill(period);
  await panel.getByLabel('Source period unit').selectOption(unit);
  await panel.getByRole('button', {name: 'Update LFO', exact: true}).click();
  await page.waitForTimeout(700);
  await panel.getByRole('button', {name: 'Close source editor'}).click();
}

async function inspectStudio() {
  const browser = await chromium.launch({executablePath, args: ['--use-angle=metal']});
  try {
    const page = await browser.newPage({viewport: {width: 1440, height: 1100}, deviceScaleFactor: 2});
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.goto(origin + '/perform');
    const card = page.locator('article.live-effect-card[data-live-instance=' + JSON.stringify(id) + ']');
    await card.waitFor();
    const shot = async (locator, name) => {
      await locator.scrollIntoViewIfNeeded();
      await locator.screenshot({path: join(directory, 'studio-' + name + '.png')});
    };

    // Compact strip: the monitor follows the selected driven parameter.
    const monitor = card.locator('.strip-monitor');
    await monitor.waitFor();
    for (const [label, name] of [['Tint', 'monitor-tint'], ['Position', 'monitor-position'], ['Level', 'monitor-level']]) {
      await monitor.getByLabel('Monitored parameter').selectOption({label});
      await page.waitForTimeout(label === 'Level' ? 2600 : 500);
      await shot(monitor, name);
      if (label === 'Tint') await check(async () => {
        assert.equal(await monitor.locator('.parameter-space').count(), 0, 'a colour never opens the vector view');
        assert.equal(await monitor.locator('.parameter-color').count(), 1);
      });
      if (label === 'Position') await check(async () => {
        assert.equal(await monitor.locator('.parameter-space .space-axis-label').count(), 3);
        assert.match(await monitor.locator('.parameter-space .space-magnitude').innerText(), /[0-9]/);
      });
    }

    await card.getByRole('button', {name: /^Expand /}).click();
    const row = param => card.locator('[data-param=' + param + ']');
    await row('tint').locator('.parameter-visualizer').waitFor();
    await page.waitForTimeout(600);
    await shot(row('tint'), 'tint');
    await check(async () => {
      assert.equal(await row('tint').locator('.parameter-space').count(), 0);
      assert.equal(await row('tint').getByRole('button', {name: '3D'}).count(), 0);
      assert.equal(await row('tint').locator('.parameter-color').count(), 1);
    });
    await row('tint').getByRole('button', {name: 'History'}).click();
    await page.waitForFunction(() => document.querySelector('[data-param=tint] .parameter-history path')?.getAttribute('d')?.includes('L'));
    await page.waitForTimeout(2200);
    await shot(row('tint'), 'tint-history');
    await shot(row('position'), 'position');
    await check(async () => {
      const labels = await row('position').locator('.space-axis-label').allTextContents();
      assert.deepEqual(labels.sort(), ['X', 'Y', 'Z']);
    });

    // One LFO, five periods: the grid units follow the period and the meter.
    await row('level').getByRole('button', {name: 'History'}).click();
    for (const entry of periods) {
      await sourcePeriod(page, card, entry);
      await row('level').getByLabel('Parameter history beat window').selectOption(entry.window);
      await page.waitForTimeout(Number(entry.window) * 500 + 1200);
      await shot(row('level'), 'level-' + entry.name);
      await check(async () => {
        const grid = row('level').locator('.parameter-history .musical-grid');
        assert.equal(await grid.getAttribute('data-minor'), entry.minor, entry.name + ' minor unit');
        assert.equal(await grid.getAttribute('data-major'), entry.major, entry.name + ' major unit');
        // A boundary of a coarser unit (a bar that starts a phrase) takes that unit's tint.
        const coarse = units.slice(units.indexOf(entry.major)).map(unit => 'line.' + unit).join(', ');
        assert.ok(await grid.locator(coarse).count() >= 1, entry.name + ' draws its cycle unit');
      });
    }

    await shot(row('shape'), 'shape');
    await check(async () => {
      const picker = row('shape').getByRole('radiogroup', {name: 'Shape'});
      assert.equal(await picker.locator('.shape-glyph').count(), 3);
      await picker.getByRole('radio', {name: 'Sine'}).click();
      await row('shape').locator('label output').filter({hasText: 'sine'}).waitFor();
      assert.equal(await picker.getByRole('radio', {name: 'Sine'}).getAttribute('aria-checked'), 'true');
      await shot(row('shape'), 'shape-selected-sine');
      await picker.getByRole('radio', {name: 'Sine'}).press('ArrowRight');
      await row('shape').locator('label output').filter({hasText: 'saw'}).waitFor();
    });
    await shot(card, 'card');

    const selector = page.locator('article.live-effect-card[data-live-instance=' + JSON.stringify(selectorId) + ']');
    await selector.getByRole('button', {name: /^Expand /}).click();
    const wave = selector.locator('[data-param=wave]');
    await wave.getByLabel('Live waveform shape').locator('svg polyline').waitFor();
    await page.waitForTimeout(900);
    await shot(wave, 'waveform-picker');
    await check(async () => {
      const picker = wave.getByRole('radiogroup', {name: 'Waveform'});
      assert.equal(await picker.locator('.shape-glyph').count(), 4);
      await picker.getByRole('radio', {name: 'Saw'}).click();
      await page.waitForFunction(() => document.querySelector('[data-param=wave] [role=radio][aria-checked=true]')?.textContent.includes('Saw'));
      assert.ok(await wave.locator('.waveform-compact line.waveform-tick').count() > 0, 'the live shape keeps its musical ticks');
      await page.waitForTimeout(900);
      await shot(wave, 'waveform-picker-saw');
    });
    assert.deepEqual(errors, []);
    console.log(baseline ? 'Captured the baseline Studio rows.' : 'Checked colour, grid, vector and shape widgets in Studio.');
  } finally { await browser.close(); }
}

await mkdir(directory, {recursive: true});
await capturePush();
await inspectStudio();
