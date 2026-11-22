// Exercise the optimized Studio against a dedicated capture-only show.
import assert from 'node:assert/strict';
import { chromium } from 'playwright-core';

const origin = process.argv[2] || 'http://127.0.0.1:8693';
async function snapshot() {
  const response = await fetch(origin + '/api/state', { headers: { Accept: 'application/json' } });
  assert.equal(response.status, 200);
  return response.json();
}
const initial = await snapshot();
assert.equal(initial.status.output.type, 'preview');
assert.equal(initial['lighting-scenes'].groups.length, 0);
assert.ok(initial['lighting-scenes']['eligible-fixture-ids'].length >= 2);

const browser = await chromium.launch({
  executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'],
});
try {
  const page = await browser.newPage({ viewport: { width: 820, height: 1000 }, hasTouch: true });
  const problems = [];
  page.on('pageerror', error => problems.push(error.message));
  await page.goto(origin);
  const live = page.getByRole('button', { name: /^Live ·/ });
  if (await live.isVisible()) await live.tap();
  const panel = page.getByRole('region', { name: 'Fixture group lighting' });
  const choices = panel.getByRole('combobox', { name: 'Lighting fixture group' });
  await choices.selectOption('custom');
  await panel.getByLabel('Group name').fill('Browser wash');
  const first = panel.getByRole('checkbox').first();
  await first.tap();
  await page.waitForFunction(() => document.querySelector('.scene-custom input[type=checkbox]')?.checked);
  const target = panel.getByRole('slider', { name: 'Group light target' });
  await target.focus();
  await target.press('ArrowLeft');
  await target.press('ArrowRight');
  assert.equal(await target.inputValue(), '72');
  async function submit(label) {
    const response = page.waitForResponse(r => r.request().method() === 'POST' && new URL(r.url()).pathname === '/api/lighting-scene');
    await panel.getByRole('button', { name: label, exact: true }).tap();
    const reply = await response;
    assert.equal(reply.status(), 200, await reply.text());
    return (await snapshot())['lighting-scenes'].groups;
  }
  const [applied] = await submit('Apply target');
  assert.equal(applied.target, 0.72);
  assert.equal(applied['fixture-ids'].length, 1);
  await page.waitForFunction(() => {
    const text = document.querySelector('.scene-reading')?.textContent || '';
    return /Output \d+% · requested 72%/.test(text);
  });
  assert.equal(await choices.inputValue(), 'group:studio.scene/browser-wash');
  await choices.selectOption('group:studio/all-lights');
  await panel.getByRole('button', { name: 'Apply target', disabled: true }).waitFor();
  await choices.selectOption('group:studio.scene/browser-wash');
  const [varied] = await submit('Something different');
  assert.ok(varied.generation > applied.generation);
  await page.setViewportSize({ width: 390, height: 843 });
  if (await live.isVisible()) await live.tap();
  assert.equal(await panel.evaluate(node => node.scrollWidth <= node.clientWidth + 1), true);
  assert.deepEqual(await submit('Release group'), []);
  assert.deepEqual(problems, []);
  console.log('Touch and keyboard set a 72% target, verified measured output, prevented an overlapping group, varied the scene and released it at phone width.');
} finally {
  await browser.close();
}
