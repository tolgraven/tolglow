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
  await panel.locator('.scene-custom input[type=checkbox]').nth(1).tap();
  await page.waitForFunction(() => document.querySelectorAll('.scene-custom input:checked').length === 2);
  const target = panel.getByRole('slider', { name: 'Group light target' });
  await target.focus();
  await target.press('ArrowLeft');
  await target.press('ArrowRight');
  const requested = Number(await target.inputValue()) / 100;
  async function submit(label) {
    const response = page.waitForResponse(r => r.request().method() === 'POST' && new URL(r.url()).pathname === '/api/lighting-scene');
    await panel.getByRole('button', { name: label, exact: true }).tap();
    const reply = await response;
    assert.equal(reply.status(), 200, await reply.text());
    return (await snapshot())['lighting-scenes'].groups;
  }
  const [applied] = await submit('Apply target');
  assert.equal(applied.target, requested);
  assert.equal(applied['fixture-ids'].length, 2);
  await page.waitForFunction(() => {
    const text = document.querySelector('.scene-reading')?.textContent || '';
    return /Output \d+% · requested \d+%/.test(text);
  });
  assert.equal(await choices.inputValue(), 'group:studio.scene/browser-wash');
  await choices.selectOption('group:studio/all-lights');
  await panel.getByRole('button', { name: 'Apply target', disabled: true }).waitFor();
  await choices.selectOption('group:studio.scene/browser-wash');
  const [varied] = await submit('Something different');
  assert.ok(varied.generation > applied.generation);

  await panel.getByLabel('Brightness reference').selectOption('global');
  const [matched] = await submit('Apply target');
  assert.equal(matched['brightness-reference'], 'global');
  const comparison = panel.locator('.source-readings');
  await comparison.locator('summary').tap();
  await comparison.getByRole('columnheader', { name: 'Rig scale' }).waitFor();
  await comparison.getByRole('rowheader').first().waitFor();

  const controls = page.getByRole('region', { name: 'Lighting policies', exact: true });
  const budget = controls.getByRole('region', { name: 'Light output budget', exact: true });
  const response = controls.getByRole('region', { name: 'Output response envelope', exact: true });
  const activity = controls.getByRole('region', { name: 'Visual activity', exact: true });
  async function policyEdit(action) {
    const [reply] = await Promise.all([
      page.waitForResponse(r => r.request().method() === 'POST' && new URL(r.url()).pathname === '/api/settings'),
      action(),
    ]);
    assert.equal(reply.status(), 200, await reply.text());
    return snapshot();
  }
  const apply = (section, name) => policyEdit(() => section.getByRole('button', { name, exact: true }).tap());
  await policyEdit(() => budget.getByLabel('Enable budgets').tap());
  await budget.getByLabel('Maximum active sources').fill('2');
  await budget.getByLabel('When over budget').selectOption('priority');
  await budget.getByLabel('Standby behavior').selectOption('all');
  let saved = await apply(budget, 'Apply budget');
  assert.equal(saved.settings['lighting-budget'].global['max-active'], 2);
  assert.equal(saved.settings['lighting-budget'].global.strategy, 'priority');
  assert.equal(saved.settings['lighting-budget'].global['standby-mode'], 'all');

  const chooser = controls.getByLabel('Budget and envelope scope');
  const [location] = await chooser.selectOption({ label: applied['group-id'] });
  await page.waitForFunction(location => document.querySelector('[aria-label="Light output budget"]')?.dataset.location === location, location);
  const groupCount = budget.getByLabel('Maximum active sources');
  await groupCount.focus();
  await groupCount.press('ControlOrMeta+A');
  await groupCount.press('1');
  await budget.getByRole('button', { name: 'Apply budget', disabled: false }).waitFor();
  const standby = budget.getByRole('slider', { name: 'Standby brightness' });
  await standby.focus();
  await standby.press('Home');
  await standby.press('ArrowRight');
  saved = await apply(budget, 'Apply budget');
  const selected = saved.settings['lighting-budget'].groups[applied['group-id']];
  assert.equal(selected['max-active'], 1);
  assert.equal(selected['standby-level'], 0.01);
  assert.equal(saved.settings['lighting-budget'].global['max-active'], 2);
  await policyEdit(() => response.getByLabel('Enable envelopes').tap());
  await response.getByLabel('Attack duration').fill('1/2');
  await response.getByLabel('Attack unit').selectOption('beats');
  saved = await apply(response, 'Apply response');
  assert.equal(saved.settings['output-response'].groups[applied['group-id']].attack.beats, 0.5);
  await response.getByLabel('Release time source').selectOption('variable');
  await response.getByLabel('Release variable path').fill('[:audio/energy]');
  await response.getByLabel('Release fallback duration').fill('1/8');
  saved = await apply(response, 'Apply response');
  assert.deepEqual(saved.settings['output-response'].groups[applied['group-id']].release.beats,
    { variable: ['audio/energy'], default: 0.125 });
  await response.getByLabel('Release time source').selectOption('cue');
  const cue = response.getByLabel('Release control cue');
  await cue.selectOption({ index: 1 });
  const parameter = response.getByLabel('Release control parameter');
  await parameter.locator('option').nth(1).waitFor({state:'attached'});
  await parameter.selectOption({ index: 1 });
  saved = await apply(response, 'Apply response');
  const binding = saved.settings['output-response'].groups[applied['group-id']].release.beats;
  assert.ok(binding['cue-id'] && binding.param);
  assert.equal(binding.default, 0.125);
  await policyEdit(() => activity.getByLabel('Meter activity').tap());
  await page.waitForFunction(() => {
    const panel = document.querySelector('[aria-label="Visual activity"]');
    return /Visual activity\d+%/.test(panel?.textContent || '');
  });

  saved = await apply(budget, 'Remove group override');
  assert.ok(!saved.settings['lighting-budget'].groups?.[applied['group-id']]);
  assert.equal(saved.settings['lighting-budget'].global['max-active'], 2);
  await apply(response, 'Remove group override');
  await policyEdit(() => budget.getByLabel('Enable budgets').tap());
  await policyEdit(() => response.getByLabel('Enable envelopes').tap());
  await policyEdit(() => activity.getByLabel('Meter activity').tap());
  await page.setViewportSize({ width: 390, height: 843 });
  if (await live.isVisible()) await live.tap();
  for (const region of [panel, controls]) {
    assert.equal(await region.evaluate(node => node.scrollWidth <= node.clientWidth + 1), true);
  }
  assert.deepEqual(await submit('Release group'), []);
  const bound = await fetch(origin + '/api/lighting-scene', {
    method: 'POST', headers: { 'Content-Type': 'application/json', Accept: 'application/json' },
    body: JSON.stringify({ op: 'apply', 'group-id': 'qa-energy', 'fixture-ids': applied['fixture-ids'],
      target: 0.42, 'target-var': ['audio/energy'], 'expected-epoch': initial.epoch, 'runtime-id': initial['runtime-id'] }),
  });
  assert.ok(bound.ok, await bound.text());
  const boundOption = choices.locator('option').filter({ hasText: /^qa energy/ });
  await boundOption.waitFor({ state: 'attached' });
  await choices.selectOption(await boundOption.getAttribute('value'));
  await panel.getByRole('button', {name: 'Set manual target'}).waitFor();
  await target.focus();
  await target.press('ArrowRight');
  const [following] = await submit('Something different');
  assert.deepEqual(following['target-var'], ['audio/energy']);
  const [manual] = await submit('Set manual target');
  assert.equal(manual.target, 0.43);
  assert.ok(!manual['target-var']);
  assert.deepEqual(await submit('Release group'), []);
  const auto = page.locator('details').filter({ has: page.locator('#auto-start') });
  await auto.locator('summary').tap();
  await auto.getByLabel('Choose cues by visual activity').tap();
  const activityTarget = auto.getByRole('slider', { name: 'Target visual activity' });
  await activityTarget.focus();
  await activityTarget.press('ArrowLeft');
  const goal = Number(await activityTarget.inputValue()) / 100;
  async function autoCommand(name) {
    const [reply] = await Promise.all([
      page.waitForResponse(r => r.request().method() === 'POST' && new URL(r.url()).pathname === '/api/auto'),
      auto.getByRole('button', { name, exact: true }).tap(),
    ]);
    assert.equal(reply.status(), 200, await reply.text());
    return (await snapshot()).auto;
  }
  assert.equal((await autoCommand('Start auto')).config['activity-target'], goal);
  assert.equal((await autoCommand('Stop auto'))['enabled?'], false);
  await auto.getByLabel('Choose cues by visual activity').tap();
  assert.ok(!Object.hasOwn((await autoCommand('Start auto')).config, 'activity-target'));
  await autoCommand('Stop auto');
  assert.deepEqual(problems, []);
  console.log('Touch and keyboard verified common-reference brightness, rig and group budgets, fixed and bound response times, visual activity metering, optional activity-based cue selection and independent group override removal through Ring. The phone layout fit both performance panels.');
} finally {
  await browser.close();
}
