// Run against a dedicated preview show with a cue page and grouped fixtures.
import assert from 'node:assert/strict';
import { chromium } from 'playwright-core';

const origin = process.argv[2] || 'http://127.0.0.1:8773';
async function state() {
  const r = await fetch(origin + '/api/state', {headers: {Accept: 'application/json'}});
  assert.equal(r.status, 200);
  return r.json();
}
const before = await state();
const signalID = 'qa/phrase-' + Date.now();
assert.equal(before.status.output.type, 'preview');
assert.ok(before.pages?.length && before.cues?.length && before.rig.fixtures.some(f => f.groups?.length));
const browser = await chromium.launch({executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
try {
  const page = await browser.newPage({viewport: {width: 820, height: 1000}, hasTouch: true});
  const problems = [];
  page.on('pageerror', e => problems.push(e.message));
  await page.goto(origin);
  const live = page.getByRole('button', {name: /^Live ·/});
  if (await live.isVisible()) await live.tap();
  const pilot = page.getByRole('region', {name: 'Energy autopilot', exact: true});
  const hdr = page.getByRole('region', {name: 'HDR emission', exact: true});
  function box(root, name) { return root.getByRole('combobox', {name, exact: true}); }
  async function write(control) {
    const [response] = await Promise.all([page.waitForResponse(r => r.url().endsWith('/api/settings') && r.request().method() === 'POST'), control.click()]);
    assert.ok(response.ok(), 'settings endpoint rejected this edit: ' + await response.text());
    const snap = await state();
    assert.ok(!snap.status.error, JSON.stringify(snap.status));
    return snap;
  }
  async function reset(root, label) {
    const toggle = root.getByRole('checkbox', {name: label, exact: true});
    if (await toggle.isChecked()) await write(toggle);
  }
  await reset(pilot, 'Enable autopilot');
  await reset(hdr, 'Enable HDR emission');
  // Configure before enabling; the first committed frame must use the chosen scope.
  const scope = box(pilot, 'Affected fixtures');
  const pageScope = await scope.locator('option').filter({hasText: 'Cue page ·'}).first().textContent();
  const groupScope = await scope.locator('option').filter({hasText: 'Group ·'}).first().textContent();
  await scope.selectOption({label: pageScope});
  await box(pilot, 'Approach').selectOption({label: 'Next decision'});
  await box(pilot, 'Preferred changes').selectOption({label: 'Adjust existing parameters'});
  await pilot.getByRole('textbox', {name: 'Averaging window', exact: true}).fill('3/2');
  await pilot.getByRole('textbox', {name: 'Decision interval', exact: true}).fill('1/4');
  await pilot.locator('summary').filter({hasText: 'Target attack and release'}).click();
  await pilot.getByRole('textbox', {name: 'Target attack duration', exact: true}).fill('1/2');
  await box(pilot, 'Target release response').selectOption({label: 'Blend per frame'});
  await pilot.getByLabel('Target release fraction', {exact: true}).fill('1/8');
  await box(pilot, 'Brightness target source').selectOption({label: 'Audio energy'});
  await box(pilot, 'Visual activity target source').selectOption({label: 'Waveform'});
  await box(pilot, 'Visual activity target phase').selectOption({label: 'Since activation'});
  await pilot.getByRole('textbox', {name: 'Visual activity target period', exact: true}).fill('3/4');
  const wave = box(pilot, 'Visual activity target waveform');
  await wave.focus();
  await wave.selectOption({label: 'Triangle'});
  const enable = pilot.getByRole('checkbox', {name: 'Enable autopilot', exact: true});
  const started = (await write(enable)).settings.autopilot;
  assert.ok(started['enabled?'] && started.scope['page-id']);
  assert.deepEqual(started.window, {bars: 1.5});
  assert.deepEqual(started['target-response'], {attack: {beats: 0.5}, release: {blend: 0.125}});
  assert.deepEqual(started.interval, {beats: 0.25});
  assert.deepEqual(started.targets.brightness.variable, ['audio/energy']);
  assert.deepEqual(started.targets.activity.period, {beats: 0.75});
  assert.equal(started.targets.activity.phase, 'local');
  assert.equal(started.targets.activity.waveform, 'triangle');
  const meters = pilot.getByRole('region', {name: 'Measured energy'});
  await meters.getByRole('meter', {name: 'Visual activity rolling average'}).waitFor();
  assert.match(await meters.textContent(), /in band/);
  await scope.selectOption({label: groupScope});
  const group = (await write(pilot.getByRole('button', {name: 'Apply autopilot settings'}))).settings.autopilot;
  assert.ok(group.scope['group-id'] && !group.scope['page-id']);

  await pilot.locator('summary').filter({hasText: 'Shared signals'}).click();
  await pilot.getByLabel('New signal name').fill(':' + signalID);
  await pilot.getByRole('button', {name: 'Add shared waveform'}).click();
  await box(pilot, 'Brightness target source').selectOption({label: 'Shared signal'});
  await box(pilot, 'Brightness target shared signal').selectOption({label: signalID});
  const shared = (await write(pilot.getByRole('button', {name: 'Apply autopilot settings'}))).settings.autopilot;
  assert.equal(shared.targets.brightness.signal, signalID);
  assert.equal(shared.signals[signalID].waveform, 'sine');

  await write(enable);
  await hdr.locator('summary').filter({hasText: 'Gain and spatial spill'}).click();
  await box(hdr, 'Spill radius (m) source').selectOption({label: 'Rhythmic random'});
  await box(hdr, 'Spill radius (m) distribution').selectOption({label: 'Triangular'});
  await hdr.getByRole('textbox', {name: 'Spill radius (m) period', exact: true}).fill('1/2');
  await hdr.locator('summary').filter({hasText: 'Individual gains'}).click();
  const chooseGain = box(hdr, 'Gain override for');
  await chooseGain.selectOption({label: await chooseGain.locator('option').filter({hasText: 'Cue ·'}).first().textContent()});
  const gain = hdr.getByRole('slider', {name: 'Override gain', exact: true});
  await gain.focus(); await gain.press('End');
  const hs = (await write(hdr.getByRole('checkbox', {name: 'Enable HDR emission', exact: true}))).settings.hdr;
  assert.ok(hs['enabled?']);
  assert.equal(hs.radius.random.distribution, 'triangular');
  assert.deepEqual(hs.radius.period, {beats: 0.5});
  assert.ok(Object.values(hs['cue-gains']).includes(4));
  assert.ok(!(await state()).settings.autopilot['enabled?']);
  await box(hdr, 'Affected fixtures').selectOption({label: groupScope});
  assert.ok((await write(hdr.getByRole('button', {name: 'Apply HDR settings'}))).settings.hdr.scope['group-id']);
  await box(hdr, 'Affected fixtures').selectOption({label: 'Entire rig'});
  assert.equal((await write(hdr.getByRole('button', {name: 'Apply HDR settings'}))).settings.hdr.scope, 'global');

  const envelope = page.getByRole('region', {name: 'Output response envelope'});
  await box(envelope, 'Attack response').selectOption('linear');
  await box(envelope, 'Attack unit').selectOption('beats');
  await envelope.getByRole('textbox', {name: 'Attack duration', exact: true}).fill('1/2');
  await box(envelope, 'Release response').selectOption('blend');
  await envelope.getByRole('textbox', {name: 'Release fraction', exact: true}).fill('1/4');
  const shaped = (await write(envelope.getByRole('button', {name: 'Apply response'}))).settings['output-response'].global;
  assert.deepEqual(shaped.attack, {mode: 'linear', beats: 0.5});
  assert.deepEqual(shaped.release, {blend: 0.25});

  await page.setViewportSize({width: 390, height: 844});
  for (const root of [pilot, hdr]) {
    await root.scrollIntoViewIfNeeded();
    const bounds = await root.boundingBox();
    assert.ok(bounds.x >= 0 && bounds.x + bounds.width <= 391, 'Panel overflows the phone');
    assert.ok((await box(root, 'Affected fixtures').boundingBox()).height >= 44);
  }
  await reset(hdr, 'Enable HDR emission');
  assert.deepEqual(problems, []);
  console.log('Verified mixed energy control and standalone HDR through Ring on tablet and phone widths.');
} finally {
  await browser.close();
}
