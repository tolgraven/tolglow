// Author and revise a gradient sample in the block builder (#775).
// Run by tolglow.qa.block-builder-gradient, which starts the preview-only show and
// checks the published definition and the rendered colours between the phases.
//   node dev/studio/block-builder-gradient-qa.mjs <origin> author|edit <directory>
import assert from 'node:assert/strict';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, phase, directory] = process.argv.slice(2);
assert.ok(origin && ['author', 'edit'].includes(phase) && directory, 'Pass the show URL, author or edit, and a capture directory');
const show = await (await fetch(origin + '/api/state', {headers: {accept: 'application/json'}})).json();
assert.equal(show.status.output.type, 'preview');

const executablePath = process.env.CHROMIUM_PATH || process.env.CHROMIUM || '/usr/bin/chromium';
const context = await chromium.launchPersistentContext(join(directory, 'chrome-profile'), {
  executablePath, headless: true, args: ['--headless=new', '--use-angle=metal'],
  viewport: {width: 1280, height: 1000}, deviceScaleFactor: 2});

// The gradient is the first parameter, which the builder used to offer to every input.
// The private varying hue is a colour (its base reads tint), so it is never a position.
const authored = `; Tide gel: a gel whose transmission follows a gradient along x
{:id :tide-gel :version 1 :name "Tide gel"
 :params {:palette {:type :gradient
                    :default [{:position 0.0 :color [1.0 0.0 0.0]}
                              {:position 0.5 :color [0.0 1.0 0.0]}
                              {:position 1.0 :color [0.0 0.0 1.0]}]}
          :level {:type :number :default 1.0 :bounds [0.0 1.0]}
          :tint {:type :color :default [1.0 0.6 0.2]}}
 :outputs {:color [1.0 1.0 1.0]}
 :blocks {[:varyings :hue] {:base {:op :param :key :tint} :stages []}}}
`;

const combo = (scope, name) => scope.getByRole('combobox', {name, exact: true});
async function optionLabels(select) {
  await select.locator('option').first().waitFor({state: 'attached'});
  return select.locator('option').allInnerTexts();
}
const chosen = select => select.evaluate(node => node.selectedOptions[0]?.textContent);
async function settles(select, label) {
  for (let i = 0; i < 40 && await chosen(select) !== label; i++) await new Promise(r => setTimeout(r, 50));
  assert.equal(await chosen(select), label);
}

async function publish(page) {
  const ack = page.waitForResponse(response => response.url().endsWith('/api/definition') && response.request().method() === 'POST');
  await page.getByRole('button', {name: 'Validate & publish'}).click();
  const response = await ack;
  assert.equal(response.status(), 200, await response.text());
}

async function openColorStage(page) {
  const panel = page.locator('.block-editor:visible');
  await panel.getByRole('button', {name: 'Assign blocks'}).click();
  return panel;
}

try {
  const page = context.pages()[0] || await context.newPage();
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(origin + '/library');
  const source = page.getByLabel('Declarative EDN definition');

  if (phase === 'author') {
    await source.fill(authored);
    const panel = await openColorStage(page);
    const bindings = await optionLabels(combo(panel, 'Block binding'));
    assert.ok(bindings.includes('params / level'), bindings.join(', '));
    assert.ok(!bindings.some(label => label.includes('palette')), 'a gradient is not a numeric binding: ' + bindings.join(', '));
    // A colour varying's base is a colour input: tint is its valid choice, level broadcasts.
    await combo(panel, 'Block binding').selectOption({label: 'varyings / hue'});
    assert.equal(await chosen(combo(panel, 'base')), 'parameter');
    assert.deepEqual(await optionLabels(combo(panel, 'base parameter')), ['level', 'tint']);
    assert.equal(await chosen(combo(panel, 'base parameter')), 'tint');
    await combo(panel, 'Block binding').selectOption({label: 'outputs / color'});
    await combo(panel, 'New block').selectOption({label: 'Color gel'});
    await panel.getByRole('button', {name: 'Add stage'}).click();
    const stage = panel.locator('.block-stage').filter({hasText: '1 · Color gel'});

    // A scalar input offers only the scalar parameter.
    const wet = combo(stage, 'Wet amount');
    assert.ok(!(await optionLabels(wet)).includes('Gradient sample'), 'a scalar input has no gradient mode');
    await wet.selectOption({label: 'parameter'});
    assert.deepEqual(await optionLabels(combo(stage, 'Wet amount parameter')), ['level']);
    await stage.locator('.block-input').first().getByRole('button', {name: 'Reset'}).click();
    await settles(wet, 'constant');

    // The colour input samples the gradient at the fixture's x position.
    await stage.locator('summary', {hasText: 'Inputs'}).click();
    const transmission = combo(stage, 'transmission');
    const modes = await optionLabels(transmission);
    assert.ok(modes.includes('Gradient sample'), modes.join(', '));
    assert.ok(modes.includes('Private varying'), 'a colour input reads the colour varying: ' + modes.join(', '));
    await transmission.selectOption({label: 'parameter'});
    assert.deepEqual(await optionLabels(combo(stage, 'transmission parameter')), ['level', 'tint'],
                     'a colour input reads a colour and, broadcast, a number');
    await transmission.selectOption({label: 'Gradient sample'});
    await stage.getByRole('group', {name: 'transmission gradient sample', exact: true}).waitFor();
    assert.deepEqual(await optionLabels(combo(stage, 'transmission gradient')), ['palette']);
    assert.equal(await stage.locator('.block-gradient-sample .parameter-color.gradient').getAttribute('aria-label'), 'Gradient with 3 stops');
    await combo(stage, 'transmission extend').selectOption({label: 'mirror'});
    const position = combo(stage, 'transmission position');
    const positionModes = await optionLabels(position);
    for (const mode of ['constant', 'Fixture position', 'Musical period', 'parameter'])
      assert.ok(positionModes.includes(mode), mode + ' in ' + positionModes.join(', '));
    assert.ok(!positionModes.includes('Private varying'), 'the colour varying is not a position: ' + positionModes.join(', '));
    await position.selectOption({label: 'Musical period'});
    for (const name of ['transmission position phase reference', 'transmission position period unit'])
      await combo(stage, name).waitFor();
    await stage.getByRole('textbox', {name: 'transmission position period', exact: true}).waitFor();
    await stage.getByRole('spinbutton', {name: 'transmission position phase (cycles)', exact: true}).waitFor();
    await position.selectOption({label: 'parameter'});
    assert.deepEqual(await optionLabels(combo(stage, 'transmission position parameter')), ['level']);
    await position.selectOption({label: 'Fixture position'});
    await combo(stage, 'transmission position axis').selectOption({label: 'X'});
    await stage.scrollIntoViewIfNeeded();
    await stage.screenshot({path: join(directory, 'block-builder-gradient-author.png')});

    await panel.getByRole('button', {name: 'Write stages to EDN'}).click();
    await page.waitForFunction(() => document.querySelector('#definition-source').value.includes(':op :gradient'));
    const written = await source.inputValue();
    assert.ok(written.startsWith('; Tide gel: a gel whose transmission'), 'the comment survives');
    assert.match(written, /\{:op :gradient,?\s+:key :palette,?\s+:args \[\{:op :position,?\s+:axis :x\}\],?\s+:extend :mirror\}/);
    assert.ok(!written.includes(':amount'), 'the reset wet amount is not written');
    await publish(page);
    await page.locator('#validation-result').screenshot({path: join(directory, 'block-builder-gradient-published.png')});
    console.log('Authored and published a gradient sample:\n' + written);
  } else {
    await page.locator('.definition-item', {hasText: 'Tide gel'}).filter({hasText: 'Effect'}).click();
    await page.waitForFunction(() => document.querySelector('#definition-source').value.includes(':op :gradient'));
    const panel = await openColorStage(page);
    await combo(panel, 'Block binding').selectOption({label: 'outputs / color'});
    const stage = panel.locator('.block-stage').filter({hasText: '1 · Color gel'});
    await stage.locator('summary', {hasText: 'Inputs'}).click();
    assert.equal(await chosen(combo(stage, 'transmission')), 'Gradient sample');
    assert.equal(await chosen(combo(stage, 'transmission gradient')), 'palette');
    assert.equal(await chosen(combo(stage, 'transmission extend')), 'mirror');
    assert.equal(await chosen(combo(stage, 'transmission position')), 'Fixture position');
    assert.equal(await chosen(combo(stage, 'transmission position axis')), 'X');
    await combo(stage, 'transmission extend').selectOption({label: 'repeat'});
    await stage.scrollIntoViewIfNeeded();
    await stage.screenshot({path: join(directory, 'block-builder-gradient-edit.png')});
    await panel.getByRole('button', {name: 'Write stages to EDN'}).click();
    await page.waitForFunction(() => document.querySelector('#definition-source').value.includes(':extend :repeat'));
    const written = (await source.inputValue()).replace(/:version 1\b/, ':version 2');
    assert.ok(written.includes(':version 2'));
    await source.fill(written);
    await publish(page);
    console.log('Loaded the published sample, changed its extension and published version 2.');
  }
  assert.deepEqual(errors, []);
} finally { await context.close(); }
