// Run through tolglow.qa.cue-staging-browser. Phase "studio" stages Ember from
// its cue actions, edits the staged level twice, undoes and redoes the second
// edit and launches it. Phase "repl" finds a stage the REPL opened and edited,
// then cancels it. Staged cue launches: docs/repl-studio-parity.md (#477).
import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, directory, phase] = process.argv.slice(2);
const executablePath = process.env.CHROMIUM_PATH || '/usr/bin/chromium';
await mkdir(directory, {recursive: true});

async function state(page) {
  const response = await page.request.get(origin + '/api/state', {headers: {accept: 'application/json'}});
  assert.ok(response.ok());
  return response.json();
}

async function until(page, predicate, what) {
  for (let attempt = 0; attempt < 80; attempt++) {
    const show = await state(page);
    if (predicate(show)) return show;
    await new Promise(resolve => setTimeout(resolve, 100));
  }
  throw new Error('Timed out waiting for ' + what);
}

const stages = show => show['staged-cues'] || [];
const stagedLevel = show => stages(show)[0]?.params?.level;
const stageSteps = show => show['operator-history'].nodes.filter(node => node['stage-id']);

async function setLevel(card, value) {
  const slider = card.getByLabel('level for the staged cue', {exact: true});
  await slider.focus();
  await slider.fill(String(value));
  await slider.blur();
}

async function openPerform(page) {
  await page.goto(origin + '/perform');
  const cues = page.getByRole('group', {name: 'Performance sections'}).getByRole('button', {name: 'Cues', exact: true});
  if (await cues.isVisible()) await cues.click();
}

async function studioPhase(page) {
  await openPerform(page);
  const pad = page.locator('#cue-grid [data-cue-id="ember"]:enabled');
  await pad.waitFor();
  await pad.focus();
  await page.keyboard.press('Shift+F10');
  const actions = page.getByRole('dialog', {name: 'Ember', exact: true});
  await actions.getByRole('button', {name: 'Stage and adjust', exact: true}).click();
  const panel = page.getByRole('region', {name: 'Staged cues'});
  const card = panel.getByRole('listitem', {name: 'Staged Ember'});
  await card.waitFor();
  await actions.waitFor({state: 'hidden'});
  let show = await until(page, s => stages(s).length === 1, 'the stage to open');
  assert.equal(show.instances.length, 0, 'a staged cue renders nothing');
  assert.ok(await card.getByRole('button', {name: 'Undo', exact: true}).isDisabled());

  await setLevel(card, 0.35);
  await until(page, s => stagedLevel(s) === 0.35 && stageSteps(s).length === 1, 'the first staged edit');
  await setLevel(card, 0.6);
  show = await until(page, s => stagedLevel(s) === 0.6 && stageSteps(s).length === 2, 'the second staged edit');
  const actors = stageSteps(show).map(node => node.actor);
  assert.deepEqual(actors, ['Studio', 'Studio']);
  await card.getByRole('button', {name: 'Undo', exact: true}).click();
  await until(page, s => stagedLevel(s) === 0.35, 'undo on the stage');
  await card.locator('output').filter({hasText: '0.35'}).waitFor();
  await card.getByRole('button', {name: 'Redo', exact: true}).click();
  show = await until(page, s => stagedLevel(s) === 0.6, 'redo on the stage');
  assert.equal(show.instances.length, 0);
  await card.locator('output').filter({hasText: '0.60'}).waitFor();
  await panel.screenshot({path: join(directory, 'staged-cue-studio.png')});

  await card.getByRole('button', {name: 'Launch now', exact: true}).click();
  show = await until(page, s => stages(s).length === 0 && s.instances.length === 1, 'the staged launch');
  assert.equal(show.instances[0]['cue-id'], 'ember');
  assert.equal(show.instances[0].params.level, 0.6);
  assert.equal(stageSteps(show).length, 0, 'the launched instance owns the staged steps');
  await panel.waitFor({state: 'hidden'});
  return {launched: show.instances[0].id, level: show.instances[0].params.level,
          history: show['operator-history'].nodes.map(node => [node.id, node.after ?? null])};
}

async function replPhase(page) {
  await openPerform(page);
  const panel = page.getByRole('region', {name: 'Staged cues'});
  const card = panel.getByRole('listitem', {name: 'Staged Ice'});
  await card.waitFor();
  await card.locator('output').filter({hasText: '0.42'}).waitFor();
  assert.ok(await card.getByRole('button', {name: 'Undo', exact: true}).isEnabled(),
            'the REPL edit is the stage\'s current history step');
  await panel.screenshot({path: join(directory, 'staged-cue-from-repl.png')});
  await card.getByRole('button', {name: 'Cancel', exact: true}).click();
  const show = await until(page, s => stages(s).length === 0, 'the Studio cancel');
  await panel.waitFor({state: 'hidden'});
  return {instances: show.instances.map(instance => instance['cue-id']), steps: stageSteps(show).length};
}

const browser = await chromium.launch({
  executablePath,
  args: ['--headless=new', '--disable-dev-shm-usage', '--use-angle=swiftshader', '--enable-unsafe-swiftshader'],
});
try {
  const context = await browser.newContext({viewport: {width: 1440, height: 900}});
  const page = await context.newPage();
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  const result = phase === 'repl' ? await replPhase(page) : await studioPhase(page);
  assert.deepEqual(errors, []);
  console.log(JSON.stringify({phase, ...result}));
  await writeFile(join(directory, `${phase}.json`), JSON.stringify(result, null, 2));
} finally {
  await browser.close();
}
