import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, output] = process.argv.slice(2);
assert.ok(origin && output, 'Specify the disposable Studio origin and capture directory.');
await mkdir(output, {recursive: true});

const get = async path => {
  const response = await fetch(origin + path, {headers: {accept: 'application/json'}});
  assert.ok(response.ok, await response.clone().text());
  return response.json();
};
const initial = await get('/api/state');
assert.equal(initial.status.output.type, 'preview');
const owner = {'runtime-id': initial['runtime-id'], 'expected-epoch': initial.epoch};
const post = (path, body) => fetch(origin + path, {
  method: 'POST', headers: {accept: 'application/json', 'content-type': 'application/json'},
  body: JSON.stringify({...owner, ...body})
});
const repl = async source => {
  const response = await post('/api/repl/eval', {source, 'request-id': crypto.randomUUID(), 'session-id': 'main'});
  const body = await response.text();
  assert.ok(response.ok, body);
  const events = body.split('\n').filter(line => line.startsWith('data: ')).map(line => JSON.parse(line.slice(6)));
  const ending = events.at(-1);
  assert.equal(ending?.status, 'completed', body);
};

await repl(`(do (require '[tolglow.show :as show] '[afterglow.reactive :as rx])
  (def reactions (show/reactions runtime))
  (rx/ratom reactions :studio-qa/level 0.375)
  (rx/ratom reactions :studio-qa/gain-value 0.625)
  (defn browser-qa-gain [{:keys [source]} _] @(source :studio-qa/gain-value))
  (rx/reg-sub! reactions :studio-qa/gain browser-qa-gain
    {:kind :sub :id :studio-qa/gain
     :implementation {:type :var :symbol (str (symbol #'browser-qa-gain))}}))`);

const browser = await chromium.launch({executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
const id = 'reactive-authoring-proof';
const title = 'Reactive authoring proof';
const errors = [];
const geometry = {};

async function open(width, height, hasTouch) {
  const page = await browser.newPage({viewport: {width, height}, hasTouch});
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(origin + '/library');
  await page.locator('.definition-item').first().waitFor();
  return page;
}

const visualBuilder = page => page.getByRole('button', {name: 'Visual builder', exact: true}).click();
const form = page => page.locator('#visual-editor');
const optIn = page => form(page).getByRole('checkbox', {name: 'Evaluate when inputs change'});
const selectedEffect = (page, version) => page.locator('.definition-item').filter({hasText: title})
  .filter({hasText: `Effect · v${version}`});
const loadEffect = async (page, version) => {
  await selectedEffect(page, version).click();
  await page.locator('#definition-status').filter({hasText: `${id} · v${version}`}).waitFor();
  await visualBuilder(page);
};
const capture = (page, name) => page.screenshot({path: join(output, name + '.png'), animations: 'disabled'});
const validationError = async page => {
  const feedback = page.locator('#validation-result.error');
  await feedback.waitFor();
  return feedback.innerText();
};
const registrySource = async () => (await get(`/api/definition?kind=effects&id=${id}`)).source;
const definitionPost = request => request.url().endsWith('/api/definition') && request.method() === 'POST';
const publish = async page => {
  const pending = page.waitForResponse(response => definitionPost(response.request()));
  await page.getByRole('button', {name: 'Validate & publish'}).click();
  return pending;
};
const waitForIntensity = (page, intensity) => page.waitForFunction(async expected => {
  const response = await fetch('/api/state', {headers: {accept: 'application/json'}});
  return (await response.json()).output.values['head-1'].intensity === expected;
}, intensity);

try {
  const desktop = await open(1440, 960, false);
  await visualBuilder(desktop);
  assert.equal(await optIn(desktop).isChecked(), false);
  await optIn(desktop).check();
  assert.equal(await form(desktop).getByLabel('Intensity', {exact: true}).count(), 0);
  assert.equal(await form(desktop).getByRole('checkbox', {name: 'Sample at a fixed interval'}).count(), 0);
  await form(desktop).getByLabel('Identifier', {exact: true}).fill(id);
  await form(desktop).getByLabel('Name', {exact: true}).fill(title);
  await form(desktop).getByLabel('Duration value', {exact: true}).fill('512');
  await desktop.getByLabel('Target fixture 1', {exact: true}).selectOption({label: 'head-1'});
  await desktop.getByLabel('Input 1 Ratom ID').fill(':studio-qa/level');
  await desktop.getByRole('button', {name: 'Add input', exact: true}).click();
  await desktop.getByLabel('Input 2 alias').fill('level');
  await desktop.getByLabel('Input 2 Subscription query').fill('[:studio-qa/gain]');
  await desktop.getByLabel('Transition (beats)').fill('1/3');
  await desktop.getByRole('button', {name: 'Validate & publish'}).click();
  assert.match(await validationError(desktop), /use each alias once/);
  await capture(desktop, '01-duplicate-alias');
  await desktop.getByLabel('Input 2 alias').fill('gain');
  await capture(desktop, '02-ready');
  assert.ok((await publish(desktop)).ok());
  await desktop.locator('#definition-status').filter({hasText: /^Published$/}).waitFor();
  await selectedEffect(desktop, 1).waitFor();
  assert.match(await registrySource(), /:transition \{:beats 1\/3\}/);
  await capture(desktop, '03-owner-published');

  await loadEffect(desktop, 1);
  await desktop.getByLabel('Input 1 Subscription query').waitFor();
  const aliases = await desktop.locator('.reactive-binding').evaluateAll(rows =>
    rows.map(row => row.querySelector('input').value));
  const level = aliases.indexOf('level') + 1;
  assert.ok(level > 0);

  await repl(`(let [definition (get-in @(:state runtime) [:model :registry :effects :${id}])
                     revised (assoc-in definition [:outputs :dimmer]
                               {:op :multiply :args [0.5 {:op :reactive :key :level}]})]
                 (deref (show/submit! runtime :definition
                            {:kind :effects :definition revised :expected-version 1}) 2000 :timeout))`);
  await selectedEffect(desktop, 2).waitFor();
  const ownerSource = await registrySource();
  await desktop.getByLabel('Transition (beats)').fill('1/2');
  assert.equal((await publish(desktop)).ok(), false);
  assert.match(await validationError(desktop), /Definition changed/);
  assert.equal(await registrySource(), ownerSource);
  await capture(desktop, '04-owner-rejected-stale-edit');

  await loadEffect(desktop, 2);
  await desktop.getByLabel(`Input ${level} signal type`).selectOption('subscription');
  await desktop.getByLabel(`Input ${level} Subscription query`).fill('[:studio-qa/gain]');
  assert.ok((await publish(desktop)).ok());
  await selectedEffect(desktop, 3).waitFor();
  await desktop.locator('#definition-status').filter({hasText: /^Published$/}).waitFor();
  const source = await registrySource();
  assert.match(source, /:level \[:subscription \[:studio-qa\/gain\]\]/);
  assert.match(source, /:op :multiply\s+:args \[0\.5 \{:op :reactive\s+:key :level\}\]/);
  assert.match(source, /:transition \{:beats 1\/3\}/);
  await capture(desktop, '05-edited-owner-definition');

  const launch = await post('/api/command', {op: 'cue/launch', 'cue-id': id, epoch: initial.epoch,
    'show-id': initial['show-id'], 'command-id': crypto.randomUUID(), source: {adapter: 'web', 'session-id': id}});
  assert.ok(launch.ok, await launch.clone().text());
  const ack = await launch.json();
  assert.equal(ack.status, 'accepted');
  await waitForIntensity(desktop, 0.3125);

  const downloadPending = desktop.waitForEvent('download');
  await desktop.locator('#export-show').click();
  const checkpoint = await downloadPending;
  await loadEffect(desktop, 3);
  await desktop.getByLabel('Transition (beats)').fill('1/2');
  await repl(`(deref (show/submit! runtime {:op :master/set :value 0.8}) 2000 :timeout)`);
  await desktop.locator('#master-value').filter({hasText: '80%'}).waitFor();
  const importPending = desktop.waitForResponse(response =>
    response.url().endsWith('/api/import') && response.request().method() === 'POST');
  await desktop.locator('#import-show').setInputFiles(await checkpoint.path());
  assert.ok((await importPending).ok());
  await desktop.locator('#master-value').filter({hasText: '100%'}).waitFor();
  await selectedEffect(desktop, 3).waitFor();
  const restored = await get('/api/state');
  assert.equal(restored.epoch, initial.epoch + 1);
  assert.equal(restored['runtime-id'], owner['runtime-id']);
  const restoredSource = await registrySource();
  assert.equal(restoredSource, source);
  let stalePosts = 0;
  const countStalePost = request => { if (definitionPost(request)) stalePosts++; };
  desktop.on('request', countStalePost);
  await desktop.getByRole('button', {name: 'Validate & publish'}).click();
  assert.match(await validationError(desktop), /The show changed\. Reload/);
  desktop.off('request', countStalePost);
  assert.equal(stalePosts, 0);
  assert.equal(await registrySource(), restoredSource);
  await capture(desktop, '06-reload-required-after-restore');

  await waitForIntensity(desktop, 0.3125);
  owner['expected-epoch'] = restored.epoch;
  await repl(`(reset! (afterglow.reactive/source (tolglow.show/reactions runtime) :studio-qa/gain-value) 0.75)`);
  await waitForIntensity(desktop, 0.375);

  await loadEffect(desktop, 3);
  assert.equal(await desktop.getByLabel('Transition (beats)').inputValue(), '1/3');
  await desktop.getByLabel('Transition (beats)').fill('1/4');
  assert.ok((await publish(desktop)).ok());
  await selectedEffect(desktop, 4).waitFor();
  await desktop.locator('#definition-status').filter({hasText: /^Published$/}).waitFor();
  assert.match(await registrySource(), /:transition \{:beats 1\/4\}/);
  await capture(desktop, '07-published-after-reload');

  for (const [name, width, height] of [['phone', 390, 844], ['ipad', 834, 1010]]) {
    const page = await open(width, height, true);
    await page.locator('#definition-search').fill(title);
    await loadEffect(page, 4);
    assert.equal(await optIn(page).isChecked(), true);
    await page.locator('.reactive-effect-editor').scrollIntoViewIfNeeded();
    geometry[name] = await page.evaluate(() => ({overflow: document.documentElement.scrollWidth - innerWidth,
      minControlHeight: Math.min(...Array.from(document.querySelectorAll('.reactive-fields input, .reactive-fields select, .reactive-fields button'),
        element => element.getBoundingClientRect().height))}));
    assert.deepEqual(geometry[name], {overflow: 0, minControlHeight: 44});
    await capture(page, name);
    await page.close();
  }
  assert.deepEqual(errors, []);
  const evidence = {effect: id, versions: [1, 2, 3, 4], ownerConflictPreservedRevision: 2,
    restoredEpoch: restored.epoch, stalePosts, restoredDraftRepublishedVersion: 4,
    previewIntensity: 0.3125, reboundPreviewIntensity: 0.375, geometry, errors};
  await writeFile(join(output, 'evidence.json'), JSON.stringify(evidence, null, 2) + '\n');
  console.log(JSON.stringify(evidence));
} finally {await browser.close();}
