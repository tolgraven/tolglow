// Invalid-edit recovery for a running cue, edited in real Neovim WASM against a disposable
// capture-only Studio host built with npm run studio:build.
// CHROMIUM_PATH=... node dev/studio/code-cue-recovery-qa.mjs http://127.0.0.1:8500 target/code-cue-recovery
//
// Part 1 mounts the pinned cue buffer engine on the Studio page and uses the host's
// definition API as its validator. The host reports a data path, not a source position, so
// the finding's position is the bad token located in the exact draft the host rejected.
// Part 2 drives the Definitions editor with Neovim selected, the product path today.
import assert from 'node:assert/strict';
import {mkdir} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, output] = process.argv.slice(2);
assert.ok(origin && output, 'Specify the disposable Studio origin and capture directory.');
await mkdir(output, {recursive: true});

const request = async (path, body) => {
  const response = await fetch(origin + path, body
    ? {method: 'POST', headers: {accept: 'application/json', 'content-type': 'application/json'}, body: JSON.stringify(body)}
    : {headers: {accept: 'application/json'}});
  return {status: response.status, body: await response.json()};
};
const state = async () => (await request('/api/state')).body;
const initial = await state();
assert.equal(initial.status.output.type, 'preview', 'Run this only against a capture-only host.');
const owner = {'expected-epoch': initial.epoch, 'runtime-id': initial['runtime-id']};
const stamp = Date.now().toString(36);
const green = '[0.0 1.0 0.0]', blue = '[0.0 0.0 1.0]';
const effectSource = (id, name) => [
  `{:id :${id}`,
  ' :version 1',
  ` :name "${name}" :outputs {:color {:op :const :value ${green}}`,
  '                          :dimmer {:op :param :key :level}}',
  ' :duration {:bars 2} :envelope {:attack {:beats 0} :release {:beats 0}}',
  ' :params {:level {:type :number :default 1.0 :bounds [0.0 1.0]}}}'].join('\n');
const command = (op, body) => request('/api/command', {op, epoch: initial.epoch, 'show-id': initial['show-id'],
  'command-id': crypto.randomUUID(), source: {adapter: 'web', 'session-id': 'code-cue-recovery-qa'}, ...body});

const running = [];
async function launchCue(id, name) {
  const effect = await request('/api/definition', {...owner, kind: 'effects', source: effectSource(id, name)});
  assert.equal(effect.status, 200, JSON.stringify(effect.body));
  const cue = await request('/api/definition', {...owner, kind: 'cues',
    source: `{:id :${id} :name "${name}" :effect-id :${id} :params {} :reload-policy :preserve-state}`});
  assert.equal(cue.status, 200, JSON.stringify(cue.body));
  const launch = await command('cue/launch', {'cue-id': id, launch: {mode: 'persistent'}});
  assert.equal(launch.status, 200, JSON.stringify(launch.body));
  running.push(launch.body['instance-id']);
  return launch.body['instance-id'];
}
// The show as the running cue sees it: registry version, instance, compiled version and colour.
async function show(id) {
  for (let wait = 0; ; wait++) {
    const current = await state();
    const instances = current.instances.filter(instance => instance['cue-id'] === id);
    const observed = {version: current.effects.find(effect => effect.id === id)?.version,
      instances: instances.map(instance => [instance.id, instance['effect-version']]),
      color: current.output.values['head-1']['emitted-color']};
    if (wait >= 20 || observed.instances.every(([, version]) => version === observed.version)) return observed;
    await new Promise(resolve => setTimeout(resolve, 100));
  }
}

const browser = await chromium.launch({executablePath: process.env.STUDIO_CHROMIUM || process.env.CHROMIUM_PATH || '/usr/bin/chromium',
  args: ['--no-sandbox', '--use-gl=angle', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
const errors = [];
const report = {};
try {
  const page = await browser.newPage({viewport: {width: 1280, height: 900}});
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(origin + '/library', {waitUntil: 'domcontentloaded'});
  await page.waitForFunction(() => typeof globalThis.tolglowLoadEngine === 'function');

  // Part 1: pinned buffer, host validation, versioned findings.
  const pinnedId = `code-cue-recovery-${stamp}-pinned`;
  const pinnedInstance = await launchCue(pinnedId, 'Cue ☃ pinned recovery');
  const pinnedStart = await show(pinnedId);
  assert.deepEqual(pinnedStart, {version: 1, instances: [[pinnedInstance, 1]], color: [0, 1, 0]});
  // Edit the authored multi-line text; the registry keeps its own one-line printing.
  await page.evaluate(async ({text}) => {
    const {NeovimEngine} = await globalThis.tolglowLoadEngine('neovim');
    const host = document.createElement('div');
    host.id = 'pinned-cue-qa';
    host.style.cssText = 'position:fixed;left:20px;top:20px;width:900px;height:260px;z-index:1000';
    document.body.append(host);
    const qa = globalThis.pinnedCueQa = {changes: [], failures: []};
    qa.editor = new NeovimEngine(host, {pinBuffer: true, fileName: '/home/user/cue.edn',
      onChange: (_text, draft) => qa.changes.push(draft), onError: error => qa.failures.push(error)});
    await qa.editor.start(text);
    qa.findings = () => qa.editor.nvim.request('nvim_exec_lua', [
      'return vim.tbl_map(function(d) return {lnum=d.lnum, col=d.col, message=d.message} end, vim.diagnostic.get(...))', [qa.editor.pinnedBuffer.bufferId]]);
  }, {text: effectSource(pinnedId, 'Cue ☃ pinned recovery')});
  const keys = async (...sequence) => {
    await page.locator('#pinned-cue-qa .nvim-input').focus();
    for (const key of sequence) key.length > 1 && !key.includes(' ') && /^[A-Z]/.test(key) ? await page.keyboard.press(key) : await page.keyboard.type(key);
  };
  const delivered = matches => page.waitForFunction(text => globalThis.pinnedCueQa.changes.at(-1)?.source.includes(text), matches)
    .then(() => page.evaluate(() => globalThis.pinnedCueQa.changes.at(-1)));
  const apply = (draft, findings) => page.evaluate(({draft, findings}) => globalThis.pinnedCueQa.editor.setDraftDiagnostics(draft, findings), {draft, findings});
  const findings = () => page.evaluate(() => globalThis.pinnedCueQa.findings());
  const validate = draft => request('/api/definition', {...owner, kind: 'effects', 'expected-version': 1, source: draft.source});
  // A host rejection located at the token it names, in Neovim's zero-based line and UTF-8 byte column.
  const locate = (draft, token, message) => {
    const lines = draft.source.split('\n'), lnum = lines.findIndex(line => line.includes(token));
    const col = Buffer.byteLength(lines[lnum].slice(0, lines[lnum].indexOf(token)));
    return {lnum, col, end_col: col + Buffer.byteLength(token), message, severity: 1};
  };

  await keys('/const', 'Enter', 'cw', 'conts', 'Escape');
  const broken = await delivered(':op :conts');
  const rejected = await validate(broken);
  assert.equal(rejected.status, 422);
  assert.equal(rejected.body.message, 'Invalid effect expression');
  assert.deepEqual(rejected.body.details.errors.map(error => error.path), [['outputs', 0, 'value', 'op']]);
  const finding = locate(broken, ':conts', `${rejected.body.message}: ${rejected.body.details.errors[0].message}`);
  assert.equal(finding.lnum, 2);
  assert.equal(await apply(broken, [finding]), true);
  // ☃ is three UTF-8 bytes and one screen cell, so the byte column is two past the character column.
  const lineText = broken.source.split('\n')[finding.lnum];
  const charCol = Array.from(lineText.slice(0, lineText.indexOf(':conts'))).length;
  assert.equal(finding.col - charCol, 2);
  const screen = await page.evaluate(async ({lnum, message, charCol}) => {
    const {editor} = globalThis.pinnedCueQa;
    const text = row => editor.cells[row].map(([cell]) => cell).join('');
    const row = editor.cells.findIndex((_, r) => text(r).trimStart().startsWith(`${lnum + 1} `));
    for (let wait = 0; wait < 50 && !text(row).includes(message); wait++) await new Promise(resolve => setTimeout(resolve, 20));
    const numberWidth = text(row).indexOf(' :name'), [cell, attr] = editor.cells[row][numberWidth + charCol];
    const hl = editor.highlights[attr] || {};
    return {row: text(row).trimEnd(), cell, underlined: Boolean(hl.underline || hl.undercurl)};
  }, {lnum: finding.lnum, message: finding.message.slice(0, 30), charCol});
  assert.ok(screen.row.includes(finding.message.slice(0, 30)), screen.row);
  assert.equal(screen.cell, ':');
  assert.equal(screen.underlined, true);
  assert.deepEqual(await findings(), [{lnum: 2, col: finding.col, message: finding.message}]);
  await page.locator('#pinned-cue-qa').screenshot({path: join(output, 'pinned-finding.png')});
  assert.deepEqual(await show(pinnedId), pinnedStart);

  // A further edit drops the finding, and the reply for the earlier changedtick is refused.
  await keys('A', ' ;wip', 'Escape');
  const further = await delivered(';wip');
  assert.ok(further.changedtick > broken.changedtick);
  assert.deepEqual(await findings(), []);
  assert.equal(await apply(broken, [finding]), false);
  // A reply that arrives after the next edit is refused too.
  const inFlight = validate(further);
  await keys(':s/conts/const/', 'Enter', `:s/${green.slice(1, -1)}/${blue.slice(1, -1)}/`, 'Enter');
  const fixed = await delivered(blue);
  const lateReply = await inFlight;
  assert.equal(lateReply.status, 422);
  assert.equal(await apply(further, [locate(further, ':conts', lateReply.body.message)]), false);
  assert.deepEqual(await findings(), []);
  assert.deepEqual(await show(pinnedId), pinnedStart);
  const accepted = await validate(fixed);
  assert.equal(accepted.status, 200, JSON.stringify(accepted.body));
  assert.equal(accepted.body.version, 2);
  assert.equal(await apply(fixed, []), true);
  const pinnedEnd = await show(pinnedId);
  assert.deepEqual(pinnedEnd, {version: 2, instances: [[pinnedInstance, 2]], color: [0, 0, 1]});
  const pinnedFailures = await page.evaluate(() => { const {editor, failures} = globalThis.pinnedCueQa; editor.stop(); document.getElementById('pinned-cue-qa').remove(); return failures; });
  assert.deepEqual(pinnedFailures, []);
  assert.equal((await command('instance/end', {'instance-id': pinnedInstance})).status, 200);
  running.splice(running.indexOf(pinnedInstance), 1);
  report.pinned = {instance: pinnedInstance, start: pinnedStart, rejected: {status: rejected.status, path: rejected.body.details.errors[0].path},
    finding: {lnum: finding.lnum, byteCol: finding.col, charCol}, ticks: [broken.changedtick, further.changedtick, fixed.changedtick], end: pinnedEnd};

  // Part 2: the Definitions editor with Neovim, publishing through the product events.
  const studioId = `code-cue-recovery-${stamp}-studio`, studioName = `Cue ☃ studio recovery ${stamp}`;
  const studioInstance = await launchCue(studioId, studioName);
  const studioStart = await show(studioId);
  assert.deepEqual(studioStart, {version: 1, instances: [[studioInstance, 1]], color: [0, 1, 0]});
  await page.reload({waitUntil: 'domcontentloaded'});
  await page.locator('.definition-item').filter({hasText: studioName}).filter({hasText: 'Effect · v1'}).click();
  await page.locator('#definition-status').filter({hasText: `${studioId} · v1`}).waitFor();
  // Record what the Definitions editor's Neovim delivers through onChange. Each change
  // replaces the publication feedback with "Unpublished changes", including the echo of
  // the initial load and an edit that arrives after its own publication reply.
  await page.evaluate(async () => {
    const {NeovimEngine} = await globalThis.tolglowLoadEngine('neovim');
    const start = NeovimEngine.prototype.start;
    NeovimEngine.prototype.start = function (source) {
      const deliver = this.onChange;
      globalThis.definitionDeliveries = [];
      this.onChange = (text, ...rest) => { globalThis.definitionDeliveries.push(text); deliver(text, ...rest); };
      return start.call(this, source);
    };
  });
  await page.locator('#editor-engine').selectOption('neovim');
  await page.locator('#neovim-status').filter({hasText: 'running in WebAssembly'}).waitFor();
  const loaded = (await request(`/api/definition?kind=effects&id=${studioId}`)).body.source;
  const reached = text => page.waitForFunction(expected => globalThis.definitionDeliveries.at(-1) === expected, text);
  const publish = async () => {
    const response = page.waitForResponse(r => r.url().endsWith('/api/definition') && r.request().method() === 'POST');
    await page.getByRole('button', {name: 'Validate & publish'}).click();
    return (await response).status();
  };
  const feedback = async (ready, what) => {
    const read = () => page.evaluate(() => ({status: document.querySelector('#definition-status')?.textContent,
      result: document.querySelector('#validation-result')?.textContent, error: document.querySelector('#validation-result')?.classList.contains('error')}));
    for (let wait = 0; wait < 100; wait++) {
      const current = await read();
      if (ready(current)) return current;
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    throw new Error(`Studio did not show ${what}: ${JSON.stringify(await read())}`);
  };
  await page.locator('#neovim-host .nvim-input').focus();
  await page.keyboard.type('$x');
  await reached(loaded.slice(0, -1));
  assert.equal(await publish(), 400);
  const error = (await feedback(current => current.error, 'the rejection')).result;
  assert.match(error, /EOF|delimiter|Unmatched/i);
  assert.deepEqual(await show(studioId), studioStart);
  await page.screenshot({path: join(output, 'studio-rejected.png')});
  await page.locator('#neovim-host .nvim-input').focus();
  await page.keyboard.type(`:undo | s/${green.slice(1, -1)}/${blue.slice(1, -1)}/`);
  await page.keyboard.press('Enter');
  await reached(loaded.replace(green, blue));
  assert.equal(await publish(), 200);
  await feedback(current => /Published/.test(current.status) && !current.error, 'the publication');
  const studioEnd = await show(studioId);
  const deliveries = await page.evaluate(() => globalThis.definitionDeliveries.length);
  assert.deepEqual(studioEnd, {version: 2, instances: [[studioInstance, 2]], color: [0, 0, 1]});
  await page.screenshot({path: join(output, 'studio-recovered.png')});
  assert.equal((await feedback(() => true, 'the final status')).status, 'Published');
  report.studio = {instance: studioInstance, start: studioStart, rejected: error, end: studioEnd, deliveries};
  assert.deepEqual(errors, []);
  console.log(JSON.stringify({status: 'passed', ...report}, null, 2));
} finally {
  for (const instance of running) await command('instance/end', {'instance-id': instance}).catch(() => {});
  await browser.close();
}
