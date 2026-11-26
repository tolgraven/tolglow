import test from 'node:test';
import assert from 'node:assert/strict';
import {execFile} from 'node:child_process';
import {mkdtemp, readFile, readdir, rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {promisify} from 'node:util';
import {parseDesktopSession} from '../../../dev/preview/desktop-session.mjs';

const record = (console = 'Yes', locked) => `{"kCGSSessionOnConsoleKey"=${console}${locked ? `,"CGSSessionScreenIsLocked"=${locked}` : ''}}`;
const registry = (root = 'No', records = [record()]) => `+-o Root\n  | "IOConsoleLocked" = ${root}\n  | "IOConsoleUsers" = (${records.join(',')})\n`;

test('explicit root unlock plus one console accepts absent or false session lock', () => {
  for (const records of [[record()], [record('Yes', 'No')], [record('No'), record()]])
    assert.equal(parseDesktopSession(registry('No', records)).status, 'unlocked');
});

test('root or session lock always blocks qualification', () => {
  assert.equal(parseDesktopSession(registry('Yes')).status, 'locked');
  assert.equal(parseDesktopSession(registry('No', [record('Yes', 'Yes')])).status, 'locked');
  assert.equal(parseDesktopSession(registry('No', [record(), record('No', 'Yes')])).status, 'locked');
});

test('missing or uninterpretable flags fail closed', () => {
  for (const input of ['', registry().replace(/.*IOConsoleLocked.*\n/, ''), registry('Unknown'),
    registry('No', []), registry('No', ['{}']), registry('No', [record('Unknown')]),
    registry('No', [record('Yes', 'Unknown')]), registry().replace(/.*IOConsoleUsers.*\n/, '')])
    assert.equal(parseDesktopSession(input).status, 'unknown');
});

test('ambiguous root, console ownership and duplicate flags fail closed', () => {
  for (const input of [registry('No', [record(), record()]), registry('No', [record('No')]),
    registry()+'  | "IOConsoleLocked" = No\n', registry()+registry(),
    registry('No', ['{"kCGSSessionOnConsoleKey"=Yes,"kCGSSessionOnConsoleKey"=No}'])])
    assert.equal(parseDesktopSession(input).status, 'unknown');
});

test('quoted private fields cannot inject session flags or escape records', () => {
  const session = '{"private"="fake }, { \\"kCGSSessionOnConsoleKey\\"=Yes", "kCGSSessionOnConsoleKey"=Yes}';
  const result = parseDesktopSession(registry('No', [session]));
  assert.equal(result.status, 'unlocked');
  assert.deepEqual(Object.keys(result).sort(), ['reason', 'status']);
  assert(!JSON.stringify(result).includes('fake'));
});

test('truncated session records fail closed', () => {
  assert.equal(parseDesktopSession(registry().replace('})', ')')).status, 'unknown');
});

test('locked OS response rejects real benchmark entry before starting external work', {timeout: 10000}, async () => {
  const dir = await mkdtemp(join(tmpdir(), 'room-session-refusal-'));
  const output = join(dir, 'receipt');
  const entry = new URL('../../../dev/preview/measure-room.mjs', import.meta.url).href;
  const script = `import child from 'node:child_process';
    import {syncBuiltinESMExports} from 'node:module';
    import {promisify} from 'node:util';
    const forbidden = () => {throw new Error('External work started before desktop qualification');};
    const probe = forbidden;
    probe[promisify.custom] = async file => {
      if (file !== '/usr/sbin/ioreg') forbidden();
      return {stdout: '  | "IOConsoleLocked" = Yes\\n', stderr: ''};
    };
    child.execFile = probe; child.execFileSync = forbidden; child.spawn = forbidden;
    syncBuiltinESMExports();
    Object.defineProperty(process, 'platform', {value: 'darwin'});
    process.argv = ['node', 'measure-room', 'http://127.0.0.1:9223', ${JSON.stringify(output)}];
    await import(${JSON.stringify(entry)});`;
  try {
    await assert.rejects(promisify(execFile)(process.execPath, ['--input-type=module', '-e', script]), error => error.code === 2);
    const receipt = JSON.parse(await readFile(join(output, 'room-error.json'), 'utf8'));
    assert.equal(receipt.desktopSession.preflight.status, 'locked');
    assert.equal(receipt.qualified, false);
    assert.deepEqual(await readdir(output), ['room-error.json']);
  } finally {await rm(dir, {recursive: true, force: true});}
});
