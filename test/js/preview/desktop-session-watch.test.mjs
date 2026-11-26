import test from 'node:test';
import assert from 'node:assert/strict';
import child from 'node:child_process';
import {syncBuiltinESMExports} from 'node:module';
import {promisify} from 'node:util';

// Answer the ioreg read at the process boundary; the watch, its timer and the
// parser are real. This file owns its process, so the replacement stays local.
const unlocked = '  | "IOConsoleLocked" = No\n  | "IOConsoleUsers" = ({"kCGSSessionOnConsoleKey"=Yes})\n';
const screenLocked = unlocked.replace('=Yes}', '=Yes,"CGSSessionScreenIsLocked"=Yes}');
let answers = [], reads = 0;
const ioreg = () => {throw new Error('The session reader must use the promisified execFile.');};
ioreg[promisify.custom] = async file => {
  assert.equal(file, '/usr/sbin/ioreg'); reads++;
  const answer = answers.length > 1 ? answers.shift() : answers[0];
  if (answer instanceof Error) throw answer;
  return {stdout: answer, stderr: ''};
};
child.execFile = ioreg; syncBuiltinESMExports();
Object.defineProperty(process, 'platform', {value: 'darwin'});
const {watchDesktopSession} = await import('../../../dev/preview/desktop-session.mjs');

const settle = async () => {for (let i = 0; i < 2; i++) await new Promise(resolve => setImmediate(resolve));};
async function watching(t, sequence) {
  t.mock.timers.enable({apis: ['setTimeout']});
  answers = sequence; reads = 0;
  const watch = watchDesktopSession(), state = {failed: false};
  watch.failed.then(() => {state.failed = true;});
  await settle();
  return {watch, state, async seconds(n) {for (let i = 0; i < n; i++) {t.mock.timers.tick(1000); await settle();}}};
}
const statuses = watch => watch.checks.map(check => check.status);

test('a lock during the sample fails the watch within one poll and stops polling', async t => {
  const {watch, state, seconds} = await watching(t, [unlocked, unlocked, screenLocked]);
  await seconds(1);
  assert.deepEqual(statuses(watch), ['unlocked', 'unlocked']); assert.equal(state.failed, false);
  await seconds(1);
  assert.equal(state.failed, true); assert.equal(watch.failure.reason, 'session-lock');
  await seconds(3);
  assert.equal(reads, 3);
  assert.equal((await watch.stop()).status, 'locked');
  assert.deepEqual(statuses(watch), ['unlocked', 'unlocked', 'locked', 'locked']);
});

test('an unlocked twenty-second sample records one check a second plus the closing check', async t => {
  const {watch, state, seconds} = await watching(t, [unlocked]);
  await seconds(20);
  assert.equal((await watch.stop()).status, 'unlocked');
  assert.equal(watch.checks.length, 22); assert.equal(watch.failure, undefined); assert.equal(state.failed, false);
  await seconds(3);
  assert.equal(reads, 22);
});

test('one failed read invalidates the sample even when later reads recover', async t => {
  const {watch, state, seconds} = await watching(t, [unlocked, new Error('ioreg timed out'), unlocked]);
  await seconds(1);
  assert.equal(state.failed, true);
  assert.equal((await watch.stop()).status, 'unlocked');
  assert.equal(watch.failure.reason, 'ioreg-read-failed');
  assert.deepEqual(statuses(watch), ['unlocked', 'unknown', 'unlocked']);
});

test('a watch that outlives its receipt limit fails closed', async t => {
  const {watch, seconds} = await watching(t, [unlocked]);
  await seconds(40);
  assert.equal(reads, 31); assert.equal(watch.checks.length, 32);
  assert.equal(watch.failure.reason, 'monitor-check-limit');
  assert.equal((await watch.stop()).reason, 'monitor-check-limit');
  assert.equal(watch.checks.length, 32);
});
