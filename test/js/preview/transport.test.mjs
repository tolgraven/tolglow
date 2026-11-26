import test from 'node:test';
import assert from 'node:assert/strict';
import {LatestStateChannel} from '../../../js/preview/mailbox.js';
import {CommittedFrames, presentFrame, showIdentity} from '../../../js/preview/presentation.js';

test('slow renderer receives one state at a time and the newest pending projection', () => {
  const sent = [], channel = new LatestStateChannel(message => sent.push(message));
  channel.offer({revision: 1}); channel.open();
  for (let revision = 2; revision <= 1000; revision++) channel.offer({revision});
  assert.deepEqual(sent.map(message => message.state.revision), [1]);
  assert.equal(channel.coalesced, 998);
  assert.equal(channel.acknowledge(99), false);
  assert.equal(sent.length, 1);
  channel.acknowledge(1);
  assert.deepEqual(sent.map(message => message.state.revision), [1, 1000]);
  channel.close(); channel.offer({revision: 1001}); channel.acknowledge(2);
  assert.equal(sent.length, 2); assert.equal(channel.pending, null);
});

test('presentation rejects previous shows, epochs and frames and closes every bitmap', () => {
  const shown = [], closed = [], identity = showIdentity({'show-id': 'show', epoch: 2});
  const make = (frameId, showId, epoch) => ({frameId, showId, epoch, bitmap: {frameId, close() {closed.push(frameId);}}});
  const present = bitmap => shown.push(bitmap.frameId);
  assert.equal(presentFrame(make(4, 'show', 1), identity, 3, present), false);
  assert.equal(presentFrame(make(5, 'previous-show', 2), identity, 3, present), false);
  assert.equal(presentFrame(make(2, 'show', 2), identity, 3, present), false);
  assert.equal(presentFrame(make(6, 'show', 2), identity, 3, present), true);
  assert.deepEqual(shown, [6]); assert.deepEqual(closed, [4, 5, 2, 6]);
  assert.throws(() => presentFrame(make(7, 'show', 2), identity, 6, () => {throw new Error('context lost');}), /context lost/);
  assert.equal(closed.at(-1), 7);
});

test('readers of a worker-presented canvas wait for the animation frame after the one that follows a report', () => {
  let pending = null, nextId = 0;
  const host = {requestAnimationFrame(callback) {assert.equal(pending, null, 'one animation callback at a time'); pending = callback; return ++nextId;},
    cancelAnimationFrame() {pending = null;}};
  const animationFrame = () => {const callback = pending; pending = null; callback?.();};
  const read = [], frames = new CommittedFrames(batch => read.push(batch.slice()), host, 3);
  frames.add(1);
  // The browser commits a worker canvas at the display's next frame. The page
  // animation frame that follows a report still holds the image before it.
  animationFrame();
  assert.deepEqual(read, [], 'the first animation frame after a report is too early');
  frames.add(2);
  animationFrame();
  assert.deepEqual(read, [[1]]);
  animationFrame();
  assert.deepEqual(read, [[1], [2]]);
  assert.equal(pending, null, 'an idle canvas leaves no animation callback behind');
  // A starved page keeps the newest reports, in order, and never grows without bound.
  for (let report = 3; report <= 8; report++) frames.add(report);
  animationFrame(); animationFrame();
  assert.deepEqual(read.at(-1), [6, 7, 8]);
  frames.add(9); animationFrame(); frames.add(10);
  frames.clear();
  assert.equal(pending, null);
  animationFrame(); animationFrame();
  assert.deepEqual(read.at(-1), [6, 7, 8], 'cleared reports are never read');
  // A reader that stops its owner during a read leaves nothing scheduled.
  const stopping = new CommittedFrames(() => stopping.clear(), host);
  stopping.add(1); stopping.add(2); animationFrame(); stopping.add(3); animationFrame();
  assert.equal(pending, null);
});
