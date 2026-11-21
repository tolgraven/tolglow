import test from 'node:test';
import assert from 'node:assert/strict';
import {LatestStateChannel} from '../../../js/preview/mailbox.js';
import {presentFrame, showIdentity} from '../../../js/preview/presentation.js';

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
