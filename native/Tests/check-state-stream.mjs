import assert from 'node:assert/strict';
import {once} from 'node:events';

const origin = new URL(process.argv[2]);
origin.protocol = origin.protocol === 'https:' ? 'wss:' : 'ws:';
origin.pathname = '/api/state/stream';
origin.searchParams.set('session', 'native-package-check');
const socket = new WebSocket(origin);
const signal = AbortSignal.timeout(12000);
try {
  const receive = once(socket, 'message', {signal});
  const [event] = await receive;
  const text = event.data;
  assert.ok(typeof text === 'string' && text.includes('tolglow-state/2') && text.includes('~:full'),
    'The compiled Ring host must send a full Transit snapshot.');
  const packet = JSON.parse(text);
  const token = packet[packet.indexOf('~:token') + 1];
  assert.match(token, /^[a-z0-9-]+$/i);
  socket.send(JSON.stringify(['^ ', '~:op', '~:ack', '~:token', token]));
  console.log('Received and acknowledged the Studio Transit snapshot.');
} finally {
  socket.close();
}
