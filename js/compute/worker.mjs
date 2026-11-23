import {createComputeDispatcher} from './dispatcher.mjs';

const dispatch = createComputeDispatcher({onAvailability: message => self.postMessage(message)});
self.onmessage = async ({data}) => {
  const reply = await dispatch(data);
  self.postMessage(reply, reply.values ? [reply.values.buffer] : []);
  if (data?.op === 'dispose' && reply.ok) self.close();
};
