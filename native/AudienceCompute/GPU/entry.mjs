import transit from 'transit-js';
import {createComputeDispatcher} from '@tolglow/compute-dispatcher';

const kw = transit.keyword, get = (m,k) => m.get(kw(k));
const encode = entries => transit.writer('json').write(transit.map(entries));
function object(x) {
  if (transit.isKeyword(x)) return x.name();
  if (transit.isMap(x)) {
    const out = Object.create(null);
    x.forEach((v,k) => { out[transit.isKeyword(k) ? k.name() : k] = object(v); });
    return out;
  }
  return Array.isArray(x) ? x.map(object) : x;
}
function floats(bytes) {
  if (!(bytes instanceof Uint8Array) || bytes.byteLength % 4) throw Error('Invalid position buffer');
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  return Float32Array.from({length:bytes.byteLength/4}, (_,i) => view.getFloat32(i*4,true));
}
function packed(values) {
  if (!(values instanceof Float32Array) || values.length > 8192) throw Error('Invalid GPU result');
  const bytes = new Uint8Array(values.length*4), view = new DataView(bytes.buffer);
  values.forEach((v,i) => view.setFloat32(i*4,v,true));
  return bytes;
}

export function start(canvas) {
  const dispatch = createComputeDispatcher({canvas});
  return {
    async probe() {
      const result = await dispatch({op:'probe',requestId:0});
      return result.ok === true;
    },
    async compute(text) {
      const message = transit.reader('json').read(text), op = get(message,'op').name();
      const id = get(message,'id'), job = get(message,'job');
      let request;
      if (op === 'prepare') {
        const uv = get(message,'surface-uvs');
        request = {op,id,requestId:1,plan:object(get(message,'plan')),positions:floats(get(message,'positions')),
                   surfaceUvs:uv ? floats(uv) : undefined};
      } else if (op === 'run') {
        request = {op,id,requestId:job,start:get(message,'start'),count:get(message,'count'),
                   uniforms:object(get(message,'uniforms'))};
      } else { throw Error('Unsupported guest operation'); }
      const result = await dispatch(request);
      if (!result.ok) {
        const reply=[kw('op'),kw('error'),kw('id'),id];
        if (job) reply.push(kw('job'),job);
        return [encode(reply)];
      }
      if (op === 'prepare') {
        const replies = [];
        if (result.evicted) replies.push(encode([kw('op'),kw('error'),kw('id'),result.evicted]));
        replies.push(encode([kw('op'),kw('ready'),kw('id'),id]));
        return replies;
      }
      return [encode([kw('op'),kw('result'),kw('job'),job,kw('values'),packed(result.values)])];
    },
    stop() { return dispatch({op:'dispose',requestId:0}); }
  };
}
