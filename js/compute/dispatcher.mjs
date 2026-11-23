import {createComputeExecutor} from './executor.mjs';

const unavailable = message => Object.assign(new Error(message), {code:'unavailable'});
const calibration = {stride:1, outputs:[{varying:'out_0', components:1, offset:0}],
  source:'#version 300 es\nprecision highp float; layout(location=0) in vec3 v_position; out float out_0; void main(){out_0=v_position.x*2.0;gl_Position=vec4(0,0,0,1);}'};

export function createComputeDispatcher({canvas, onAvailability = () => {}} = {}) {
  let executor, proof, suspended = false, disposed = false, running = false;
  function getExecutor() {
    if (disposed || suspended) throw unavailable('Compute worker is inactive');
    if (!executor) {
      if (!canvas && typeof OffscreenCanvas !== 'function') throw unavailable('Offscreen WebGL canvas is unavailable');
      executor = createComputeExecutor({canvas:canvas ?? new OffscreenCanvas(1,1), onAvailability:(available, reason) => {
        proof = undefined; onAvailability({op:'availability', available, reason});
      }});
    }
    return executor;
  }

  async function probe() {
    const active = getExecutor();
    if (!proof) {
      const start = performance.now();
      // A spare short-lived preparation proves compile, upload, feedback and
      // readback. The private ID cannot collide with a host-created group.
      active.prepare({id:'@probe', plan:calibration, positions:new Float32Array([1,0,0,-3,0,0])});
      try {
        const result = await active.run({id:'@probe', start:0, count:2});
        if (result[0] !== 2 || result[1] !== -6) throw unavailable('Transform feedback probe returned incorrect values');
        proof = {...active.capabilities, probeMs:performance.now()-start};
      } finally {active.release('@probe');}
    }
    return proof;
  }

  return async function dispatch(data) {
    const requestId = data?.requestId;
    if (!(Number.isSafeInteger(requestId) || (typeof requestId === 'string' && requestId.length <= 256)))
      return {ok:false, error:{code:'invalid-input', message:'Invalid request correlation ID'}};
    const answer = (value = {}) => ({requestId, ok:true, ...value});
    let ownsJob = false;
    try {
      if (data.op === 'dispose') {
        disposed = true; executor?.dispose(); executor = null; return answer();
      }
      if (disposed) throw unavailable('Compute worker was disposed');
      if (data.op === 'suspend') {
        suspended = true; proof = undefined; executor?.suspend(); return answer();
      }
      if (data.op === 'resume') {suspended = false; executor?.resume(); return answer();}
      if (running) throw Object.assign(new Error('Worker already has an in-flight job'), {code:'busy'});
      running = true; ownsJob = true;
      if (data.op === 'probe') {return answer({capabilities:await probe()});}
      if (data.id === '@probe') throw unavailable('Reserved prepared group identifier');
      const active = getExecutor();
      if (data.op === 'prepare') {if (!proof) await probe(); return answer(active.prepare(data));}
      if (data.op === 'run') {
        const values = await active.run(data);
        return answer({values});
      }
      throw Object.assign(new Error('Unknown compute worker operation'), {code:'invalid-input'});
    } catch (error) {
      return {requestId, ok:false, error:{code:error.code || 'gpu-error', message:String(error.message).slice(0,512)}};
    } finally {if (ownsJob) running = false;}
  };
}
