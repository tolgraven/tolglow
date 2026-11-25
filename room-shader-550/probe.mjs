import {eligiblePlan, findSource, schema, sourceKey} from './plan.mjs';

const mode = new URL(self.location).searchParams.get('mode');
const report = {programs: {}, stages: [], hooks: {}, planFetchMs: 0, planJsonMs: 0};
const saved = new Map();
let entries = null;
const add = (entry, label, elapsed) => entry[label] = (entry[label] || 0) + elapsed;

export const probe = {
  phase: 'initialization',
  async load() {
    if (!['restore', 'stale'].includes(mode)) return;
    const before = performance.now();
    const response = await fetch('/plan/' + mode);
    if (!response.ok) throw new Error('Probe plan unavailable');
    const json = await response.text();
    report.planFetchMs = performance.now() - before;
    const parse = performance.now();
    entries = eligiblePlan(JSON.parse(json), ROOM_BUILD);
    report.planJsonMs = performance.now() - parse;
  },
  hook(elapsed) {add(report.hooks, this.phase, elapsed);},
  start(parameters) {
    const elapsedStart = performance.now();
    const record = {name: parameters.shaderName || parameters.shaderType, phase: this.phase,
      spotLights: parameters.numSpotLights, hit: false, assemblyMs: 0, lookupMs: 0};
    if (mode === 'capture' || entries) {
      record.key = sourceKey(parameters);
      record.cached = findSource(entries, record.key, parameters);
      record.hit = Boolean(record.cached);
      record.lookupMs = performance.now() - elapsedStart;
    }
    report.stages.push(record);
    return record;
  },
  assembled(record, parameters, source, elapsed) {
    record.assemblyMs = elapsed;
    if (mode === 'capture') {
      const bucket = saved.get(record.key) || [];
      if (!findSource(saved, record.key, parameters)) bucket.push({
        baseVertex: parameters.vertexShader, baseFragment: parameters.fragmentShader, ...source
      });
      saved.set(record.key, bucket);
    }
    delete record.key;
    delete record.cached;
    return source;
  },
  publish(frameId, timings) {
    postMessage({type: 'room-startup-probe', frameId, timings, report,
      plan: mode === 'capture' ? {schema, build: ROOM_BUILD, entries: [...saved]} : null});
  }
};
