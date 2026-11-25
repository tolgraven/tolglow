import {createHash} from 'node:crypto';
import {readFile, writeFile} from 'node:fs/promises';
import {createServer} from 'node:http';
import {setTimeout as sleep} from 'node:timers/promises';
import {join} from 'node:path';
import {gunzipSync} from 'node:zlib';
import {startHiddenChrome} from './hidden-chrome-client.mjs';

const variants = (process.env.WORKERS || 'old,new,new,old').split(',');
const baseline = process.env.BASELINE_STUDIO_DIR;
const candidate = process.env.CANDIDATE_STUDIO_DIR || 'resources/studio';
const receipt = process.env.RECEIPT;
if (!receipt) throw Error('Set RECEIPT to an output path outside the repository.');
if (variants.some(variant => variant !== 'old' && variant !== 'new')) {
  throw Error('WORKERS must contain only the old and new labels.');
}
if (variants.includes('old') && !baseline) {
  throw Error('Set BASELINE_STUDIO_DIR to the directory containing the baseline release assets.');
}
const oldWorker = baseline ? await readFile(join(baseline, 'preview-worker.js')) : null;
const oldProxy = baseline ? await readFile(join(baseline, 'preview.js')) : null;
const newWorker = await readFile(join(candidate, 'preview-worker.js'));
const proxy = await readFile(join(candidate, 'preview.js'));
const recorded = await readFile(process.env.ROOM_REPLAY || new URL('./mixed-stage-motion.json.gz', import.meta.url));
const frames = recorded[0] === 0x1f && recorded[1] === 0x8b ? gunzipSync(recorded) : recorded;
const clock = `
let index = 0, pulses = 0;
const pending = new Map();
self.requestAnimationFrame = cb => {pending.set(++index, cb); return index;};
self.cancelAnimationFrame = id => pending.delete(id);
self.addEventListener('message', event => {
  if (event.data.type !== 'test-vsync') return;
  if (++pulses % 80 === 0) postMessage({type:'clock-debug', pending:pending.size, requests:index, diff:event.data.absolute - performance.timeOrigin - performance.now()});
  const due = [...pending.values()];
  pending.clear();
  for (const cb of due) cb(event.data.absolute - performance.timeOrigin);
});
await import('/' + new URL(location.href).searchParams.get('worker') + '.mjs');`;

const pageContent = `<!doctype html><style>body{margin:0}canvas{display:block;width:1817px;height:1092px}</style><canvas></canvas><output></output><script type="module">
const data = await fetch('/replay.json').then(r=>r.json());
const canvas = document.querySelector('canvas');
const status = document.querySelector('output');
const variant = new URL(location.href).searchParams.get('worker');
const {RoomPreview} = await import(variant === 'old' ? '/master-proxy.mjs' : '/preview.mjs');
const preview = new RoomPreview(canvas, status, {
  workerFactory: () => new Worker('/driver.mjs?worker=' + variant, {type:'module'})
});
const testRate = Number(new URL(location.href).searchParams.get('hz'));
preview.displayHz = testRate;
const state = {events:[], failures:[], lastIndex:-1};
preview.setQuality('mobile');
preview.setLightingMode('interpolate');
preview.setWorkLight(0);
preview.setCamera(new URL(location.href).searchParams.get('camera'));
preview.observedFrame = (_canvas, id, epoch, showFrame) => state.events.push(['presented', performance.now(), showFrame,
  data.frames[state.lastIndex % data.frames.length]?.['frame-number']]);
const post = preview.worker.postMessage.bind(preview.worker);
preview.worker.postMessage = (...args) => {
  const [message] = args;
  if (message.type === 'state') state.events.push(['state-post', performance.now(), message.id, message.state.snapshot?.['frame-number']]);
  return post(...args);
};
preview.worker?.addEventListener('message', ({data}) => {
  if (data.type === 'clock-debug') state.events.push(['clock-debug',performance.now(),data]);
  if (data.type === 'error') state.failures.push(data.message);
  if (['frame','render-start','render-retired','state-ack'].includes(data.type)) {
    state.events.push([data.type,performance.now(),data.frameId??data.id]);
  }
});
preview.update({...data.frames[0], rig:data.rig});
if (!preview.worker) throw Error('Worker missing');
await new Promise(resolve => {
  preview.worker.addEventListener('message',function ready(event) {
    if (event.data.type !== 'ready') return;
    preview.worker.removeEventListener('message',ready);
    resolve();
  });
});
canvas.dispatchEvent(new CustomEvent('tolglow-preview-external',{detail:{owner:'replay',active:true}}));
window.driverPulse = (index, pulse) => {
  if (index !== null && index !== state.lastIndex) {
    state.lastIndex = index;
    state.events.push(['host', performance.now(), data.frames[index % data.frames.length]['frame-number']]);
    preview.update({...data.frames[index % data.frames.length],rig:data.rig});
  }
  if (pulse) preview.worker?.postMessage({type:'test-vsync',absolute:performance.timeOrigin+performance.now()});
  if (state.failures.length || preview.fallback || !preview.worker) {
    throw Error(status.textContent + ' ' + state.failures.join('; '));
  }
};
window.inspect = () => ({
  hidden:document.hidden, focused:document.hasFocus(), ready:preview.ready, status:status.textContent,
  telemetry:preview.getTelemetry(), backing:[canvas.width,canvas.height],
  css:[canvas.clientWidth,canvas.clientHeight], dpr:devicePixelRatio, events:state.events.slice(), boundary:performance.now()
});
window.shutdown = () => preview.dispose();
</script>`;
const routes = {'/': ['text/html',pageContent], '/preview.mjs':['text/javascript',proxy], '/driver.mjs':['text/javascript',clock],
  '/old.mjs':['text/javascript',oldWorker], '/master-proxy.mjs':['text/javascript',oldProxy],
  '/new.mjs':['text/javascript',newWorker], '/replay.json':['application/json',frames]};
const server = createServer((req,res)=>{
  const route = routes[new URL(req.url,'http://127.0.0.1').pathname];
  if (!route) {res.writeHead(404).end();return;}
  res.writeHead(200,{'Content-Type':route[0]});res.end(route[1]);
});
await new Promise(resolve=>server.listen(0,'127.0.0.1',resolve));
const hash = content => createHash('sha256').update(content).digest('hex');
const result = {sha256:{proxy:hash(proxy),oldWorker:oldWorker && hash(oldWorker),oldProxy:oldProxy && hash(oldProxy),newWorker:hash(newWorker),replay:hash(frames)}, lanes:[]};
const measure = async (variant, camera, warmMs=4600, measureMs=6000, hz=60) => {
  const chrome = await startHiddenChrome();
  const pulses = [];
  async function drive(ms, offset) {
    const start = performance.now();
    const period = 1000 / hz, phase = 7;
    let nextDraw = 0, nextInput = Math.ceil(offset / 25);
    while (Math.min(nextDraw * period, nextInput * 25 + phase - offset) < ms) {
      const vsync = nextDraw * period;
      const input = nextInput * 25 + phase - offset;
      const at = Math.min(vsync, input);
      const pause = start + at - performance.now();
      if (pause > 0) await sleep(pause);
      const pulse = vsync === at;
      const index = input === at ? nextInput++ : null;
      if (pulse) {pulses.push(performance.now() - start + offset); nextDraw++;}
      await chrome.evaluate((i, draw) => window.driverPulse(i, draw), index, pulse);
    }
  }
  try {
    await chrome.page('Page.navigate',{url:'http://127.0.0.1:' + server.address().port + '/?worker=' + variant + '&camera=' + camera + '&hz=' + hz});
    await chrome.waitFor(()=>typeof window.driverPulse === 'function');
    await drive(warmMs, 0);
    const first = await chrome.evaluate(()=>window.inspect());
    if (!first.hidden || first.focused || !first.ready) throw Error(JSON.stringify({...first,events:first.events.slice(-12), clock:first.events.filter(e=>e[0]==='clock-debug'), starts:first.events.filter(e=>e[0]==='render-start').length}));
    await drive(measureMs, warmMs);
    const end = await chrome.evaluate(()=>window.inspect());
    if (!end.hidden || end.focused || !end.ready || chrome.pageErrors.length ||
        end.telemetry.thread !== 'worker' || end.telemetry.softwareRenderer || end.telemetry.targetRefreshHz !== hz) {
      throw Error(JSON.stringify({errors:chrome.pageErrors,...end,events:end.events.slice(-12)}));
    }
    const history = end.events.filter(event => event[1] >= first.boundary);
    const presented = history.filter(event=>event[0]==='presented');
    const gaps = presented.slice(1).map((event,i)=>event[1]-presented[i][1]).sort((a,b)=>a-b);
    let unpublished = 0, peakUnpublished = 0;
    for (const [kind] of history) {
      if (kind==='render-start') unpublished++;
      if (kind==='frame' || kind==='render-retired') unpublished=Math.max(0,unpublished-1);
      peakUnpublished=Math.max(peakUnpublished,unpublished);
    }
    const duration = (end.boundary-first.boundary)/1000;
    const arrivals = new Map(end.events.filter(e=>e[0]==='host').map(e=>[e[2],e[1]]));
    const inputDelays = presented.filter(e=>arrivals.has(e[2])).map(e=>e[1]-arrivals.get(e[2])).sort((a,b)=>a-b);
    const ages = presented.map(event => event[3] - event[2]).sort((a,b)=>a-b);
    const seen = new Map(end.events.filter(event=>event[0]==='state-post' && event[3]!=null).map(event=>[event[2],event[1]]));
    const applyDelays = history.filter(event=>event[0]==='state-ack' && seen.has(event[2])).map(event=>event[1]-seen.get(event[2])).sort((a,b)=>a-b);
    const q = (a,p) => a[Math.ceil(a.length*p)-1];
    const host = history.filter(event=>event[0]==='host').length;
    const freshness = {host, inputToPresentationP50:q(inputDelays,.5),inputToPresentationP95:q(inputDelays,.95), ageFramesP50:q(ages,.5), ageFramesP95:q(ages,.95),
      applyMsP50:q(applyDelays,.5),applyMsP95:q(applyDelays,.95),applied:applyDelays.length};
    await writeFile(receipt+'-'+result.lanes.length+'-events.json',JSON.stringify(history));
    const lane = {variant,camera,hz, duration, frames:presented.length, fps:presented.length/duration,
      gapP50:gaps[Math.ceil(gaps.length*.5)-1], gapP95:gaps[Math.ceil(gaps.length*.95)-1],
      uniqueStates:new Set(presented.map(event=>event[2])).size,
      peakUnpublished, freshness, pulses:pulses.filter(p=>p>=warmMs).length, css:end.css, backing:end.backing, telemetry:end.telemetry};
    result.lanes.push(lane);
    await writeFile(receipt,JSON.stringify(result,null,2));
    console.log(JSON.stringify({...lane,telemetry:Object.fromEntries(['pixels','deviceRenderer','thread','displayPixels'].map(k=>[k,end.telemetry[k]]))}));
    await chrome.evaluate(()=>window.shutdown());
  } finally {await chrome.close();}
};
try {
  const hz = Number(process.env.BENCH_HZ || 60);
  const camera = process.env.CAMERA || 'audience';
  for (const variant of variants) await measure(variant, camera, 4600, 6000, hz);
} finally {await new Promise(resolve=>server.close(resolve));}
