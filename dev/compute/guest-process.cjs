// Exercise the full-app GPU bundle over a stage-owned loopback WebSocket.
// This replaces the Apple radio link only, and never gives the page a stage credential.
const {chromium} = require('playwright-core');
const {randomUUID} = require('node:crypto');
const {existsSync, writeFileSync} = require('node:fs');
const {join, resolve} = require('node:path');
const {setTimeout: delay} = require('node:timers/promises');

function fields(text) {
  const [tag, ...entries] = JSON.parse(text);
  if (tag !== '^ ' || entries.length % 2 !== 0) throw Error('Expected a Transit map');
  const pairs = {};
  for (let i = 0; i < entries.length; i += 2) pairs[entries[i]] = entries[i + 1];
  return pairs;
}

async function peer(origin, bundle, binary, dir, index) {
  const env = {...process.env};
  delete env.TOLGLOW_AUDIENCE_COMPUTE_TOKEN;
  delete env.TOLGLOW_COMPUTE_TOKEN;
  delete env.TOLGLOW_STUDIO_TOKEN;
  const browser = await chromium.launch({executablePath: binary, env,
    args: ['--no-sandbox','--use-angle=swiftshader','--enable-unsafe-swiftshader']});
  try {
    const page = await browser.newPage();
    await page.addScriptTag({path:bundle});
    const available = await page.evaluate(async () => {
      window.gpu = AudienceGPU.start(document.createElement('canvas'));
      return gpu.probe();
    });
    if (!available) throw Error('Bundled guest failed GPU calibration');
    const socket = new WebSocket(origin.replace('http:', 'ws:') + '/api/compute/stream', {headers:{Origin:origin}});
    const caps = ['^ ','~:max-result-floats',8192,'~:guest?',true,'~:subject',randomUUID()];
    socket.onopen = () => socket.send(JSON.stringify(['^ ','~:op','~:hello','~:token',
      process.env.TOLGLOW_AUDIENCE_COMPUTE_TOKEN,'~:caps',caps]));
    const stats = {policy:null, prepared:0, returned:0, dropped:0, rows:0};
    let chain = Promise.resolve();
    const failed = new Promise((_, reject) => {
      socket.onerror = () => reject(Error('Stage compute socket failed'));
      socket.onmessage = ({data}) => {
        chain = chain.then(async () => {
          const msg = fields(data), op = msg['~:op'];
          if (op === '~:welcome') {
            const policy = msg['~:verification'];
            if (policy !== '~:' + (process.env.TOLGLOW_AUDIENCE_VERIFICATION || 'full')) throw Error('Unverified host');
            stats.policy = policy;
            return;
          }
          if (!stats.policy) throw Error('Job arrived before verified admission');
          const replies = await page.evaluate(text => gpu.compute(text), data);
          for (const reply of replies) {
            switch (fields(reply)['~:op']) {
              case '~:ready': stats.prepared++; break;
              case '~:result':
                if (index === 0 && existsSync(join(dir, 'lose-peer'))) {
                  stats.dropped++;
                  socket.close(1000, 'Participant leaves during readback');
                  return;
                }
                stats.returned++;
                stats.rows += msg['~:count'];
                break;
            }
            if (socket.readyState === WebSocket.OPEN) socket.send(reply);
          }
        }).catch(reject);
      };
    });
    return {stats, failed, close:async () => {socket.close(); await browser.close();}};
  } catch (error) { await browser.close(); throw error; }
}

async function main([origin, dir]) {
  if (new URL(origin).hostname !== '127.0.0.1' || !process.env.TOLGLOW_AUDIENCE_COMPUTE_TOKEN)
    throw Error('Run only against the local guest-admission proof listener with a fresh stage credential');
  const bundle = resolve(process.env.TOLGLOW_GUEST_BUNDLE || 'native/AudienceCompute/Phone/audience-compute.js');
  const binary = process.env.TOLGLOW_QA_CHROMIUM || '/usr/bin/chromium';
  const peers = [];
  try {
    // Probe each new context before adding a second one to keep cold compilation measurable.
    for (let i = 0; i < 2; i++) peers.push(await peer(origin,bundle,binary,dir,i));
    const done = async () => {
      const end = Date.now() + 125000;
      while (!existsSync(join(dir, 'finish'))) {
        if (Date.now() > end) throw Error('No finish receipt from host');
        await delay(50);
      }
    };
    await Promise.race([done(), ...peers.map(p => p.failed)]);
    if (peers.some(p => !p.stats.policy || !p.stats.returned) || peers[0].stats.dropped !== 1)
      throw Error('Missing verified enrollment, working GPU or intentional peer loss');
    writeFileSync(join(dir, 'guest.json'), JSON.stringify(peers.map(p => p.stats), null, 2));
  } finally { await Promise.all(peers.map(p => p.close())); }
}
main(process.argv.slice(2)).catch(error => {console.error(error); process.exitCode = 1;});
