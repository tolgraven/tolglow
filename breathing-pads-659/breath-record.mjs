// Screencast the Perform cue grid for a few seconds (#659). node breath-record.mjs <origin> <out-dir>
import fs from 'node:fs';
import path from 'node:path';
import {createRequire} from 'node:module';
const require = createRequire(process.env.PLAYWRIGHT_FROM + '/package.json');
const {chromium} = require('playwright-core');
const [origin, out] = process.argv.slice(2);
fs.mkdirSync(path.join(out, 'cast'), {recursive: true});
const sleep = ms => new Promise(r => setTimeout(r, ms));
const read = async (p, o = {}) => (await fetch(origin + p, {...o, headers: {accept: 'application/json', 'content-type': 'application/json'}})).json();
const show = await read('/api/state');
const command = (op, body) => read('/api/command', {method: 'POST', body: JSON.stringify({op, epoch: show.epoch,
  'show-id': show['show-id'], 'runtime-id': show['runtime-id'], 'command-id': crypto.randomUUID(),
  source: {adapter: 'web', 'session-id': 'breath-rec'}, ...body})});
const endAll = async () => { for (const i of (await read('/api/state')).instances) await command('instance/end', {'instance-id': i.id}); };
const browser = await chromium.launch({executablePath: '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome', headless: true});
try {
  const context = await browser.newContext({viewport: {width: 1200, height: 900}, deviceScaleFactor: 1});
  const page = await context.newPage();
  await page.goto(origin + '/perform');
  await page.getByRole('button', {name: 'Night studies'}).click();
  await page.locator('#cue-grid .cue-cell:not(.empty)').first().waitFor();
  await endAll(); await sleep(2500);
  for (const cue of ['amber', 'ember', 'ice', 'indigo', 'orchid', 'sage', 'breathe', 'sweep']) await command('cue/launch', {'cue-id': cue, launch: {mode: 'persistent'}});
  const moon = await command('cue/launch', {'cue-id': 'moon', launch: {mode: 'persistent'}});
  await command('cue/launch', {'cue-id': 'spot', launch: {mode: 'persistent'}, grid: {phrases: 1}});
  await page.locator('#cue-grid .cue-cell.queued').first().waitFor();
  await sleep(1000);
  const box = await page.locator('#cue-grid').boundingBox();
  const cdp = await context.newCDPSession(page);
  const frames = [];
  cdp.on('Page.screencastFrame', async f => {
    frames.push({t: f.metadata.timestamp, data: f.data});
    await cdp.send('Page.screencastFrameAck', {sessionId: f.sessionId}).catch(() => {});
  });
  await cdp.send('Page.startScreencast', {format: 'png', everyNthFrame: 1});
  setTimeout(() => command('instance/end', {'instance-id': moon['instance-id']}), 1500);
  await sleep(5000);
  await cdp.send('Page.stopScreencast');
  const name = i => `cast/c${String(i).padStart(4, '0')}.png`;
  frames.forEach((f, i) => fs.writeFileSync(path.join(out, name(i)), Buffer.from(f.data, 'base64')));
  const list = frames.map((f, i) => `file '${name(i)}'\nduration ${((frames[i + 1]?.t ?? f.t + 0.033) - f.t).toFixed(4)}`).join('\n');
  fs.writeFileSync(path.join(out, 'cast.txt'), list + `\nfile '${name(frames.length - 1)}'\n`);
  fs.writeFileSync(path.join(out, 'cast.json'), JSON.stringify({frames: frames.length, seconds: frames.at(-1).t - frames[0].t, crop: box}));
  console.log(frames.length, frames.at(-1).t - frames[0].t, JSON.stringify(box));
  await endAll();
} finally { await browser.close(); }
