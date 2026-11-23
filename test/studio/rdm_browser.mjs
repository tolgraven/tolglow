import {chromium} from 'playwright-core';
import fs from 'node:fs';
// Run only against dev/tolglow/rdm_qa.clj, which owns the external loopback peer.
const connection=JSON.parse(fs.readFileSync(process.env.RDM_QA_CONNECTION_FILE || 'target/rdm-qa.json','utf8'));
if (connection['loopback-peer'] !== true || !/^http:\/\/127\.0\.0\.1:\d+$/.test(connection['studio-url'])) throw new Error('Expected owned loopback QA connection');
const base=connection['studio-url'];
const direct = process.env.RDM_QA_TRANSPORT === 'art-net';
const artifacts=process.env.RDM_QA_ARTIFACTS || 'target/rdm-browser';
fs.mkdirSync(artifacts,{recursive:true});
const browser=await chromium.launch({executablePath:process.env.STUDIO_CHROMIUM || '/usr/bin/chromium',headless:true,args:['--no-sandbox','--disable-dev-shm-usage']});
const report={checks:[],errors:[],screenshots:[]};
const page=await browser.newPage({viewport:{width:1440,height:1000}});
page.on('pageerror',e=>report.errors.push(e.message));
let statusFetches = 0;
page.on('request', request => {
  if (request.method() === 'GET' && new URL(request.url()).pathname === '/api/rdm') statusFetches++;
});
const check=(name,ok)=>{if(!ok)throw new Error(name);report.checks.push(name)};
try {
 await page.goto(`${base}/rig`);
 const panel=page.getByRole('region',{name:'RDM fixture management'});
 await panel.waitFor();
 await page.getByRole('button',{name:'Enable RDM',exact:true}).waitFor();
 check('passive initial status', (await page.request.get(`${base}/api/rdm`).then(r=>r.json())).jobs.length===0);
 if (direct) {
   const config = connection['art-net'];
   await page.getByLabel('RDM transport').selectOption('art-net');
   await page.getByLabel('Gateway IPv4 address').fill(config.host);
   await page.getByLabel('Gateway UDP port').fill(String(config.port));
   await page.getByLabel('Controller UID').fill(config['controller-uid']);
   await page.getByText('Connection settings', {exact: true}).click();
   await page.getByLabel('Local UDP port').fill('0');
   const form = panel.locator('.rdm-session');
   for (const [name, width, height] of [['ipad', 1024, 768], ['phone', 390, 844]]) {
     await page.setViewportSize({width, height});
     await form.scrollIntoViewIfNeeded();
     await form.screenshot({path: `${artifacts}/gateway-${name}.png`});
     check(`${name} gateway form fits`, await form.evaluate(el => el.scrollWidth <= el.clientWidth + 1));
   }
   await page.setViewportSize({width: 1440, height: 1000});
 } else {
   await page.getByLabel('OLA TCP port',{exact:true}).fill(String(connection['ola-port']));
 }
 await page.getByRole('button',{name:'Enable RDM',exact:true}).click();
 await page.getByRole('button',{name:'Disable RDM',exact:true}).waitFor();
 await page.getByRole('button',{name:'Discover',exact:true}).click();
 await page.getByLabel('Discovered device',{exact:true}).waitFor();
 await page.getByLabel('Discovered device',{exact:true}).selectOption('1234:01020304');
 await page.getByRole('button',{name:'Read device',exact:true}).click();
 await panel.getByText('Test Lighting',{exact:true}).waitFor();
 check('discovery and typed readback',true);
 await panel.locator('.rdm-fixture').first().click();
 await page.getByLabel('Discovered device',{exact:true}).selectOption('1234:01020304');
 await page.getByRole('button',{name:'Link selected fixture',exact:true}).click();
 await panel.getByText('Fixture linked. Its show patch is unchanged.',{exact:true}).waitFor();
 check('explicit fixture linking',true);
 await page.getByLabel('New DMX address',{exact:true}).fill('13');
 await page.getByRole('button',{name:'Set device address',exact:true}).click();
 await panel.getByRole('alert').waitFor();
 check('overlap rejected visibly', (await panel.getByRole('alert').innerText()).toLowerCase().includes('overlap'));
 await page.getByLabel('New DMX address',{exact:true}).fill('200');
 await page.getByRole('button',{name:'Set device address',exact:true}).click();
 await panel.getByText('200 / 1',{exact:true}).waitFor();
 let status=await page.request.get(`${base}/api/rdm`).then(r=>r.json());
 check('physical write keeps show address',status.fixtures[0].address===1);
 await page.getByRole('button',{name:'Adopt observed address',exact:true}).click();
 await panel.getByText('Observed address adopted into the show patch.',{exact:true}).waitFor();
 status=await page.request.get(`${base}/api/rdm`).then(r=>r.json());
 check('explicit show patch adoption',status.fixtures[0].address===200);
 await page.getByLabel('New device label',{exact:true}).fill('Front left');
 await page.getByRole('button',{name:'Set label',exact:true}).click();
 await panel.getByText('Front left',{exact:true}).waitFor();
 check('device label write and readback',true);
 await page.getByRole('button',{name:'Identify on',exact:true}).click();
 await panel.getByLabel('Observed identify').filter({hasText: /^On$/}).waitFor();
 await page.getByRole('button',{name:'Identify off',exact:true}).click();
 await panel.getByLabel('Observed identify').filter({hasText: /^Off$/}).waitFor();
 check('identify toggles and readback',true);
 await panel.getByText('Advanced GET / SET',{exact:true}).click();
 await page.getByLabel('Parameter ID',{exact:true}).fill('0x9999');
 await page.getByRole('button',{name:'Send command',exact:true}).click();
 await panel.getByText('Device response: unknown-pid',{exact:true}).waitFor();
 check('generic unsupported PID remains visible',true);
 for(const [name,width,height] of [['desktop',1440,1000],['ipad',1024,768],['phone',390,844]]) {
   await page.setViewportSize({width,height});
   await panel.scrollIntoViewIfNeeded();
   await page.screenshot({path:`${artifacts}/${name}.png`,fullPage:false});
   check(`${name} no horizontal overflow`,await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth+1));
   report.screenshots.push(`${artifacts}/${name}.png`);
 }
 await page.getByRole('button',{name:'Disable RDM',exact:true}).click();
 await page.getByRole('button',{name:'Enable RDM',exact:true}).waitFor();
 check('explicit disconnect',true);
 check('all Rig status changes arrived by push', statusFetches === 0);
 check('no component errors',report.errors.length===0);
 console.log(`Passed ${report.checks.length} RDM browser checks; report: ${artifacts}/report.json`);
} finally {fs.writeFileSync(`${artifacts}/report.json`,JSON.stringify(report,null,2));await browser.close();}
