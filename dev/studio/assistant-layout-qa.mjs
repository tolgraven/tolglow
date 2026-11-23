// Run against an empty, disposable preview-only Studio. Changes only its model connection.
import assert from 'node:assert/strict';
import { mkdir, writeFile } from 'node:fs/promises';
import { createServer } from 'node:http';
import { join } from 'node:path';
import { chromium } from 'playwright-core';

const [origin, directory, mode] = process.argv.slice(2);
const before = mode === 'before';
const screens = [['phone',390,844], ['ipad',1024,768], ['desktop',1440,900]];
const measurements = {};
const browser = await chromium.launch({ executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium' });
await mkdir(directory, { recursive: true });
const calls = [];
const provider = createServer(async (req, res) => {
  let body = '';
  for await (const chunk of req) body += chunk;
  calls.push(JSON.parse(body));
  res.writeHead(200, {'Content-Type':'text/event-stream'});
  res.end('data: {"choices":[{"delta":{"content":"The preview rig is ready."}}]}\n\ndata: {"choices":[{"delta":{},"finish_reason":"stop"}]}\n\ndata: [DONE]\n\n');
});
await new Promise(resolve => provider.listen(0, '127.0.0.1', resolve));
const endpoint = 'http://127.0.0.1:' + provider.address().port + '/v1/chat/completions';

async function open(name, width, height) {
  const page = await browser.newPage({ viewport:{width,height}, hasTouch:name !== 'desktop' });
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.goto(origin + '/assistant');
  await page.locator('#view-assistant:not([hidden])').waitFor();
  await page.getByText('Studio connected', {exact:true}).waitFor({state:'attached'});
  return {page, errors};
}
async function measure(page) {
  return page.evaluate(() => {
    const rect = sel => {
      const {x,y,width,height} = document.querySelector(sel).getBoundingClientRect();
      return [x,y,width,height].map(Math.round);
    };
    return {provider:rect('#provider-kind'),model:rect('#provider-model'),save:rect('#save-provider'),
            overflow:document.documentElement.scrollWidth-innerWidth};
  });
}
async function verifyEntry(name, width, height) {
  const {page, errors} = await open(name,width,height);
  try {
    await page.locator('#provider-credential').waitFor();
    measurements[name] = await measure(page);
    await page.screenshot({path:join(directory, name + '-entry.png')});
    if (!before) {
      const check = measurements[name];
      assert.equal(check.overflow, 0);
      assert.ok(check.save[1]+check.save[3] <= height, name + ' save above fold');
      for (const key of ['provider','model','save']) assert.ok(check[key][3]>=44, name + ' ' + key + ' touch size');
      assert.equal(await page.getByLabel('Clear saved key').count(),0);
      assert.equal(await page.locator('#provider-endpoint').isVisible(),false);
      await page.getByText('Custom endpoint', {exact:true}).click();
      await page.locator('#provider-endpoint').waitFor();
      await page.locator('#provider-kind').selectOption('self-hosted');
      await page.getByLabel('Server endpoint').waitFor();
      assert.equal(await page.locator('#chat-send').isEnabled(),false);
    }
    assert.deepEqual(errors, []);
  } finally { await page.close(); }
}
async function verifyRetry() {
  const page = await browser.newPage();
  try {
    await page.route('**/api/providers', route => route.abort());
    await page.goto(origin + '/assistant');
    await page.getByRole('button',{name:'Retry connection settings'}).waitFor();
    assert.equal(await page.locator('#save-provider').count(),0);
    await page.unroute('**/api/providers');
    await page.getByRole('button',{name:'Retry connection settings'}).click();
    await page.locator('#provider-kind').waitFor();
  } finally { await page.close(); }
}
async function verifyConnection() {
  const {page, errors} = await open('ipad',1024,768);
  try {
    await page.locator('#chat-prompt').fill('Inspect the preview rig.');
    await page.locator('#provider-kind').selectOption('self-hosted');
    await page.locator('#provider-model').fill('studio-check');
    await page.locator('#provider-endpoint').fill(endpoint);
    await page.screenshot({path:join(directory,'ipad-setup.png')});
    await page.locator('#provider-endpoint').fill('ftp://localhost/model');
    await page.locator('#save-provider').click();
    await page.locator('.provider-panel [role="alert"]').waitFor();
    assert.equal(calls.length,0);
    await page.locator('#provider-endpoint').fill(endpoint);
    await page.locator('#provider-credential').fill('local-layout-test');
    await page.locator('#provider-credential').press('Enter');
    await page.locator('.chat-settings .provider-panel:not([open])').waitFor();
    assert.equal(calls.length,0);
    assert.equal(await page.locator('#chat-prompt').inputValue(),'Inspect the preview rig.');
    await page.screenshot({path:join(directory,'ipad-ready.png')});
    await page.locator('#chat-send').click();
    await page.getByText('The preview rig is ready.',{exact:true}).waitFor();
    assert.equal(calls.length,1);
    await page.reload();
    await page.getByText('The preview rig is ready.',{exact:true}).waitFor();
    assert.equal(calls.length,1);
    await page.screenshot({path:join(directory,'ipad-conversation.png')});
    await page.locator('.chat-settings .provider-panel summary').click();
    assert.equal(await page.locator('#provider-credential').inputValue(),'');
    await page.getByLabel('Clear saved key').click();
    await page.waitForFunction(() => document.querySelector('.provider-clear input')?.checked);
    await page.locator('#save-provider').click();
    await page.getByLabel('Clear saved key').waitFor({state:'detached'});
    assert.equal(calls.length,1);
    assert.deepEqual(errors,[]);
  } finally { await page.close(); }
}
try {
  for(const dimensions of screens) await verifyEntry(...dimensions);
  if (!before) {
    await verifyRetry();
    await verifyConnection();
  }
  await writeFile(join(directory,'metrics.json'),JSON.stringify(measurements,null,2));
  console.log(JSON.stringify(measurements));
} finally {
  await browser.close();
  await new Promise(resolve => provider.close(resolve));
}
