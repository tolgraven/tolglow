// Run via lein run -m tolglow.qa.workspace-status after building Studio.
import assert from 'node:assert/strict';
import {once} from 'node:events';
import {mkdir, writeFile} from 'node:fs/promises';
import {connect} from 'node:net';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, output, gate] = process.argv.slice(2);
const browser = await chromium.launch({executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium'});

async function snapshot(request) {
  const response = await request.get(origin + '/api/state', {headers: {Accept: 'application/json'}});
  assert.ok(response.ok(), 'Read the public show snapshot');
  return response.json();
}

try {
  await mkdir(output, {recursive: true});
  const page = await browser.newPage({viewport: {width: 1024, height: 820}});
  const reads = [], errors = [];
  page.on('pageerror', error => errors.push(error.message));
  page.on('request', request => {
    if (new URL(request.url()).pathname === '/api/workspaces/status') reads.push(request.method());
  });
  await page.goto(origin + '/develop');
  await page.waitForFunction(() => document.querySelector('.workspace-selection select')?.value === 'lighting');
  await page.getByRole('button', {name: 'Checks & preview', exact: true}).click();
  const card = page.locator('.workspace-check').first();
  await card.getByRole('button', {name: 'Run check', exact: true}).waitFor();
  const capture = name => page.screenshot({path: join(output, name + '.png')});
  await capture('ready');

  // Submit from a second operator. A callback from this page's button would bypass the subscription.
  const initial = await snapshot(page.request);
  assert.equal(initial['frame-number'], 0);
  const started = await page.request.post(origin + '/api/workspaces/check', {
    headers: {Accept: 'application/json'},
    data: {'runtime-id': initial['runtime-id'], 'expected-epoch': initial.epoch, id: 'lighting', profile: 'unit'}
  });
  assert.ok(started.ok(), 'Submit the named repository check');
  await card.locator('[data-check-status="running"]').waitFor();
  await capture('running');
  const release = connect(Number(gate), '127.0.0.1');
  await once(release, 'connect');
  release.end();
  await card.locator('[data-check-status="passed"]').waitFor();
  await capture('passed');
  assert.equal((await snapshot(page.request))['frame-number'], 0);
  // Covers several intervals of the retired status poll, including a route remount.
  await page.waitForTimeout(3500);
  await page.goto(origin + '/develop');
  await page.getByRole('button', {name: 'Checks & preview', exact: true}).click();
  await card.locator('[data-check-status="passed"]').waitFor();
  assert.deepEqual(reads, []);
  assert.deepEqual(errors, []);
  await writeFile(join(output, 'results.json'), JSON.stringify({
    externalCheck: ['running', 'passed'], frameNumber: 0, resumedStatus: 'passed',
    idleObservationMs: 3500, statusHTTPReads: reads, pageErrors: errors
  }, null, 2) + '\n');
} finally {
  await browser.close();
}
