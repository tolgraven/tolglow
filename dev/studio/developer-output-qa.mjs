// Run via lein run -m tolglow.qa.developer-output after building Studio and its local Docker image.
import assert from 'node:assert/strict';
import {once} from 'node:events';
import {connect} from 'node:net';
import {mkdir, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium} from 'playwright-core';

const [origin, output, providerGate] = process.argv.slice(2);
const browser = await chromium.launch({executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium'});
let page;
try {
  await mkdir(output, {recursive: true});
  page = await browser.newPage({viewport: {width: 1100, height: 845}});
  const reads = [], errors = [], queries = [], bodies = [];
  page.setDefaultTimeout(90000);
  page.on('pageerror', error => errors.push(error.message));
  page.on('request', req => {
    const path = new URL(req.url()).pathname;
    if (req.method() === 'GET' && ['/api/application-agents/events', '/api/terminal'].includes(path)) reads.push(path);
  });
  page.on('websocket', socket => {
    socket.on('framesent', ({payload}) => {
      const data = payload.toString();
      if (data.includes('~:queries')) queries.push(data);
    });
    socket.on('framereceived', ({payload}) => bodies.push(payload.toString()));
  });
  const pane = name => page.getByRole('navigation', {name: 'Development workspace views'})
                            .getByRole('button', {name, exact: true});
  const photo = name => page.screenshot({path: join(output, name + '.png')});
  await page.goto(origin + '/develop');
  await page.waitForFunction(() => document.querySelector('.workspace-selection select')?.value === 'lighting');
  await pane('Coding agent').click();
  await page.locator('#application-agent-prompt').fill('Check the fixture configuration.');
  await page.locator('#application-agent-start').click();
  const transcript = page.getByLabel('Application agent progress stream');
  await transcript.getByText('Checking the fixture configuration.').waitFor();
  await photo('agent-running');
  await pane('Files').click();
  const signal = connect(Number(providerGate), '127.0.0.1');
  await once(signal, 'connect');
  signal.end();
  await pane('Coding agent').click();
  await transcript.getByText('The fixture configuration is ready.').waitFor();
  await page.locator('.application-agent-progress-heading').getByText('completed', {exact: true}).waitFor();
  await photo('agent-completed');

  await pane('Terminal').click();
  await page.getByRole('button', {name: 'Open Docker shell', exact: true}).click();
  const running = page.getByRole('button', {name: 'End shell', exact: true});
  await running.waitFor();
  const term = page.locator('.xterm-helper-textarea');
  await term.focus();
  await page.keyboard.type("printf 'studio %s via %s\\n' output socket");
  await page.keyboard.press('Enter');
  await page.waitForFunction(() => document.querySelector('.xterm-rows')?.textContent.includes('studio output via socket'));
  await photo('terminal-running');
  await pane('Files').click();
  await pane('Terminal').click();
  await page.waitForFunction(() => document.querySelector('.xterm-rows')?.textContent.includes('studio output via socket'));
  await term.focus();
  await page.keyboard.type('exit 0');
  await page.keyboard.press('Enter');
  await page.getByText('Process exited with code 0').waitFor();
  await photo('terminal-exited');
  await pane('Files').click();
  await page.waitForTimeout(1600);
  assert.deepEqual(reads, []);
  assert.deepEqual(errors, []);
  assert.ok(queries.some(p => p.includes('~:development/task-events')));
  assert.ok(queries.some(p => p.includes('~:development/terminal-output')));
  assert.ok(!queries.at(-1).includes('~:development/task-events') && !queries.at(-1).includes('~:development/terminal-output'));
  assert.ok(!bodies.some(p => p.includes('fixture-key-not-a-real-credential')));
  const show = await (await page.request.get(origin + '/api/state', {headers: {Accept: 'application/json'}})).json();
  assert.equal(show['frame-number'], 0);
  await writeFile(join(output, 'results.json'), JSON.stringify({
    agent: 'fixture messages replayed and completed', terminal: 'Docker zsh remount replay and exit 0',
    finalPane: 'Files', frame: 0, outputPollingRequests: reads, pageErrors: errors
  }, null, 2) + '\n');
} catch (error) {
  if (page) {
    console.error((await page.locator('body').innerText()).slice(0,2600));
    await page.screenshot({path: join(output, 'failed.png')});
  }
  throw error;
} finally {
  await browser.close();
}
