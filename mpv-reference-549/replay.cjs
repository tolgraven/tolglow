const assert = require('node:assert/strict');
const {spawn} = require('node:child_process');
const {once} = require('node:events');
const {existsSync, readFileSync, unlinkSync} = require('node:fs');
const {join} = require('node:path');
const {chromium} = require('playwright-core');

const scratch = process.env.TOLGLOW_REFERENCE_QA;
const output = process.env.TOLGLOW_REFERENCE_EVIDENCE;
const password = process.env.TOLGLOW_REFERENCE_TEST_AUTH;
const ipc = join(scratch, 'player.sock');
const pause = ms => new Promise(resolve => setTimeout(resolve, ms));
let player;

async function startPlayer() {
  if (existsSync(ipc)) unlinkSync(ipc);
  player = spawn(process.env.MPV_BIN || 'mpv', [
    '--no-config', '--ao=null', '--vo=null', '--ytdl=no', '--load-scripts=no', '--loop-file=inf',
    `--input-ipc-server=${ipc}`, join(scratch, 'track.wav'),
  ], {stdio: 'ignore', env: {...process.env,
    ...(process.env.MPV_LIB_DIR ? {LD_LIBRARY_PATH: process.env.MPV_LIB_DIR} : {})}});
  for (let attempt = 0; attempt < 120 && !existsSync(ipc); ++attempt) await pause(25);
  assert.ok(existsSync(ipc), 'mpv should listen on the fixture socket');
}

async function stopPlayer() {
  if (player && player.exitCode === null && player.signalCode === null) {
    const exited = once(player, 'exit');
    player.kill('SIGTERM');
    await exited;
  }
}

async function main() {
  const port = Number(readFileSync(join(scratch, 'port'), 'utf8'));
  const baseURL = `http://127.0.0.1:${port}`;
  const browser = await chromium.launch({
    executablePath: process.env.STUDIO_CHROMIUM || '/usr/bin/chromium',
    args: ['--no-sandbox'],
  });
  try {
    await startPlayer();
    const anonymous = await browser.newContext({baseURL});
    assert.equal((await anonymous.request.get('/api/section-reference')).status(), 401);
    await anonymous.close();

    const viewport = {width: 1152, height: 900};
    const context = await browser.newContext({
      baseURL, viewport, httpCredentials: {username: 'studio', password},
    });
    const page = await context.newPage();
    const panel = page.getByRole('region', {name: 'Track reference'});
    const reconnect = panel.getByRole('button', {name: 'Reconnect mpv', exact: true});
    const errors = [];
    const requests = [];
    page.on('pageerror', error => errors.push(error.message));
    page.on('request', request => {
      if (new URL(request.url()).pathname === '/api/section-reference/reconnect')
        requests.push(request.postData());
    });

    const status = async () => {
      const reply = await context.request.get('/api/section-reference');
      assert.ok(reply.ok(), 'operator should be able to read track-reference status');
      return reply.json();
    };
    const attach = async () => {
      await reconnect.click();
      await panel.getByText('Aligned', {exact: true}).waitFor();
      for (let attempt = 0; attempt < 80; ++attempt) {
        const current = await status();
        if (current.status === 'aligned') return current;
        await pause(50);
      }
      throw new Error('Could not read a current aligned reference');
    };
    const post = (path, data) => context.request.post(path, {data});
    const scroll = () => page.evaluate(() => {
      document.getElementById('reference-reconnect')?.scrollIntoView({block: 'center'});
    });

    await page.goto('/connections', {waitUntil: 'domcontentloaded'});
    const first = await attach();
    const identity = {'runtime-id': first['runtime-id'], 'expected-epoch': first.epoch};
    assert.ok((await post('/api/settings', {...identity, scope: 'section-patterns', values: {'override?': true}})).ok());
    await panel.getByText('Patterns suspended', {exact: true}).waitFor();
    await scroll();

    let second;
    await page.screencast.start({
      path: join(output, 'reconnect.webm'), size: viewport, fps: 15,
    });
    try {
      await page.screencast.showActions({duration: 800});
      await pause(950);
      await stopPlayer();
      await panel.getByText('Unavailable', {exact: true}).waitFor();
      await pause(1350);
      await startPlayer();
      second = await attach();
      assert.notEqual(first['session-id'], second['session-id']);
      await pause(1750);
    } finally {
      await page.screencast.stop();
    }

    assert.ok((await post('/api/command', {...identity, op: 'show/restart'})).ok());
    await panel.getByText('Stale', {exact: true}).waitFor();
    const stale = await post('/api/section-reference/reconnect', identity);
    assert.equal(stale.status(), 422);
    assert.equal((await stale.json()).reason, 'stale-show-session');
    const third = await attach();
    assert.ok(third.epoch > second.epoch);
    assert.notEqual(second['session-id'], third['session-id']);
    assert.equal(third['override?'], true);
    assert.equal(requests.length, 3);
    assert.ok(requests.every(body => body.includes('runtime-id') && body.includes('expected-epoch')
                                  && !/socket|catalog|track\.wav/.test(body)));
    assert.deepEqual(errors, []);
    await scroll();
    await page.screenshot({path: join(output, 'connections.png')});
    console.log('Authenticated Chromium and real mpv verified alignment, player reconnect, stale-epoch refusal, show restart, and preserved override.');
  } finally {
    await browser.close();
    await stopPlayer();
  }
}

main().catch(error => {
  console.error(error.message);
  process.exitCode = 1;
});
