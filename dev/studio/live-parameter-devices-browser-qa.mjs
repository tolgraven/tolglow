// Browser check for parameter devices and the shared parameter frame (#625)
// against tolglow.qa.live-parameter-devices-studio.
//
//   $W/bin/repl run tolglow.qa.live-parameter-devices-studio 18980 target/parameter-devices-qa
//   CHROMIUM_PATH=... node dev/studio/live-parameter-devices-browser-qa.mjs http://127.0.0.1:18980 target/parameter-devices-qa/out
//
// Uses the production Studio build in headless Chrome with the real GPU. It
// opens Connections, presses Show parameter devices, checks every listed slot
// against /api/live-parameter-devices (show/live-parameter-devices at the
// REPL), waits for the shared frame Studio mounts, then mounts a second view
// as the Live parameter device would (same instance, parameter, window and
// style at a size within 10%) and checks that the server renders it once and
// presents it to both. It closes Studio's page and checks that view expires
// while the Live view keeps reading, and that rendering stops when the last
// view leaves. Stills at desktop and phone widths
// and report.json are written. Headless desktop Chrome is not evidence for a
// foreground display, iPad Safari or a Live host.
import assert from 'node:assert/strict';
import {mkdirSync, writeFileSync} from 'node:fs';
import {join, resolve} from 'node:path';
import {chromium} from 'playwright-core';

const [origin = 'http://127.0.0.1:18980', output = 'target/parameter-devices-qa/out'] = process.argv.slice(2);
const executablePath = process.env.CHROMIUM_PATH || '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome';
mkdirSync(output, {recursive: true});
const report = {origin, startedAt: new Date().toISOString(), checks: {}};
const open = [];
const pass = (name, detail = true) => { report.checks[name] = detail; console.log('ok', name); };
const sleep = ms => new Promise(r => setTimeout(r, ms));

async function json(path, body) {
  const response = await fetch(origin + path, body === undefined
    ? {headers: {Accept: 'application/json'}}
    : {method: 'POST', headers: {Accept: 'application/json', 'Content-Type': 'application/json'}, body: JSON.stringify(body)});
  assert.equal(response.status, 200, path);
  return response.json();
}

async function launch(name, {width, height, scale = 1, touch = false}) {
  const context = await chromium.launchPersistentContext(resolve(output, '..', 'chrome-profile-' + name), {
    executablePath, headless: true,
    args: ['--headless=new', '--use-angle=metal', '--enable-gpu', '--ignore-gpu-blocklist', '--hide-scrollbars'],
    viewport: {width, height}, deviceScaleFactor: scale, hasTouch: touch,
  });
  open.push(context);
  const page = context.pages()[0] || await context.newPage();
  const problems = [];
  page.on('pageerror', error => problems.push(error.message));
  page.on('console', message => { if (message.type() === 'error') problems.push('console: ' + message.text()); });
  return {page, problems};
}

async function showDevices(page) {
  await page.goto(origin + '/connections', {waitUntil: 'domcontentloaded'});
  const panel = page.locator('section[aria-label="Live runs"]');
  await panel.waitFor({timeout: 20000});
  await panel.locator('#parameter-devices-refresh').click();
  // A first click right after a cold GPU Chrome start was once lost under
  // other lanes' load; press again once before failing.
  try { await panel.locator('ul.parameter-device-channels').waitFor({timeout: 20000}); }
  catch { await panel.locator('#parameter-devices-refresh').click();
          await panel.locator('ul.parameter-device-channels').waitFor({timeout: 20000}); }
  await panel.locator('.parameter-devices').scrollIntoViewIfNeeded();
  return panel;
}

try {
  const projection = await json('/api/live-parameter-devices');
  const channel = projection.channels.find(c => c['cue-id'] === 'ember');
  assert.ok(channel, 'the QA show has an ember channel');
  assert.deepEqual(channel.tracks.map(t => [t['track-name'], t.present, t.missing, t.stale]), [['Tolglow run ember', [0], [], []]]);
  pass('the REPL projection served at /api/live-parameter-devices', {slots: channel.slots.length, tracks: channel.tracks});

  const desktop = await launch('desktop', {width: 1440, height: 1000});
  const panel = await showDevices(desktop.page);
  const shown = await panel.evaluate(section => ({
    channel: section.querySelector('ul.parameter-device-channels > li > span')?.textContent,
    slots: [...section.querySelectorAll('ol.parameter-device-slots > li')].map(li => li.textContent),
    overflow: section.scrollWidth - section.clientWidth,
  }));
  assert.equal(shown.slots.length, channel.slots.length);
  assert.ok(shown.slots.every((text, i) => text.startsWith(channel.slots[i].name)), JSON.stringify(shown.slots));
  assert.ok(shown.channel.includes('Tolglow run ember: ready'), shown.channel);
  pass('Studio lists the device slots the REPL returns', shown);

  const img = panel.locator('figure.parameter-frame img');
  await img.waitFor({timeout: 10000});
  await desktop.page.waitForFunction(() => {
    const el = document.querySelector('figure.parameter-frame img');
    return el && el.complete && el.naturalWidth > 0;
  }, null, {timeout: 10000});
  const natural = await img.evaluate(el => [el.naturalWidth, el.naturalHeight]);
  assert.deepEqual(natural, [320, 64]);
  pass('Studio mounts the shared frame', {natural});

  const before = await json('/api/parameter-frames');
  assert.equal(before.keys.length, 1);
  const key = before.keys[0];
  // The Live device's view: same instance, parameter, window and style, 4% wider.
  const live = await json('/api/parameter-frames/subscribe', {...key.request, width: 333, consumer: 'live-device'});
  assert.equal(live['shared?'], true);
  const t0 = (await json('/api/parameter-frames')).keys[0];
  await sleep(1000);
  const t1 = (await json('/api/parameter-frames')).keys[0];
  const views = Object.fromEntries(t1.views.map(v => [v.consumer, v.presented]));
  const renders = t1.renders - t0.renders;
  assert.equal(t1.key, key.key);
  assert.ok(renders > 3, 'the frame keeps rendering');
  pass('a Live-device view joins Studio\'s render: one render, two presentations', {
    rendersPerSecond: renders, views, cost: (await json('/api/parameter-frames')).cost});
  await desktop.page.screenshot({path: join(output, 'parameter-devices-desktop.png'), fullPage: false});
  await panel.screenshot({path: join(output, 'parameter-devices-panel.png')});

  // Close Studio's page (a full navigation runs no unmount): its lease expires
  // unread after five seconds while the Live view, still reading, keeps the frame.
  await desktop.page.goto(origin + '/perform', {waitUntil: 'domcontentloaded'});
  const liveReader = setInterval(() => fetch(origin + '/api/parameter-frames/frame?lease=' + live.lease).catch(() => {}), 250);
  await sleep(6500);
  clearInterval(liveReader);
  const afterHide = await json('/api/parameter-frames');
  assert.deepEqual(afterHide.keys[0].views.map(v => v.consumer), ['live-device']);
  pass('closing Studio\'s page expires its view; the Live view keeps rendering', {views: afterHide.keys[0].views, expired: afterHide.counts.expired});
  await json('/api/parameter-frames/unsubscribe', {lease: live.lease});
  await sleep(300);
  const idle = await json('/api/parameter-frames');
  assert.equal(idle.keys.length, 0);
  assert.equal(idle['running?'], false);
  pass('the last view leaving stops rendering', {running: idle['running?'], counts: idle.counts});

  const phone = await launch('phone', {width: 390, height: 844, scale: 3, touch: true});
  const phonePanel = await showDevices(phone.page);
  await phone.page.waitForFunction(() => {
    const el = document.querySelector('figure.parameter-frame img');
    return el && el.complete && el.naturalWidth > 0;
  }, null, {timeout: 10000});
  const phoneOverflow = await phonePanel.evaluate(section => section.scrollWidth - section.clientWidth);
  await phonePanel.screenshot({path: join(output, 'parameter-devices-phone.png')});
  assert.ok(phoneOverflow <= 1, 'no horizontal overflow at phone width: ' + phoneOverflow);
  pass('phone width', {overflow: phoneOverflow, dpr: 3});

  for (const {problems} of [desktop, phone]) assert.deepEqual(problems.filter(p => !/favicon/.test(p)), []);
  pass('no page errors');
  report.ok = true;
} catch (error) {
  report.ok = false;
  report.error = String(error.stack || error);
  console.error(error);
  process.exitCode = 1;
} finally {
  writeFileSync(join(output, 'report.json'), JSON.stringify(report, null, 2));
  for (const context of open) await context.close().catch(() => {});
}
