// Use a preview-only Ring show with a two-stage [:outputs :color] block binding
// and at least one patched fixture. Supply its instance ID and, optionally, a
// fixture ID from a rig with over 48 fixtures to exercise search and pinning.
import assert from 'node:assert/strict';
import {chromium} from 'playwright-core';

const [origin, instance, fixtureID] = process.argv.slice(2);
assert.ok(origin && instance, 'Pass the Ring origin and running effect instance ID.');
const browser = await chromium.launch({executablePath:process.env.CHROMIUM_PATH || '/usr/bin/chromium'});
try {
  const page = await browser.newPage({viewport:{width:1024,height:768},hasTouch:true});
  const leases = [], errors = [];
  let releaseDemand;
  const released = new Promise(resolve => { releaseDemand = resolve; });
  page.on('pageerror', error => errors.push(error.message));
  page.on('websocket', socket => socket.on('framesent', ({payload}) => {
    const text = String(payload);
    if (text.includes('queries')) {
      const tracing = text.includes('runtime/block-trace');
      if (leases.at(-1) === true && !tracing) releaseDemand();
      leases.push(tracing);
    }
  }));
  await page.goto(origin);
  const selectedCard = page.locator(`article.live-effect-card[data-live-instance=${JSON.stringify(instance)}]`);
  await selectedCard.getByRole('button',{name:/^Expand /}).tap();
  assert.ok(!leases.includes(true));
  const toggle = selectedCard.getByRole('button',{name:'Block trace'});
  await toggle.tap();
  await selectedCard.locator('.block-trace-viewport').scrollIntoViewIfNeeded();
  const plot = selectedCard.getByRole('region',{name:'Block signal trace'});
  await plot.waitFor();
  await page.waitForFunction(() => document.querySelector('.block-trace-result polyline')?.getAttribute('points').includes(' '));
  await selectedCard.getByLabel('Binding',{exact:true}).selectOption({label:'outputs / color'});
  await selectedCard.getByLabel('Trace stage').selectOption('1');
  await selectedCard.getByLabel('Trace component').selectOption('2');
  assert.equal(await plot.locator('.block-trace-legend span').count(),4);
  const fixture = selectedCard.getByLabel('Fixture',{exact:true});
  assert.ok(await fixture.getByRole('option').count() <= 50, 'Fixture menu exceeded its row budget');
  if (fixtureID) {
    const search = selectedCard.locator('.block-inspector').getByRole('searchbox',{name:'Find fixture'});
    await search.fill(fixtureID);
    await fixture.selectOption({label:fixtureID});
    await search.fill('no-matching-fixture');
    assert.equal(await fixture.locator('option:checked').textContent(),fixtureID,'The selected fixture stays pinned');
  } else {
    await fixture.selectOption({index:1});
  }
  await selectedCard.locator('.block-trace-viewport').scrollIntoViewIfNeeded();
  await plot.waitFor();
  await toggle.tap();
  await page.waitForFunction(() => !document.querySelector('.block-trace-grid'));
  await Promise.race([
    released,
    new Promise((_, reject) => { const timeout = setTimeout(() => reject(new Error('Remote demand remained active')),5000);timeout.unref(); })
  ]);
  assert.ok(leases.includes(true), 'Studio requested the trace');
  assert.equal(leases.at(-1),false,'Closing releases the remote trace');
  assert.deepEqual(errors,[]);
} finally { await browser.close(); }
