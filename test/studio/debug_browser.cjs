// Run with a preview-only Studio host that includes the Debug journal.
const assert = require('node:assert/strict');
const {chromium} = require('playwright-core');

(async () => {
  const browser = await chromium.launch({
    executablePath: process.env.STUDIO_CHROMIUM || '/usr/bin/chromium',
    args: ['--no-sandbox', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']
  });
  const baseURL = process.env.STUDIO_URL || 'http://127.0.0.1:8548';
  try {
    const context = await browser.newContext({baseURL, viewport:{width:1360, height:900}});
    const page = await context.newPage();
    const errors = [], httpPolls = [], sockets = [];
    page.on('pageerror', e => errors.push(e.message));
    page.on('request', r => { if (new URL(r.url()).pathname === '/api/debug/events') httpPolls.push(r.url()); });
    page.on('websocket', socket => { if (socket.url().includes('/api/debug/events')) sockets.push(socket); });
    await page.goto('/debug');
    const root = page.locator('#view-debug');
    await root.getByText('Connected', {exact:true}).waitFor();
    await root.locator('.debug-row').first().waitFor({timeout:16000});
    await root.getByRole('button', {name:'Pause feed'}).click();
    const paused = root.getByRole('button', {name:/Resume/});
    const held = await root.locator('.debug-row').count();
    await page.waitForFunction(() => /Resume · [1-9]/.test(document.querySelector('.debug-feed-tools button')?.textContent || ''));
    assert.equal(await root.locator('.debug-row').count(), held);
    const row = root.locator('.debug-row').first();
    await row.click();
    assert.match(await root.locator('.debug-inspector').innerText(), /Timestamp/);
    const type = await row.locator('strong').innerText();
    await root.getByLabel('Event type', {exact:true}).selectOption(type);
    assert((await root.locator('.debug-row strong').allInnerTexts()).every(v => v===type));
    await paused.click();
    if (process.env.STUDIO_QA_IMAGES) await page.screenshot({path:process.env.STUDIO_QA_IMAGES+'/debug-events.png'});

    await root.getByRole('button', {name:'DMX output'}).click();
    await root.getByLabel('Output universe').selectOption({index:1});
    await root.locator('.debug-channel').first().waitFor();
    assert.equal(await root.locator('.debug-channel').count(), 64);
    await root.getByLabel('Channel bank').selectOption('7');
    assert.match(await root.locator('.debug-channel').last().getAttribute('aria-label'), /^Channel 512, value \d+$/);
    if (process.env.STUDIO_QA_IMAGES) await page.screenshot({path:process.env.STUDIO_QA_IMAGES+'/debug-dmx.png'});
    for (const [width,height] of [[834,1112],[390,844]]) {
      await page.setViewportSize({width,height});
      assert(await root.evaluate(node => node.getBoundingClientRect().right <= innerWidth+1));
      assert(await root.getByLabel('Output universe').evaluate(el=>el.getBoundingClientRect().height >=44));
    }
    if (process.env.STUDIO_QA_IMAGES) await page.screenshot({path:process.env.STUDIO_QA_IMAGES+'/debug-phone.png'});
    const closed = sockets[0].waitForEvent('close');
    await page.locator('nav a[data-view="rig"]').click();
    await closed;
    assert.equal(sockets.length,1);
    assert.deepEqual(httpPolls,[]);
    assert.deepEqual(errors,[]);
    console.log('Chromium verified real Transit events, typed timestamps, freeze and resume, event detail and filters, 512-channel DMX banks, tablet and phone layouts, and socket teardown.');
  } finally { await browser.close(); }
})().catch(e => {console.error(e); process.exitCode=1;});
