// Run on an isolated preview-only Studio with the event subscriber backend.
const assert = require('node:assert/strict');
const {chromium} = require('playwright-core');

(async () => {
  const baseURL = process.env.STUDIO_URL || 'http://127.0.0.1:8549';
  const browser = await chromium.launch({executablePath:process.env.STUDIO_CHROMIUM || '/usr/bin/chromium',
    args:['--no-sandbox','--use-angle=swiftshader','--enable-unsafe-swiftshader']});
  const id = 'test/debug-' + Date.now();
  try {
    const context = await browser.newContext({baseURL, viewport:{width:834,height:1112}, hasTouch:true});
    const page = await context.newPage();
    const peer = await context.newPage();
    const errors = [];
    for (const p of [page,peer]) {
      p.on('pageerror', error => errors.push(error.message));
      await p.goto('/debug');
      await p.locator('#view-debug').getByRole('button',{name:'Subscribers',exact:true}).click();
    }
    const root = page.locator('.debug-subscribers');
    await root.getByRole('button',{name:'New subscriber'}).click();
    await root.getByLabel('Subscriber ID',{exact:true}).fill(id);
    await root.getByLabel('Event',{exact:true}).selectOption('clock/beat');
    await root.getByLabel('Candidate IDs').selectOption('ember');
    await root.getByLabel('Launch',{exact:true}).selectOption('one-shot');
    await root.getByLabel('Duration',{exact:true}).fill('1/4');
    await root.getByRole('button',{name:'Add step'}).click();
    const step = root.locator('.debug-action').nth(1);
    await step.getByLabel('Action',{exact:true}).selectOption('instance/opacity');
    await step.getByLabel('Opacity',{exact:true}).fill('1');
    await step.getByLabel('Run after',{exact:true}).selectOption('beats');
    await step.getByLabel('Interval',{exact:true}).fill('1/8');
    await step.getByRole('button',{name:'Move up step 2'}).click();
    await root.getByLabel('Enabled',{exact:true}).click();
    await page.waitForFunction(()=>!document.querySelector('.debug-check input').checked);
    if (process.env.STUDIO_QA_IMAGES) await page.screenshot({path:process.env.STUDIO_QA_IMAGES+'/debug-subscriber.png'});
    await root.getByRole('button',{name:'Save subscriber'}).click();
    const registration = p=>p.locator('.debug-registration').filter({hasText:id});
    await registration(peer).waitFor();
    const response = await context.request.get('/api/event-subscribers');
    const saved = (await response.json()).subscribers.find(s=>s.id===id);
    assert.equal(saved.actions[0].op,'instance/opacity');
    assert.equal(saved.actions[0].delay.beats,0.125);
    assert.equal(saved.actions[1].launch.duration.beats,0.25);
    assert.deepEqual(saved.actions[1].select.ids,['ember']);
    assert.equal(saved['enabled?'],false);
    await registration(page).click();
    await root.getByLabel('Enabled',{exact:true}).click();
    await page.waitForFunction(()=>document.querySelector('.debug-check input').checked);
    await root.getByRole('button',{name:'Save subscriber'}).click();
    await registration(peer).filter({hasText:'Enabled'}).waitFor();
    await peer.locator('#view-debug').getByRole('button',{name:'Event feed'}).click();
    await peer.locator('.debug-row').filter({hasText:id}).first().waitFor({timeout:16000});
    await registration(page).click();
    await root.getByRole('button',{name:'Remove subscriber'}).click();
    await registration(page).waitFor({state:'detached'});
    assert.deepEqual(errors,[]);
    console.log('Chromium verified creating, reordering, disabling, saving and removing an event subscriber, exact fractional delays and durations, a real clock match, and cross-tab propagation.');
  } finally {await browser.close();}
})().catch(error=>{console.error(error);process.exitCode=1;});
