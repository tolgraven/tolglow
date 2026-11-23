// Exercise the shared route metadata through the optimized Studio and Ring asset server.
import assert from 'node:assert/strict';
import {mkdir, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {chromium, webkit} from 'playwright-core';

const [origin, directory] = process.argv.slice(2);
const profiles = [
  ['phone', 390, 844], ['ipad-portrait', 820, 1180],
  ['ipad', 1024, 768], ['desktop', 1440, 900]
];

async function measureShell(page) {
  return page.evaluate(() => {
    const bounds = element => {
      const {left, right, top, bottom, width, height} = element.getBoundingClientRect();
      return {left, right, top, bottom, width, height};
    };
    const rail = ['.page-tab', '.grid-toolbar .segmented', '.cue-cell', '.grid-footer button'];
    const links = [...document.querySelectorAll('a.nav-item')].map(link => {
      const icon = link.querySelector('.nav-icon');
      const title = [...link.querySelectorAll('.nav-title,.nav-short')].find(el => el.offsetWidth);
      return {
        url: link.pathname, name: link.getAttribute('aria-label'), label: title.textContent,
        size: parseFloat(getComputedStyle(title).fontSize), box: bounds(link), text: bounds(title),
        mask: getComputedStyle(icon).maskImage, shortcut: link.querySelector('.nav-key').textContent
      };
    });
    return {
      overflow: document.documentElement.scrollWidth - innerWidth, links,
      gutter: rail.map(selector => bounds(document.querySelector(selector)).left),
      firstRow: [...document.querySelectorAll('.cue-cell[data-y="0"]')].map(bounds)
    };
  });
}

async function verify(page, id, width) {
  await page.goto(origin + '/perform');
  await page.getByRole('button', {name: /^Ember, column/}).waitFor();
  await page.locator('.connection .connected').waitFor({state: 'attached'});
  await page.evaluate(() => document.fonts.ready);
  const report = await measureShell(page);
  await writeFile(join(directory, id + '.json'), JSON.stringify(report, null, 2));
  await page.screenshot({path: join(directory, id + '.png')});
  assert.equal(report.overflow, 0, id + ' document overflow');
  assert.ok(Math.max(...report.gutter) - Math.min(...report.gutter) <= 1, id + ' grid gutters');
  assert.equal(report.firstRow.length, 8);
  assert.ok(report.firstRow.every(pad => pad.left >= 0 && pad.right <= width), id + ' visible pads');
  assert.equal(report.links.length, 8);
  for (const {mask, name, size, box, text} of report.links) {
    assert.ok(size >= 10 && text.left >= box.left - 1 && text.right <= box.right + 1, id + ' label ' + name);
    if (width <= 1000) assert.ok(box.height >= 44 && box.width >= 44, id + ' touch target ' + name);
    const url = /^url\("([^"]+)"\)$/.exec(mask)?.[1];
    assert.ok(url, name + ' has an icon mask');
    const res = await page.request.get(url);
    assert.ok(res.ok(), name + ' asset served');
    assert.match(await res.text(), /<svg\s/);
    await page.evaluate(async src => { const img = new Image(); img.src = src; await img.decode(); }, url);
  }
  const debug = page.getByRole('link', {name: 'Debug', exact: true});
  await debug.click();
  await page.waitForURL('**/debug');
  await page.keyboard.press('1');
  await page.waitForURL('**/perform');
  assert.equal(await page.getByRole('link', {name: 'Perform', exact: true}).getAttribute('aria-current'), 'page');
}

await mkdir(directory, {recursive: true});
const browser = process.env.STUDIO_TEST_BROWSER === 'webkit'
  ? await webkit.launch()
  : await chromium.launch({executablePath: process.env.CHROMIUM_PATH || '/usr/bin/chromium',
      args: ['--disable-dev-shm-usage', '--use-angle=swiftshader', '--enable-unsafe-swiftshader']});
try {
  for (const [id, width, height] of profiles) {
    const page = await browser.newPage({viewport: {width, height}, hasTouch: id !== 'desktop'});
    await verify(page, id, width);
    await page.close();
  }
} finally { await browser.close(); }
