// Slot-grid checks for parameter cards (#795), measured on the real page.
// Every card fills a standard slot (compact 1x1, wide 2x1, large 2x2) of one
// column and row unit; the same widget type takes the same slot; every row has
// one height; dense packing leaves no gap a card could have filled; and no
// control leaves its slot or receives a neighbour's press.
import assert from 'node:assert/strict';
import {join} from 'node:path';

const SPANS = {compact:[1,1], wide:[2,1], large:[2,2]};

function measure(surface) {
  const px = value => value.split(' ').filter(Boolean).map(parseFloat);
  const box = el => { const r = el.getBoundingClientRect(); return {x:r.x, y:r.y, w:r.width, h:r.height}; };
  const interactive = '[role=slider],button,input,select,[tabindex]:not([tabindex="-1"])';
  return [...document.querySelectorAll(`[data-slot-grid="${surface}"]`)].map(bank => {
    const style = getComputedStyle(bank);
    const outer = box(bank);
    const pad = {l:parseFloat(style.paddingLeft), t:parseFloat(style.paddingTop), r:parseFloat(style.paddingRight)};
    const effect = bank.closest('[data-live-instance]');
    const cards = [...bank.children].filter(el => el.matches('.param-row')).map(card => {
      const controls = [...card.querySelectorAll(interactive)].filter(el => {
        const r = el.getBoundingClientRect();
        return r.width > 0 && r.height > 0 && !el.closest('[popover]');
      });
      return {
        param: card.dataset.param || card.dataset.group, control: card.dataset.control, slot: card.dataset.slot,
        expanded: Boolean(card.querySelector('.parameter-rings.open, .parameter-visualizer')),
        box: box(card),
        controls: controls.map(el => {
          const r = box(el);
          const cx = r.x + r.w / 2, cy = r.y + r.h / 2;
          const onScreen = cx >= 0 && cy >= 0 && cx < innerWidth && cy < innerHeight;
          const hit = onScreen ? document.elementFromPoint(cx, cy) : null;
          return {label: el.getAttribute('aria-label') || el.textContent.trim().slice(0, 24), box:r,
                  // Fixed page chrome may cover a scrolled control; another slot must not.
                  hitChecked: Boolean(hit), hitInside: !hit || card.contains(hit) || !hit.closest('.param-row')};
        }),
      };
    });
    return {
      effect: effect?.querySelector('h3')?.textContent, effectBox: effect && box(effect),
      columns: px(style.gridTemplateColumns), rows: px(style.gridTemplateRows),
      columnGap: parseFloat(style.columnGap), rowGap: parseFloat(style.rowGap),
      contentLeft: outer.x + pad.l, contentTop: outer.y + pad.t, contentWidth: outer.w - pad.l - pad.r,
      overflow: bank.scrollWidth - bank.clientWidth, cards,
    };
  });
}

const near = (a, b, tolerance = 1.5) => Math.abs(a - b) <= tolerance;
const overlap = (a, b) => Math.max(0, Math.min(a.x + a.w, b.x + b.w) - Math.max(a.x, b.x))
  * Math.max(0, Math.min(a.y + a.h, b.y + b.h) - Math.max(a.y, b.y));

function place(bank) {
  const [unit] = bank.columns, gap = bank.columnGap, row = bank.rows[0], rowGap = bank.rowGap;
  const n = bank.columns.length;
  const used = n * unit + (n - 1) * gap;
  const x0 = bank.contentLeft + (bank.contentWidth - used) / 2;
  return bank.cards.map(card => ({...card,
    col: Math.round((card.box.x - x0) / (unit + gap)),
    row: Math.round((card.box.y - bank.contentTop) / (row + rowGap)),
    span: [Math.round((card.box.w + gap) / (unit + gap)), Math.round((card.box.h + rowGap) / (row + rowGap))]}));
}

export function checkBanks(banks, where) {
  const sizes = {};
  for (const bank of banks) {
    const name = `${where} ${bank.effect}`;
    assert.ok(bank.columns.length >= 2, `${name}: the grid holds at least two columns`);
    assert.ok(bank.columns.every(c => near(c, bank.columns[0], 0.5)), `${name}: one column unit`);
    assert.ok(bank.rows.every(r => near(r, bank.rows[0], 0.5)), `${name}: one row height per row (${bank.rows})`);
    assert.ok(bank.overflow <= 1, `${name}: the bank does not overflow`);
    const [unit] = bank.columns, row = bank.rows[0];
    const placed = place(bank);
    const occupied = new Set();
    for (const card of placed) {
      const id = `${name} ${card.param}`;
      const expected = card.expanded ? SPANS.large : SPANS[card.slot];
      assert.ok(expected, `${id}: names a standard slot (${card.slot})`);
      assert.deepEqual(card.span, expected, `${id}: fills its ${card.expanded ? 'expanded ' : ''}${card.slot} slot`);
      assert.ok(near(card.box.w, expected[0] * unit + (expected[0] - 1) * bank.columnGap), `${id}: whole slot width`);
      assert.ok(near(card.box.h, expected[1] * row + (expected[1] - 1) * bank.rowGap), `${id}: whole slot height`);
      assert.ok(card.col >= 0 && card.col + card.span[0] <= bank.columns.length, `${id}: inside the grid columns`);
      for (let r = 0; r < card.span[1]; r++) for (let c = 0; c < card.span[0]; c++) {
        const cell = `${card.row + r}:${card.col + c}`;
        assert.ok(!occupied.has(cell), `${id}: shares no slot`);
        occupied.add(cell);
      }
      const key = `${card.control}${card.expanded ? '+expanded' : ''}`;
      (sizes[key] ||= []).push([card.box.w, card.box.h, id]);
      for (const control of card.controls) {
        const b = control.box, c = card.box;
        assert.ok(b.x >= c.x - 1 && b.y >= c.y - 1 && b.x + b.w <= c.x + c.w + 1 && b.y + b.h <= c.y + c.h + 1,
          `${id}: control ${control.label} stays inside its slot`);
        assert.ok(control.hitInside, `${id}: a press on ${control.label} reaches its own slot`);
      }
      if (bank.effectBox) {
        const e = bank.effectBox, c = card.box;
        assert.ok(c.x >= e.x - 1 && c.x + c.w <= e.x + e.w + 1, `${id}: inside its effect card`);
      }
    }
    for (let i = 0; i < placed.length; i++) for (let j = i + 1; j < placed.length; j++)
      assert.ok(overlap(placed[i].box, placed[j].box) <= 1, `${name}: ${placed[i].param} and ${placed[j].param} do not overlap`);
    // Dense packing: no card fits in free slots earlier than its own position.
    const rows = Math.max(0, ...placed.map(c => c.row + c.span[1]));
    const free = (r, c) => r >= 0 && c >= 0 && c < bank.columns.length && !occupied.has(`${r}:${c}`);
    for (const card of placed) {
      for (let r = 0; r <= card.row; r++) for (let c = 0; c < bank.columns.length; c++) {
        if (r === card.row && c >= card.col) break;
        let fits = true;
        for (let dr = 0; dr < card.span[1] && fits; dr++) for (let dc = 0; dc < card.span[0] && fits; dc++)
          fits = free(r + dr, c + dc) && r + dr < Math.max(rows, r + card.span[1]);
        assert.ok(!fits, `${name}: ${card.param} could have filled the free slot at row ${r}, column ${c}`);
      }
    }
  }
  for (const [type, list] of Object.entries(sizes))
    for (const [w, h, id] of list)
      assert.ok(near(w, list[0][0]) && near(h, list[0][1]), `${where}: every ${type} card has one slot size (${id} ${w}x${h} vs ${list[0][2]} ${list[0][0]}x${list[0][1]})`);
  return Object.fromEntries(Object.entries(sizes).map(([type, list]) => [type, list[0].slice(0, 2).map(Math.round)]));
}

async function banks(page, surface) { return page.evaluate(measure, surface); }

async function setExpanded(page, title, open) {
  const card = page.locator('[data-live-instance]', {has: page.getByRole('heading', {name: title, exact: true})}).first();
  const button = card.getByRole('button', {name: `${open ? 'Expand' : 'Collapse'} ${title} controls`});
  if (await button.count()) await button.click();
  await card.locator(open ? '[data-slot-grid=detail]' : '[data-slot-grid=strip]').waitFor();
  return card;
}

export async function checkCardGrid(page, profile, directory, measurement) {
  const result = measurement.slots = {};
  result.strip = checkBanks(await banks(page, 'strip'), `${profile} strip`);
  const study = page.locator('[data-live-instance]', {has: page.getByRole('heading', {name: 'Widget study', exact: true})}).first();
  // Rings opened in the strip take the large slot.
  const stripRings = study.locator('.rings-trigger');
  await stripRings.click();
  await study.locator('.parameter-rings.open').waitFor();
  checkBanks(await banks(page, 'strip'), `${profile} strip, rings open`);
  await study.screenshot({path: join(directory, `${profile}-strip-rings.png`)});
  await page.keyboard.press('Escape');
  await study.locator('.parameter-rings.open').waitFor({state: 'detached'});

  for (const title of ['Widget study', 'Orbital wash', 'Slot study']) await setExpanded(page, title, true);
  await study.scrollIntoViewIfNeeded();
  result.detail = checkBanks(await banks(page, 'detail'), `${profile} detail`);
  checkBanks(await banks(page, 'strip'), `${profile} strip beside detail`);
  await study.locator('.live-effect-body').screenshot({path: join(directory, `${profile}-detail.png`)});
  await study.locator('.rings-trigger').click();
  await study.locator('.parameter-rings.open').waitFor();
  checkBanks(await banks(page, 'detail'), `${profile} detail, rings open`);
  await study.locator('.live-effect-body').screenshot({path: join(directory, `${profile}-detail-rings.png`)});
  await page.keyboard.press('Escape');
  await study.locator('.parameter-rings.open').waitFor({state: 'detached'});
  for (const title of ['Widget study', 'Orbital wash', 'Slot study']) await setExpanded(page, title, false);
  for (const surface of ['strip', 'detail'])
    for (const type of ['rotary', 'fader', 'rings', 'color', 'boolean', 'enum', 'xy', 'xyz', 'range', 'axes', 'shape'])
      assert.ok(result[surface][type], `${profile} ${surface}: the scene shows a ${type} card`);

  // Only a macro's member cards take custom sizes, in its saved order.
  const macro = page.getByRole('region', {name: 'Macro Card set'});
  const members = macro.locator('[data-live-instance]');
  const names = async () => members.locator('h3').allTextContents();
  const sizes = async () => members.evaluateAll(cards => cards.map(c => c.dataset.cardSize));
  assert.deepEqual(await names(), ['Indigo', 'Moon', 'Solitary'], `${profile}: macro cards follow the saved order`);
  assert.deepEqual(await sizes(), ['medium', 'large', 'small'], `${profile}: macro cards keep their saved sizes`);
  assert.equal(await page.locator('.live-effect-grid>[data-live-instance]:not(.live-macro *)[data-card-size]').count(), 0,
    `${profile}: default cards never take a custom size`);
  const widths = await members.evaluateAll(cards => cards.map(c => Math.round(c.getBoundingClientRect().width)));
  const grid = await macro.locator('.live-effect-grid').evaluate(g => getComputedStyle(g).gridTemplateColumns.split(' ').length);
  const full = await macro.locator('.live-effect-grid').evaluate(g => Math.round(g.getBoundingClientRect().width));
  assert.ok(Math.abs(widths[1] - full) <= 1, `${profile}: a large card takes the whole row`);
  if (grid >= 2) assert.ok(widths[0] > widths[2] * 1.8, `${profile}: a medium card is two small columns wide (${widths})`);
  else assert.ok(widths.every(w => Math.abs(w - full) <= 1), `${profile}: one column holds every size at full width`);
  const indigo = macro.locator('[data-live-instance]', {has: page.getByRole('heading', {name: 'Indigo', exact: true})});
  await indigo.getByRole('button', {name: 'Small card for Indigo'}).click();
  await page.waitForFunction(() => document.querySelector('.live-macro [data-live-instance]')?.dataset.cardSize === 'small');
  await indigo.getByRole('button', {name: 'Move Indigo card later'}).click();
  await page.waitForFunction(() => document.querySelectorAll('.live-macro [data-live-instance] h3')[1]?.textContent === 'Indigo');
  const state = await page.evaluate(async () => (await (await fetch('/api/state')).json()).compositions
    .find(c => String(c.id).includes('card-set')).cards);
  assert.deepEqual(state.map(c => [String(c['slot-id']).replace(/^:/, ''), String(c.size).replace(/^:/, '')]),
    [['moon', 'large'], ['indigo', 'small'], ['solitary', 'small']], `${profile}: the show committed the new arrangement`);
  await macro.screenshot({path: join(directory, `${profile}-macro.png`)});
  await indigo.getByRole('button', {name: 'Move Indigo card earlier'}).click();
  await page.waitForFunction(() => document.querySelector('.live-macro [data-live-instance] h3')?.textContent === 'Indigo');
  await indigo.getByRole('button', {name: 'Medium card for Indigo'}).click();
  await page.waitForFunction(() => document.querySelector('.live-macro [data-live-instance]')?.dataset.cardSize === 'medium');
  result.macro = {widths, columns: grid};
  console.info(profile, 'slot sizes', JSON.stringify(result));
}
