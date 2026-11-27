// Slot-grid checks for parameter cards (#795), measured on the real page.
// Every card fills a standard slot (compact 1x1, wide 2x1, large 2x2) of one
// column and row unit; the same widget type takes the same slot; every row has
// one height; the bank is as short as its width allows, as narrow as that
// height needs, and leaves no more empty cells than the best arrangement of
// its cards (computed here, independently of the packer it checks); DOM order
// is reading order; no control leaves its slot or receives a neighbour's
// press; pads are square; opening rings or showing a plot moves no card.
import assert from 'node:assert/strict';
import {join} from 'node:path';

const SPANS = {compact:[1,1], wide:[2,1], large:[2,2]};

// The fewest empty cells any arrangement of these spans leaves in `cols`
// columns, ignoring order: an exhaustive search over the first free cell.
export function optimumEmpty(spans, cols) {
  const area = spans.reduce((sum, [w, h]) => sum + w * h, 0);
  const tallest = Math.max(1, ...spans.map(([, h]) => h));
  for (let rows = Math.max(tallest, Math.ceil(area / cols)); ; rows++) {
    const budget = rows * cols - area, failed = new Set();
    const counts = {};
    for (const [w, h] of spans) counts[`${w}x${h}`] = (counts[`${w}x${h}`] || 0) + 1;
    const fits = (occupied, cell, w, h) => {
      const r = Math.floor(cell / cols), c = cell % cols;
      if (c + w > cols || r + h > rows) return false;
      for (let dr = 0; dr < h; dr++) for (let dc = 0; dc < w; dc++) if (occupied.has(cell + dr * cols + dc)) return false;
      return true;
    };
    const step = (cell, occupied, empties) => {
      if (Object.values(counts).every(n => n === 0)) return true;
      if (cell >= rows * cols) return false;
      if (occupied.has(cell)) { const next = new Set(occupied); next.delete(cell); return step(cell + 1, next, empties); }
      const key = `${cell}|${[...occupied].sort((a, b) => a - b)}|${JSON.stringify(counts)}|${empties}`;
      if (failed.has(key)) return false;
      for (const shape of Object.keys(counts)) {
        if (!counts[shape]) continue;
        const [w, h] = shape.split('x').map(Number);
        if (!fits(occupied, cell, w, h)) continue;
        const next = new Set(occupied);
        for (let dr = 0; dr < h; dr++) for (let dc = 0; dc < w; dc++) if (dr || dc) next.add(cell + dr * cols + dc);
        counts[shape]--;
        const ok = step(cell + 1, next, empties);
        counts[shape]++;
        if (ok) return true;
      }
      if (empties < budget && step(cell + 1, occupied, empties + 1)) return true;
      failed.add(key);
      return false;
    };
    if (step(0, new Set(), 0)) return {empty: budget, rows};
  }
}

function measure(surface) {
  const px = value => value.split(' ').filter(Boolean).map(parseFloat);
  const box = el => { const r = el.getBoundingClientRect(); return {x:r.x, y:r.y, w:r.width, h:r.height}; };
  const interactive = '[role=slider],button,input,select,[tabindex]:not([tabindex="-1"])';
  return [...document.querySelectorAll(`[data-slot-grid="${surface}"]`)].map(bank => {
    const style = getComputedStyle(bank);
    const outer = box(bank);
    const pad = {l:parseFloat(style.paddingLeft), t:parseFloat(style.paddingTop), r:parseFloat(style.paddingRight)};
    const effect = bank.closest('[data-live-instance]');
    const unit = parseFloat(style.getPropertyValue('--slot-col')), gap = parseFloat(style.getPropertyValue('--slot-gap'));
    const cards = [...bank.children].filter(el => el.matches('.param-row')).map(card => {
      const overlay = el => Boolean(el.closest('.rings-components'));
      const controls = [...card.querySelectorAll(interactive)].filter(el => {
        const r = el.getBoundingClientRect();
        return r.width > 0 && r.height > 0 && !el.closest('[popover]') && !overlay(el);
      });
      // A control keeps a usable size inside its slot: a fader track at least
      // 40px long, any other slider at least 32px each way.
      const cramped = [...card.querySelectorAll('.fader-track')].filter(t => !overlay(t) && t.getBoundingClientRect().height < 40).length
        + [...card.querySelectorAll('[role=slider]:not(.parameter-fader)')].filter(el => {
          const r = el.getBoundingClientRect(); return !overlay(el) && r.width > 0 && (r.width < 32 || r.height < 32);
        }).length;
      return {
        param: card.dataset.param || card.dataset.group, control: card.dataset.control, slot: card.dataset.slot, cramped,
        box: box(card), pads: [...card.querySelectorAll('.parameter-xy')].map(box),
        controls: controls.map(el => {
          const r = box(el);
          const cx = r.x + r.w / 2, cy = r.y + r.h / 2;
          const onScreen = cx >= 0 && cy >= 0 && cx < innerWidth && cy < innerHeight;
          const hit = onScreen ? document.elementFromPoint(cx, cy) : null;
          // Fixed page chrome or an open ring overlay may cover a control; another slot must not.
          return {label: el.getAttribute('aria-label') || el.textContent.trim().slice(0, 24), box:r,
                  hitInside: !hit || card.contains(hit) || !hit.closest('.param-row') || Boolean(hit.closest('.rings-components'))};
        }),
      };
    });
    return {
      effect: effect?.querySelector('h3')?.textContent, effectBox: effect && box(effect),
      columns: px(style.gridTemplateColumns), rows: px(style.gridTemplateRows),
      declaredColumns: Number(bank.dataset.columns), declaredRows: Number(bank.dataset.rows),
      maxColumns: Math.max(1, Math.floor((bank.clientWidth + gap) / (unit + gap))),
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
  const x0 = bank.contentLeft + (bank.contentWidth - (n * unit + (n - 1) * gap)) / 2;
  return bank.cards.map(card => ({...card,
    col: Math.round((card.box.x - x0) / (unit + gap)),
    row: Math.round((card.box.y - bank.contentTop) / (row + rowGap)),
    span: [Math.round((card.box.w + gap) / (unit + gap)), Math.round((card.box.h + rowGap) / (row + rowGap))]}));
}

export function checkBanks(banks, where, {padCap} = {}) {
  const sizes = {};
  for (const bank of banks) {
    const name = `${where} ${bank.effect}`;
    assert.equal(bank.columns.length, bank.declaredColumns, `${name}: the grid has the packer's column count`);
    assert.ok(bank.columns.every(c => near(c, bank.columns[0], 0.5)), `${name}: one column unit`);
    assert.ok(bank.rows.every(r => near(r, bank.rows[0], 0.5)), `${name}: one row height per row (${bank.rows})`);
    assert.ok(bank.overflow <= 1, `${name}: the bank does not overflow`);
    const [unit] = bank.columns, row = bank.rows[0];
    const placed = place(bank);
    const occupied = new Set();
    let area = 0;
    for (const card of placed) {
      const id = `${name} ${card.param}`;
      const expected = SPANS[card.slot];
      assert.ok(expected, `${id}: names a standard slot (${card.slot})`);
      assert.deepEqual(card.span, expected, `${id}: fills its ${card.slot} slot`);
      assert.ok(near(card.box.w, expected[0] * unit + (expected[0] - 1) * bank.columnGap), `${id}: whole slot width`);
      assert.ok(near(card.box.h, expected[1] * row + (expected[1] - 1) * bank.rowGap), `${id}: whole slot height`);
      assert.ok(card.col >= 0 && card.col + card.span[0] <= bank.columns.length, `${id}: inside the grid columns`);
      assert.equal(card.cramped, 0, `${id}: its controls keep a usable size in the slot`);
      for (const pad of card.pads) {
        assert.ok(near(pad.w, pad.h), `${id}: the pad is square (${pad.w}x${pad.h})`);
        if (padCap) assert.ok(pad.w <= padCap + 1, `${id}: the pad stays within ${padCap}px`);
        if (padCap && card.slot === 'large') assert.ok(pad.w >= Math.min(padCap, card.box.w - 70, card.box.h - 140) - 1,
          `${id}: the pad uses the slot up to its cap (${pad.w}px)`);
      }
      for (let r = 0; r < card.span[1]; r++) for (let c = 0; c < card.span[0]; c++) {
        const cell = `${card.row + r}:${card.col + c}`;
        assert.ok(!occupied.has(cell), `${id}: shares no slot`);
        occupied.add(cell);
      }
      area += card.span[0] * card.span[1];
      (sizes[card.control] ||= []).push([card.box.w, card.box.h, id]);
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
    // Keyboard and screen-reader order follow the screen.
    const reading = [...placed].sort((a, b) => a.row - b.row || a.col - b.col).map(c => c.param);
    assert.deepEqual(placed.map(c => c.param), reading, `${name}: DOM order is reading order`);
    // No wasted space beyond the best arrangement, and no empty trailing column.
    const rows = Math.max(0, ...placed.map(c => c.row + c.span[1]));
    const empty = rows * bank.columns.length - area;
    const spans = placed.map(c => c.span);
    const best = optimumEmpty(spans, bank.columns.length);
    assert.equal(empty, best.empty, `${name}: ${empty} empty cells where the best arrangement leaves ${best.empty}`);
    assert.equal(rows, best.rows, `${name}: ${rows} rows where the best arrangement needs ${best.rows}`);
    const widest = Math.max(...spans.map(([w]) => w));
    // As short as the width allows, and no wider than that height needs.
    for (let c = widest; c <= Math.max(widest, bank.maxColumns); c++) {
      const other = optimumEmpty(spans, c);
      assert.ok(other.rows >= rows, `${name}: ${c} columns would need only ${other.rows} rows`);
      if (other.rows === rows) assert.ok(c >= bank.columns.length, `${name}: ${c} columns hold it in as few rows`);
    }
    for (let c = 0; c < bank.columns.length; c++)
      assert.ok([...Array(rows).keys()].some(r => occupied.has(`${r}:${c}`)), `${name}: column ${c} holds a card`);
  }
  for (const [type, list] of Object.entries(sizes))
    for (const [w, h, id] of list)
      assert.ok(near(w, list[0][0]) && near(h, list[0][1]), `${where}: every ${type} card has one slot size (${id} ${w}x${h} vs ${list[0][2]} ${list[0][0]}x${list[0][1]})`);
  return Object.fromEntries(Object.entries(sizes).map(([type, list]) => [type, list[0].slice(0, 2).map(Math.round)]));
}

async function banks(page, surface) { return page.evaluate(measure, surface); }

const cardBoxes = page => page.evaluate(() => [...document.querySelectorAll('[data-slot-grid]>.param-row')]
  .map(el => { const r = el.getBoundingClientRect(); return [el.dataset.param || el.dataset.group, Math.round(r.x + scrollX), Math.round(r.y + scrollY), Math.round(r.width), Math.round(r.height)].join(' '); }));

// Evaluate in the application's own REPL session, where `runtime` is this show.
async function repl(page, source) {
  const ending = await page.evaluate(async source => {
    const response = await fetch('/api/repl/eval', {
      method: 'POST', headers: {'Content-Type': 'application/json', Accept: 'application/json'},
      body: JSON.stringify({source, 'request-id': crypto.randomUUID(), 'session-id': 'card-grid'})});
    const lines = (await response.text()).split('\n').filter(line => line.startsWith('data: '));
    return {ok: response.ok, last: lines.length ? JSON.parse(lines.at(-1).slice(6)) : null};
  }, source);
  assert.ok(ending.ok && ending.last?.status === 'completed', `REPL ${source}: ${JSON.stringify(ending)}`);
  return ending.last.result;
}

const effectCard = (page, title) => page.locator('[data-live-instance]', {has: page.getByRole('heading', {name: title, exact: true})}).first();

async function setExpanded(page, title, open) {
  const card = effectCard(page, title);
  const button = card.getByRole('button', {name: `${open ? 'Expand' : 'Collapse'} ${title} controls`});
  if (await button.count()) await button.click();
  await card.locator(open ? '[data-slot-grid=detail]' : '[data-slot-grid=strip]').waitFor();
  return card;
}

// Opened rings draw over the grid: no card moves, the knobs stay inside the
// bank's width and take presses, and closing them restores the slot.
async function checkRingsOverlay(page, card, where) {
  const before = await cardBoxes(page);
  await card.locator('.rings-trigger').click();
  const overlay = card.locator('.parameter-rings.open .rings-components');
  await overlay.waitFor();
  assert.deepEqual(await cardBoxes(page), before, `${where}: opening rings moves no card`);
  const geometry = await overlay.evaluate(panel => {
    const bank = panel.closest('[data-slot-grid]').getBoundingClientRect(), r = panel.getBoundingClientRect();
    const knobs = [...panel.querySelectorAll('[role=slider]')].map(k => {
      const b = k.getBoundingClientRect(), hit = document.elementFromPoint(b.x + b.width / 2, b.y + b.height / 2);
      return {w: b.width, mine: Boolean(hit && panel.contains(hit))};
    });
    const trigger = panel.closest('.parameter-rings').querySelector('.rings-trigger').getBoundingClientRect();
    const onTrigger = document.elementFromPoint(trigger.x + trigger.width / 2, trigger.y + trigger.height / 2);
    return {inside: r.left >= bank.left - 1 && r.right <= bank.right + 1, knobs,
            toggle: Boolean(onTrigger?.closest('.rings-trigger'))};
  });
  assert.ok(geometry.toggle, `${where}: the rings and their toggle stay uncovered`);
  assert.ok(geometry.inside, `${where}: the ring overlay stays within the bank`);
  assert.equal(geometry.knobs.length, 4, `${where}: one knob per ring`);
  for (const knob of geometry.knobs) {
    assert.ok(knob.w >= 32, `${where}: ring knobs keep a usable size`);
    assert.ok(knob.mine, `${where}: the overlay takes presses on its knobs`);
  }
  return async (path) => {
    if (path) await card.screenshot({path});
    await page.keyboard.press('Escape');
    await overlay.waitFor({state: 'detached'});
    assert.deepEqual(await cardBoxes(page), before, `${where}: closing rings moves no card`);
  };
}

export async function checkCardGrid(page, profile, directory, measurement) {
  const result = measurement.slots = {};
  result.strip = checkBanks(await banks(page, 'strip'), `${profile} strip`, {padCap: 176});
  const study = effectCard(page, 'Widget study');
  await (await checkRingsOverlay(page, study, `${profile} strip`))(join(directory, `${profile}-strip-rings.png`));

  for (const title of ['Widget study', 'Orbital wash', 'Slot study']) await setExpanded(page, title, true);
  await study.scrollIntoViewIfNeeded();
  result.detail = checkBanks(await banks(page, 'detail'), `${profile} detail`, {padCap: 220});
  checkBanks(await banks(page, 'strip'), `${profile} strip beside detail`, {padCap: 176});
  await study.locator('.live-effect-body').screenshot({path: join(directory, `${profile}-detail.png`)});
  await (await checkRingsOverlay(page, study, `${profile} detail`))(join(directory, `${profile}-detail-rings.png`));
  for (const title of ['Widget study', 'Orbital wash', 'Slot study']) await setExpanded(page, title, false);
  for (const surface of ['strip', 'detail'])
    for (const type of ['rotary', 'fader', 'rings', 'color', 'boolean', 'enum', 'xy', 'xyz', 'range', 'axes', 'shape'])
      assert.ok(result[surface][type], `${profile} ${surface}: the scene shows a ${type} card`);

  // Only a macro's member cards take custom sizes, in its saved order, and
  // the size chooses their content: small its leading controls, medium the
  // strip, large the Detail bank.
  const macro = page.getByRole('region', {name: 'Macro Card set'});
  const members = macro.locator('[data-live-instance]');
  const names = () => members.locator('h3').allTextContents();
  const state = () => members.evaluateAll(cards => cards.map(c => ({
    size: c.dataset.cardSize, density: c.dataset.density, controls: c.querySelectorAll('[data-slot-grid]>.param-row').length,
    width: Math.round(c.getBoundingClientRect().width), height: Math.round(c.getBoundingClientRect().height)})));
  const grid = macro.locator('.live-macro-grid');
  const tracks = Number(await grid.getAttribute('data-columns'));
  const full = await grid.evaluate(g => Math.round(g.getBoundingClientRect().width));
  // Saved order is Study set (medium), Orbit set (large), Solitary (small).
  // With two or more tracks the small card backfills the row beside the
  // medium one instead of leaving it empty; with one track nothing moves.
  const expected = tracks > 1 ? ['Study set', 'Solitary', 'Orbit set'] : ['Study set', 'Orbit set', 'Solitary'];
  assert.deepEqual(await names(), expected, `${profile}: macro cards follow the saved order, backfilled`);
  const measured = Object.fromEntries((await names()).map((n, i) => [n, i]));
  const all = await state();
  const [study2, orbit, solitary] = ['Study set', 'Orbit set', 'Solitary'].map(n => all[measured[n]]);
  assert.deepEqual([study2.size, orbit.size, solitary.size], ['medium', 'large', 'small'], `${profile}: saved sizes`);
  assert.deepEqual([study2.density, orbit.density], ['strip', 'detail'], `${profile}: medium shows the strip, large the Detail bank`);
  assert.equal(study2.controls, 5, `${profile}: a medium card shows every control`);
  assert.equal(await macro.locator('.live-effect-expand').count(), 0, `${profile}: a macro card's size replaces its Detail toggle`);
  assert.equal(await macro.locator('.card-size-picker').count(), 0, `${profile}: sizes are hidden outside Arrange`);
  assert.ok(near(orbit.width, full), `${profile}: a large card takes the whole row`);
  if (tracks === 6) {
    assert.ok(near(study2.width, (full - 9) / 2, 2), `${profile}: a medium card is half the row (${study2.width} of ${full})`);
    assert.ok(near(solitary.width, (full - 18) / 3, 2), `${profile}: a small card is a third of the row (${solitary.width} of ${full})`);
  } else if (tracks === 2) {
    assert.ok(near(study2.width, (full - 9) / 2, 2) && near(solitary.width, (full - 9) / 2, 2), `${profile}: small and medium are half the row`);
  } else {
    assert.ok([study2, solitary].every(c => near(c.width, full)), `${profile}: one track holds every size at full width`);
  }
  assert.ok(orbit.height > study2.height, `${profile}: a large card is taller than a medium one`);
  assert.ok(study2.height > solitary.height, `${profile}: a medium card is taller than a small one`);
  assert.equal(await page.locator('.live-effect-grid>[data-live-instance][data-card-size]').count(), 0,
    `${profile}: default cards never take a custom size`);

  await macro.getByRole('button', {name: 'Arrange Card set'}).click();
  const studyCard = members.filter({has: page.getByRole('heading', {name: 'Study set', exact: true})});
  await studyCard.getByRole('button', {name: 'Small card for Study set'}).click();
  await page.waitForFunction(() => document.querySelector('.live-macro [data-live-instance]')?.dataset.cardSize === 'small');
  const small = (await state())[(await names()).indexOf('Study set')];
  assert.ok(small.controls < 5 && small.controls >= 1, `${profile}: a small card shows its leading controls (${small.controls})`);
  await macro.locator('.live-effect-more').first().waitFor();
  await studyCard.getByRole('button', {name: 'Move Study set card later'}).click();
  await page.waitForFunction(() => document.querySelectorAll('.live-macro [data-live-instance] h3')[1]?.textContent === 'Study set');
  const committed = await page.evaluate(async () => (await (await fetch('/api/state')).json()).compositions
    .find(c => String(c.id).includes('card-set')).cards);
  assert.deepEqual(committed.map(c => [String(c['slot-id']).replace(/^:/, ''), String(c.size).replace(/^:/, '')]),
    [['orbit', 'large'], ['study', 'small'], ['solitary', 'small']], `${profile}: the show committed the new arrangement`);
  await macro.screenshot({path: join(directory, `${profile}-macro-arrange.png`)});
  // A REPL edit through the same command reaches the open board.
  await repl(page, `@(tolglow.show/submit! runtime :composition-cards {:id :qa/card-set :resize {:slot-id :solitary :size :medium}})`);
  await page.waitForFunction(() => [...document.querySelectorAll('.live-macro [data-live-instance]')]
    .find(c => c.querySelector('h3')?.textContent === 'Solitary')?.dataset.cardSize === 'medium');
  await repl(page, `@(tolglow.show/submit! runtime :composition-cards {:id :qa/card-set :cards [{:slot-id :study :size :medium} {:slot-id :orbit :size :large}]})`);
  await page.waitForFunction(() => document.querySelector('.live-macro [data-live-instance]')?.dataset.cardSize === 'medium');
  await macro.getByRole('button', {name: 'Finish arranging Card set'}).click();
  await macro.locator('.card-size-picker').first().waitFor({state: 'detached'});
  await macro.screenshot({path: join(directory, `${profile}-macro.png`)});
  result.macro = {tracks, full, widths: [study2.width, orbit.width, solitary.width], heights: [study2.height, orbit.height, solitary.height]};
  console.info(profile, 'slot sizes', JSON.stringify(result));
}
