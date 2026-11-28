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
      fluid: bank.dataset.slotScale === 'fluid',
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

export function checkBanks(banks, where) {
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
        // The pad fills its slot: the largest square beside the Z fader and
        // above the readout (slot padding, title and source take the rest).
        const fader = card.control === 'xyz' ? (where.includes('strip') ? 34 : 48) : 0;
        const fit = Math.min(card.box.w - 14 - fader, card.box.h - 60 - 44);
        assert.ok(pad.w >= fit - 3, `${id}: the pad fills its slot (${Math.round(pad.w)}px of ${Math.round(fit)}px)`);
      }
      for (let r = 0; r < card.span[1]; r++) for (let c = 0; c < card.span[0]; c++) {
        const cell = `${card.row + r}:${card.col + c}`;
        assert.ok(!occupied.has(cell), `${id}: shares no slot`);
        occupied.add(cell);
      }
      area += card.span[0] * card.span[1];
      // A macro card's bank stretches its tracks to the card: one size per
      // widget type within that bank; every other bank shares one size.
      (sizes[bank.fluid ? `${card.control} in ${name}` : card.control] ||= []).push([card.box.w, card.box.h, id]);
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
  return Object.fromEntries(Object.entries(sizes).filter(([type]) => !type.includes(' in ')).map(([type, list]) => [type, list[0].slice(0, 2).map(Math.round)]));
}

async function banks(page, surface) {
  // Grids draw first fit at once and the exact layout when the browser is idle.
  await page.waitForFunction(() => !document.querySelector('[data-slot-grid][data-packing=quick]'), null, {timeout: 5000});
  return page.evaluate(measure, surface);
}

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
    const own = panel.closest('.param-row');
    const covered = [...own.parentElement.children].filter(row => row !== own && row.matches('.param-row')).filter(row => {
      const b = row.getBoundingClientRect();
      return Math.min(b.right, r.right) - Math.max(b.left, r.left) > 2 && Math.min(b.bottom, r.bottom) - Math.max(b.top, r.top) > 2;
    }).map(row => row.dataset.param ? 'param/' + row.dataset.param : row.dataset.group).sort();
    return {inside: r.left >= bank.left - 1 && r.right <= bank.right + 1, knobs, covered,
            recorded: (own.dataset.overlayCovers || '').split(' ').filter(Boolean).sort(),
            toggle: Boolean(onTrigger?.closest('.rings-trigger'))};
  });
  assert.ok(geometry.toggle, `${where}: the rings and their toggle stay uncovered`);
  assert.deepEqual(geometry.covered, geometry.recorded, `${where}: the overlay covers exactly the cards it records`);
  assert.ok(geometry.inside, `${where}: the ring overlay stays within the bank`);
  assert.equal(geometry.knobs.length, 4, `${where}: one knob per ring`);
  for (const knob of geometry.knobs) {
    assert.ok(knob.w >= 32, `${where}: ring knobs keep a usable size`);
    assert.ok(knob.mine, `${where}: the overlay takes presses on its knobs`);
  }
  return async (path) => {
    if (path) await card.screenshot({path});
    // Light dismiss: a press anywhere outside the rings and their knobs closes them.
    await card.locator('h3').click();
    await overlay.waitFor({state: 'detached'});
    assert.deepEqual(await cardBoxes(page), before, `${where}: closing rings moves no card`);
    await card.locator('.rings-trigger').click();
    await overlay.waitFor();
    await page.keyboard.press('Escape');
    await overlay.waitFor({state: 'detached'});
    return geometry.covered;
  };
}

export async function checkCardGrid(page, profile, directory, measurement) {
  const result = measurement.slots = {};
  result.strip = checkBanks(await banks(page, 'strip'), `${profile} strip`);
  const study = effectCard(page, 'Widget study');
  result.ringsCover = {strip: await (await checkRingsOverlay(page, study, `${profile} strip`))(join(directory, `${profile}-strip-rings.png`))};

  for (const title of ['Widget study', 'Orbital wash', 'Slot study']) await setExpanded(page, title, true);
  await study.scrollIntoViewIfNeeded();
  result.detail = checkBanks(await banks(page, 'detail'), `${profile} detail`);
  checkBanks(await banks(page, 'strip'), `${profile} strip beside detail`);
  await study.locator('.live-effect-body').screenshot({path: join(directory, `${profile}-detail.png`)});
  result.ringsCover.detail = await (await checkRingsOverlay(page, study, `${profile} detail`))(join(directory, `${profile}-detail-rings.png`));
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
  // Saved order is Study set (medium), Solitary (small), Orbit set (large),
  // and the board reads in saved order at every width: Arrange controls
  // what is seen.
  const DEFAULT = '[{:slot-id :study :size :medium} {:slot-id :solitary :size :small} {:slot-id :orbit :size :large}]';
  assert.deepEqual(await names(), ['Study set', 'Solitary', 'Orbit set'], `${profile}: macro cards read in the saved order`);
  const named = async () => { const all = await state(); return Object.fromEntries((await names()).map((n, i) => [n, all[i]])); };
  const first = await named();
  const [study2, orbit, solitary] = ['Study set', 'Orbit set', 'Solitary'].map(n => first[n]);
  assert.deepEqual([study2.size, orbit.size, solitary.size], ['medium', 'large', 'small'], `${profile}: saved sizes`);
  assert.deepEqual([study2.density, orbit.density], ['strip', 'detail'], `${profile}: medium shows the strip, large the Detail bank`);
  assert.equal(study2.controls, 5, `${profile}: a medium card shows every control`);
  const orbitCard = members.filter({has: page.getByRole('heading', {name: 'Orbit set', exact: true})});
  assert.equal(await orbitCard.locator('.live-effect-advanced:visible, .layer-priority-source:visible').count(), 0,
    `${profile}: a large card shows its controls and plots, not the inspector`);
  assert.equal(await orbitCard.locator('details.live-effect-inspector:not([open])').count(), 1, `${profile}: the inspector waits behind a disclosure`);
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
  if (tracks > 1) {
    // The small card beside the medium one fills that band: no hole under it.
    assert.ok(near(solitary.height, study2.height, 2), `${profile}: a small card beside a medium one takes its height (${solitary.height} vs ${study2.height})`);
  } else {
    assert.ok(study2.height > solitary.height, `${profile}: on one track a medium card is taller than a small one`);
  }
  // A card's bank fills its height: at most one row unit of blank space
  // between the last thing in its body and its action row.
  const bands = await members.evaluateAll(cards => cards.map(card => {
    const body = card.querySelector('.live-effect-body'), footer = card.querySelector('.performance-instance');
    const bank = card.querySelector('[data-slot-grid]');
    const last = Math.max(...[...body.children].map(c => c.getBoundingClientRect().bottom));
    return {name: card.querySelector('h3').textContent, blank: Math.round(footer.getBoundingClientRect().top - last),
            row: parseFloat(getComputedStyle(bank).getPropertyValue('--slot-row'))};
  }));
  for (const {name, blank, row} of bands)
    assert.ok(blank <= row, `${profile}: ${name} has ${blank} px of blank space above its action row (one row is ${row} px)`);
  result.macroBlank = Object.fromEntries(bands.map(b => [b.name, b.blank]));
  // A lone control in a macro card uses its Detail form (knob, slider and
  // exact entry) at a bounded size, not one enlarged knob.
  const lone = members.filter({has: page.getByRole('heading', {name: 'Solitary', exact: true})});
  assert.equal(await lone.locator('[data-slot-grid=detail] .numeric-exact').count(), 1, `${profile}: a lone-control card shows the exact entry`);
  const knobs = await members.locator('.parameter-rotary-control').evaluateAll(all => all.map(k => Math.round(k.getBoundingClientRect().width)));
  assert.ok(knobs.every(w => w <= 210), `${profile}: no macro knob is wider than 210 px (${knobs})`);
  const loneKnob = (await lone.locator('.parameter-rotary-control').boundingBox()).width;
  assert.ok(loneKnob >= 110, `${profile}: the lone knob still uses its room (${Math.round(loneKnob)} px)`);
  result.knobs = {lone: Math.round(loneKnob), max: Math.max(...knobs)};
  assert.equal(await page.locator('.live-effect-grid>[data-live-instance][data-card-size]').count(), 0,
    `${profile}: default cards never take a custom size`);

  if (tracks === 6) {
    // A card's width depends on its size, never on its siblings: M, S, S
    // keeps M at half the row and each S at a third.
    await repl(page, `@(tolglow.show/submit! runtime :composition-cards {:id :qa/card-set :cards [{:slot-id :study :size :medium} {:slot-id :orbit :size :small} {:slot-id :solitary :size :small}]})`);
    await page.waitForFunction(() => [...document.querySelectorAll('.live-macro [data-live-instance]')].every(c => c.dataset.cardSize !== 'large'));
    const mss = await named();
    assert.ok(near(mss['Study set'].width, (full - 9) / 2, 2), `${profile}: with M, S, S a medium card is still half the row (${mss['Study set'].width} of ${full})`);
    for (const n of ['Orbit set', 'Solitary'])
      assert.ok(near(mss[n].width, (full - 18) / 3, 2), `${profile}: with M, S, S a small card is still a third (${mss[n].width} of ${full})`);
    assert.equal(await grid.getAttribute('data-columns'), '6', `${profile}: the macro board keeps its six tracks`);
    await macro.screenshot({path: join(directory, `${profile}-macro-mss.png`)});
    await repl(page, `@(tolglow.show/submit! runtime :composition-cards {:id :qa/card-set :cards ${DEFAULT}})`);
    await page.waitForFunction(() => [...document.querySelectorAll('.live-macro [data-live-instance]')].some(c => c.dataset.cardSize === 'large'));
  }
  await macro.getByRole('button', {name: 'Arrange Card set'}).click();
  const solitaryCard = members.filter({has: page.getByRole('heading', {name: 'Solitary', exact: true})});
  await solitaryCard.getByRole('button', {name: 'Large card for Solitary'}).waitFor();
  assert.equal(await solitaryCard.getByRole('button', {name: 'Medium card for Solitary'}).count(), 0,
    `${profile}: a single-control member offers only small and large`);
  const studyCard = members.filter({has: page.getByRole('heading', {name: 'Study set', exact: true})});
  await studyCard.getByRole('button', {name: 'Small card for Study set'}).click();
  await page.waitForFunction(() => document.querySelector('.live-macro [data-live-instance]')?.dataset.cardSize === 'small');
  const small = (await named())['Study set'];
  const smallParams = await studyCard.locator('[data-slot-grid]>.param-row').evaluateAll(rows => rows.map(r => r.dataset.param).sort());
  assert.deepEqual(smallParams, ['level', 'steps', 'tone'], `${profile}: a small card fills its four cells with every control that fits (${small.controls})`);
  await macro.locator('.live-effect-more').first().waitFor();
  result.pins = await checkPins(page, profile, directory, studyCard);
  // Every Arrange move changes what is seen, and ‹ › stop at the ends.
  const move = async (title, direction, expected) => {
    const before = await names();
    const card = members.filter({has: page.getByRole('heading', {name: title, exact: true})});
    await card.getByRole('button', {name: `Move ${title} card ${direction}`}).click();
    await page.waitForFunction(expected => [...document.querySelectorAll('.live-macro [data-live-instance] h3')].map(h => h.textContent).join('|') === expected,
                               expected.join('|'));
    assert.notDeepEqual(await names(), before, `${profile}: moving ${title} ${direction} changes the visible order`);
  };
  await move('Study set', 'later', ['Solitary', 'Study set', 'Orbit set']);
  await move('Orbit set', 'earlier', ['Solitary', 'Orbit set', 'Study set']);
  assert.ok(await members.first().getByRole('button', {name: /card earlier$/}).isDisabled(), `${profile}: the first card cannot move earlier`);
  assert.ok(await members.last().getByRole('button', {name: /card later$/}).isDisabled(), `${profile}: the last card cannot move later`);
  const committed = await page.evaluate(async () => (await (await fetch('/api/state')).json()).compositions
    .find(c => String(c.id).includes('card-set')).cards);
  assert.deepEqual(committed.map(c => [String(c['slot-id']).replace(/^:/, ''), String(c.size).replace(/^:/, '')]),
    [['solitary', 'small'], ['orbit', 'large'], ['study', 'small']], `${profile}: the show committed the new arrangement`);
  await macro.screenshot({path: join(directory, `${profile}-macro-arrange.png`)});
  // A REPL edit through the same command reaches the open board.
  await repl(page, `@(tolglow.show/submit! runtime :composition-cards {:id :qa/card-set :resize {:slot-id :solitary :size :large}})`);
  await page.waitForFunction(() => [...document.querySelectorAll('.live-macro [data-live-instance]')]
    .find(c => c.querySelector('h3')?.textContent === 'Solitary')?.dataset.cardSize === 'large');
  await repl(page, `@(tolglow.show/submit! runtime :composition-cards {:id :qa/card-set :cards ${DEFAULT}})`);
  await page.waitForFunction(() => document.querySelector('.live-macro [data-live-instance]')?.dataset.cardSize === 'medium'
                                   && document.querySelectorAll('.live-macro [data-live-instance] h3')[1]?.textContent === 'Solitary');
  await macro.getByRole('button', {name: 'Finish arranging Card set'}).click();
  await macro.locator('.card-size-picker').first().waitFor({state: 'detached'});
  await macro.screenshot({path: join(directory, `${profile}-macro.png`)});
  result.packMs = await measurePackTime(page);
  assert.ok(!result.packMs.error, result.packMs.error);
  for (const [range, {max}] of Object.entries(result.packMs))
    assert.ok(max < 500, `${profile}: packing ${range} cards stays bounded (${max} ms)`);
  result.macro = {tracks, full, widths: [study2.width, orbit.width, solitary.width], heights: [study2.height, orbit.height, solitary.height]};
  console.info(profile, 'slot sizes', JSON.stringify(result));
}

// Pinned controls (#795): in Arrange, the operator pins which controls a
// small card shows. The card shows exactly those, in pin order and as many as
// fit, with an "N more" note. Studio and the REPL send the same
// :composition-cards command and each sees the other's change.
async function checkPins(page, profile, directory, card) {
  const shown = () => card.locator('[data-slot-grid]>.param-row').evaluateAll(rows => rows.map(r => r.dataset.param || r.dataset.group));
  const pinsAre = async expected => {
    await page.waitForFunction(expected => {
      const card = [...document.querySelectorAll('.live-macro [data-live-instance]')].find(c => c.querySelector('h3')?.textContent === 'Study set');
      return (card?.dataset.cardPins || '') === expected;
    }, expected, {timeout: 10000});
    // The bank re-packs once the new controls mount.
    await page.waitForFunction(() => !document.querySelector('.live-macro [data-slot-grid][data-packing=quick]'), null, {timeout: 5000});
  };
  const more = async () => Number((await card.locator('.live-effect-more').textContent()).match(/\d+/)[0]);
  const pin = name => card.getByRole('button', {name: `Pin ${name} on the small Study set card`}).click();
  const unpin = name => card.getByRole('button', {name: `Unpin ${name} on the small Study set card`}).click();
  const picker = card.locator('.card-pin-picker');
  await picker.waitFor();
  assert.equal(await picker.locator('button').count(), 5, `${profile}: the pin picker lists every control of the member`);
  // Studio pins three compact controls: exactly those show, in pin order.
  await pin('Steps'); await pinsAre('steps');
  await pin('Tone'); await pinsAre('steps tone');
  await pin('Level'); await pinsAre('steps tone level');
  assert.deepEqual(await shown(), ['steps', 'tone', 'level'], `${profile}: a pinned small card shows exactly its pins, in pin order`);
  assert.equal(await more(), 2, `${profile}: the pinned card counts the controls it leaves out`);
  const places = await picker.locator('button[aria-pressed=true]').evaluateAll(bs => Object.fromEntries(
    bs.map(b => [b.dataset.pinControl, b.querySelector('.card-pin-place')?.textContent])));
  assert.deepEqual(places, {'param/steps': '1', 'param/tone': '2', 'param/level': '3'}, `${profile}: each pinned control shows its place`);
  await card.screenshot({path: join(directory, `${profile}-macro-pinned.png`)});
  await unpin('Tone'); await pinsAre('steps level');
  assert.deepEqual(await shown(), ['steps', 'level'], `${profile}: unpinning removes the control from the small card`);
  // The show committed the pins with the macro's cards.
  const committed = await page.evaluate(async () => (await (await fetch('/api/state')).json()).compositions
    .find(c => String(c.id).includes('card-set')).cards.find(c => String(c['slot-id']).includes('study')).pins);
  assert.deepEqual(committed.map(p => String(p).replace(/^:/, '')), ['steps', 'level'], `${profile}: the show committed the pins`);
  // A REPL change through the same command reaches the open board: Aim and
  // Level pinned, and only Aim fits in four cells.
  await repl(page, `@(tolglow.show/submit! runtime :composition-cards {:id :qa/card-set :pins {:slot-id :study :pins [:aim :level]}})`);
  await pinsAre('aim level');
  assert.deepEqual(await shown(), ['aim'], `${profile}: pinned controls show as many as fit`);
  assert.equal(await more(), 4, `${profile}: the rest are counted`);
  assert.equal(await picker.locator('button[aria-pressed=true]').count(), 2, `${profile}: the picker shows the REPL's pins`);
  // An unknown parameter is refused and changes nothing.
  const refused = await repl(page, `(:status @(tolglow.show/submit! runtime :composition-cards {:id :qa/card-set :pin {:slot-id :study :parameter :nowhere}}))`);
  assert.match(String(refused), /rejected/, `${profile}: a pin naming no parameter of the member is refused (${refused})`);
  assert.equal(await card.getAttribute('data-card-pins'), 'aim level', `${profile}: a refused pin changes nothing`);
  await repl(page, `@(tolglow.show/submit! runtime :composition-cards {:id :qa/card-set :pins {:slot-id :study :pins nil}})`);
  await pinsAre('');
  assert.deepEqual((await shown()).sort(), ['level', 'steps', 'tone'], `${profile}: without pins the small card shows its leading controls again`);
  return {studio: ['steps', 'tone', 'level'], repl: ['aim']};
}

// A whole view detached into its own window (#669): the view's content
// leaves the main page, which shows a placeholder; its slot grids and the
// macro board re-pack when that window resizes; Redock and closing the window
// each bring the view back and leave nothing behind.
export async function checkDetachedView(page, directory) {
  const errors = [];
  await page.goto(new URL('/perform', page.url()).href);
  await page.locator('[data-live-instance]').first().waitFor({state: 'attached'});
  const popupFor = async label => {
    const popup = page.waitForEvent('popup');
    await page.getByRole('button', {name: `Open ${label} in its own window`}).click();
    const window = await popup;
    window.on('pageerror', error => errors.push(error.message));
    return window;
  };
  const perform = await popupFor('Perform');
  await perform.waitForSelector('#view-perform .live-macro-grid', {timeout: 30000});
  await perform.bringToFront();
  assert.equal(await perform.evaluate(() => typeof window.tolglowLoadEngine), 'function', 'the view window loads its own engine loader');
  await page.locator('[data-detached-view=perform]').waitFor();
  await page.screenshot({path: join(directory, 'desktop-view-placeholder.png')});
  assert.equal(await page.locator('#view-perform').count(), 0, 'a detached view is not rendered in the main page as well');
  assert.equal(await page.locator('.view-tab[data-view=perform][data-detached=true]').count(), 1, 'its tab is marked');
  const counts = await Promise.all([page, perform].map(p => p.evaluate(() => document.querySelectorAll('canvas#preview').length)));
  assert.deepEqual(counts, [0, 1], 'the Room preview lives in the view window only');
  // Re-pack in the popup: the macro board's tracks and Orbital wash's strip
  // follow the window's width.
  const geometry = () => perform.evaluate(() => {
    const board = document.querySelector('.live-macro-grid');
    const bank = [...document.querySelectorAll('[data-live-instance]')]
      .find(c => c.querySelector('h3')?.textContent === 'Orbital wash')?.querySelector('[data-slot-grid]');
    return {tracks: Number(board?.dataset.columns), board: board?.clientWidth, columns: Number(bank?.dataset.columns), bank: bank?.clientWidth};
  });
  const settle = async width => {
    await perform.setViewportSize({width, height: 1000});
    await perform.waitForFunction(width => innerWidth === width, width);
    await perform.waitForFunction(() => {
      const board = document.querySelector('.live-macro-grid');
      const expected = board.clientWidth >= 960 ? 6 : board.clientWidth >= 640 ? 2 : 1;
      return Number(board.dataset.columns) === expected && !document.querySelector('[data-slot-grid][data-packing=quick]');
    }, null, {timeout: 10000, polling: 100});
    return geometry();
  };
  const wide = await settle(2400);
  await perform.screenshot({path: join(directory, 'desktop-view-window-wide.png')});
  // Above the phone breakpoint, where Perform shows one section at a time.
  const narrow = await settle(700);
  assert.ok(wide.tracks > narrow.tracks, `the macro board re-packs in the view window (${JSON.stringify(wide)} -> ${JSON.stringify(narrow)})`);
  assert.ok(wide.columns > narrow.columns, `a strip bank re-packs in the view window (${wide.columns} -> ${narrow.columns})`);
  await perform.screenshot({path: join(directory, 'desktop-view-window-narrow.png')});
  // Commands from the view window reach the show over the main page's socket.
  const order = expected => perform.waitForFunction(expected => [...document.querySelectorAll('.live-macro [data-live-instance] h3')]
    .map(h => h.textContent).join('|') === expected, expected.join('|'), {timeout: 10000});
  await perform.getByRole('button', {name: 'Arrange Card set'}).click();
  await perform.getByRole('button', {name: 'Move Study set card later'}).click();
  await order(['Solitary', 'Study set', 'Orbit set']);
  await perform.getByRole('button', {name: 'Move Study set card earlier'}).click();
  await order(['Study set', 'Solitary', 'Orbit set']);
  await perform.getByRole('button', {name: 'Finish arranging Card set'}).click();
  // Redock from the main page closes the window and brings Perform back.
  const closed = perform.waitForEvent('close');
  await page.getByRole('button', {name: 'Redock'}).click();
  await closed;
  await page.locator('#view-perform .live-macro-grid').waitFor();
  assert.equal(await page.locator('[data-detached-view], .view-tab[data-detached]').count(), 0, 'Redock leaves no placeholder or marker');
  // Closing the window itself docks the view too: here, Rig with its 3D editor.
  await page.goto(new URL('/rig', page.url()).href);
  await page.locator('#view-rig').waitFor();
  const rig = await popupFor('Rig & space');
  await rig.waitForSelector('#view-rig', {timeout: 30000});
  await page.locator('[data-detached-view=rig]').waitFor();
  // Navigate without reloading: a reload of the main page closes its windows.
  await page.locator('a[data-view=perform]').first().click();
  await page.locator('#view-perform').waitFor();
  assert.equal(await rig.locator('#view-rig').count(), 1, 'a detached view stays in its window while the main page shows another view');
  await rig.close();
  await page.waitForFunction(() => !document.querySelector('.view-tab[data-detached]'), null, {timeout: 5000});
  await page.goto(new URL('/rig', page.url()).href);
  await page.locator('#view-rig').waitFor();
  assert.equal(await page.locator('[data-detached-view]').count(), 0, 'closing the window docks the view');
  assert.deepEqual(errors, [], 'no errors in the view windows');
  return {wide, narrow};
}

// A Live panel popped out into its own window re-packs its slot grids when
// that window resizes: the grids' ResizeObserver comes from the window that
// owns them (#795). The window keeps one size tier, so nothing remounts and
// only the observers can re-pack.
export async function checkDetachedRepack(page, directory) {
  // Count ResizeObserver constructions in Studio's window and the pop-out's.
  await page.context().addInitScript(() => {
    const Native = window.ResizeObserver;
    window.__resizeObservers = 0;
    if (Native) window.ResizeObserver = class extends Native {
      constructor(callback) { super(callback); window.__resizeObservers++; }
    };
  });
  await page.reload();
  await page.locator('[data-live-instance]').first().waitFor({state: 'attached'});
  const popup = page.waitForEvent('popup');
  await page.locator('#edit-layout').click();
  await page.locator('#view-perform [data-panel="live"] .layout-detach').click();
  const live = await popup;
  await live.waitForSelector('[data-live-instance] [data-slot-grid]', {timeout: 20000});
  // A ResizeObserver reports at its document's next rendering opportunity,
  // and a background window may not render. A person resizing the panel's
  // window is looking at it, so bring it to the front as they would.
  await live.bringToFront();
  const tier = () => live.evaluate(() => document.querySelector('.panel-window-content[data-panel="live"]')?.dataset.tier);
  const orbital = () => live.evaluate(() => {
    const bank = [...document.querySelectorAll('[data-live-instance]')]
      .find(c => c.querySelector('h3')?.textContent === 'Orbital wash')?.querySelector('[data-slot-grid]');
    return bank && {columns: Number(bank.dataset.columns), width: bank.clientWidth,
                    visible: document.visibilityState, focused: document.hasFocus()};
  });
  // Resize, then poll (bounded) until the grid reports a column count that
  // matches the new width: the packer's count is never more than fits, and
  // for Orbital wash (two 2x2 pads and six knobs) it uses at least six
  // columns once nine or more fit.
  const settle = async width => {
    await live.setViewportSize({width, height: 1000});
    await live.waitForFunction(width => innerWidth === width, width);
    await live.waitForFunction(() => {
      const bank = [...document.querySelectorAll('[data-live-instance]')]
        .find(c => c.querySelector('h3')?.textContent === 'Orbital wash')?.querySelector('[data-slot-grid]');
      if (!bank) return false;
      const style = getComputedStyle(bank), unit = parseFloat(style.getPropertyValue('--slot-col')), gap = parseFloat(style.getPropertyValue('--slot-gap'));
      const fit = Math.floor((bank.clientWidth + gap) / (unit + gap)), columns = Number(bank.dataset.columns);
      return columns <= Math.max(2, fit) && (fit < 9 || columns >= 6);
    }, null, {timeout: 5000, polling: 100}).catch(() => null);
    return {...await orbital(), tier: await tier()};
  };
  // A bank mounted inside the pop-out observes it from the pop-out's own
  // window: toggling Detail remounts Slot study's bank, and only the
  // pop-out's counter moves.
  const counts = async () => [await page.evaluate(() => window.__resizeObservers), await live.evaluate(() => window.__resizeObservers)];
  const [mainBefore, popBefore] = await counts();
  const slot = live.locator('[data-live-instance]', {has: live.getByRole('heading', {name: 'Slot study', exact: true})});
  await slot.getByRole('button', {name: 'Expand Slot study controls'}).click();
  await slot.locator('[data-slot-grid=detail]').waitFor();
  await slot.getByRole('button', {name: 'Collapse Slot study controls'}).click();
  await slot.locator('[data-slot-grid=strip]').waitFor();
  const [mainAfter, popAfter] = await counts();
  assert.equal(mainAfter, mainBefore, 'no observer for a pop-out bank is built in Studio\'s window');
  assert.ok(popAfter >= popBefore + 2, `the pop-out builds the observers for its own banks (${popBefore} -> ${popAfter})`);
  const wide = await settle(1400);
  const narrow = await settle(680);
  assert.equal(narrow.tier, wide.tier, 'the pop-out keeps its size tier, so nothing remounts');
  assert.ok(wide.width > narrow.width + 100, `the pop-out's bank narrows (${wide.width} -> ${narrow.width})`);
  assert.ok(narrow.columns < wide.columns, `the popped-out Live panel re-packs when its window narrows (${JSON.stringify(wide)} -> ${JSON.stringify(narrow)})`);
  const unit = 92, gap = 6;
  assert.ok(narrow.columns * (unit + gap) - gap <= narrow.width + 1, 'the re-packed grid fits the narrow window');
  await live.screenshot({path: join(directory, 'desktop-live-window-narrow.png')});
  await live.close();
  return {wide, narrow};
}

// slot-grid/pack's cost in the optimized ClojureScript build, in this page:
// random banks of 5 to 24 cards (a fixed seed), each packed once without the
// view's cache, at a column limit of 8. Returns p50/p99/max milliseconds.
export async function measurePackTime(page) {
  return page.evaluate(() => {
    const pack = globalThis.tolglow?.authoring?.slot_grid?.pack_js;
    if (!pack) return {error: 'pack_js is not exported'};
    let seed = 795;
    const random = () => (seed = (seed * 1103515245 + 12345) % 2147483648) / 2147483648;
    const shapes = [[1, 1], [1, 1], [1, 1], [2, 1], [2, 2]];
    const stats = {};
    for (const [from, to] of [[5, 8], [9, 12], [13, 18], [19, 24]]) {
      const times = [];
      for (let i = 0; i < 60; i++) {
        const n = from + Math.floor(random() * (to - from + 1));
        const spans = Array.from({length: n}, () => shapes[Math.floor(random() * shapes.length)]);
        const start = performance.now();
        pack(spans, 8, false);
        times.push(performance.now() - start);
      }
      times.sort((a, b) => a - b);
      const at = q => +times[Math.min(times.length - 1, Math.floor(q * times.length))].toFixed(2);
      stats[`${from}-${to}`] = {p50: at(0.5), p99: at(0.99), max: +times.at(-1).toFixed(2)};
    }
    return stats;
  });
}

// An LFO removed elsewhere (here, from the REPL) while its editor is open:
// once the show confirms the absence, the editor offers Attach again, with no
// stale Detach or Update (#795 follow-up).
export async function checkOutsideUnbind(page) {
  const card = page.locator('[data-live-instance]', {has: page.getByRole('heading', {name: 'Slot study', exact: true})}).first();
  const id = await card.getAttribute('data-live-instance');
  await card.getByRole('button', {name: 'Attach source for Level', exact: true}).click();
  const panel = card.getByRole('region', {name: 'Level source'});
  await panel.getByRole('button', {name: 'Attach LFO', exact: true}).click();
  await panel.getByRole('button', {name: 'Detach LFO', exact: true}).and(page.locator(':enabled')).waitFor();
  await repl(page, `@(tolglow.show/submit! runtime :modulation-unbind {:instance-id ${JSON.stringify(id)} :param :level})`);
  await panel.getByRole('button', {name: 'Attach LFO', exact: true}).waitFor({timeout: 5000});
  assert.equal(await panel.getByRole('button', {name: 'Detach LFO', exact: true}).count(), 0, 'no stale Detach after an outside unbind');
  assert.equal(await panel.getByRole('button', {name: 'Update LFO', exact: true}).count(), 0, 'no stale Update after an outside unbind');
  await panel.getByRole('button', {name: 'Close source editor'}).click();
}
