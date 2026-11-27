// Capture the real cards at screen width, then verify the relationships a player sees.
import assert from 'node:assert/strict';
import {join} from 'node:path';

function geometry(board) {
  const box = element => {
    const {x,y,width,height} = element.getBoundingClientRect();
    return {x, y, width, height, right:x+width, bottom:y+height};
  };
  return [...board.querySelectorAll('[data-live-instance]')].map(card => {
    const bank = card.querySelector('[data-slot-grid]');
    const cells = [...bank.children].map(cell => {
      const knob=cell.querySelector('.parameter-rotary-control');
      return {box:box(cell), label:box(cell.querySelector('label')), source:box(cell.querySelector('.param-source-trigger')),
        dial:knob?.offsetWidth ? box(knob) : null};
    });
    return {name:card.querySelector('h3').textContent, card:box(card), header:box(card.querySelector('header')),
      title:box(card.querySelector('h3')), phase:box(card.querySelector('.live-effect-phase')),
      tags:[...card.querySelectorAll('.category-tag')].map(box), details:card.querySelector('.live-effect-expand') && box(card.querySelector('.live-effect-expand')),
      actionsBox:box(card.querySelector('.live-effect-header-actions')),
      footer:box(card.querySelector('.performance-instance')), actions:[...card.querySelectorAll('.performance-actions>.button')].map(box),
      bank:box(bank),
      overflow:bank.scrollWidth-bank.clientWidth, cells};
  });
}

const spread = values => values.length ? Math.max(...values)-Math.min(...values) : 0;
export async function checkCardAlignment(page, profile, directory, measurement) {
  const cards = await page.locator('.live-board').evaluate(geometry);
  const orbital = cards.find(card => card.name === 'Orbital wash');
  const rows = new Set(orbital.cells.map(cell => Math.round(cell.box.y)));
  const firstRow = orbital.cells.filter(cell => Math.abs(cell.box.y - orbital.cells[0].box.y) < 1);
  // Cards sit on the slot grid (#795): compare cells of one row and one slot height.
  const oneRow = firstRow.filter(cell => Math.abs(cell.box.height - Math.min(...firstRow.map(c=>c.box.height))) < 1);
  const left = Math.min(...orbital.cells.map(cell=>cell.box.x)), right = Math.max(...orbital.cells.map(cell=>cell.box.right));
  measurement.bank = {cellsInFirstRow:firstRow.length,rows:rows.size,overflow:orbital.overflow,
    unusedLeft:Math.round(left-orbital.bank.x),unusedRight:Math.round(orbital.bank.right-right),
    column:Math.round(Math.min(...orbital.cells.map(cell=>cell.box.width))),
    labelSpread:Math.round(spread(firstRow.map(cell=>cell.label.y))),
    sourceSpread:Math.round(spread(oneRow.map(cell=>cell.source.y))),
    dialSpread:Math.round(spread(oneRow.filter(cell=>cell.dial).map(cell=>cell.dial.y)))};
  const small = cards.filter(card=>card.cells.length<=2);
  const firstCard = small[0];
  const firstCardRow = small.filter(card => Math.abs(card.card.y-firstCard.card.y)<2);
  measurement.smallCards = {titles:small.map(card=>Math.round(card.title.y-card.card.y)),
    footerSpread:Math.round(spread(firstCardRow.map(card=>card.footer.y))),
    footerOffset:small.map(card=>Math.round(card.footer.y-card.card.y))};
  console.info(profile, 'cue bank', measurement.bank, 'action rails', measurement.smallCards);
  const before = process.env.STUDIO_LAYOUT_BASELINE==='1';
  if (!before) {
    for(const card of cards) {
      assert.ok(card.overflow<=1,profile+' '+card.name+' bank fits');
      // A macro member has no Detail toggle: its large size is its Detail (#795).
      assert.ok(card.title.right<=(card.details||card.actionsBox).x+1,profile+' '+card.name+' title clears details');
      assert.ok(card.title.bottom<=card.phase.y,profile+' '+card.name+' state has its own line');
      for(const tag of card.tags) assert.ok(Math.abs(tag.y-card.phase.y)<=2,profile+' '+card.name+' category and state align');
      assert.ok(spread(card.actions.map(button=>button.y))<=1,profile+' '+card.name+' system actions align');
    }
    assert.ok(Math.abs(measurement.bank.unusedLeft-measurement.bank.unusedRight)<=1,profile+' the slot grid is centred in its bank');
    assert.ok(measurement.bank.labelSpread<=1,profile+' bank labels share a baseline');
    assert.ok(measurement.bank.sourceSpread<=1,profile+' bank source targets share a baseline');
    assert.ok(measurement.bank.dialSpread<=1,profile+' numeric dials share a centerline');
    assert.ok(spread(measurement.smallCards.titles)<=1,profile+' cue titles share an inset');
    assert.ok(measurement.smallCards.footerSpread<=1,profile+' parallel card actions share a baseline');
    // Two XYZ pads take large 2x2 slots; the six compact cards fill the rest densely.
    assert.ok(firstRow.length>=2,profile+' cards share the first row');
    for(const cell of orbital.cells) assert.ok(cell.source.width>=44,profile+' source can be touched');
  }
  if(profile==='phone') {
    const card=page.locator('[data-live-instance]',{has:page.getByRole('heading',{name:'Orbital wash'})});
    await card.evaluate(node=>scrollBy(0,node.getBoundingClientRect().top-163));
    await page.screenshot({path:join(directory,'phone-bank.png')});
  }
}
