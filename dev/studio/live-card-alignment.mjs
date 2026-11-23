// Capture the real cards at screen width, then verify the relationships a player sees.
import assert from 'node:assert/strict';
import {join} from 'node:path';

function geometry(board) {
  const box = element => {
    const {x,y,width,height} = element.getBoundingClientRect();
    return {x, y, width, height, right:x+width, bottom:y+height};
  };
  return [...board.querySelectorAll('[data-live-instance]')].map(card => {
    const bank = card.querySelector('.control-strip');
    const cells = [...bank.children].map(cell => {
      const knob=cell.querySelector('.parameter-rotary-control');
      return {box:box(cell), label:box(cell.querySelector('label')), source:box(cell.querySelector('.param-source-trigger')),
        dial:knob?.offsetWidth ? box(knob) : null};
    });
    return {name:card.querySelector('h3').textContent, card:box(card), header:box(card.querySelector('header')),
      title:box(card.querySelector('h3')), phase:box(card.querySelector('.live-effect-phase')),
      footer:box(card.querySelector('.performance-instance')), actions:[...card.querySelectorAll('.performance-actions>.button')].map(box),
      bank:box(bank),
      overflow:bank.scrollWidth-bank.clientWidth, cells};
  });
}

const spread = values => Math.max(...values)-Math.min(...values);
export async function checkCardAlignment(page, profile, directory, measurement) {
  const cards = await page.locator('.live-board').evaluate(geometry);
  const orbital = cards.find(card => card.name === 'Orbital wash');
  const rows = new Set(orbital.cells.map(cell => Math.round(cell.box.y)));
  const firstRow = orbital.cells.filter(cell => Math.abs(cell.box.y - orbital.cells[0].box.y) < 1);
  measurement.bank = {columns:firstRow.length,rows:rows.size,overflow:orbital.overflow,
    unusedRight:Math.round(orbital.bank.right-firstRow.at(-1).box.right),
    labelSpread:Math.round(spread(firstRow.map(cell=>cell.label.y))),
    sourceSpread:Math.round(spread(firstRow.map(cell=>cell.source.y))),
    dialSpread:Math.round(spread(firstRow.filter(cell=>cell.dial).map(cell=>cell.dial.y)))};
  const small = cards.filter(card=>card.cells.length<=2);
  const firstCard = small[0];
  const firstCardRow = small.filter(card => Math.abs(card.card.y-firstCard.card.y)<2);
  measurement.smallCards = {titles:small.map(card=>Math.round(card.title.y-card.card.y)),
    footerSpread:Math.round(spread(firstCardRow.map(card=>card.footer.y))),
    footerOffset:small.map(card=>Math.round(card.footer.y-card.card.y))};
  const before = process.env.STUDIO_LAYOUT_BASELINE==='1';
  if (!before) {
    for(const card of cards) {
      assert.ok(card.overflow<=1,profile+' '+card.name+' bank fits');
      assert.ok(spread(card.actions.map(button=>button.y))<=1,profile+' '+card.name+' system actions align');
    }
    assert.ok(measurement.bank.unusedRight<=1,profile+' full bank width');
    assert.ok(measurement.bank.labelSpread<=1,profile+' bank labels share a baseline');
    assert.ok(measurement.bank.sourceSpread<=1,profile+' bank source targets share a baseline');
    assert.ok(measurement.bank.dialSpread<=1,profile+' numeric dials share a centerline');
    assert.ok(spread(measurement.smallCards.titles)<=1,profile+' cue titles share an inset');
    assert.ok(measurement.smallCards.footerSpread<=1,profile+' parallel card actions share a baseline');
    assert.equal(firstRow.length,profile==='phone'?4:8,profile+' all controls in the expected grid');
    assert.equal(rows.size,profile==='phone'?2:1,profile+' expected number of bank rows');
    for(const cell of orbital.cells) assert.ok(cell.source.width>=44,profile+' source can be touched');
  }
  if(profile==='phone') {
    const card=page.locator('[data-live-instance]',{has:page.getByRole('heading',{name:'Orbital wash'})});
    await card.evaluate(node=>scrollBy(0,node.getBoundingClientRect().top-163));
    await page.screenshot({path:join(directory,'phone-bank.png')});
  }
}
