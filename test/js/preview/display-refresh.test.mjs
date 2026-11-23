import assert from 'node:assert/strict';
import test from 'node:test';
import {DisplayRefresh} from '../../../js/preview/display-clock.js';
import {presentationHz} from '../../../js/preview/render-quality.js';

function display() {
  let tick, nextBurst, elapsed = 0;
  const host = {
    requestAnimationFrame(callback) {tick = callback; return 1;},
    cancelAnimationFrame() {tick = null;},
    setTimeout(callback) {nextBurst = callback; return 2;},
    clearTimeout() {nextBurst = null;}
  };
  const observed = [], sample = new DisplayRefresh(hz => observed.push(hz), host);
  function runBurst(gap) {
    for (let i = 0; tick && i < 80; i++) {
      const callback = tick; tick = null;
      elapsed += gap * (i % 7 === 0 ? 2 : 1);
      callback(elapsed);
    }
    assert.equal(tick, null, 'the sample leaves no continuous animation callback');
  }
  return {sample, observed, runBurst, scheduled: () => !!nextBurst, resample() {
    const callback = nextBurst; nextBurst = null; callback();
  }};
}

test('a foreground burst measures 60 and 120 Hz and stops completely when hidden', () => {
  const d = display();
  d.sample.setActive(true); d.runBurst(1000 / 60);
  assert.deepEqual(d.observed, [60]);
  d.resample(); d.runBurst(1000 / 120);
  assert.deepEqual(d.observed, [60, 120]);
  d.sample.setActive(false);
  assert.equal(d.scheduled(), false);
  d.sample.setActive(true); d.runBurst(1000 / 30);
  assert.deepEqual(d.observed, [60, 120, 30]);
  d.sample.setActive(false);
});

test('very slow callbacks do not relabel the physical display or spin forever', () => {
  const d = display();
  d.sample.setActive(true); d.runBurst(300);
  assert.deepEqual(d.observed, []);
  assert.ok(d.scheduled());
  d.sample.setActive(false);
});

test('hardware quality uses its display budget while software and manual low retain their cap', () => {
  assert.equal(presentationHz('desktop', 'auto', false, 60), 60);
  assert.equal(presentationHz('desktop', 'auto', false, 120), 120);
  assert.equal(presentationHz('mobile', 'mobile', false, 60), 60);
  assert.equal(presentationHz('low', 'auto', false, 60), 60);
  assert.equal(presentationHz('low', 'low', false, 60), 10);
  assert.equal(presentationHz('low', 'auto', false, null), 10);
  assert.equal(presentationHz('low', 'auto', true, 120), 10);
});
