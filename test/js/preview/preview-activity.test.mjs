import test from 'node:test';
import assert from 'node:assert/strict';
import {PreviewActivity} from '../../../js/preview/preview-activity.js';

test('intersection, document visibility, window focus and page lifecycle all gate the preview', () => {
  const doc = new EventTarget(), win = new EventTarget(), changes = [];
  let intersect, disconnected = 0, focused = true;
  doc.hasFocus = () => focused;
  win.IntersectionObserver = class {
    constructor(callback) {intersect = (ratio, isIntersecting = ratio > 0) => callback([{intersectionRatio: ratio, isIntersecting}]);}
    observe() {}
    disconnect() {disconnected++;}
  };
  const gate = new PreviewActivity({}, active => changes.push(active), doc, win);
  assert.equal(gate.active, false, 'await the first intersection, including clipped ancestors');
  intersect(0, true); intersect(.5); intersect(.75);
  doc.hidden = true; doc.dispatchEvent(new Event('visibilitychange'));
  focused = false; win.dispatchEvent(new Event('blur'));
  doc.hidden = false; doc.dispatchEvent(new Event('visibilitychange'));
  assert.equal(gate.active, false, 'visible tab in a background window');
  intersect(0); focused = true; win.dispatchEvent(new Event('focus'));
  assert.equal(gate.active, false, 'focused window with canvas offscreen');
  intersect(1); win.dispatchEvent(new Event('pagehide'));
  win.dispatchEvent(new Event('focus')); intersect(.1);
  assert.equal(gate.active, false, 'page awaiting restoration');
  win.dispatchEvent(new Event('pageshow')); doc.dispatchEvent(new Event('fullscreenchange'));
  assert.deepEqual(changes, [true, false, true, false, true]);
  gate.dispose(); intersect(0); win.dispatchEvent(new Event('blur'));
  assert.deepEqual(changes, [true, false, true, false, true]);
  assert.equal(disconnected, 1);
});
