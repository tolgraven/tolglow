import {test} from 'node:test';
import assert from 'node:assert/strict';
import {CanvasPlayback, canPresentCanvas} from '../../../js/preview/playback.js';

test('one owned canvas capture carries the presented scene until the PiP window closes', async () => {
  const canvas = new EventTarget(), video = new EventTarget(), visibility = [], states = [];
  let starts = 0, stops = 0;
  canvas.addEventListener('tolglow-preview-external', event => visibility.push(event.detail));
  canvas.captureStream = () => { starts++; return {getTracks: () => [{stop: () => stops++}]}; };
  video.play = async () => {};
  video.pause = () => {};
  video.ownerDocument = {pictureInPictureEnabled: true};
  video.requestPictureInPicture = async () => video.dispatchEvent(new Event('enterpictureinpicture'));
  assert.equal(canPresentCanvas(canvas, video, video.ownerDocument), true);
  const media = new CanvasPlayback(canvas, video, value => states.push(value), assert.fail);
  assert.equal(starts, 0);
  await media.prepare(); await media.prepare();
  assert.equal(starts, 1);
  await media.present();
  video.dispatchEvent(new Event('leavepictureinpicture'));
  assert.deepEqual(states, ['preparing', 'ready', 'opening', 'active', 'idle']);
  assert.deepEqual(visibility, [true, false]);
  assert.equal(stops, 1);
  assert.equal(video.srcObject, null);
  media.dispose();
  assert.equal(stops, 1);
});

test('unmount stops an in-flight capture and ignores the late video.play completion', async () => {
  const canvas = new EventTarget(), video = new EventTarget();
  let resolve, stops = 0;
  canvas.captureStream = () => ({getTracks: () => [{stop: () => stops++}]});
  video.play = () => new Promise(done => {resolve = done;});
  video.pause = () => {};
  video.ownerDocument = {};
  const media = new CanvasPlayback(canvas, video, () => {}, assert.fail);
  const pending = media.prepare();
  media.dispose();
  resolve();
  await pending;
  assert.equal(stops, 1);
  assert.equal(media.state, 'idle');
});
