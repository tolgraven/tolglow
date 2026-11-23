// The video consumes the already presented canvas, including worker bitmaps.
// A capture owns its media tracks until PiP closes, preparation is cancelled,
// or the Studio panel unmounts. It never acquires the microphone or camera.
export function canPresentCanvas(canvas, video, doc = document) {
  return typeof canvas?.captureStream === 'function' && Boolean(
    (doc.pictureInPictureEnabled && video.requestPictureInPicture) ||
    (video.webkitSetPresentationMode && video.webkitSupportsPresentationMode));
}

export class CanvasPlayback {
  constructor(canvas, video, changed, failed) {
    this.canvas = canvas;
    this.video = video;
    this.changed = changed;
    this.failed = failed;
    this.state = 'idle';
    this.external = false;
    this.abort = new AbortController();
    const options = {signal: this.abort.signal};
    const onEnter = () => {
      if (!this.stream) return;
      this.publish(true);
      this.set('active');
    };
    video.addEventListener('enterpictureinpicture', onEnter, options);
    video.addEventListener('leavepictureinpicture', () => this.release(), options);
    video.addEventListener('webkitpresentationmodechanged', () => {
      if (video.webkitPresentationMode === 'picture-in-picture') onEnter();
      else if (this.state === 'active') this.release();
    }, options);
  }

  set(state) { this.state = state; this.changed(state); }

  publish(visible) {
    if (this.external === visible) return;
    this.external = visible;
    this.canvas.dispatchEvent(new CustomEvent('tolglow-preview-external', {detail: visible}));
  }

  async prepare() {
    if (this.state !== 'idle') return;
    this.set('preparing');
    try {
      // Render resolution comes from the active renderer. Asking captureStream
      // for a maximum frame rate does not create pixels or schedule new frames.
      const stream = this.canvas.captureStream(60);
      this.stream = stream;
      this.video.srcObject = stream;
      await this.video.play();
      if (this.stream === stream && !this.abort.signal.aborted) this.set('ready');
    } catch (error) {
      if (!this.abort.signal.aborted && this.state === 'preparing') {
        this.release(); this.failed(error.message);
      }
    }
  }

  async present() {
    if (this.state !== 'ready') return;
    this.set('opening');
    try {
      if (this.video.ownerDocument.pictureInPictureEnabled && this.video.requestPictureInPicture) {
        await this.video.requestPictureInPicture();
      } else if (this.video.webkitSupportsPresentationMode?.('picture-in-picture')) {
        this.video.webkitSetPresentationMode('picture-in-picture');
      } else {
        throw new Error('This browser cannot present captured canvas video in Picture in Picture.');
      }
    } catch (error) {
      if (!this.abort.signal.aborted) { this.release(); this.failed(error.message); }
    }
  }

  close() {
    const doc = this.video.ownerDocument;
    if (doc.pictureInPictureElement === this.video) doc.exitPictureInPicture().catch(() => {});
    else if (this.video.webkitPresentationMode === 'picture-in-picture') {
      this.video.webkitSetPresentationMode('inline');
    }
    this.release();
  }

  release() {
    this.publish(false);
    this.video.pause();
    this.video.srcObject = null;
    this.stream?.getTracks().forEach(track => track.stop());
    this.stream = null;
    if (this.state !== 'idle') this.set('idle');
  }

  dispose() { this.abort.abort(); this.close(); }
}
