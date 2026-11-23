# Offscreen display capture

This is a browser/process adapter, not a second UI application. The product UI
lives in `src/studio/tolglow/studio/push_display.cljs` as re-frame/Reagent code.

- Keep startup explicit. Require an installed Chromium executable and a loopback
  `/push-display` URL; never download a browser during device connection.
- Preserve the stable `tolglowPushDisplay.ready` / `.update(view)` interface and
  exact 960x160, scale-one viewport. Apply a view through synchronous re-frame
  dispatch, then allow React to commit before capture.
- Bound view/PNG sizes, startup/capture deadlines and process lifetime. Only one
  screenshot may be in flight. The JVM owns the latest pending-view mailbox.
- Mount the shared venue renderer only for an explicitly selected scene view,
  await its submitted frame, and release it when leaving. JavaScript is limited to the browser/process integration boundary.
- Synthetic browser tests establish boundary behavior. Measure the actual built
  re-frame page separately and record browser/CPU/environment limitations.
- Native Wayang output keeps the shared exclusive lease. Captures and native
  writes run outside device-input callbacks and pure show/effect computation.
