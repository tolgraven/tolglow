# Owned room renderer

`npm run preview:build` produces the ESM entry at `/studio/preview.js` and worker
at `/studio/preview-worker.js` from this
folder. Three.js and esbuild are pinned in the root npm manifest/lock; renderer
telemetry reports the actual Three revision. `studio:build` bundles the
engine before the re-frame release; this is specialized graphics, not a second
application UI. All procedural geometry/textures here are project source; no
third-party model/image assets were imported. Three/addon licensing stays in the
bundled legal notice.

`RoomPreview(canvas, statusElement)` preserves `update(snapshot)`, `resize()`,
`setFog(value)`, `burst(options?)`, `resetCamera()`, and `dispose()`. Additional
controls are `setWorkLight(0..1)`, `setQuality('auto'|'low'|'mobile'|'desktop'|'high')`,
`setCamera('audience'|'overview')`, `focusFixture(id)`, `ready`, `whenReady`, and
`getTelemetry()`. The owned canvas exposes `canvas.tolglowPreview` for inspection;
disposal removes that reference. Re-frame owns UI controls and fullscreen state.
`pause()`/`resume()` control scheduling; `await draw()` requests a current-show
frame while paused. `SceneRenderer` is exported separately for graphics diagnostics
which need field/mesh access; production uses `RoomPreview`.

`scene-renderer.js` contains the only scene/render implementation. With worker
capabilities, `worker.js` owns a new OffscreenCanvas and all WebGL/fog resources.
`worker-proxy.js` keeps DOM OrbitControls and presents transferred ImageBitmaps.
One state message can be in flight; the latest pending graphics projection
replaces intermediate snapshots. Only emitted output, rig and show/frame identity
cross that boundary. One bitmap can be outstanding; the worker waits for its
presentation acknowledgement before scheduling another draw. Consumed, rejected
and stale-show/epoch bitmaps are closed, and show changes clear the old image.
`getTelemetry()` reports the execution thread, coalescing and discarded frames.

The visible canvas is never transferred. A failed worker startup leaves its
context untouched so the same SceneRenderer can acquire WebGL on it. After a
bitmap presentation context exists, errors remain explicit and retain the last
frame; the proxy does not attempt an invalid canvas context switch. Hidden or
offscreen views stop render scheduling, controls remain on the UI thread, and
disposal disconnects listeners/observers, closes pending state and terminates the
worker. Main-thread fallback uses completion-paced backpressure. Actual Safari
and physical-device support still require their own checks.

The scene renders PBR concrete, stage, curtain, passive speaker context, instanced
lattice trusses, and articulated fixture body/yoke/lens geometry. Structural rig
changes rebuild geometry; output snapshots update lights and head orientation.
No browser effect code is evaluated. `emitted-color` is already RGBW/intensity/
master resolved, so it is used directly, without applying those factors twice.
Missing emitted output means an unlit fixture. Explicit worklight and environment
illumination are preview-only and can be set to zero for a dark show.

Rig coordinates remain metres, Y up. The old analytic preview's tilt convention
was wrong: the real CPU renderer uses elevation 0 along +Z and -90 degrees down.
`math.js` prioritizes world `direction`/`aim`, otherwise reverses the CPU servo
calibration and fixture rotation. A checked-in oracle is captured by the actual
`afterglow.render.frame/resolve-frame`, with geometry and servo round-trip tests.
Regenerate it from the repo root with
`clojure -M src/engines/preview/capture-oracle.clj`.

A finite aperture uses the same lens radius as the mesh (default .119m, optional
`lens-radius-m`). The optical origin stays at the saved fixture position. A
virtual cone apex is placed behind that lens by radius/tan(field half-angle),
then both surface lighting and fog are clipped at the actual lens plane. Shadow
near planes start at the lens. A bounded shader uniform applies the same clip to
Three's PBR surface contribution, preventing invented illumination behind the
aperture. The fog integrates the same virtual cone and attenuation. Spotlights
use a soft field-angle/focus estimate; absent fixture photometry is not calibrated
lux, IES, exact focus mechanics or internal strobe simulation.

The pipeline is opaque linear-HDR color plus depth → spatial fog integration →
bilateral depth-aware fog upsample/composite → restrained bloom → ACES/sRGB.
Fog occupies the entire room. Direct cone intervals are analytic, so narrow
shafts are not randomly missed between ray steps. Density extinction and explicit
worklight/reflected-light estimates integrate outside those intervals. The
ambient/bounce term is a bounded hemisphere approximation, **not global
illumination**. The density field carries plume advection/diffusion history;
procedural sub-grid detail is only applied where dense plume material exists.
See [fog.md](fog.md) for field/shader contracts and verification.

Auto detects software renderers and uses a low tier: DPR.75, 180k pixels, one
256px shadow map, a 24-step phone fog budget and 10Hz. This prevents SwiftShader
from inheriting the full desktop work budget. Explicit quality selection remains
available. The fallback starts its idle budget after each completed draw, with
no fixed cap that could let long renders saturate the UI thread. The worker
grows its cadence budget gradually and recovers quickly, so a one-time shader
compile cannot leave later light changes waiting seconds. Neither policy is
proof of real-device frame rate; worker isolation is the control-responsiveness
boundary.

Desktop caps DPR1.5, 1.5M pixels, 4×1024 shadow maps and 64 fixture meshes; mobile
caps DPR1, .5M pixels, 2×512 shadow maps and 32 meshes; high caps DPR1.75, 2.8M
pixels, 4×2048 maps and 128 meshes. All tiers currently light at most 16 fixtures,
matching the host's existing preview-output cap, and report the limit. Hidden/
offscreen scenes pause. Context restoration regenerates environment resources.
Quality changes resample the current density/temperature field and clone active
machine history instead of resetting the atmosphere. Disposal releases controls,
listeners, observers, geometry, unique materials,
textures, shadow maps, render targets and postprocessing resources.

## Current evidence and remaining acceptance

- `npm run preview:test` exercises projection/real CPU oracle, density evolution,
  bounded stepping and actual WebGL2 fog execution where Chromium is available.
- Actual SwiftShader tests verify zero amount/empty field/behind-opaque output
  difference0, real PCF comparison-depth attenuation, exact clear-shadow-path
  parity, no invented blackout light,
  room ambient scattering, narrow-shaft coverage, and finite-aperture width with
  zero scattering behind the optical plane. This is software GPU evidence.
- Current fullscene images are under `/tmp/studio-qa/preview-optics-r187-fixed/`,
  covering blackout, no-fog surface light, one narrow finite-aperture beam,
  eight crossing lights, a dense plume and its later dispersion. All six captures
  have no shader/browser errors; the eight-light scene has four real PCF maps.
  Earlier `/tmp/studio-qa/fog-volume-v2/` and `fog-volume-v3/` pairs separately
  show room haze/plumes with every spotlight black under the preview worklight.
  The r187 controlled images exposed a shadow-texture migration bug missed by
  the older ratio-only test. Sampling comparison depth textures and asserting
  exact unoccluded equality fixed it; all eight beams now remain visible.
- `fog-visual-qa.mjs` reads the real host rig but replaces only the browser-local
  output for deterministic diagnostic scenes; it never mutates a show or sends
  hardware output. Its evidence JSON retains inputs and field times.
- Initial complex-shader compilation in software Chromium can take seconds;
  the captured 5077ms first lit draw is **not a steady-state frame benchmark**.

`lighting-visual-qa.mjs` records controlled blackout, no-fog surface, narrow
finite-aperture, crossing-light, dense-plume and dispersed-plume scenes using
the actual host rig with browser-only output projections. It never modifies the
show or opens hardware output.

`browser-qa.mjs` exercises the actual re-frame application's camera, atmosphere,
worklight, quality and expanded/fullscreen controls at desktop, iPad and phone
viewports. It does not send lighting commands or claim physical touch-device
performance. Captures and per-control dimensions go under `/tmp/studio-qa/preview-app`.
The current source checkpoint passes all 17 preview tests, including real
WebGL2 execution, and the actual desktop/iPad/phone viewport control matrix.
Expanded views pass for all three, desktop fullscreen passes, and every preview
control measured at least 44px high. The software-GPU Auto profile was observed
as `low` in all three contexts. Those tests do not establish integrated show
input latency or physical mobile performance; the separate Studio owner checks
cue/parameter responsiveness with the preview visible.

Visual acceptance remains open. The revised room is materially more detailed
than the analytic box prototype, but thick plume billow detail, current mobile/
iPad layouts and expanded/fullscreen interaction still require direct review.
No AAA, physical photometry, real GPU frame-rate, Safari or device claim follows
from compilation or these controlled captures.
