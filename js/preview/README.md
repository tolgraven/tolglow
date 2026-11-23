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
cross that boundary. A WebGL completion fence resolves before a bitmap is
published; the worker polls it asynchronously while holding, without applying,
the one upstream state message. One bitmap can be outstanding; the worker waits
for its presentation acknowledgement before scheduling another draw. Consumed, rejected
and stale-show/epoch bitmaps are closed, and show changes clear the old image.
`getTelemetry()` reports the execution thread, coalescing and discarded frames.

The visible canvas is never transferred. A failed worker startup leaves its
context untouched so the same SceneRenderer can acquire WebGL on it. After a
bitmap presentation context exists or real rendering has started, errors remain
explicit and retain the last frame; the proxy does not switch an active or
stalled renderer onto the main thread. Hidden or
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
Spotlight slots remain present with zero RGB during blackout. The shadow budget
has a stable slot count, preferring active fixtures and filling unused slots with
dark fixtures. This warms the actual shader layout before readiness, avoiding a
new light-count program when a cue starts; zero RGB still emits exactly no light.
Telemetry separates active emission count from allocated shadow slots.

Rig coordinates are metres with Y up. The local beam points along +Z at 0°
elevation. The bounded show projection includes `:pose` from Afterglow for
each selected head with two committed servo channels. Its mechanical degrees
retain the chosen wrap, and its world direction includes clamped travel and
mount rotation. The yoke, lens, spotlight, shadow and fog follow this pose.
Samples lacking both axes can still display an available world direction or aim.
Optional interpolation uses the two mechanical angles and snaps across large
end-stop reversals. Calibrated motion describes the command sent to the fixture;
motor response and optical calibration depend on the actual hardware.

Regenerate the CPU fixture oracle from the Tolglow root with
`lein run -m clojure.main dev/tolglow/capture_preview_oracle.clj`.

A finite aperture uses the same lens radius as the mesh (default .119m, optional
`lens-radius-m`). The optical origin stays at the saved fixture position. A
virtual cone apex is placed behind that lens by radius/tan(field half-angle),
then both surface lighting and fog are clipped at the actual lens plane. Shadow
near planes start at the lens. A bounded shader uniform applies the same clip to
Three's PBR surface contribution, preventing invented illumination behind the
aperture. The fog integrates the same virtual cone and attenuation. Spotlights
use a soft field-angle/focus estimate; absent fixture photometry is not calibrated
lux, IES, exact focus mechanics or internal strobe simulation.

A fixture’s `beam-angle` is its **full included angle in degrees**. A defined
angle stays fixed as cues change unless the fixture supplies `zoom-range-deg`.
With no optics data, the preview uses 12° at minimum zoom and 46° at maximum.
The fixture's projected `relative-brightness` scales lens emission, surface
illumination and direct fog scattering once. Zero represents no visible output.

With `pseudo-fixture` enabled, Afterglow’s projected `beam-footprint` gives
Studio the center, axes and radii of the ellipse on the receiving surface.
The top of the stage, the three opaque room walls, the floor and finite custom
receiver panels participate. The preview tests five points on the ellipse
against actual scene meshes. An intervening curtain, speaker, truss or other
fixture removes the blocked sample from the reflected energy. The remaining
area drives a colored diffuse lobe directed back into the room. Its intensity
depends on distance, incidence, material albedo and the fog along the incident
path. Nearby surfaces receive more of this spill. The ellipse radius bounds
near-field irradiance. A shadow map tests visibility on the reflected path.

The phone and iPad quality tiers allocate one shadowed reflection, the desktop
tiers up to two, only for rigs opting in. A fixed light pool follows the strongest
available patches. A changed color, occluded patch or blackout updates that pool
from the committed output. Surface textures use mean linear reflectance. The
five samples approximate clipped and partly shadowed areas for one diffuse
reflection. The resulting preview lights have no DMX address.

`npm run preview:test` runs the browser test files sequentially so independent
software WebGL contexts do not contend with the worker responsiveness check.

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
pixels, 4×2048 maps and 128 meshes. All tiers share up to 16 persistent spotlights. The host projects up to 128
emitting targets across universes, ranked by moving-head channels, peak
emitted RGB, then stable ID hash. Up to 16 fixed emitters retain coverage when moving
heads saturate the projection. The scene likewise includes an emitting fixed
fixture in its spotlight budget for a mixed rig. Sparse frames include up to 16 rows total
with idle fixtures filled in patch order. Only selected compact rows expand
into maps and receive calibrated poses. The scene assigns its mesh budget to
the selected rig fixture, then emitting fixtures, retaining occupants on ties.
Shadow-mapped slots get first choice of the active emitters. Their textures
survive a solo changing fixture ID. The camera tour visits lit fixtures actually
assigned a spotlight. Unprojected fixtures may be selected and positioned using
their rig definition. Their lenses emit no light. All physical universes still
receive the complete packed frame. Hidden/
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
- `dev/preview/fog-visual-qa.mjs` reads the real host rig but replaces only the browser-local
  output for deterministic diagnostic scenes; it never mutates a show or sends
  hardware output. Its evidence JSON retains inputs and field times.
- Initial complex-shader compilation in software Chromium can take seconds;
  the captured 5077ms first lit draw is **not a steady-state frame benchmark**.

`dev/preview/lighting-visual-qa.mjs` records controlled blackout, no-fog surface, narrow
finite-aperture, crossing-light, dense-plume and dispersed-plume scenes using
the actual host rig with browser-only output projections. It never modifies the
show or opens hardware output.

`dev/preview/browser-qa.mjs` exercises the actual re-frame application's camera, atmosphere,
worklight, quality and expanded/fullscreen controls at desktop, iPad and phone
viewports. It does not send lighting commands or claim physical touch-device
performance. Captures and per-control dimensions go under `/tmp/studio-qa/preview-app`.
The current source checkpoint passes all 20 preview tests, including real
WebGL2 execution, and the actual desktop/iPad/phone viewport control matrix.
Expanded views pass for all three, desktop fullscreen passes, and every preview
control measured at least 44px high. The software-GPU Auto profile was observed
as `low` in all three contexts. Those tests do not establish integrated show
input latency or physical mobile performance; the separate Studio owner checks
cue/parameter responsiveness with the preview visible.

Worker verification additionally exercises a real eight-light scene while a
20ms UI callback continually supplies projections for ten seconds; RAF-driven
DOM changes also check compositor progress. State
coalescing, an epoch change during an actual draw, bitmap ownership, resizing,
disposal and an asynchronous worker startup exception all pass. A subsequent
same-page Studio pause/render comparison exposed a shared GPU compositor backlog:
bitmap transfer acknowledgement completed while submitted GPU work was still
queued. The worker now waits for actual GPU completion before publishing and
uses that total duration for pacing. In the first fenced Chromium/SwiftShader
run, 495 UI callbacks had a maximum gap of 101ms and 40 frames completed in ten
seconds. A representative frame submitted in 4.5ms but took another 100ms to
finish; the recovered budget was 210ms rather than pretending it cost 4.5ms.
`workerFrameMs` includes submission, `gpuCompletionWaitMs` and bitmap transfer
cost, separately reported as `bitmapTransferMs`. Fence polling measures wall time
until completion, not precise GPU elapsed queries or physical-device throughput.
Evidence is in `/tmp/studio-qa/worker-test-fenced.log` and
`/tmp/studio-qa/preview-worker-app/`.
The black-to-lit transition kept all 18 linked programs, with no extra shader
variant. GitHub CI installs the pinned Playwright Chromium and supplies its
executable explicitly, so these browser tests cannot silently skip there.

The actual same-page Studio gate now passes while rendering: 29 parameter edits
over ten seconds had DOM response median 36.1ms, p95 188ms, maximum 207ms; the
paused comparison had median 23.8ms. Actual command responses had median 124ms
and maximum 208ms, with no browser errors. The prior unfenced renderer grew to
8.3s DOM delays on the same workflow. Evidence:
`/tmp/studio-qa/visible-worker-controls-fenced-{results,summary}.json` and `.png`.
The measured final software-GPU frame took 248ms including completion and paced
at 495ms. This accepts control responsiveness for that measured workload, not
high preview FPS or a physical iPad/Safari performance claim.

Visual acceptance remains open. The revised room is materially more detailed
than the analytic box prototype, but thick plume billow detail, current mobile/
iPad layouts and expanded/fullscreen interaction still require direct review.
No AAA, physical photometry, real GPU frame-rate, Safari or device claim follows
from compilation or these controlled captures.

### Calibrated fog material

The ray march, incident beams and reflected-light samples use Afterglow's
`:optics/fog-transmission` material. Distances are metres. A spatially varying
plume is integrated in short segments. Each segment consumes the locally
sampled density and the room's calibrated extinction. Direct illumination on
an off-axis sample travels the distance from the optical plane to that point.

After changing the Afterglow material, refresh the static module and bundles
from the Tolglow root:

```sh
lein run -m afterglow.render.fog-optics js/preview/fog-optics.generated.js
npm run preview:build
```

`lein test tolglow.fog-optics-test` detects stale source. The browser fog test
renders a uniform eight-metre sightline at two calibrated extinction values
and compares the readback to the scalar material.
