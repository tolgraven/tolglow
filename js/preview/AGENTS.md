# Preview scene engine

Read docs/architecture/preview-scene-contract.md before changes. This is the one
specialized Three.js scene owner, not an alternative application UI. Re-frame
owns controls; committed JVM emitted output is authoritative.

- `index.js` selects the lifecycle boundary. `scene-renderer.js` is the single
  scene/target/postprocessing implementation for both execution environments.
- `worker-proxy.js` owns DOM input/camera and visibility; `worker.js` owns the
  OffscreenCanvas and all graphics work. Keep one state message in flight and
  replace pending snapshots. The hardware worker can retain two frames total.
  Await each GPU fence before publishing its bitmap. Same-show emitted output
  can update inputs to the next draw while that fence is pending. Drain before
  scene, control or viewport changes. Close rejected bitmaps, fence show/epoch
  changes and stop all worker resources on disposal.
- The first worker frame decides who presents. Where the browser supports it,
  the proxy then hands the visible canvas to the worker, which shows each
  completed bitmap itself and only reports it. No page task sits between a
  finished frame and the display, so a busy Studio thread cannot pace the room.
  Otherwise the proxy presents one transferred bitmap at a time and acknowledges
  it. Never hand the canvas over before that first frame: startup failure must
  leave it usable by the shared main-thread capability fallback. Page-side
  readers of a handed-over canvas run one animation frame after the report.
- The worker samples the display rate itself and uses the higher of its own and
  the proxy's sample. Starved animation callbacks only ever under-report.
- `render-quality.js` owns physical pixel budgets and asynchronous GPU measurements.
  `film.js` owns the display transform, image antialiasing and grain.
- `geometry.js`, `room.js`, `materials.js` own procedural geometry/materials.
- `math.js` is the explicit rig/output projection. Match CPU elevation/calibration
  and fixture rotation. Never invent light output when emitted-color is absent.
- `fog*` owns the fog field/pass. `room_renderer_fog_fixture_optics` is the sole
  renderer/fog owner; coordinate before starting any graphics edits.
- No transparent cone geometry. Fog uses a spatial density field, opaque depth,
  light visibility, phase scattering and extinction. Zero fog leaves surfaces.
- Work light is an explicit preview-only setting. Display transforms are not lux
  calibration. Procedural venue decoration is not a controlled fixture.
- Rebuild static geometry only for structural rig changes. Bound light/shadow,
  pixel, fog and texture budgets. Pause hidden/offscreen and dispose everything.
- `npm run preview:build` bundles to resources/studio/preview.js and
  preview-worker.js. Never hand-edit
  the generated bundle. Run Node projection tests and actual browser/canvas QA.
- Software GPU screenshots prove rendered appearance and shader execution only;
  retain physical mobile/GPU and photometric limits in documentation.
