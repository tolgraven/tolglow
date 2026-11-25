# Preview scene engine

Read docs/architecture/preview-scene-contract.md before changes. This is the one
specialized Three.js scene owner, not an alternative application UI. Re-frame
owns controls; committed JVM emitted output is authoritative.

- `index.js` selects the lifecycle boundary. `scene-renderer.js` is the single
  scene/target/postprocessing implementation for both execution environments.
- `worker-proxy.js` owns DOM input/camera, visibility and bitmap presentation;
  `worker.js` owns the OffscreenCanvas and all graphics work. Keep one state
  message and one transferred bitmap in flight; replace pending snapshots.
  The hardware worker can retain two frames total, including a transferred
  frame awaiting acknowledgement. Await each GPU fence before publishing its
  bitmap, and drain submitted work before applying new scene state or resizing.
  Close rejected bitmaps, fence show/epoch changes and stop all worker resources
  on disposal. Never transfer the visible canvas: startup failure must leave it
  usable by the shared main-thread capability fallback.
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
