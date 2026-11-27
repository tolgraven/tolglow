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
  bitmap. Same-show emitted output can update inputs to the next draw while
  that fence is pending. Drain before scene, control or viewport changes.
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
- Full lights (the prominence-ranked `fullLights` budget) keep the volumetric
  path: fog uses a spatial density field, opaque depth, light visibility, phase
  scattering and extinction. Zero fog leaves surfaces.
- Transparent cone geometry is allowed only for faked beams (`beams.js`): lights
  outside the full-light budget, and every lit fixture in the low tier's
  non-raymarched haze. Operator decision on #782: measured on an M4 Pro, every
  forward SpotLight and raymarched fog light costs per covered pixel, so
  rigs beyond 8-16 lights showed no beam at all for the rest. Faked cones keep
  those lights visible for about 0.2 ms. They read the same density field and
  opaque depth, scale with the same single-scattering estimate, draw nothing at
  zero fog, and their floor/wall footprints come from `footprints.js`. Never use
  them to replace a full light's volumetric beam.
- Work light is an explicit preview-only setting. Display transforms are not lux
  calibration. Procedural venue decoration is not a controlled fixture.
- Rebuild static geometry only for structural rig changes. Bound light/shadow,
  pixel, fog and texture budgets. Pause hidden/offscreen and dispose everything.
- `npm run preview:build` bundles to resources/studio/preview.js and
  preview-worker.js. Never hand-edit
  the generated bundle. Run Node projection tests and actual browser/canvas QA.
- Software GPU screenshots prove rendered appearance and shader execution only;
  retain physical mobile/GPU and photometric limits in documentation.
