# 3D rig and movement-path editor

Open **Rig & space** to edit the same room and fixture model used by the live
preview. The editor uses re-frame/Reagent; saving and publishing use the shared
Ring operations. Fixture names, IDs, channels, addresses, capabilities and
calibration remain in the draft when a pose changes.

## Place fixtures and trusses

Choose **Move**, then drag a fixture or truss in the scene. Choose the editing
plane: floor XZ, front XY, or side YZ. Moving preserves the object's coordinate
perpendicular to that plane. **Rotate** turns around its normal. The selected
object is outlined; the object selector and precise position/rotation fields
provide keyboard-accessible alternatives. The six nudge buttons move 0.1 metre.
World X is left/right, Y is height, and Z is depth; rotation fields use degrees.

**Explore** restores the normal camera controls, including walk and auto tour.
The camera is suspended while an editing tool is active. Escape cancels a drag
and returns to Explore. Pointer cancellation, lost capture, blur and hidden-page
transitions cancel an unfinished gesture. Existing path handles retain their
height and grab offset when dragged.

Pose changes are drafts. **Save rig** applies the validated rig through
`/api/rig`; a rejected patch remains visible as an error. **Undo** keeps up to
30 editor pose/path checkpoints, with a continuous drag represented by one
checkpoint. **Discard draft** restores the currently committed rig and clears
path authoring. Changing shows or server runtime clears old editor drafts and
pending publication receipts.

The existing fixture library below the editor supplies search/import/add/patch,
and the collapsible top-down placement view remains available. Room dimensions,
fixture patch fields and truss creation remain in their existing panels. These
panels use the same rig draft; their independent edits are not added to the 3D
editor's undo history.

## Draw a moving-head path

1. Select **Draw path**, choose the plane and its offset, and click the scene to
   place points. Snap is in metres; zero disables it. The grid indicates the
   plane for new points. Drag an existing handle to reshape a path, or edit its
   XYZ values in the point list. **Add point** provides a keyboard alternative.
2. Select the patched pan/tilt fixtures that should follow the path. Set its ID,
   name, positive duration in beats/bars/phrases, playback mode, and curve.
   Fractional durations are supported. Close the path to connect the final
   point back to the first. The backend validates 2–256 finite world-space
   points and rejects a zero-length path.
3. Save any rig draft first. **Create cue** atomically publishes the ordinary
   movement effect and its cue without launching it. **Create + run** also
   launches it explicitly: one-shot for Once, persistent for Loop/Back and forth.
   The path controls aim only; combine it with an emission cue to produce light.

Open **Saved paths** to edit a published path. Its original effect/cue identity
and version are retained. Publishing rejects a stale version or an ID already
occupied when creating a new path. Path editor data persists in ordinary show
files, so Export show / Import show retains editable paths and their targets.
The [backend contract](aim-paths.md) describes musical keyframe spacing and
ordinary effect/cue execution.

## Rendering and input ownership

`RigEditorInput` performs bounded CPU screen-space proxy picking and ray/plane
intersection using the current camera. It emits identity-tagged interactions to
re-frame; it never launches lighting commands. No worker reply or GPU completion
is needed to process a pointer. Picking/markers are bounded to 2048 fixtures and
24 trusses; the object selector and numeric fields still address the complete rig.
Detailed fixture meshes and active light counts keep the existing quality caps.

The existing `SceneRenderer` draws both the worker and capability fallback.
Fixture/truss pose changes update transforms while retaining geometry. Structural
changes still rebuild the affected scene. Authoring hints are drawn onto the
presented canvas after the fog/compositor passes, using the same renderer, and
do not contribute physical output or simulated fixture emission. The worker's
latest-state mailbox and show/epoch fencing remain in effect.

## Verification

Run `npm run studio:test`, `npm run studio:build`, and `npm run preview:test`.
The behavioral browser gate is:

```sh
node src/engines/preview/editor-browser-qa.mjs http://127.0.0.1:8515 /tmp/rig-editor-qa
```

Use a dedicated `./bin/studio --port 8515` with no physical-output options. The
script checks the preview sink before mutating its show. It drives the compiled
application and real API, writes screenshots and a JSON evidence report, and
covers placement, undo/cancel, rotation, numeric precision, saves, path creation,
explicit launching, file export/import, touch cancellation, fallback and teardown.
It also checks pixels from the presented canvas so a functioning form cannot
hide an absent 3D path overlay.

The [retained browser evidence](benchmarks/rig-editor-browser.json) records the
validated revision, workflows, pixel check and renderer telemetry. Final gates
passed 164 frontend tests / 882 assertions, 516 JVM pure tests / 4,080 assertions,
38 preview tests, and an optimized 179-file release with zero warnings.
Observed results and scope are also recorded in the work ledger. Chromium touch and
viewport emulation do not verify physical iPad Safari, moving-head mechanics,
room calibration, or high-refresh performance on a hardware GPU.
