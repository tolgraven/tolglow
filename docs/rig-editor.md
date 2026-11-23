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
node dev/preview/editor-browser-qa.mjs http://127.0.0.1:8515 /tmp/rig-editor-qa
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

## Aim at a surface

Open **Rig & space → Aim at surface**. Select patched pan/tilt heads in the desired
order, then tap the floor, back wall, side walls, stage top or a configured receiving
plane. The current camera ray picks the nearest front-facing receiver rectangle.
Decorative room meshes have no picking proxy. To enter an exact point, choose the
receiver, enter world X/Y/Z in metres and press **Apply aim**. For an aim point
in open space, choose **World coordinates** with **Aim at one point**. Only a completed
tap changes a head. A drag, cancelled touch or tap outside the receivers has no
effect. Save a placement draft before applying live aim.

* **Aim at one point** assigns the picked world position to each selected head.
  That manual aim overrides its movement cues. Its intensity and color still
  come from the show.
* **Place beams edge to edge** aims the first selected head at the point and
  links the other heads in order along the chosen receiver axis.
* **Follow the moving leader** releases the selected heads' manual aims and
  creates the same links. The first head now follows its movement cues.
* **Release selected** removes their manual aims and each edge link incident
  to a selected head. Their ordinary movement cues continue to run.

Headless hosts use the same queued `:surface-aim` operation. The corresponding
Ring path is `POST /api/surface-aim`. Supply `:targets` in selection order,
`:mode` (`:together`, `:edge-to-edge`, `:follow`, or `:release`), `:surface-id`,
and the world `:point` for a point placement. Edge modes accept `:along` as
`:u`, `:negative-u`, `:v` or `:negative-v`. The show stores links in
`:rig :edge-tracking` and direct targets under `:variables :surface-aim`. Show files retain both.
Restarting a show releases the manual point override. Removing a head from
the patch also releases its manual aim.

Add a receiver to the rig with a unique ID, center, normal, horizontal axis,
and width/height in metres. The vertical axis is the normal crossed with the
projected horizontal axis. The five standard room IDs are reserved.

```clojure
{:surfaces [{:id :scenery/screen :point [0 2 -3] :normal [0 0 1]
             :u-axis [1 0 0] :size [4 2]}]
 :edge-tracking [{:leader :spot-left :follower :spot-right
                  :surface-id :scenery/screen :along [1 0 0]}]}
```

Define links in leader-first order. Each follower has one leader. The show
rejects cycles, unpatched heads and links whose direction leaves the plane.

## Room clearance

Open **Rig → Room & fixtures** to inspect the phone calibration. **Measured fog
clearance** gives the total half-life in seconds, extinction per metre, sightline
length and the fit's R². A clearing-tail measurement can leave extinction
unmeasured. The total already includes ventilation. The configured air changes
per hour and deposition half-life are visible as a fallback.

Choose **Use manual clearance** to remove the measurement from the draft and
edit those two inputs. **Save rig** applies the change to the running show.

## Sculpt a pixel layout

On **Rig**, choose **Pixel layouts** in the 3D editor. Enter a layout ID or let
Studio generate one. **New path** starts an empty strip. Tap or click the scene
to add anchors in wire order. Drag an anchor, choose another editing plane to
change its depth, or enter its X, Y and Z in metres. **Close the path** connects
the last anchor back to the first. The model spaces pixels by distance along
the complete path.

**New grid** gives you row and column counts and two independent 3D step
vectors. Enable **Alternate row direction** for serpentine wiring. **New helix**
uses an origin, radius, height, turns and starting phase around the vertical
axis. Drag the origin on the canvas to move either shape.

Choose the wiring order, start universe and address. Studio shows the inclusive
last occupied slot, including pixels that span a universe boundary. A new layout
starts in the next unused universe after the existing patch. Enter a fixture
group to target the layout alongside other fixtures.

Choose **Individual pixels** to drag a single sampled pixel, or enter its
one-based wire number to reach any pixel and edit its three coordinates.
**Reset pixel to shape** removes the override. Large layouts show up to 2,048
equally spaced wire indices and highlight the exact pixel you select by number.
Dots and handles are placement guides. Light output in the scene follows the
committed rig and running effects.

**Undo** reverses one edit or drag. Cancelling a gesture restores its starting
position. **Discard draft** restores the running rig. **Save rig layout** commits
the shared rig draft, including pending edits to other fixtures or the room.
The saved layout can be reopened from the layout selector.

## Sculpt a cue volume

In **Rig → Sculpt volume**, choose a sphere, cube or rounded box and use
**Replace shape**. Drag the centre in the selected editing plane to move the
whole volume. Drag a coloured axis handle to stretch it about its pivot.
Numeric controls also work by touch and keyboard. **Undo sculpt** restores the
preceding edit. The outer, fainter contour marks the outside falloff distance.

Choose a primitive and use **Union**, **Intersect** or **Subtract** to combine
it with the selected shape. **Editing shape** selects the whole volume or a
specific part for numeric edits. **Carry shape on a bouncing ball** wraps
the geometry in a moving body with an initial velocity and restitution.

Select fixture origins, intersecting beams or heads that can aim into the
volume. Set strength, inside and outside falloff and the response curve.
**Save volume** registers a named volume for spatial controls. **Register +
launch cue** creates a regular cue on the selected page and launches it. Its
paint, registered cues, effects and optional macro travel together. The
resulting cue can be relaunched or stopped from the grid. Under **Parameter
modulation**, bind field or color components to live variables or waveforms.
