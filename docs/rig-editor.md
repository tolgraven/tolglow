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

## Place fixtures from the library

Choose **Place fixture** and a published fixture type, then tap the floor, a
wall, the ceiling or a truss. A truss is picked along its length within 25 cm;
anything else uses the show's receiving planes plus a ceiling at the room's
height. The new fixture sits **Distance from surface** away along the surface
normal and is mounted along it: it stands on floors and the stage, hangs under
the ceiling and trusses, and lies flat on walls with its home beam pointing
down. It takes the next free DMX address after the highest occupied slot (a
fixture that would cross channel 512 starts the next universe) and a fresh
`<type>-<n>` ID that is free in both the draft and the committed rig (a namespaced
type contributes its name only). A tap that drags, a second finger or a tap outside every mount
places nothing.

A placement is a draft edit, so **Undo** removes it and **Save rig** patches
it. The draft entry carries `:placement? true`. On save the host instantiates
every marked entry from its published `:fixture-type-id`, whatever its ID, and
removes the mark: channels, footprint and type data come from the registry; the
ID, name, universe, address, position, rotation, RGBW policy, calibration,
numeric photometry (beam angle, zoom range, lens radius, maximum watts, measured
relative brightness) and any other per-instance keys come from the entry. Its
channels may differ from the type's only by `:inverted-from 0` reversal, and only
where the type has no partial pivot of its own. `:placement? false` is removed and
any other value is refused, so the mark is never stored. The library's patch checks apply, so
a moving head needs its documented pan and tilt travel before it can be saved.
Unmarked entries are never re-derived: renaming a fixture or submitting a whole
saved rig keeps every key, and an untyped fixture without channels still gets
Afterglow's patch error. The REPL uses the same admission; `placement/place`
returns a marked entry:

```clojure
(show/submit! runtime :rig
  (update (:rig (show/snapshot runtime)) :fixtures conj
          (placement/place (get-in @(:state runtime) [:model :registry :fixtures "robe-spot"])
                           (get-in @(:state runtime) [:model :rig :fixtures])
                           {:point [0 4 0] :normal [0 -1 0]})))
```

`tolglow.rig.placement` is shared CLJC: `place`, `mount-rotation`, `remount`,
`face`, `mounting`, `reverse-channel` and `axis-range` are the functions Studio
calls.

## Mount and aim a fixture's home

Selecting a fixture in **Place fixture**, **Aim home** or **Calibrate aim** shows
**Mounting**. The orange line is the home beam (pan 0°, tilt 0°) and the blue
line the mounting axis (the fixture's base-to-head direction). **Standing**,
**Hanging** and the four wall buttons turn the fixture onto that axis and keep its
home beam as close to the previous direction as the mount allows. **Aim home**
drags a fixture's home beam towards the pointer on the editing plane and spins
it only about its mounting axis. The rotation fields remain the exact form.

Moving heads also show **Pan travel** and **Tilt travel**: the documented minimum
and maximum mechanical angle, where home falls in that travel, and **Reverse**,
which sets the channel's `:inverted-from 0` for a motor that runs against the
model. A channel whose type sets a partial pivot keeps it and shows no toggle. Each of these is one undoable draft edit. A reversed legacy
`[centre -half]` calibration shows as its equivalent range; editing it stores
that range with **Reverse** switched, which sends the same DMX for every angle.

## Calibrate a moving head's aim

**Calibrate aim** works on the selected moving head. Tap a measured floor or wall
point, or enter it, then **Check aim**. The host solves the pan and tilt with
Afterglow's renderer from the committed fixture with the draft position, rotation
and travel substituted, and returns mechanical degrees, the 8-bit coarse DMX each channel sends after
its reversal (the high byte of a 16-bit channel, as Afterglow encodes it), and
whether the travel reaches the point. The check never
changes the show. **Send test aim** points the real head at the target through
the ordinary surface-aim command (`:mode :together` in world coordinates), so
the rig must be saved first.

To derive the mount, jog the head until its beam centre hits a target and
**Record current pan/tilt**; the committed output supplies the values, or enter
the DMX values you jogged to. Observation fields show the DMX the channel sends,
after reversal, as a console does; Studio converts them to model values for the
fit. Observations, checks and fits belong to the selected head: selecting another
head starts afresh (the target point stays), drops answers still in flight, and
**Apply fitted rotation** only applies a fit to the head it was made for. With three or more spread targets, **Fit mounting**
runs `tolglow.calibration.geometry/fit-aim`, the fitter the phone calibration
uses. It reports the rotation, standing/hanging/wall, the angular error and the
inliers. Without **Pan/tilt direction and travel match the manual** the result
stays a candidate: axis direction and mounting are ambiguous together. **Apply
fitted rotation** writes it to the draft.

The Ring paths are `POST /api/fixtures/aim-check` with `{:fixture-id :point}` and
`POST /api/fixtures/aim-fit` with `{:fixture-id :samples :axis-verified?}`, each
optionally with `:position`, `:rotation-deg` and `:calibration` overrides. The
REPL calls `(show/fixture-action! runtime :aim-check {...})` and `:aim-fit`.

The fit solves mounting rotation only. It does not fit travel ranges or
reversal; set those from the manual, check aim, and adjust. Physical accuracy
needs a real head and measured targets.

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
Chromium touch and
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
