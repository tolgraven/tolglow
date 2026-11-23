# iPhone LiDAR rig calibration

The first implementation is a tap-assisted capture pipeline, a tested pure fitter,
and a live Ring calibration service. The phone collects actual ARKit camera,
scene-depth and color observations; the host identifies a selected fixture from
its settled color sequence and proposes position and mounting rotation. Applying
that proposal uses the existing rig validator and preserves fixture IDs,
definition references, DMX addresses, sparse/fine channels and axis calibration.

It does **not** yet automatically detect/segment fixture lenses in video, infer
hidden fixtures, infer unknown pan/tilt motor profiles, export reconstructed room
meshes, or establish physical measurement accuracy. Unsigned iOS/watchOS targets now build in macOS CI; a LiDAR device is still
required to verify capture and physical measurement accuracy. No physical output was activated during
implementation/testing.

## Workflow

1. Start the host with a deliberately configured physical sink and independent
   output publisher. Merely opening the scanner or connecting a preview host
   cannot output calibration patterns. Existing OLA/Art-Net sinks remain the
   output boundary.
2. Open **Scan rig** on the phone, connect to the Studio origin and explicitly
   start its camera. The app checks camera permission and ARKit `.sceneDepth`
   support. A device without LiDAR gets an explanation instead of invented depth.
3. Tap a visible floor point corresponding to the rig's origin. Select **Floor
   +Z**, and tap a second level floor point at least half a metre away in the
   rig's positive-Z direction. Gravity supplies +Y. This is an operator-provided
   alignment, not automatic correspondence with an existing room.
4. Select fixtures and explicitly **Arm calibration output**. The host creates a
   session scoped to those IDs. **Next pattern** publishes a temporary, high
   priority effect through the ordinary cue/definition/instance control path.
   Only one selected fixture is driven at a time. Other human instances keep
   running and advancing; cancellation does not rewind their state.
5. Tap the fixture lens with **Lens** selected. Keep it visible while advancing
   through red, green and blue, holding each pattern for at least two authoritative
   transport beats. A stopped transport prevents advancement; physical time is
   used only for camera/output alignment, settling and connection leases. Motor
   poses repeat this code at four pan/tilt settings. Tap the solid beam landing
   spot with **Beam spot** for each new pose. Surface depth and color are captured
   at up to 10 Hz per track. A tracked point moving more than 15 cm away from its
   selected AR-world point is rejected; this is not a video object tracker.
6. **Finish / release calibration lights**, then **Fit captured evidence**. Position
   needs a lens track seen through all three colors and at least three depth
   samples. Aim needs at least three geometrically diverse measured beam targets.
   Verify the fixture's mechanical pan/tilt calibration before allowing the fitter
   to classify standing/inverted/sideways mounting.
7. Review the candidate's metric residual, inliers and unresolved diagnostics.
   Applying a reviewed candidate validates the whole rig and rejects unrelated rig
   edits made since the snapshot. Multiple candidates may be applied sequentially.
   Studio's existing 3D rig editor is the manual correction surface; this change
   does not introduce a competing scene/editor model.

Starting a new AR world/reset requires a new scan and alignment. Backgrounding,
AR interruption or a transport failure stops capture/heartbeats. Foreground
cancellation is best effort; the host independently releases calibration instances
when its 15-second lease expires. A session also has a 15-minute hard limit.
Restoring a show or creating a host from an armed artifact suspends calibration
and removes only its owned instances. The normal show state keeps its own behavior.

## Data and numerical conventions

The artifact is stored at `[:extensions :tolglow.calibration/session]`, so the
existing complete-show serializer includes observations, successful evidence,
candidates, rig snapshots and review state. It contains EDN/JSON-compatible data,
not AR sessions, pixel buffers, sockets, callback closures or credentials. The
original rig is retained separately from the optimistic current base rig.

All positions are metres in right-handed, +Y-up coordinates. Matrices are row-major
proper rigid 4×4 transforms. ARKit camera coordinates look down -Z, while image
pixel Y increases downward. Given original captured-image intrinsics
`[fx fy cx cy]`, pixel `[u v]` and depth `z`, camera position is:

```clojure
[(* (/ (- u cx) fx) z)
 (* -1 (/ (- v cy) fy) z)
 (- z)]
```

Depth is distance from the **camera plane**, not distance along the ray. The native
adapter maps captured-image pixel coordinates to the smaller registered depth map;
it does not substitute depth-map resolution into the original intrinsics. Both
camera pose and explicit AR-world-to-rig alignment are applied. Confidence-zero,
out-of-bounds, nonfinite and zero/negative depth are rejected. Lens positions and
beam landing points remain different observation kinds.

Position uses a median-seeded, weighted outlier fit and reports RMS in metres.
Aim uses bounded deterministic rotation hypotheses, angular consensus and a
weighted quaternion rotation fit. Resulting rotations use the renderer's existing
X-then-Y-then-Z Euler convention. The **actual Afterglow renderer** is the test
oracle, including negative `half-circle` calibration values and coarse/fine DMX.
The fit keeps known axis direction/offset/range intact. An unknown axis reversal
can be absorbed into a different mounting rotation, so it cannot honestly infer
both from these observations alone. Without an explicitly verified axis profile,
position may be reviewable while mounting remains `unknown`.

Reported residuals measure agreement of these observations, not absolute LiDAR
accuracy or a calibrated statistical confidence interval. Reflections, a lens's
physical offset from its motor pivot, depth bias, wall/fog confusion, insufficient
viewpoints and poor pose spread remain real measurement concerns. Rescan or use
manual correction when evidence is weak. Profile/pivot offsets and automatic
axis-profile identification require additional measured constraints.

## Physical publication evidence

Command admission is **not** proof that a pattern reached output. The evaluator
attaches a session/step marker and expected controlled-channel bytes. The normal
renderer supplies those expected bytes, including channel inversion and coarse/fine
scatter. After a complete render, the scheduler checks those slots against the
actual immutable packet. It offers only small metadata to an optional bounded
mailbox **after a successful synchronous sink send**. The publisher does not invoke
a calibration callback, fit observations, or copy another universe set.

A receipt records epoch, publisher generation, source frame ID/sample timestamp,
actual publication timestamp, presentation policy, verification and dropped-offer
count. Faded, interpolated, blackout/waiting, mismatching or gap-spanning frames
cannot identify a fixture. Repeated unmodified presentations of a frozen frame are
valid stable-pattern evidence. Receipt success means the configured sink returned
success; it is not an acknowledgement of photons or DMX fixture motion. Camera
color agreement and settling intervals provide separate observation evidence.

The native client estimates AR-frame-to-host clock offset using an HTTP round trip
bracketed by AR frame timestamps. It requires ongoing frame intervals/arrival age
at most 50 ms and includes that bound in uncertainty. The host rejects uncertainty
over 100 ms. These are initial conservative timing assumptions that need device
measurement, particularly rolling shutter and long exposures. An observation's
entire uncertainty interval must lie in a continuous run of compatible successful
receipts, with no inter-receipt gap over 100 ms. The first symbol after a motor pose
waits 750 physical milliseconds; subsequent symbols wait 150 ms. Slow motors and
moving spots still need longer operator holds and may fail residual checks.

The pending native observation queue is 128, uploaded in batches of at most 64.
The host accepts at most 128 per request, retains at most 4096 observations and a
4096-receipt recent ring. Successful matches retain their own receipt proof with
the observation before that ring rolls over, so earlier fixtures remain fit-able.
A capture exceeding observation capacity stops visibly; split a large rig into
smaller calibration sessions. The output mailbox holds 256 receipts; drops are
explicit and invalidate exposure windows. HTTP GET returns a summary rather than
retransmitting all capture evidence each second. Complete show files retain the
full artifact.

## Ring and REPL contract

`GET /api/calibration` returns runtime/epoch, physical clock, output capability,
fixture summaries and the current session summary. `POST /api/calibration` uses
the existing guarded Ring operation queue and normal JSON normalization:

- `:start`: fresh `:session-id`, selected `:fixture-ids`, `:world-id`, rigid
  `:world-to-rig`, `:clock {:offset-ms … :uncertainty-ms …}`, explicit `:armed? true`.
- `:step`: session ID and next sequential `:step-index`; the transport must have
  advanced at least two beats since the previous step.
- `:observe`: session ID and bounded `:observations` with original camera metadata.
- `:heartbeat`: renew only an already active session's 15-second lease.
- `:cancel`: release only this session's instances/temporary definitions, retain
  evidence. Also used for normal finish before fitting.
- `:solve`: after release, optionally supply `:verified-axis-fixture-ids`. The host
  computes the fit outside the frame-owner lock, then admits it only if the
  evidence revision still matches. Direct client submission of a solved result
  is rejected.
- `:apply`: explicitly selected reviewed `:fixture-ids`, with rig-change validation.

REPL clients use `studio/submit! runtime :calibration payload`; pure offline tools
may use `tolglow.calibration.session` and `tolglow.calibration.geometry` directly
with captured/synthetic observations. No alternate OSC listener is introduced.

## Measure a wall, floor or platform

1. Connect the iPhone to Studio, start the LiDAR camera and mark the floor origin and positive Z direction. This workflow works on a preview-only host.
2. Walk along the surface so ARKit can track its extent. Choose **Room surface** and tap inside the tracked patch. The yellow outline freezes the smallest enclosing rectangle of the detected boundary. Review its width, height and center in rig metres. A rectangle can span a doorway or an irregular gap. Move and tap again if the estimate is poor.
3. Enter a short name such as `booth-wall` and tap **Add receiver to rig**. To update it in a later scan, choose the same name under **Remeasure a saved surface** and tap **Replace saved measurement**. Studio can use `scan-booth-wall` as an aim or footprint receiver. A running cue that targets this surface can follow its new position.
4. Save the show normally. Its Transit document includes the plane geometry and the AR world and anchor IDs. Camera frames stay on the phone. Existing room walls and other receiving surfaces remain in the preview, so check for coincident or obstructing surfaces.

`GET /api/calibration` includes names and revisions of measured receivers. `POST /api/venue-surface` accepts the reviewed geometry, `:surface-name`, `:world-id`, `:anchor-id`, `:runtime-id` and `:expected-epoch`. Supply the listed `:expected-surface-revision` to update an existing measurement. An edit to its geometry invalidates the revision. Other rig edits survive. Clojure callers can submit the same map with `(show/submit! runtime :venue-surface measurement)`.

Reality tracking supplies an oriented rectangular extent. The reference-card procedure below measures neutral diffuse return. Textures, mesh topology and individually resolved LED emitters require separate measurements. Compare scans of a measured wall and platform from several viewpoints on a physical LiDAR device to quantify placement, orientation and size errors.

## Validation and remaining work

Evidence recorded for this initial implementation:

- Focused calibration: **8 tests / 84 assertions**, all passing. Integrated pure:
  **465 tests / 3,690 assertions**, all passing; the subsequent parity lane:
  **487 tests / 3,817 assertions**, all passing. Parity includes the full-show import
  regression added after the pure run.

- Focused Clojure tests cover coordinate transforms, reflection/scale rejection,
  depth outliers, actual renderer roundtrips, reversed axes, mounting ambiguity,
  temporal/color matching, gaps and failed sends, bounded mailbox behavior,
  portable evidence, stale rig rejection, host arming/cancellation/lease cleanup,
  Ring clock access and native wire normalization.
- The existing pure test lane remains required, including all scheduler tests.
- `python3 native/Tests/check_contract.py` and
  `python3 native/Tests/check_rig_calibration.py` are static manifest/source checks.
- After merging master through `ff24662`, [CI run 44891609591](https://github.com/tolgraven/tolglow/actions/runs/44891609591)
  passed Java 17/21/25 pure and parity lanes, Studio and the native gate. The parity
  lane ran **529 tests / 4,448 assertions**. Local parity before the final aim-path
  merge ran **521 tests / 4,397 assertions** against the workflow-pinned Afterglow.
- [The macOS job](https://github.com/tolgraven/tolglow/actions/runs/44891609591/job/136808451227)
  used **Xcode 16.4 / Swift 6.1.2**: all **seven Swift package tests** passed and
  both **unsigned iOS/watchOS simulator targets built successfully**. The first SDK
  build found a pre-existing Watch `DisclosureGroup` availability error; its
  device/session details now use a supported `NavigationLink` screen.
- The native CI gate runs when native sources or its workflow change. It performs
  static contracts, Swift tests, XcodeGen and `native/Tests/check_macos.sh`.
- **Still unrun:** simulator app launch, signing, real camera permission/UI, LiDAR
  accuracy, physical fixture output, motor calibration and device timing. A passed
  compiler/build gate does not establish these. Follow the build with a deliberately
  armed controlled-rig capture and independent manual survey comparison.

Next work: automatic lens/blob tracks and occlusion handling; adaptive confidence
and timing from measured exposure/output latency; explicit pivot offsets and
multi-axis profile identification; camera/world relocalization; measured mesh
export into the shared rig editor; calibration overlay/review in re-frame Studio;
and tested device-specific fixtures with independent measured ground truth.

Primary Apple references:

- [Displaying a point cloud using scene depth](https://developer.apple.com/documentation/arkit/displaying-a-point-cloud-using-scene-depth)
- [ARDepthData](https://developer.apple.com/documentation/arkit/ardepthdata)
- [ARWorldTrackingConfiguration](https://developer.apple.com/documentation/arkit/arworldtrackingconfiguration)
- [ARCamera](https://developer.apple.com/documentation/arkit/arcamera)
- [ARFrame timestamp](https://developer.apple.com/documentation/arkit/arframe/timestamp)
- [Visualizing and interacting with a reconstructed scene](https://developer.apple.com/documentation/arkit/visualizing-and-interacting-with-a-reconstructed-scene)

## Measure a receiver's diffuse return

A matte neutral card with a rated reflectance fraction and tolerance provides a local reference under one fixture's white spectrum. Use an already measured receiver and a stationary fixture with rig coordinates and a saved white brightness measurement. Complete fixture photometry first if the fixture list is empty.

1. Aim the fixture at the receiver center shown in **Scan rig → Measure surface reflectance**. Use the front of the plane. Keep the room clear of fog, changing ambient light, shadows and specular glare. The host checks source distance and incidence against the rig and rejects grazing or back-side illumination.
2. Mount the rear wide camera on a tripod facing the same point. Enter the nominal fraction and absolute tolerance printed on the reference card's certificate. For example, an 18% card with a one-percentage-point tolerance uses `0.18` and `0.01`. Remove any diffuser used for fixture photometry.
3. Choose shutter speed and arm. The host lights the selected fixture full white and darkens other patched emitters through the existing photometry lease. Exposure, focus, white balance and SDR transfer stay fixed. Cover the entire outlined camera sampling area with the card flush against the receiver. Tap **Measure reference**.
4. Remove the card and tap **Measure uncovered surface**. Replace the card flush at the same point and tap **Verify reference**. Each action acquires 15 linearized frames with the fixture dark and 15 with it white. The phone checks its stance and the host checks that the same physical phase remained confirmed across exposure. Movement, clipping, weak light, flicker or a reference drift above 5% invalidates the run.
5. Review the card-relative ratio, the reflectance estimate and its observed range. **Save to receiver** stores the three pairs' summaries, reference rating, source geometry, shutter and derived values at `[:rig :surfaces i :reflectance]`. The WebGL receiver uses the neutral estimate for both direct illumination and secondary reflected light. Save the show to keep the measurement.

The two card signals bracket the surface reading. Dark subtraction removes steady ambient light. The estimated fraction equals the surface signal divided by the mean card signal, multiplied by the card's rated fraction. Measured source distance, incidence and brightness identify the illumination. Their common factors cancel because the card and bare surface occupy the same spot. The range uses the 10th and 90th percentile frame readings of each phase, both card observations and the card's rated tolerance. It describes observed variation. Unknown iPhone image processing, spectral response, tripod translation, surface curvature, card placement and non-diffuse reflections can contribute additional error. Enter matte, homogeneous patches as separate receivers when materials vary.

REPL callers submit `:start-surface`, `:step`, `:heartbeat`, `:read-surface` and `:apply-surface` through the photometry service. Each reading carries the current step index and presentation generation. The `:cancel` operation or an expired lease releases the output override. See `tolglow.fixture-photometry-test` for the full exchange with captured output.

For physical qualification, compare estimates of several certified neutral tiles, including one held out from calibration. Repeat from different tripod positions and distances under the same fixed fixture spectrum, then repeat with another fixture. Record the card certificate, camera model, shutter, incidence, reference drift, estimate, range and independent instrument reading. The current measurement supplies neutral diffuse luminance. Chromatic and specular response require independent measurements.
