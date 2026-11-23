# Measure the optional 3D Room on a Mac

This measures the production WebGL room in a real foreground Chrome window. The launcher creates a fresh eight-head demo on an ephemeral loopback port with preview-only output. It opens and closes one tab in an existing local Chrome debugging session. It does not use the currently running lighting show.

Use a separate checkout or worktree for each pinned source. From this Tolglow checkout, use the Afterglow revision from its architecture workflow. Install Node 26, JDK 21 and Leiningen, then build the release before sampling.

```sh
export AFTERGLOW_ROOT=/absolute/path/to/the/paired/afterglow
pin=$(sed -nE 's/^ +ref: ([a-f0-9]{40})$/\1/p' .github/workflows/tests.yml | sort -u)
test "$pin" = "$(git -C "$AFTERGLOW_ROOT" rev-parse HEAD)"
npm ci
npm run studio:build
node dev/preview/measure-room.mjs http://127.0.0.1:9223 target/room-device
```

Start the local Chrome CDP wrapper before the last command. Use Chrome's normal graphics defaults and native display scaling. Keep the test window visible and focused, and leave the camera and window size unchanged. The script brings its tab forward, waits for the real renderer and expands Room. It attaches to an existing Chrome process without restarting it. Other tabs and browsing data stay under the operator's control.

Two scenarios each warm for eight seconds then sample for twenty. The first launches Ember on eight heads with a 0.22 work light and 0.25 fog density. The second also launches Slow sweep and raises fog to 0.70. Both use Auto quality and the normal show clock. The captures and report go in the output directory.

Read `presented.hz` for completed room frames. The worker counts accepted bitmaps at the visible canvas. Main-thread rendering counts completed scene and film passes. `windowRaf.hz` describes the browser display clock. The report includes the full room timing series, p50/p95/max frame intervals, CSS and backing resolution, DPR, internal scene resolutions, observed quality tiers, shadows, lit heads, fog statistics, movement samples, and the browser’s renderer description. The release JS hashes and repository revisions identify what actually ran.

A valid measurement keeps the window active, presents at least two frames in each sample, shows light on the eight-head rig, and observes pan change during Slow sweep. Software renderers are identified in the report and produce exit code 2. Compare actual frame rate and p95 against the measured display cadence and the selected tier’s target. Performance below either is a measurement, not grounds to drop the sample.

Safari/WebKit may expose only a generic renderer string or omit the unmasked renderer extension. Record the OS hardware model independently when measuring Safari and keep the browser-reported identity as observed. Safari's identity string alone cannot establish the GPU, power state or frame cadence. This launcher uses Chrome CDP and makes no Safari performance inference. Room playback is separate from Afterglow JVM, native shader and production ArtDMX throughput measurements.

## Fixed-tier comparison

Keep the same native Chrome window on the same display, plugged in or on battery consistently. Each invocation starts a fresh preview-only show. The two controls below use the same camera, eight heads, cue launches, work light and fog levels as Auto.

```sh
ROOM_QUALITY=desktop node dev/preview/measure-room.mjs http://127.0.0.1:9223 target/room-desktop
ROOM_QUALITY=mobile node dev/preview/measure-room.mjs http://127.0.0.1:9223 target/room-mobile
```

Sampling starts after both eight warm seconds and four additional renderer frames following the correct tier. The initial startup, shader compilation and warm interval are excluded from reported rates. The per-second policy observations record tier, internal resolution, scale, configured rate, CPU submit time, worker completion and fence wait. The receipt records whether the measured cadence reached 95% of a single configured target and how it compares with the actual display callback rate. Manual desktop and mobile keep the internal scale at 1, subject to their independent 3.2M and 1.5M scene-pixel caps. The backing canvas can remain larger in either mode.

Auto first chooses desktop or mobile from viewport and pointer, or low for an identified software renderer. After its first draw, it compares measured work with the current tier's frame budget. Four consecutive samples above 135% reduce internal scale, with a one-second hold between changes. Eight such samples at the 0.5 floor allow a tier drop. Sixty samples below 65% can restore scale and then a tier. Mobile and desktop request 120 Hz. Low requests 10 Hz. The worker also spaces frames using measured work and its GPU fence; a fence wait includes scheduling time. A missing GPU timer is recorded as null. Chrome's M4 model identity alone does not select low.

## Attribute presentation and shader costs on one GPU

Use the same release build, browser window, fixed camera, display scale and power supply for every sample. This command opens a dedicated localhost show with eight lit moving heads in dense fog. The first and last full-render samples help detect thermal or clock drift.

```sh
ROOM_QUALITY=mobile ROOM_SCENE=dense \
ROOM_PASSES=full,cached-shadows,skip-fog-raymarch,present-cached-frame,full \
node dev/preview/measure-room.mjs http://127.0.0.1:9223 target/room-pass-mobile
```

Repeat with `ROOM_QUALITY=desktop` and a separate output directory to test a larger internal render target. Each sample runs for 20 seconds, after at least eight seconds and four ordinarily rendered frames, followed by a 2.5-second pass change warmup. The dedicated Ring JVM serves this worker from a temporary classpath overlay and verifies its HTTP response SHA before opening Chrome. The worker uses the checked-out release scene, shader and display code. Its SHA-256 and the ordinary worker SHA-256 are recorded separately.

| Sample | Work performed |
| --- | --- |
| `full` | Normal moving heads, shadow map updates, room fog, bloom and final presentation |
| `cached-shadows` | Uses the last fully rendered shadow maps. Surface lighting, fog sampling of those maps and the final composition continue. |
| `skip-fog-raymarch` | Executes the existing zero-amount branch only for the fog rendering pass. Room simulation, geometry, shadow maps and the scene copy and postprocessing continue. |
| `present-cached-frame` | Copies one fully rendered scene into a texture, then displays that same image with a full-canvas blit, the production GPU fence, bitmap transfer and page presentation. The show continues running. |

Each entry in `room.json` includes accepted Room presentations and window animation callbacks, internal and display pixel dimensions and observed pan values. `passTimingsMs` reports sample counts, medians, p95 and maxima. Opaque geometry (including shadow rendering), fog and postprocessing submission times measure JavaScript command submission. Their GPU timings appear only when the browser supports valid disjoint timer queries. The GPU fence also includes worker scheduling and poll delay. Bitmap export, DOM bitmap submission, and worker-post-through-DOM time have separate counters. A DOM bitmap submission establishes a new frame at the visible canvas; physical monitor scanout requires external instrumentation.

Compare `full` with the sample that removes one cost, at identical `sceneSizes`, scales and backing dimensions. The cached-frame case measures the presentation path at real resolution. Concurrent GPU workloads, clock changes and overlap between passes can affect differences between samples.
