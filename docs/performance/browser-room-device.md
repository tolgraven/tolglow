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

Auto first chooses desktop or mobile from viewport and pointer, or low for an identified software renderer. After its first draw, it compares measured work with the current tier's frame budget. Four consecutive samples above 135% reduce internal scale, with a one-second hold between changes. Eight such samples at the 0.5 floor allow a tier drop. Sixty samples below 65% can restore scale and then a tier. Before the display-cadence change, mobile and desktop always requested 120 Hz and low requested 10 Hz. The current Auto policy samples the foreground display, uses its measured frequency up to 120 Hz, and can use low spatial quality on hardware without forcing a 10 Hz rate. Manual low and software rendering retain the 10 Hz cap. The worker also spaces frames using measured work and its GPU fence; a fence wait includes scheduling time. A missing GPU timer is recorded as null. Chrome's M4 model identity alone does not select low.

## Physical M4 Air baseline

The local operator measured Chrome 158, ANGLE Metal, with a 16 GB M4 Air (10 CPU cores, 8 GPU cores) on AC and macOS 27.2.1. DPR was 1.8, the canvas backing was 2806 × 1260, and the display delivered approximately 60 Hz. Both fixed tiers held scale 1. Each sample had eight emitting heads and verified pan motion in the dense scene.

| Quality choice | Scene pixels | Fixed heads, room Hz | Moving heads, room Hz | Fixed / moving p95 interval |
| --- | --- | ---: | ---: | --- |
| Auto before the change | 412 × 185 through 914 × 410 | 9.55 | 10.35 | Unreported |
| Desktop | 2670 × 1199 | 12.39 | 11.45 | 97.6 / 102.2 ms |
| Mobile | 1828 × 821 | 23.45 | 23.90 | 67.7 / 64.8 ms |

Desktop presented 248 and 229 frames. Mobile presented 469 and 478. The worker's completed-frame time, including scheduling, was about 40–52 ms on desktop and 21–30 ms on mobile. Direct GPU timer data was unavailable. These aggregates came from the local operator using sampler revision c5447c9b. The device owner retains the raw recordings.

On the same M4 Air, the revised Auto policy at `3df8c9c` presented 28.7481 frames/s with fixed heads and 34.5978 with the moving sweep. Previous Auto measured 9.55 and 10.35. The backing stayed at 2806 × 1260 and the foreground callback clock measured 59.996 Hz. Both scenes remained below 40 fresh Room presentations/s. A separate desktop comparison with fog off and on measured 21.399 and 14.399 frames/s for fixed heads, and 20.699 and 13.999 frames/s for moving heads. The local operator supplied the aggregates. [Measurement and source](https://github.com/tolgraven/tolglow/pull/176)

## Attribute presentation and shader costs on one GPU

Use the same release build, browser window, fixed camera, display scale and power supply for every sample. This command opens a dedicated localhost show with eight lit moving heads in dense fog. The first and last full-render samples help detect thermal or clock drift.

```sh
ROOM_QUALITY=mobile ROOM_SCENE=dense \
ROOM_PASSES=full,cached-shadows,skip-fog-raymarch,present-cached-frame,full \
node dev/preview/measure-room.mjs http://127.0.0.1:9223 target/room-pass-mobile
```

Repeat with `ROOM_QUALITY=desktop` and a separate output directory to test a larger internal render target. Each sample runs for 20 seconds, after at least eight seconds and four ordinarily rendered frames, followed by a 2.5-second pass change warmup. The private Ring process serves a diagnostic worker from a temporary classpath resource. Before opening the new tab, the harness verifies the HTTP response against its locally built SHA-256. Browser errors and the fallback reason are written to `room-error.json` if startup fails. The worker uses the checked-out release scene, shader and display code. Its SHA-256 and the ordinary worker SHA-256 are recorded separately.

| Sample | Work performed |
| --- | --- |
| `full` | Normal moving heads, shadow map updates, room fog, bloom and final presentation |
| `cached-shadows` | Uses the last fully rendered shadow maps. Surface lighting, fog sampling of those maps and the final composition continue. |
| `skip-fog-raymarch` | Executes the existing zero-amount branch only for the fog rendering pass. Room simulation, geometry, shadow maps and the scene copy and postprocessing continue. |
| `present-cached-frame` | Copies one fully rendered scene into a texture, then displays that same image with a full-canvas blit, the production GPU fence, bitmap transfer and page presentation. The show continues running. |

Each entry in `room.json` includes accepted Room presentations and window animation callbacks, internal and display pixel dimensions and observed pan values. `passTimingsMs` reports sample counts, medians, p95 and maxima. Opaque geometry (including shadow rendering), fog and postprocessing submission times measure JavaScript command submission. Their GPU timings appear only when the browser supports valid disjoint timer queries. The GPU fence also includes worker scheduling and poll delay. Bitmap export, DOM bitmap submission, and worker-post-through-DOM time have separate counters. A DOM bitmap submission establishes a new frame at the visible canvas; physical monitor scanout requires external instrumentation.

Compare `full` with the sample that removes one cost, at identical `sceneSizes`, scales and backing dimensions. The cached-frame case measures the presentation path at real resolution. Concurrent GPU workloads, clock changes and overlap between passes can affect differences between samples.

## OS session qualification and controlled comparisons

On macOS the harness reads only the `IOConsoleLocked` root flag and each console session's on-console and screen-locked flags from `ioreg` (`dev/preview/desktop-session.mjs`). It never records the other session fields. A locked, missing or ambiguous answer stops the run before the Ring host starts. The harness checks again before the browser attaches and before each scenario and sample, and once a second during every sample. Any change ends the sample. Other platforms report `unsupported-platform` and do not run. The receipts are written to `room.json` under `desktopSession`. The lock flags do not report display sleep or a covered window. Those cases rely on Chrome's visibility and focus events and on the measured callback rate. Headless Chrome reports focus and visibility without presenting to a display, so its samples are recorded with an `invalid` entry and the run exits with code 2.

The browser sample prepares the pass collector first and starts it on the same synchronous boundary as the cadence window. Node-side API reads and OS checks before or after the window therefore add no frames. `sampleBoundaryEpochMs` gives the start and end of each sample as wall-clock epoch milliseconds, so the samples can be lined up with an external power or GPU-clock log. Every sample records the camera and orbit target. In a controlled comparison every sample must match the first sample's view, or the sample is marked invalid.

| Variable | Effect |
| --- | --- |
| `ROOM_WORK_LIGHT` (0–1, default 0.22), `ROOM_LIGHTING_MODE` (`hold` or `interpolate`) | Preview controls applied through the ordinary Studio inputs. They are checked before and after each sample. An empty variable means its default. |
| `ROOM_WARMUP_MS` (8000–120000) | Minimum warmup before each scenario. Controlled comparisons require at least 20000. |
| `ROOM_PLUME_SCENE=1` (or `true`) | Adds a moving dense-fog scenario that emits a local plume every four seconds. Warmup plumes are reported separately from those counted during the sample. |
| `ROOM_PASSES` at Auto quality | Freezes the Auto policy after warmup, then runs exactly one of `full,full,full` (the null control), `full,simple-fixed-loop,full`, `full,full-no-timers,full`, `full,no-composite-fallback,full` or `full,present-cached-frame,skip-fog-raymarch,cached-shadows,full`, in the dense scene. Any other sequence at Auto is refused. |
| `ROOM_FOG_FIELD` (`hold` or `decay`) | With `ROOM_PASSES`, defaults to `hold`: the fog field is held at one captured state for the whole sequence. `decay` lets it decay as in the ordinary preview; the bracket check then rejects the run. Without `ROOM_PASSES` only `decay` exists. |
| `ROOM_PAGE_TITLE` | Reuses exactly one already-open benchmark tab with this title instead of opening a new one. |
| `ROOM_KEEP_PAGE=1` (or `true`) | Leaves the benchmark tab open after the run. |

`simple-fixed-loop` removes the adaptive plume subdivision from the production fog integrator and nothing else. `no-composite-fallback` removes the composite's empty-gather fallback raymarch. `full-no-timers` keeps the timer extension enabled and issues no queries. The composite and attribution comparisons don't request the timer extension at all. The probes rewrite exact fog shader source, and `test/js/preview/room-pass-browser.test.mjs` fails if those rewrites stop matching `js/preview/fog.js`.

## Hold the fog field during a comparison

The manual preview's local fog field decays toward empty air with a 100-second time constant (`js/preview/fog-field.js`). The comparisons were first run on the superseded #489 branch, which retained a constant background haze. On master, without a hold, each later sample in a sequence renders a thinner medium than the one before it (#685).

Every `ROOM_PASSES` run now holds the field. After the first scenario's warmup, and in a controlled comparison after the startup burst has dispersed, the private diagnostic worker copies the field's density, temperature, sources and fog time once. Each later simulation step still runs, and the field is restored to that copy before its texture upload. The step and the upload therefore keep their production cost, and every frame of every sample renders the same density and fog time. The restore copies two float arrays (147 KB at the desktop fog grid). `fogHold.restores` and `fogHold.restoreMs` in `getRoomPassState()` report its count and total time, and it falls inside the worker frame time of every sample alike. The hold lives only in the private worker bundle. The Room's fog in Studio is unchanged.

Each pass sample records the medium its accepted frames rendered (`passEvidence.medium`: first, last, minimum and maximum field mean density and fog time). A held sample is invalid unless every frame rendered exactly the captured mean density and fog time. A replaced field (a tier change) or a show atmosphere taking over the field also invalidates it.

The harness then compares the first and last `full` samples of each sequence (`diagnostic.comparisons`). Mean fog density may differ by at most 1%, accepted Room frames per second by at most 5% and the median worker frame time by at most 10%. A sequence whose brackets differ by more is added to the top-level `invalid` list and the run exits 2. Each middle sample is reported relative to the mean of the two brackets, with the bracket difference alongside as the noise estimate. `full,full,full` is the null control: its middle sample should differ from the bracket mean by no more than the brackets differ from each other.

The frame-cost brackets still move under concurrent GPU work, clock changes and the Slow sweep's beam positions, none of which the hold controls. A run that fails only on frame cost has a stationary medium but is not attributable.

### Headless check on the M4 Pro (tooling, not performance evidence)

On 2026-11-27 each controlled sequence ran at Auto in the dense scene with a 20-second warmup, on master `d1770294` and with the hold (`b5b05280`), in headless Chrome 155 with ANGLE Metal on the Apple M4 Pro. Auto froze at desktop scale 1 (1235 × 528 scene, 617 × 264 fog, 48 ray steps) and the callback clock was a synthetic 60 Hz, so every sample presented 60 frames/s and only the worker frame time (submission through GPU fence) varies. Headless has no display, so every run exits 2. The console was locked, so the OS session read was answered by a synthetic unlocked record from outside the repository. A dozen other agents were loading the machine (1-minute load average 2.1 to 14.1). The values are medians of 1,200 accepted frames per sample.

Every held sample rendered exactly the same medium, mean density 0.07036 and fog time 31.7 s, in every accepted frame of every sample and in every run, because each run starts a fresh field and holds it after the same number of fixed steps. The restore took 5 µs per step. Without the hold the mean density fell from 0.069 to 0.021 over one three-sample sequence and to 0.011 over the five-sample attribution sequence.

| Sequence | Fog field | Bracket density | Bracket worker frame | Middle worker frame vs bracket mean | Bracket check |
| --- | --- | ---: | ---: | --- | --- |
| `full,full,full` | decaying, master | −62.2% | +8.0% | full +11.1% | no check on master |
| `full,full,full` | decaying, `ROOM_FOG_FIELD=decay` | −64.2% | −18.2% | full −9.1% | rejected (density, cost) |
| `full,full,full` | held, run 1 | 0 | −18.1% | full −8.3% | rejected (cost) |
| `full,full,full` | held, run 2 | 0 | +4.9% | full −1.5% | agrees |
| `full,full,full` | held, run 3 | 0 | −17.8% | full −0.6% | rejected (cost) |
| `full,simple-fixed-loop,full` | decaying, master | −64.3% | +0.6% | loop −0.3% | no check on master |
| `full,simple-fixed-loop,full` | held | 0 | +15.0% | loop −6.4% | rejected (cost) |
| `full,full-no-timers,full` | decaying, master | −64.2% | −18.9% | no timers −5.3% | no check on master |
| `full,full-no-timers,full` | held | 0 | +8.7% | no timers −1.3% | agrees |
| `full,no-composite-fallback,full` | decaying, master | −64.2% | +17.3% | no fallback −6.7% | no check on master |
| `full,no-composite-fallback,full` | held | 0 | −11.4% | no fallback −6.4% | rejected (cost) |
| attribution | decaying, master | −81.5% | −8.0% | cached frame −72.6%, skip fog −30.9%, cached shadows −10.0% | no check on master |
| attribution | held | 0 | −0.7% | cached frame −68.2%, skip fog −21.2%, cached shadows +9.3% | agrees |

With the medium held, the null control's middle sample stayed within the spread of its own brackets in all three runs (−8.3%, −1.5% and −0.6%). The bracket check rejected four of the seven held runs on frame cost alone. In those runs one bracket's median worker frame was about 13.4 ms and the other about 11.0 ms, with the fog pass's GPU timer at about 8.1 and 6.1 ms on the same medium, so the cost moved between two levels independently of the fog. Concurrent GPU work or a GPU clock change is consistent with that; the cause was not isolated.

Only the attribution sequence changes its reading. On the decaying field the later samples rendered half the medium or less, and cached shadow maps appeared to save 10% and skipping the fog raymarch 31%. On the held field, with brackets 0.7% apart, cached shadows saved nothing measurable and skipping the raymarch saved 21%. No measurement recorded on master used these comparisons. The #307 and #489 results came from the superseded branch, which kept a constant haze. A foreground run on a physical display with both brackets agreeing is still needed (#685).

## Other sample checks

- A canvas backing, CSS size or device-pixel-ratio change between the start and end of a sample invalidates it. At a fixed quality, more than one internal scene raster in a sample also invalidates it.
- `presented.overOneAndHalfTargetInterval` counts presentation gaps longer than 1.5 configured frame intervals, so a single missed refresh at 120 Hz is counted. `over25ms` remains the 60 Hz threshold.
- Each sample records the geometry of the screen holding the window (`screen`). Browsers expose no display name to the page, so note which display held the window by hand.
- The harness removes its `tolglow-room-device-*` temporary directory when it finishes, fails or is interrupted.
- The lock flags still do not cover display sleep (see above).
