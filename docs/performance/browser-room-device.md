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
