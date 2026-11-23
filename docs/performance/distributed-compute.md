# Measuring distributed shader output

The proof show patches 64 RGBW moving heads across two Art-Net universes. Eighteen persistent cues write color, dimmer, focus and a world-space aim vector. The cue at the highest priority changes its swatch and aim, fades to half opacity to expose the underlying layer, and ends. A second CPU show receives the same commands. The UDP receiver checks both complete 512-byte universes against the CPU show’s ordered transitions, including the presentation scheduler’s idle bytes before its first render. A partial sequence is reported separately and checked against the CPU bytes received for that universe.

## Enable workers in a show

Set `:compute-backend :distributed` in the `:show` map. The GPU allocation is a portion of each show frame. For a 40 Hz show, `:gpu-budget-ms 12` leaves 13 ms of its nominal 25 ms frame for other work. The host can render locally when a worker is late or absent. Set `TOLGLOW_COMPUTE_TOKEN` in the host environment to a separate random secret, for example the output of `openssl rand -hex 32`. The host accepts 24–1,024 characters. A show file can move between hosts without carrying this credential.

On a remote device, open the authenticated Studio HTTPS origin and choose **Connections → Contribute this GPU**. Enter the compute token. The client first compiles and reads back a calibration shader. When it succeeds, the host can send eligible fixture groups. The panel reports results sent and fixture rows rendered by this worker. Host-accepted results and deadline misses are measured separately. Leaving the pool, closing the tab, or hiding it releases its GPU context and socket. Thermal throttling and background suspension depend on the browser and device.

## Two local workers

Use the paired Afterglow source with distributed compute, Linux Chromium and Node. From Tolglow:

```sh
export AFTERGLOW_ROOT=/path/to/afterglow
export TOLGLOW_COMPUTE_TOKEN="$(openssl rand -hex 32)"
npm ci
npm run studio:build
lein run -m tolglow.qa.distributed-render /tmp/lighting-proof /usr/bin/chromium 40 18
```

The last two arguments set the show frame rate and the entire frame’s GPU budget in milliseconds. The Ring listener, Art-Net socket and temporary browser profiles are local to this run. Each Chromium process opens the production Connections panel, enters its token, explicitly enrolls, and renders in a dedicated WebGL2 worker. The test intercepts one outgoing readback and closes that socket while its job is leased. The host must complete the frame locally, continue with the remaining worker, and introduce no extra DMX transition.

Each run writes a unique directory. `host.json` records complete and partial frames, the number of input and output groups at the frame renderer, elapsed time from cue launch to the first accepted remote frame, end-to-end successful evaluation samples, CPU fallbacks and pool misses. `browser.json` records observed compiler acknowledgments, retained program IDs, results and the forced disconnect for each client. A client counts a result even when the host has already discarded it for missing its deadline. `connections.png` shows the surviving client. A comparison failure leaves the actual and expected transitions in `capture.edn`.

### Recorded loopback run

Two independent Chromium 156 processes used ANGLE/SwiftShader on a shared Linux machine. In both runs the first worker intentionally dropped its leased response, the other continued, and there were zero partial Art-Net frames. Every received byte matched the ordered CPU presentation timeline. A miss counts a submitted distributed batch that failed to finish by its deadline. CPU fallbacks also include cold preparation and frames that had already used their budget. The successful remote median therefore excludes requests that exceeded the deadline.

| Target Hz / GPU budget ms | Complete frames | Datagrams | Completed GPU batches | Misses | CPU fallbacks | Launch to first GPU frame ms | Successful remote median ms |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 5 / 170 | 129 | 258 | 16 | 6 | 15 | 1659 | 69.65 |
| 40 / 18 | 774 | 1548 | 10 | 202 | 294 | 7395 | 13.07 |

Each active interval presented 18 original groups as one composite group. After the highest-priority cue ended, the remaining 17 still fused to one group. Both workers prepared two program shapes before the intentional disconnect. The survivor prepared a third after the membership change. The 40 Hz run preserved the fixture stream with substantial CPU fallback. It does not establish enough remote throughput for live production on this machine.

## Full iOS or iPadOS app on the local network

To measure the radio, WKWebView, and device GPU path separately, configure the existing [authenticated Studio HTTPS reverse proxy](../studio-ui.md#connecting-an-ipad). Keep the listener on loopback. Set `TOLGLOW_COMPUTE_QA_ORIGIN` to its public HTTPS origin, optionally set `TOLGLOW_COMPUTE_QA_PORT` to its local upstream port (default 8500), and configure both the Studio sign-in token and compute token outside the show file.

```sh
lein run -m tolglow.qa.distributed-render /tmp/lighting-full-app nearby 40 18
```

Open that origin in the full app’s Studio tab, sign in, and use **Connections → Contribute this GPU**. Keep the app in the foreground. This run waits for one operator-enrolled worker, exercises the same cue edits and validates the physical byte stream. It records `clients: operator-enrolled-full-app` for this operator-selected path. Record device model, OS version and network conditions alongside its output. The sensor-only App Clip does not enroll. Local browser measurements below cover the Linux software rendering path. Physical Apple device and wireless results remain unmeasured.

## Desktop OpenGL baseline

For a Linux Mesa software run, start Xvfb and select its display, or run with xvfb-run:

```sh
LIBGL_ALWAYS_SOFTWARE=1 xvfb-run -a \
  lein with-profile +native-gpu run -m compute.native-fusion /tmp/native-fusion.json 60
```

This measurement removes WebSockets, independent output cadence and browser scheduling. Both paths use Afterglow’s native transform-feedback sessions, fixture calibration and DMX packing. Each sample updates parameters on every layer. Every DMX byte from both GPU paths must equal the original CPU reference in both universes. The latest layer is half-opaque, so the prior layer contributes to the result. Timings include preparing the chain on the first sample, resolving native fields and packing DMX. Session creation and initial setup of the original expression descriptions precede the timed samples.

A shared Linux host using OpenJDK 21 and Mesa 25.0.7 llvmpipe (LLVM 19.1.7, 256 bits) produced these results over 60 samples. Percentiles include JVM warm-up after the first sample and concurrent load on the machine.

| Path | Cold ms | Warm median ms | Warm p95 ms | Draws / readbacks | Resident programs |
| --- | ---: | ---: | ---: | ---: | ---: |
| CPU reference | 666.03 | 62.92 | 579.45 | 0 | 0 |
| Native, original 18 groups | 2002.99 | 202.47 | 835.48 | 1,080 | 5 |
| Native, fused group | 740.50 | 22.27 | 108.20 | 60 | 1 |

The fused session reused its program 59 times. The original session recorded 1,075 hits across five programs. The observed p95 exceeds a 25 ms frame, even on the faster fused path. Hardware GPU throughput requires a separate benchmark.

## Full-app audience GPU verification

Use the `guest` target to reproduce the audience worker shader and host verification path on a single computer. It starts the private Ring listener, two independent Chromium GPU processes with the exact JavaScript bundle shipped in the full iOS app, a stage-side socket per participant, and the same two-universe Art-Net/CPU comparison. Only the stage processes hold the compute credential. Each GPU must pass its bundled calibration shader and receive jobs under the stage's verification policy. During an outstanding job the first guest closes its socket. The remaining guest then receives independently recomputed full verification on the host.

```sh
export TOLGLOW_AUDIENCE_COMPUTE_TOKEN="$(openssl rand -hex 32)"
export TOLGLOW_AUDIENCE_VERIFICATION=redundant
lein run -m tolglow.qa.distributed-render /tmp/lighting-audience guest 40 18
```

Use Node 24, Chromium and the paired sources described above. If testing before the full-app and host branches are assembled, set `TOLGLOW_GUEST_BUNDLE` to the full path to `native/AudienceCompute/Phone/audience-compute.js` in the native checkout. Set `TOLGLOW_QA_CHROMIUM` for a different Chromium executable. To exercise the default all-rows policy, unset `TOLGLOW_AUDIENCE_VERIFICATION`. The fixture has 64 rows, so the report distinguishes a reference computation on one sentinel row from a full assigned slice. It counts a slice when the host verifies it, even if the deadline expires before publication. The pool's completed count tracks results returned before the deadline.

For a physical Apple radio trial, use the `audience` target on a Mac. It binds the fixed loopback port used by the audience bridge. Give the bridge the same stage compute credential, open the room by QR in the full app on two independently paired phones, and choose **Share GPU with this stage** on each. When the harness prompts, stop GPU sharing on one participant. Keep the other in the foreground through the final swatch. The result records `clients: two-stage-admitted-full-app-phones`, the verification counts, captured output, and shader timing. Record both phone models, OS versions, battery/thermal conditions and the bridge's measured round-trip time and bytes per second alongside it.

```sh
lein run -m tolglow.qa.distributed-render /tmp/lighting-audience-phones audience 40 18
```

The Linux bundle harness covers application shaders, admission, independent verification, fallback and output delivery. The Apple trial additionally covers the QR/BLE grant, native encrypted connection, Wi-Fi or peer radio, WebKit, device GPU and sustained availability.

### Captured audience results

Two independent Chromium 156 processes on the shared four-vCPU Linux machine ran the full-app WebGL2 bundle through ANGLE/SwiftShader. Each process received and rendered genuine fused programs for 64 moving heads. The UDP receiver matched every captured byte to its separate CPU show, with zero partial frames in all three runs. The host recorded no rejected fields. Output frame counts include presentations while the renderer reused its last complete result. These counts and the number of computed batches therefore measure different work.

| Host policy | Target Hz / budget ms | Two-universe frames | UDP packets | Completed remote batches | Deadline misses | Local fallbacks | Successful median ms |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Full | 5 / 170 | 50 | 100 | 11 | 3 | 6 | 35.35 |
| Redundant | 5 / 170 | 51 | 102 | 14 | 1 | 5 | 61.90 |
| Redundant | 40 / 18 | 3,723 | 7,446 | 12 | 1,472 | 2,992 | 12.38 |

At 40 Hz the two guests sent 997 and 1,087 result messages. The host verified one paired slice, four independently chosen sentinel rows and 15 full slices, some after the deadline. The host accepted 12 complete batches in time. The intentional departure forced an in-flight fallback and the surviving guest prepared the revised 17-layer shader. This shared CPU-rendered run depended heavily on local rendering. The successful median excludes deadline misses.

[Raw host and guest receipts, screenshot and animation](https://github.com/tolgraven/tolglow/tree/1fce380357a282725c337ebe7c04335373069508)
