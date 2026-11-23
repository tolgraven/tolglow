# Measuring distributed shader output

The proof show patches 64 RGBW moving heads across two Art-Net universes. Eighteen persistent cues write color, dimmer, focus and a world-space aim vector. The cue at the highest priority changes its swatch and aim, fades to half opacity to expose the underlying layer, and ends. A second CPU show receives the same commands. The UDP receiver checks both complete 512-byte universes against the CPU show’s ordered transitions, including the presentation scheduler’s idle bytes before its first render. A partial sequence is reported separately and checked against the CPU bytes received for that universe.

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
