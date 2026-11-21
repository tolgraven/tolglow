# Offscreen Push 2 display

`tolglow.studio.display/browser-display` renders the standalone re-frame display
page in an owned Chromium process and copies each PNG into an owned display
sink. It uses the same parameter schema, cue colors, category metadata and CSS
UI components as the studio. The dedicated page does not start WebGL, poll show
state, or run a separate handwritten HTML application.

The factory is inert. Opening it explicitly starts Node/Chromium, waits for the
page, and then acquires the sink. The default sink is native Wayang with its
existing process-wide exclusive lease; provide `:sink` to use an injected image
consumer without hardware. Native Push 3 display transport remains unverified.

## Use

Install the repository's pinned npm dependencies with `npm ci`. This includes
`playwright-core` 1.57.0; it does **not** install a browser. Configure an existing
Chromium executable and run the studio's HTTP server first:

```clojure
(require '[tolglow.studio.display :as display]
         '[afterglow.control.devices.repl :as devices])

(def screen
  (display/browser-display
    {:browser-executable "/path/to/chromium"
     :url "http://127.0.0.1:8500/push-display"
     :max-fps 20
     :capture-timeout-ms 2000
     :startup-timeout-ms 10000
     :shutdown-timeout-ms 1000}))

;; Supply screen as :display in the host's explicit Push 2 connection options.
;; Normal MIDI port, model, dispatch and clock options still belong to that host.
```

`:node-executable`, `:worker-script` and `:playwright-module` can be supplied for
packaging. The default worker script is resolved from the checkout classpath;
a packaged JAR must extract it to a real local file and pass that path. No shell
interpolation is used when starting Node. `:no-sandbox? true` is available for
container environments that cannot run Chromium's sandbox; it is not the default.

## Contract and ownership

The browser loads only an HTTP loopback URL whose path is `/push-display`, with
a 960x160 CSS viewport and device scale 1. It waits for
`globalThis.tolglowPushDisplay.ready === true`. Each update calls the stable
`tolglowPushDisplay.update(view)` entry point, which dispatches synchronously to
re-frame. Two animation frames allow React to paint before screenshot capture.

The source view is the portable `push-2/display-view`: title, status, tempo,
master, blackout, selected cue/effect, swatch, eight cell labels, and parameters
with their IDs, values and full schemas. Optional waveform samples are drawn
only when supplied by the trusted host. The capture worker does not invent
waveforms, render effects again, or fetch data independently.

`display/transport` accepts injected `:capture` and `:sink` maps. Its `render!`
call only replaces an immutable pending view. One worker owns one in-flight
capture and one pending mailbox slot; faster input replaces the pending view.
Capture, PNG decode, image blit and native send happen on that worker, outside
device input callbacks, show-owner computation, CAS loops and render effects.
The default cap is 20 fps, adjustable from 1–60; this is a cap, not a measured
throughput promise. Snapshots must be immutable plain data.

The returned transport also has `:status!`, which accepts its opened handle and
returns status, submitted/captured counts, mailbox replacements, last sequence,
last render duration and failure information. Browser/IPC failures mark the
worker failed and close its resources. The device session observes failure on
its next display write; hosts may additionally inspect status. `close!` marks
the worker closing, interrupts it and waits for the configured bounded shutdown.
Repeated close is harmless. An injected native operation that ignores thread
interruption cannot be forcibly unwound safely; `:shutdown-timeout?` reports
that case and the lease stays with its owner until cleanup actually completes.

Node stdin/stdout uses newline-delimited JSON. Readiness is
`{"status":"ready","width":960,"height":160}`. Requests are
`{"id":1,"view":{...}}`; successful replies contain matching `id`,
`status:"captured"`, dimensions, base64 `png` and `capture-ms`. Views are limited
to 64 KiB and PNG data to 2 MiB. The JVM separately bounds requests and responses,
validates reply identity and decoded dimensions, and kills the owned process
tree when its deadline expires. A failed screenshot is not retried implicitly.

## Verification

Run the boundary tests:

```sh
node --test src/tolglow/integrations/display_bridge/worker.test.js
```

`tolglow.studio.display-test` covers the latest-view mailbox with blocked captures,
off-caller-thread sink writes, cleanup on capture/startup failure, real owned
Node IPC timeout/PNG validation, and the shared Wayang lease with an injected
backend. The initial checkpoint is 5 JVM tests / 29 assertions and 5 Node tests.
These tests do not open USB hardware. Actual production-page capture and CPU
timings must be recorded separately; native screen refresh performance remains
unverified until tested on the user's connected Push 2.

On 2026-11-21, the actual built `/push-display` page was captured 60 times at
960x160 in headless Chromium 156.0.8078.79, Linux x64 / Node 24.22.3. Measured
median capture was 66.8 ms, p95 73.8 ms, and throughput 14.79 fps. Chromium's
reported browser-process CPU deltas totaled 2.01 seconds over 4.06 seconds wall
time, or 33.5 ms CPU per frame. These are one container's measurements with
explicit synthetic view data and no native hardware send. They establish the
working path and its present cost, not 60 fps display performance. A separate
actual JVM → owned Node/Chromium → re-frame → PNG → ImageIO run also produced
a validated 960x160 image and closed its browser process.

Reproduce and save a screenshot plus JSON measurements with:

```sh
node src/tolglow/integrations/display_bridge/benchmark.js \
  http://127.0.0.1:8500/push-display /path/to/chromium /tmp/push-display-benchmark
```

The benchmark uses Chromium's CDP process CPU counters in addition to wall time.
It excludes Java PNG decoding and physical USB output. The current screenshot
pipeline should be treated as a roughly 15 fps baseline in this environment;
texture sharing or another embedded browser backend needs separate measurement.
