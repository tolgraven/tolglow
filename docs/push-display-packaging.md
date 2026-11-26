# Packaged Push webview capture

`tolglow.studio.display/process-capture` uses the same owned Node worker from
a source checkout and a packaged JAR. Constructing the backend validates its
options but does not resolve resources, create files, or start processes.
Calling `:open!` resolves the bundled worker. A file resource runs in place; a
JAR resource is copied to a private temporary `.cjs` file owned by that opening.

The worker is included at `devices/push-display/worker.cjs` on the
classpath. There is no alternate packaged implementation. Each successful
opening owns its Node process, IPC executor and, when needed, extracted file.
Close, startup failure, capture failure and interrupted startup retire those
resources. Failed Node launch also removes the extracted file. A configured
`:worker-script` remains caller-owned and is never deleted.

An installed browser and Node remain external runtime dependencies. Configure
`:browser-executable` explicitly and `:node-executable` when it is not on PATH.
The worker resolves `playwright-core` from the application launch directory or
its source checkout. When launching elsewhere, provide `:playwright-module` as
an absolute path to the installed package. Extraction does not require copying
`node_modules` into the temporary directory and never downloads dependencies.

```clojure
(tolglow.show/push-browser-display runtime
  {:browser-executable "/path/to/chromium"
   :node-executable "/path/to/node"
   :playwright-module "/path/to/application/node_modules/playwright-core"
   :url "http://127.0.0.1:8500/push-display"})
```

The lifecycle preserves the existing re-frame page, shared widgets, exact
960×160 viewport and single outstanding capture. It does not change the
lighting output clock or the native display sink.

## Verification, 2026-11-21

- Focused JVM display suite: **10 tests / 50 assertions**, passing. The actual
  production worker is read through a `jar:` resource and run by Node; only its
  external browser is an injected process-boundary peer. Tests cover PNG
  capture, failed executable/browser startup, cancellation, inert construction,
  cleanup and existing mailbox/native-sink behavior. An OS-process boundary
  fixture also verifies that a child ignoring termination is forcibly stopped
  even when its parent exits promptly.
- Node suite: **6 tests**, passing, including execution of an extracted worker
  with dependency resolution from a separate launch directory and an absolute
  override.
- Final combined pure JVM lane after the workspace isolation fix: **462 tests /
  3,631 assertions**, passing on Temurin 17.0.21+9 and Node 26.
- `lein jar` completed; the application JAR contains the exact production
  worker bytes at the expected classpath resource name.
- Actual Chromium capture: production worker through a real `jar:` resource,
  Node 26 and Playwright against the built re-frame `/push-display` page. A
  960×160 PNG was captured and inspected; the Node process exited and its
  extracted file disappeared after close. Evidence is in
  `/tmp/studio-qa/push-packaged-capture.clj` and
  `/tmp/studio-qa/push-packaged-capture.png` in the execution workspace.

The browser check used the installed Linux Chromium and an image capture sink.
It does not establish physical Push USB behavior or display throughput. The
broader live-waveform/audio capture evidence remains in
[waveform projections](waveform-projections.md).

Startup failures retain their original error data and include the startup stage,
elapsed time, Node PID/liveness/exit code, and the bounded IPC-thread stack. Node
stderr continues to inherit the host's error stream. The real-process test also
records Node's version and whether its readiness write completed, so a future
CI timeout can distinguish process startup from a blocked protocol reader.
The existing ten-second startup deadline has not been increased. Fifty repeated
protocol lifecycles passed on the same Temurin 17.0.21+9 used by CI (Node 24);
fifty also passed on Java 21, followed by fifty on Java 17 with pinned Node 26.
These local repetitions do not establish the cause
of the intermittent CI startup timeout; CI now pins Node 26 for JVM jobs.

For an authenticated Studio listener, set `:auth-token-env` on the capture
backend to the same local variable name as Studio's `:auth :token-env`. The
worker reads the secret from its environment and restricts browser credentials
to the configured loopback URL's origin. With direct Jetty TLS, set the
capture `:url` to the HTTPS loopback endpoint and configure a certificate
trusted by the local Chromium with the matching name. A loopback listener
behind a public HTTPS proxy can keep the default HTTP capture URL.

## Venue and rig views

Use the simulator to watch the committed show output from the device display. The
rig view adds a work light, fixture markers and the selected head’s aim line.
Both views use the Studio room renderer, including its fixture meshes, fog field,
lit receiver surfaces and active optical volumes.

With a Push controller profile that supports display modes, press **Layout** for
the simulator or **Shift + Layout** for the rig. **Mix** cycles through the camera
positions. In the rig view, **Page left/right** selects a fixture. **Device** returns
to parameters. You can also set the initial view and lens in the show settings.

```clojure
{:settings {:controllers {:push-display {:mode :rig
                                        :camera :fit
                                        :selected-fixture :head-4}}}}
```

| Camera | What appears on the display |
| --- | --- |
| Fit rig | Frames both ends of each truss, patched fixture positions and the downstage floor. |
| Panorama | A 136° cylindrical audience view distributes pixels by horizontal angle. Vertical correction keeps shapes near the center in proportion. |
| Audience | An ordinary perspective view with a 100° horizontal field of view. |

The device receives a 960 × 160 image. Hardware rendering uses the mobile quality
tier at native resolution. Software rasterizers use the low tier. Cold entry has a shader compilation deadline of 45 seconds
(`:scene-startup-timeout-ms`). Subsequent scene captures have a 10-second ceiling
(`:scene-capture-timeout-ms`). Parameter controls retain their own capture deadline.
A frame already in progress finishes before the worker reads back its PNG.
Hiding the page or switching back to parameters or the target picker releases the scene and WebGL
context. Returning to a visible scene reacquires a new canvas and renders the latest snapshot. Each capture consumes the latest committed lighting values. It performs
no additional effect evaluations and opens no Studio state socket.

The device view samples up to 32 fixture bodies across moving and fixed lights,
16 trusses, 16 receiver planes, 12 fog sources and four optical volumes. The
Studio renderer illuminates up to 16 local lights per view. Page through the
whole patch in rig mode. Selecting a fixture outside the sample replaces one
of the sampled fixtures so that you can see the selection in context.

To reproduce the headless capture, build Studio and pass an empty output directory
and an installed Chromium executable. The test drives a disconnected show with
eight moving heads and a fogger through the normal renderer, Ring and device
capture worker. It records 24 PNGs and checks renderer teardown.

```sh
npm run studio:build
lein run -m tolglow.qa.push-scene /tmp/tolglow-device-venue /usr/bin/chromium
```

The 24-frame software Chromium check took 25.3 seconds for cold shader compilation
and first capture. Warm captures had a 3.86-second median and a 6.18-second maximum
on the shared SwiftShader host. These are software rendering measurements. Hardware
GPU throughput and transfer to a physical Push remain unmeasured.

### Live parameter history on the encoder bank

Selecting **History** shows a graph for each numeric, vector, or color input with an active source. A held LFO shows **Held** with its ordinary widget. A gate without a current sample shows **Waiting**. Releasing the hold starts a fresh graph. A source that actively produces one constant value keeps its flat graph. Manual parameters retain their knobs, spatial controls and waveform selectors with shape previews.

History records the effective inputs from completed show frames. A parameter block that varies across the rig uses the selected eligible fixture, or the first eligible fixture in launch order. Changing the sampled fixture starts a new trace. The target picker and other display modes release the histories. The audio page still shows compact values below its waveform.

Run `TOLGLOW_PUSH_ONLY=1 lein run -m tolglow.qa.push-parameters /tmp/push-parameters` after the production Studio build, with Chromium installed. It captures 48 native-size display frames covering static controls, moving and constant LFOs, held inputs, resumed modulation and detach.

## Tethered Push 3

Choose the MIDI input and output of the device’s User port from the port list. Set the actual captured layout, button bindings and expression mode for your unit. In Afterglow, run the read-only tethered display `list` and `inspect` commands to get the USB bus and address. The image sink and MIDI port use separate handles. The application does not select an arbitrary connected Push.

```clojure
;; runtime is a running tolglow.show host. captured contains this unit’s MIDI
;; port indexes, :user-mode? true, :layout, bindings and expression settings.
(def surface
  (tolglow.show/connect-device!
    runtime :push-3
    (assoc captured
           :session-id "desk-push-3"
           :display (tolglow.show/push-browser-display
                      runtime {:device :push-3
                               :usb {:bus 2 :address 7}
                               :browser-executable "/usr/bin/chromium"}))))
```

The offscreen page renders the same CSS parameter widgets, waveform and history views, page grid, simulator and rig controls as Push 2. Bind captured buttons to `:display/parameters`, `:display/pages`, `:display/simulator`, `:display/rig` and `:display/camera` as needed. The bindable actions use the capture’s actual IDs. The display view follows the instance selected by a User Mode or MPE pad. Choosing a different lens or display does not launch a cue. A frame repeatedly sent to USB uses the last packed raster. Only the scheduled display worker asks the browser for new visuals.

### Display start and refresh rate

Afterglow keeps one libusb context for the whole process. Creating it runs `libusb_init`, which scans every attached USB device. On the qualification Mac (M4 Pro, macOS, libusb 1.0.30) the scan stalled on unrelated hubs and drives and took 25.0 s. macOS cannot skip the scan. Once the context exists, later opens reuse it: listing took 0.81 ms and inspecting the Push took 0.90 ms. [Issue #704](https://github.com/tolgraven/tolglow/issues/704)

Tolglow starts the scan early so the display open does not wait for it:

- Building a libusb Push 3 display with `push-browser-display` starts the scan on a background thread, so it overlaps the browser start.
- A host that always drives a tethered Push 3 can enable the `:push-3-display` startup module (`:push-3-display {:enabled true}` in its config). The scan then runs alongside the rest of startup. See [application startup](application-startup.md#built-in-modules).

The scan runs at most once per process, and a display open that starts during it waits for the scan to finish. Startup never throws or waits because of it. Hosts that do not configure a Push 3, and displays built with an injected sink, never load libusb. `(tolglow.studio.display/push-3-usb-status)` reports the scan without waiting. It returns `{:status :preparing}` while the scan runs. When it ends, it returns the receipt: `{:status :ready :init-ms …}`, or `{:status :failed :error …}` when libusb is missing or will not start. After a failure the display open tries libusb again and reports its own error. Each open display handle also carries Afterglow's `:timing` and `refresh-receipt`.

The sink resends the latest frame on a 30 Hz deadline grid, so transfer time no longer lowers the rate. It used to wait 33 ms after each transfer, which gave about 19.5 Hz. A 10 s test chart at Afterglow `13403f2d` (the reviewed head of tolgraven/afterglow#162) measured on the Push 3:

| Measure | Result |
| --- | --- |
| Frames | 301 in 9,984.7 ms, 30.05 frames/s |
| Late intervals | 0 (longer than 1.5 periods), no failed transfers |
| Payload transfer | median 11.54 ms, p95 11.88 ms, max 13.86 ms |
| Frame interval | median 32.79 ms, p95 41.89 ms, max 47.85 ms |

These measurements come from Afterglow's qualification CLI. The background start from Tolglow has not been run against the physical Push.

The replay uses the production screen encoder and 24 actual Studio simulator captures. Hardware qualification covers colour and edge orientation, Live handoff, continuous audio/MIDI and disconnect. See [Afterglow’s driver, receipts and on-device instructions](https://github.com/tolgraven/afterglow/blob/master/doc/architecture/push-3-display.md).

## Browse saved shows

`tolglow.controllers.content-browser-display/connect-device!` adds a saved-show browser to the shared browser display on Push 2 or tethered Push 3. Supply explicit button captures that fit your layout. The cue pads and encoder gestures keep their live functions. Exit any active variation overlay to see the browser.

```clojure
(require '[tolglow.controllers.content-browser-display :as saved-shows])

(saved-shows/connect-device! runtime :push-2
  {:session-id "desk-push"
   :input-index verified-input-index
   :output-index verified-output-index
   :display-options {:browser-executable "/usr/bin/chromium"
                     :url "http://127.0.0.1:8500/push-display"}
   :catalog-controls (into {:toggle                 {:channel 0 :controller 56}
                           :back                   {:channel 0 :controller 55}
                           :previous               {:channel 0 :controller 62}
                           :next                   {:channel 0 :controller 63}
                           [:checkpoint :recovery] {:channel 0 :controller 103}
                           [:checkpoint :named]    {:channel 0 :controller 102}
                           :confirm                {:channel 0 :controller 104}}
                           (map (fn [row] [[:select row] {:channel 0 :controller (+ 20 row)}]))
                           (range 8))})
```

This example uses Repeat to open or close the browser, Octave Up to go back, Page Left and Right to page, and the buttons below the screen to preview the aligned show. The three restore addresses are examples for captured input replay. Map them to verified unused buttons on the selected controller and port. Omit the checkpoint and Confirm assignments for a browsing-only connection. For Push 3, pass the usual `:user-mode?`, `:expression-mode` and captured `:layout` options, and put the selected `:usb` address in `:display-options`.

The active show’s Working recovery and Latest named checkpoints appear when available. Additional files come from the optional trusted `:catalog-saves` vector. Open a row, choose a checkpoint, then press Confirm when the display asks to replace the running show. If the checkpoint changes after selection, go back and choose it again. After the owner commits, the initiating Push opens a fresh session and shows **Checkpoint restored**. Use Back to return to the live display. Restore retires the old controller holds and sessions. The initiating connection reopens automatically while it still owns its reconnect request.

From the REPL use `tolglow.controllers.content-browser-host/project` to get the row and exact source, then `preview` with the page's runtime ID, row's selection and chosen source. Pass the returned `:confirmation` to `confirm-load!` and dereference its `:receipt` for the same committed import outcome used by Push. Studio's checkpoint restore uses the same show import owner. [Host functions](../src/tolglow/controllers/content_browser_host.clj) · [#552](https://github.com/tolgraven/tolglow/issues/552)

### Browse registered effects and cues

Bind the optional `:next-kind` action in `:catalog-controls` to a captured,
unused button. In the catalog list it cycles through Saved shows, Registered
effects and Registered cues. Page through the list and use its row buttons
to preview the displayed version. Effect previews show categories and
registered cue usage. Cue previews show the referenced effect version,
categories and page usage. Both show the running instance count. Back returns
to the same page with the row highlighted. These previews are read-only.

The same catalog is available in the REPL. Use `:kind :cue` for the cue list.

```clojure
(require '[tolglow.controllers.content-browser-host :as catalog])

(let [{:keys [runtime-id items]} (catalog/project runtime [] {:kind :effect :page 0})]
  (when-let [{:keys [selection]} (first items)]
    (catalog/preview runtime [] {:runtime-id runtime-id :selection selection})))
```
