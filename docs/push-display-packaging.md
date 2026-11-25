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

The replay uses the production screen encoder and 24 actual Studio simulator captures. Hardware qualification covers colour and edge orientation, Live handoff, continuous audio/MIDI and disconnect. See [Afterglow’s driver, receipts and on-device instructions](https://github.com/tolgraven/afterglow/blob/master/doc/architecture/push-3-display.md).

## Browse saved shows

`tolglow.controllers.content-browser-display/connect-device!` adds a read-only saved-show browser to the shared browser display on Push 2 or tethered Push 3. Supply explicit button captures that fit your layout. The normal cue pads and encoder gestures remain available during browsing.

```clojure
(require '[tolglow.controllers.content-browser-display :as saved-shows])

(saved-shows/connect-device! runtime :push-2
  {:session-id "desk-push"
   :input-index verified-input-index
   :output-index verified-output-index
   :display-options {:browser-executable "/usr/bin/chromium"
                     :url "http://127.0.0.1:8500/push-display"}
   :catalog-controls (into {:toggle   {:channel 0 :controller 56}
                           :back     {:channel 0 :controller 55}
                           :previous {:channel 0 :controller 62}
                           :next     {:channel 0 :controller 63}}
                           (map (fn [row] [[:select row] {:channel 0 :controller (+ 20 row)}]))
                           (range 8))})
```

This layout uses Repeat to open or close the browser, Octave Up to go back, Page Left and Right to page, and the buttons below the screen to preview the aligned show. Verify these addresses against your controller and port. For Push 3, pass the usual `:user-mode?`, `:expression-mode` and captured `:layout` options, and put the selected `:usb` address in `:display-options`.

The active show’s Working recovery and Latest named checkpoints appear when available. Additional files come from an optional trusted `:catalog-saves` vector in [the host format](../src/tolglow/controllers/content_browser_host.clj). The preview displays checkpoint metadata. Opening, paging, and previewing leave the current show running. On-device restore and its committed result are tracked separately in [#552](https://github.com/tolgraven/tolglow/issues/552).
