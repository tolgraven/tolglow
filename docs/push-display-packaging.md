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

The display draws at 960 × 160. Cold entry can use the configured browser startup
deadline to compile shaders. Warm captures retain the ordinary capture deadline.
A frame already in progress finishes before the worker reads back its PNG.
Switching back to parameters or the target picker releases the scene and WebGL
context. Each capture consumes the latest committed lighting values. It performs
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
