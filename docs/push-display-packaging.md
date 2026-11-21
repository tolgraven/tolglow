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
(tolglow.studio/push-browser-display runtime
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
