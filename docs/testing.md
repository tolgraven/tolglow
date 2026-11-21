# Testing and compatibility gates

Use Java 17 or 21 and Clojure CLI (developed with 1.11.1.1435). Both projects pin
Clojure **1.11.4**. Keep the repositories as siblings, or set `AFTERGLOW_ROOT` for
Tolglow's legacy lane. No OLA server, MIDI device, Push or browser is required.

```sh
cd /path/to/afterglow
./bin/test pure
./bin/test legacy

cd /path/to/tolglow
./bin/test pure
./bin/test legacy
clojure -M:check-roadmap
```

`bin/test` fails on invalid suite names, dependency/loading errors and test
failures. It never silently downgrades legacy to pure. `clojure -M:test` runs
only the pure lane; use the script to assemble legacy dependencies.

## Observed baseline

On 2026-11-21, OpenJDK 21.0.13 on Linux:

| Repository/lane | Tests | Assertions | Outcome |
| --- | ---: | ---: | --- |
| Afterglow pure | 13 | 51 | Passed |
| Afterglow legacy (includes pure) | 47 | 379 | Passed |
| Tolglow pure | 10 | 57 | Passed |
| Tolglow legacy (includes pure) | 20 | 103 | Passed |

The broad lanes total **67 tests / 482 assertions**; do not add pure counts again.
CI definitions request Java 17 and 21, but hosted CI and Java 17 were not run in
this workspace. No hardware, browser, full startup, release packaging or render
throughput result is implied by these numbers.

The active application branch pairing remains unconfirmed. Afterglow `master`
lacks `Param`/`color-type` imported by Tolglow; `tolmaster` contains these but
requires missing thi.ng dependencies. See [branch compatibility](branch-compatibility.md).
Selecting and validating the user's application branch is an explicit blocked
item, not covered up by the fixture lane's successful run.

## Dependency policy

The legacy lane reads dependency vectors and repository URLs from Afterglow's
`project.clj` using the Clojure reader with read-eval disabled. It does not eval
the project, invoke Leiningen plugins, build docs or initialize the app. Preserve
`:exclusions` when converting coordinates. The small helper exists in both repos
so Tolglow can characterize a pinned historical Afterglow checkout independently;
keep the copies synchronized until shared build tooling is published.

The historical `org.deepsymmetry/beat-link 8.0.0-SNAPSHOT` could not be resolved.
**Legacy tests explicitly use released 8.0.0.** This substitution is in
`dev/legacy_deps.clj`; `project.clj` is unchanged. The tests do not exercise DJ
Link integration, so validating/replacing the application dependency remains a
separate work item. Do not silently expand overrides or claim full dependency
parity. Tests require Maven Central and Clojars downloads on first run.

Tolglow CI pins Afterglow at the survey SHA. During a coordinated change, run
both suites against the modified sibling locally, then update that CI reference
to the reviewed Afterglow commit. Also keep a baseline compatibility job if
supporting both versions. Passing against the old pin alone does not validate a
new engine API.

## Coverage and limits

* **Tolglow iterators:** eager mapping, zip truncation, nil/empty inputs and
  bounded interaction with infinite inputs. Original source was verified too.
* **Tolglow fixtures:** RGB/RGBW layouts, positions, strips, channel descriptors,
  calibration and strobe scaling using actual Afterglow dependencies.
* **Afterglow context:** nested binding, exception restoration and concurrent
  isolation without requiring the application core.
* **Engine contracts:** fixture offsets, fine channels, function ranges, timing
  snapshots, oscillator boundaries, ordered assigners, fades/scenes, channel
  packing, effect priority ties, replacement identity and graceful/forced end.
* **Experimental kernel:** state threading/replay, middleware composition,
  deterministic contribution order, independent state, terminal frame semantics,
  invalid results and real threaded completion-order independence.

Pure functions can still receive mutable Java objects or impure callbacks. The
kernel validates contract shape, not arbitrary functional purity. The executor
test proves scheduling-order independence for tested pure steps; it does not
provide a production executor, deadline management or exactly-once external I/O.

## Gates before replacing live code

1. Capture representative small rigs and 100/1,000/10,000-pixel scenes. Freeze
   metronome snapshots, seeded random streams and command logs.
2. Compare old/new assignments and final DMX bytes across LTP/HTP, RGBW,
   movement calibration/history, coarse/fine channels and resolution order.
3. Replay concurrent command arrival, duplicate input, release/force stop,
   restart epochs and hot-reload failures. Check stale worker rejection and
   exactly-once intent dispatch at the coordinator boundary.
4. Benchmark stage latency, allocation/GC, p50/p95/p99, missed deadlines and
   output/readback costs with warmup and environment metadata. Do not gate CI
   on noisy wall-clock microbenchmarks alone.
5. Record/compare device messages and view models before physical Push tests.
   Test web touch/keyboard behavior and disconnects in browser automation;
   validate real iPad Safari and phone quality budgets separately.

Keep known defects visible. First reproduce with a regression test; fix them in
a deliberate behavior change instead of rewriting the characterization to fit
new implementation. Never require side-effectful scratch namespaces in test
discovery. Test new implementations against independent expectations, not only
against themselves.
