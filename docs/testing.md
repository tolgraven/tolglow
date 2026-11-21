# Testing and compatibility gates

Use Java 17 or 21 and Clojure CLI (developed with 1.11.1.1435). Both projects pin
Clojure **1.11.4**. Keep the modified repositories as siblings: new Tolglow code
uses the companion Afterglow control/runtime/render modules in both lanes.
`AFTERGLOW_ROOT` overrides the parity dependency helper's path; the standard
Tolglow `deps.edn` local dependency points to `../afterglow`.

```sh
cd /path/to/afterglow
./bin/test pure
./bin/test parity

cd /path/to/tolglow
./bin/test pure
./bin/test parity
clojure -M:check-roadmap
```

`bin/test` fails on invalid suite names, dependency/loading errors and test
failures. It never silently downgrades parity to pure. `clojure -M:test` runs
the pure lane; the script assembles the parity dependency classpath. The
runner source in each repository lists the namespaces actually selected.
“Pure” names the lane: it includes bounded executor and loopback HTTP tests in
addition to pure functions. It does not mean every test is free of local IO.
No real OLA service, lights, MIDI ports or controller hardware is activated.

## Observed checkpoints

On 2026-11-21, OpenJDK 21.0.13 on Linux. Results below are successive or focused
checkpoints, not disjoint totals. Current parity lanes include pure tests; earlier runs called this lane legacy. Do not add
the columns or add a focused result to an integrated run containing it.

| Checkpoint | Tests | Assertions | Scope |
| --- | ---: | ---: | --- |
| Original Afterglow pure | 13 | 51 | Initial context/kernel baseline |
| Original Afterglow legacy | 47 | 379 | Initial default-master baseline, includes pure |
| Original Tolglow pure | 10 | 57 | Iterator baseline |
| Original Tolglow legacy | 20 | 103 | Initial fixtures/iterators against default master |
| Tolglow compatibility checkpoint | 32 | 184 | Merged fork and repaired legacy namespaces; includes pure then selected |
| Controls and codecs checkpoint | 36 | 1234 | Shared model/bindings/views/MIDI/OSC/Push 2 codecs |
| Latest model-only follow-up | 15 | 130 | Includes captured source identity regression |
| Device sessions | 20 | 90 | Fake MIDI/display lifecycle, threads, cleanup, real headless image rendering |
| Engine and executor | 19 | 76 | Shared-model lifecycle/reload and worker failure/deadline behavior |
| Removed bridge experiment (historical) | 12 | 52 | Withdrawn per explicit no-compatibility-layer direction |
| Authoring aggregate | 40 | 232 | Registry/compiler/composition/automation before final response assertion |
| Authoring response follow-up | 5 | 25 | Subsequent targeted response regression |
| CPU render/frame IR | 21 | 128 | Resolution, packing, patch checks, expression CPU/GLSL support |
| Integration primitives | 30 | 188 | Fixture/Live/audio/provider/agent; includes real loopback HttpClient |
| Full schema2 persistence | 19 | 165 | Active model/output, settings, agent/cache/reactive validation; no IO activation |
| Audio analysis + capture checkpoint | 26 | 214 | Block features, JavaSound boundary with injected PCM streams; no physical audio |
| Fixture manager + import | 20 | 191 | Bounded catalog/cache/GDTF subset, including public OFL provider verification |
| Integrated Afterglow legacy checkpoint | 158 | 1969 | Includes device sessions and legacy bridge |
| Earlier integrated Tolglow pure | 88 | 513 | Before Ring streaming and beat/response host tests |
| Root Afterglow pure checkpoint | 158 | 2030 | Current functional lane, before later sync/replay follow-ups |
| Root Tolglow pure checkpoint | 244 | 1780 | Before final two host/device construction regressions and sync follow-up |
| Connections with sync ownership | 16 | 158 | Inert construction, staged restore, injected capture/MIDI and guarded callbacks |
| Studio resource host | 12 | 80 | Exact state/output/time restoration, pending/stale imports, device races, Unicode files |
| Existing studio suite | 13 | 65 | Actual runtime/commands/Ring/output behavior at host checkpoint |
| Reactive registry/graph | 20 | 99 | Durable definitions/cells/actions/watcher state and transaction failures |
| Agent history and local credentials | 21 | 161 | Manager 16 / 130 + credential store 5 / 31; fake keys only |
| Fixture/registry focused | 31 | 288 | Import/service/registry, public OFL HTTP and supported GDTF subset |
| Rig + persistence | 24 | 217 | Full patch footprints, portable room/truss validation and existing show files |
| Fixture host | 3 | 59 | Actual Ring/runtime fixture publication/patch flow |
| Frontend source | 28 | 124 | re-frame/Reagent source tests; optimized build 129 files / 0 warnings |

All listed checkpoints passed with zero failures/errors. Focused agent results
were reported by the module owner; the integrated checkpoints were run by the
root integrator. The old 67 tests / 482 assertions combined baseline is
historical and no longer describes the current suites.

### Final integrated verification

The integrator owns final combined counts and source revisions. The checkpoint
results below are observed, but later sync/replay/Connections UI changes still
need their selected runs; pending results must not be inferred from older gates.

| Gate | Current final-run state |
| --- | --- |
| Afterglow pure and parity | Root pure 158 / 2030 passed; final paired revision/parity run to record |
| Tolglow pure and parity | Root pure 244 / 1780 passed before final host/sync additions; final combined/parity runs to record |
| Frontend production build | Re-frame release129 files / 0 warnings and source 28 / 124 passed; subsequent Connections UI has its own gate |
| Browser interaction | Restarted-server 9 session + 6 authoring workflows passed with 0 console/page errors; physical Safari and later Connections view pending |
| Work ledger and documentation links | Validate after final ownership/status updates |

CI defines Java 17/21 lanes, but hosted CI and local Java 17 execution have not
been observed here. Cross-repository CI must check out the reviewed new
Afterglow revision in every lane that consumes its APIs. A green old-master
fixture baseline cannot validate the new application.

## Dependency and loading policy

The parity helper reads dependencies/repository URLs from Afterglow's
`project.clj` with read-eval disabled. It does not eval the project, invoke
Leiningen plugins, build docs or initialize the app. Preserve exclusions when
converting coordinates. Keep the helper copies synchronized.

The original Beat Link `8.0.0-SNAPSHOT` was unavailable and the first tests used
an explicit stable `8.0.0` override. The current application `project.clj` now
also declares **8.0.0**, plus `thi.ng/color 1.5.1` and `thi.ng/math 0.3.2` required
by the merged fork. This is no longer merely a test-only substitution. DJ Link
hardware behavior is still unverified. Dependencies may require Maven Central
and Clojars downloads on first run; local proxy/cache configuration is not a
repository requirement or evidence of a public clean-cache run.

`tolglow.core` and `tolglow.live` now have explicit headless compatibility tests.
Loading/reloading `live` no longer installs its demo; `start-demo!` is explicit.
Those tests trap unexpected startup and exercise real functions. They do not
call the user's rig `activate-show`, start Processing sketches or open devices.
See [compatibility repairs](compatibility-progress.md) and the historical/current
[branch record](branch-compatibility.md).

## Coverage and limits

- **Legacy compatibility:** iterator/fixture expansion, sparse/fine offsets,
  parameter and color contracts, explicit show context, channels, rhythm,
  movement, fade/scenes, assigners and lifecycle. Preserve fork-specific behavior
  unless a documented new contract intentionally changes it.
- **Shared control:** validation, defaults/ranges, stable IDs/captured versions,
  monotonic publication, duplicate receipts, stale epoch/revision guards,
  source ownership, typed bindings/views and transport codecs.
- **Functional engine:** explicit state/context, deterministic ordered merging,
  real out-of-order worker completion, timed release, version reload and restart.
  Cancellation/deadline tests cover the owned executor. They do not prove that
  arbitrary user functions terminate or lack side effects.
- **Parity boundary:** preserved old source supplies feature/output oracles. The
  new runtime does not execute old closures through a compatibility adapter. The
  former bridge experiment and its tests have been removed; historical counts
  that included them are not the current suite count.
- **Device adapters:** independent sessions, held-cue release, stale callbacks,
  reconnect/handoff, palette acknowledgements/timeouts, owned scheduler and
  transport cleanup. Native Wayang/MIDI/OSC activation remains opt-in and was
  not performed in tests. Push 3 native protocol/display is not claimed.
- **Authoring/integration:** deterministic definitions/compilation/composition,
  constrained seeded policy, schema-aware mappings, provider stream parsing,
  actual local HTTP cancellation/timeout and scripted tool calls against the
  real reducer. Full Live-side clips, audio devices and external providers are
  not exercised by a protocol or loopback test.

The current re-frame app has actual browser evidence for cue/parameter control,
multitouch and cancellation, keyboard/long-press menus, real Neovim WASM edits and
publish, musical pause/resume, typed macro slots, fixture create/import/patch,
rig drag/keyboard edits and saved truss properties reaching WebGL uniforms.
Desktop, iPad-size and phone-size viewports were exercised. Viewport emulation
is not a physical iPad Safari test. A new Connections view is a subsequent gate.

The [GPU experiment](gpu-experiment.md) passed eight oracle cases/992 compared
components and nine lifecycle groups in Chromium 156 with **software SwiftShader**.
Reports state operation-specific tolerances and excluded JVM/browser/output
costs. Live bridge assets passed 15 Node tests with owned loopback HTTP and
injected LiveAPI. Neither test setup validates execution inside Ableton Live/Max.

## Benchmarks and replacement gates

[Retained benchmark reports](benchmarks/README.md) distinguish the original
assignment/packing stage, the new CPU backend before/after optimization, and
software WebGL execution. Record workload, warmup, samples, environment, checksum,
allocation, p50/p95/p99 and excluded costs. Do not gate CI on noisy wall-clock
microbenchmarks or equate a stage result with a complete show deadline.

Before replacing each legacy family, capture representative deterministic
whole-frame traces and final DMX bytes for small rigs and 100/1,000/10,000-pixel
scenes. Compare RGBW, movement calibration/history, fine channels, kind ordering
and blending. Replay concurrent commands, duplicate input, graceful/forced
release, restart and invalid reload. Exercise stale workers and publication
failure without partial output. Verify external intent dispatch at its actual
boundary; the pure reducer alone cannot prove exactly-once IO.

Keep known defects visible and add behavioral regressions before fixes. Test
against independently specified outputs rather than mirrored implementation.
Device and visual acceptance still require physical Push captures, real iPad
Safari/phone interaction and measured performance under preview/output load.
