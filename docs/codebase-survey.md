# Codebase survey

Surveyed 2026-11-21 from clean public default branches. No existing local
checkout, build configuration or AGENTS.md was present in the workspace.

This is the historical survey of the listed revisions, not the current project
layout or delivery status. The application now has `src/tolglow/`, `resources/`,
`test/`, `dev/`, `bin/`, root manifests and the separate `src/studio/` frontend.
See [the architecture index](README.md) and [work ledger](work-items.edn) for
the current implementation; original behavior remains a feature-parity reference.

| Repository | Branch | Survey baseline |
| --- | --- | --- |
| tolgraven/tolglow | effects | `01146ece7731ec06aa1ee6409342373096c8e0dc` |
| tolgraven/afterglow | master | `16b0b4cac70433e4118038dfbfd304e22d1d9018` |
| tolgraven/tolgrAVen (reference only) | master | `9daaea09577d9c7b80519e5c309583faf789c855` |

Source references below are repository-relative and identify the repository.
This is a code survey and targeted headless execution, not a hardware demo or
proof that the entire interactive application starts on a fresh installation.

**Branch compatibility:** Tolglow imports `afterglow.effects.params.Param` and
expects `color-type`, which are absent from the surveyed Afterglow `master` but
present on `tolmaster`. The latter also has undeclared thi.ng requirements.
See [the branch survey](branch-compatibility.md). The user's active branch/local
changes are not yet confirmed. The passing narrow suites establish their stated
contracts on the recorded baseline; they do not establish full app compatibility.

## Tolglow

The README originally contained only a description. Namespaces live directly in
`tolglow/`; there was no `project.clj`, `deps.edn`, test tree or CI test lane.
They are an exploratory live-coding layer over Afterglow rather than an
independently packaged application.

| Files | Existing responsibilities | Migration implications |
| --- | --- | --- |
| `core.clj`, `setup.clj`, `config.clj` | Startup, fixture configuration, global bindings and component initialization | Separate data/configuration from starting services; don't require core to discover definitions |
| `fixtures.clj` | Keyword channel descriptions, color layouts, pixel strip generation and head positions | Useful declarative seed; characterize offsets, RGBW layouts and geometry before changing schemas |
| `fx.clj`, `param.clj`, `vars.clj`, `color.clj` | Effects, parameter construction/coercion and cue-variable metadata | Mix data, constructors, dynamic show lookups and mutable closures; establish one schema and compiler |
| `cue.clj`, `page.clj` | Cue creation, layouts, grouping and generated pages | Preserve existing layouts while introducing stable cue/effect registry references |
| `random.clj` | Experimental randomization helpers | No complete capability-aware, reproducible randomization system |
| `macro.clj`, `sequencer.clj`, `show_control.clj` | Sketches and partial composition/control helpers | `secue`/`secudfx` are empty; these are not finished macro/sequencer features |
| `osc.clj` | OSC configuration and mapping experiments | Receiver contains placeholder behavior and discarded registration; do not advertise a complete adapter |
| `audio.clj`, `live.clj` | Audio stub and live-coding scratch examples | `live.clj` does not mean Ableton Live integration; it has executable top-level forms |
| `viz.cljc`, `graphics/` | Quil/Processing-style visualization experiments | Useful research, not the proposed web simulator or portable render backend |
| `utils/iters.clj` | Iterator, zip and eager map helpers | Existing behavior is now tested against the actual source |

Potential defects to reproduce in focused follow-ups include fine-channel option
naming in the fixture color helper, channel-count-based prefix offsets for
sparse/fine strip channels, zero-input multi-iterator behavior, stale protocol
extension names in color code and unresolved references from OSC/configuration.
Do not mix these into architecture changes on inspection alone. An initial
suspected `(conj!)` accumulator defect was disproved: Clojure 1.11.4 supports
that arity, and the original iterator source passed the new baseline tests.

## Afterglow

This fork has a substantial established engine, guides and packaging. Its
`project.clj` uses Clojure 1.11.4 and has build plugins, generated docs and
version resources in prep tasks. The new test entry point bypasses those prep
tasks; it does not establish that `lein uberjar` succeeds.

| Files under `src/afterglow/` | Existing responsibilities |
| --- | --- |
| `show.clj` | Show construction, fixture patching, effect ordering, render lifecycle and output |
| `effects.clj` | `IEffect`, `IAssigner`, ordered reduction, scenes, fades and chases |
| `effects/params.clj`, `rhythm.clj` | Dynamic params and metronome snapshots |
| `effects/channel.clj`, `color.clj`, `movement.clj` | Assignment resolution into output values; movement/color history |
| `channels.clj`, `transform.clj`, `fixtures/` | Channel descriptors, 3D transforms, fixture library and QXF import |
| `controllers.clj`, `controllers/*` | Existing grid/overlay protocols, discovery, MIDI and device-specific interaction |
| `web/routes/show_control.clj` | Web sessions, UI state polling, cue interactions and direct show mutations |
| `resources/public/`, `resources/templates/` | Current browser interface and preview assets (paths relative to repo root) |

The render pipeline already has useful functional seams:

1. Capture a metronome snapshot; remove finished effects.
2. Generate assigners from active effects and group them by kind and target.
3. Reduce each target's ordered assigners, passing the previous value forward
   through `effects/run-assigners`.
4. Resolve kinds in the engine's fixed order into universe buffers; output the
   frame and roll movement history forward.

`show/send-frame` uses Claypoole parallel work for effect generation and target
resolution. The problem is not absence of multithreading: effect closures hold
atoms; show variables/active effects are read at multiple points; resolvers
write shared movement state and byte arrays. Lazy generation can also shift work
outside the intended parallel stage. The default refresh binding is 40 Hz even
though a show docstring says 30. Measure the actual configured cadence.

`run-assigners` already expresses precedence and blending. Preserve that contract
before changing storage or introducing GPU execution. `code` effects call user
callbacks inside `swap!`; sequential tests do not establish exactly-once behavior
under contention. Scenes and release wrappers also hide mutable lifecycle state.

Loading apparently simple channel code pulls in `fixtures.qxf`, then web layout,
Selmer and Ring. Hardware-free tests therefore still need many application
dependencies. Removing this coupling is a future packaging task, not simulated
by replacing namespaces with test stubs.

## Devices, UI and simulator

Push 2's driver is 3,029 lines and the web show-control route is 974 lines at the
baseline. Existing `IGridController` and overlay abstractions should be studied
and adapted, not described as nonexistent. The missing boundary is a complete
device-independent semantic control layer; current drivers combine transport,
interaction policy, cue state, view models and display rendering.

The existing browser preview uses polling and a noise-based volumetric shader;
it is not evolving fog density with injection/advection/diffusion. Its current
light-count and fixture-control limitations make it a reference, not the new
simulator. AAA-level appearance on all iPads and phones is a research target;
quality tiers and measured budgets are necessary.

The tolgrAVen Neovim reference includes a WASM worker and shared-memory setup
requiring cross-origin isolation. Reuse its browser integration approach only
after accounting for asset hosting, worker startup, touch controls and the
lighting app's evaluation/diagnostic boundary. The code editor is not itself a
safe execution service.

## What changed in this foundation

Both repos now have explicit Clojure CLI test lanes. Afterglow gains genuine
show-context coverage in place of its vacuous core test, engine characterization
tests and an experimental `afterglow.runtime/render-frame` kernel. Tolglow gains
iterator and fixture characterization. Existing production effect, renderer,
controller and UI code is unchanged; the prototype is additive and unintegrated.
See [testing](testing.md) for exact evidence and limitations.
