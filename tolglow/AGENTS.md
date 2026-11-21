# Tolglow source

This tree is the application and user authoring layer on top of the Afterglow
fork. The classpath source root is the repository root, not a `src` directory.
Read the root architecture and work tracking documents before taking a task.

## Current responsibilities

- `fixtures.clj` expands the declarative maps in `config.clj` into Afterglow
  channels and pixel heads. Keep fixture mode, channel address, physical unit,
  and calibration semantics explicit. Do not infer channel footprint from the
  number of channel descriptions when fine or sparse offsets are possible.
- `fx.clj`, `param.clj`, `color.clj`, and `vars.clj` contain effect factories,
  dynamic parameter helpers, and cue variable metadata. Many depend on dynamic
  `*show*`, atoms, and eager namespace initialization. New pure code must receive
  state, time, parameters, fixture data, and randomness explicitly.
- `cue.clj` and `page.clj` construct and place cues. Preserve stable definition
  and instance identities during the registry/hot reload migration. A restart
  must invalidate old instances and callbacks instead of leaving stale cues.
- `setup.clj`, `core.clj`, `osc.clj`, and `util.clj` own current application and
  I/O integration. Put new external actions behind adapters; callbacks emit
  commands instead of changing shared effect state themselves.
- `random.clj` has an incomplete automatic show controller. `macro.clj`,
  `sequencer.clj`, `show_control.clj`, and `audio.clj` are mostly proposals or
  stubs. File existence is not evidence that these features work.
- `viz.cljc` and `graphics/` contain Quil/Processing experiments. The future
  browser simulator and rig editor share scene data and fixture output with the
  control application, but must not own the authoritative lighting clock.

## Migration constraints

Keep a pure function contract for new effects: an explicit immutable context
enters, updated local state and output data leave. Atom use is allowed at the
runtime/I/O boundary, not hidden inside new effect algorithms. Keep hot numeric
buffers privately owned by workers; do not allocate a nested map per pixel just
to satisfy an immutable interface. Worker results are merged in deterministic
priority/order before committing a frame.

Route Push, web, MIDI, OSC, automation, and agent actions through the shared
control contract. Controller implementations translate input and render shared
state; they do not define their own cue launch or parameter semantics. Keep
transport IDs distinct from cue/effect IDs. Future robot telemetry uses this
same boundary.

Keep declarative definitions as validated data. Imported fixture definitions,
effect maps, and parameter maps are not trusted executable Clojure. Authoring
code is a separate deliberate capability with validation and revision tracking.
Do not place provider secrets in definitions, scene snapshots, or browser state.

## Loading and verification

Require narrow namespaces during tests. `live.clj` contains a top-level
`add-effect!`; other application namespaces bind global configuration or depend
on graphics, controllers, and web startup. Do not use full show startup merely
to test arithmetic. Remove top-level experiments only in a scoped migration
with tests, since users may currently rely on REPL workflows.

Run the applicable baseline and integration tests before marking a task done.
Distinguish code inspection from successful namespace loading and from a real
device test. Do not call a feature implemented based on a scaffold or stub.
Record exact commands, dependencies, known failures, and remaining acceptance
criteria in the work tracker.
