# Tolglow JVM source

`src/` is the JVM classpath source root. This tree contains existing show
authoring and the new functional application. Read the root guidance
and shared [v1 contract](../../docs/implementation-contract-v1.md).

Code that configures lights, devices, saves or providers belongs in the owning show namespace. Use it directly from code, config and Studio. Keep Ring, Jetty and re-frame at the web boundary. See [public module APIs](../../docs/headless-modules.md).

Give each operator-facing show action a discoverable REPL example against the active runtime. Check that Studio can use the same domain operation and observe its outcome. Track missing paths in [REPL and Studio parity](../../docs/repl-studio-parity.md).

## Module boundaries

- `fixtures.clj` expands the original declarative patch maps into actual Afterglow channels
  and pixel heads. Preserve physical units, sparse/fine offsets, calibration and
  channel footprints. `config.clj` is user setup, not a fixture schema library.
- `fx.clj`, `param.clj`, `color.clj`, `vars.clj`, `cue.clj` and `page.clj` retain
  legacy dynamic-show/parameter workflows. Compatibility tests now load these
  against the merged fork. Do not silently replace their contracts with the new
  model. Old source is reference material for feature/functional parity only;
  do not add a runtime compatibility layer.
- `registry.clj` and `compiler.clj` validate portable definitions and compile
  supported expression data into pure steps. A definition revision is different
  from a running instance. Invalid edits retain the last valid running definition.
- `composition/` and `automation/` own reusable pure tween/envelope/sequence,
  macro/response and seeded policy logic. Use explicit logical time and RNG.
  Beat/bar/phrase authoring is primary; milliseconds are a boundary representation.
- `application` and `modules` own ordered config startup and teardown; `show`
  runs headless and owns frame commits. `studio` binds Ring routes and attaches
  observers to that same runtime. Keep configured service lifecycles usable
  from code. Exercise the actual CLI entry points when changing startup.
- `studio/connections.clj` owns provider/fixture/audio/sync/reactive resources.
  Prepare replacements outside the model lock, commit ownership with model/output,
  and retire or abort resources outside it. Capture/sync resumption follows
  clock rebase; stale callbacks retain their original epoch.
- `integrations/` contains tested fixture import, Live/audio mappings and agent
  primitives. Live-side software, actual audio routing and external providers
  need separate verification from mocked or loopback protocol tests.
- Legacy `random.clj`, `macro.clj`, `sequencer.clj`, `show_control.clj`, `audio.clj`
  and graphics experiments are not evidence that their new counterpart features
  are complete. `viz.cljc` and `graphics/` remain separate Quil experiments.

## Functional and control contracts

New effects receive explicit immutable context and return local state plus
attribute/intent data. IO, clocks and mutable resources belong to the host.
Do not allocate a nested map per pixel merely to appear functional: privately
owned primitive buffers are allowed at controlled backend boundaries. Merge
worker results by stable priority/order before committing a complete frame.

Push, web, MIDI, OSC, automation, agents and future sensor/robot inputs all use
the shared control model. No adapter invents its own launch/end semantics or
mutates legacy atoms behind the coordinator. Keep transport identity, source,
definition/cue/instance IDs and show epochs distinct.

Imported fixture maps and effect DSL documents are data, not executable reader
forms. Use safe EDN parsing and validate before publication. Code authoring is a
separate deliberate capability. Provider secrets stay in runtime configuration,
never show definitions, scene snapshots or browser state.

## Loading and verification

`core` and `live` have intentional headless compatibility tests. `live` no longer
installs its demo on require; call `start-demo!` explicitly in a show context.
This does not authorize normal unit tests to activate the user's rig, create
Processing sketches or open devices. Capture output and inject transport shells.

Run focused behavioral tests, then applicable integration lanes. Record exact
scope and counts; distinguish a pure module from a complete UI/workflow and a
source-derived protocol from physical hardware evidence. Record the owner and
affected shared APIs on the existing GitHub issue before editing.

## Expanded performance work

Follow [expressive performance](../../docs/product/expressive-performance.md) for
typed beat modulation, categories, registered waveforms/derivative/jagginess and
audio-derived signals, controller automapping, musical Live patchbay and sharing.
Keep waveform/modulation state and RNG explicit, schema-valid and persistable.
Category operations must respect overlapping membership and human ownership.
Live/Link/AbleSet and afterglow-max need verified APIs and host tests; MIDI delivery
is not proof of a clip write. Sharing manifests/provenance and deliberate privacy
selection must not weaken complete local backups.

Development workspace requirements are in
[the workspace specification](../../docs/product/development-workspace.md). Keep live show
authoring separate from branch/worktree application edits. A Quake/Bruvbox REPL,
Neovim repository/scratch workflow, agent diffs/watchers, target-specific reload
with last-good revert, and Docker/zsh terminal need actual execution and lifecycle
evidence. Capture portable locals explicitly; do not serialize opaque closures.
Keep manual, after-turn and preview reload policies visible and scoped.

Sensor and deadline contracts are in [sensors](../../docs/sensors.md) and
[output presentation](../../docs/output-presentation.md). Keep sensor model state separate
from owned credentials/sockets and require explicit fresh-session output arming.
Completed lighting packets retain original physical timestamps; presentation
never advances effect state. Fade only declared linear emission controls and
preserve discrete channels/coarse-fine semantics. Scene/fog visual work has one
owner; backend projection and shared frontend widgets have separate claims.

## Fixture brightness and performance controls

Keep source brightness, watts, visual activity and audio energy in their own units.
Use the existing calibrated meter for both local and rig-relative brightness.
Forecast auto selections through the same effects and patch as live composition.
Apply the group/rig response to authored light, enforce concurrent budgets, then
read and score the delivered emitter channels. Keep coarse/fine encoding and
bulk frame operations in Afterglow. Run the mixed 2,048-pixel/64-head benchmark
when changing that path. [Operator guide](../../docs/group-lighting-scenes.md)
