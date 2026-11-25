# Working on tolglow

Search [GitHub Issues](https://github.com/tolgraven/tolglow/issues) when a request arrives. Update the matching issue with the operator’s goal and a concrete acceptance check. Open a new issue for distinct work and link its dependencies. Keep other active requests open until their own checks pass.

When a show feature changes, review it from both the running REPL and Studio. Name the common command or data, check feedback in each direction and record any remaining gap. See [REPL and Studio parity](docs/repl-studio-parity.md).

Read the [architecture index](docs/README.md), the [v1 contract](docs/implementation-contract-v1.md)
and the relevant issue before architectural work.
The later [expressive performance requirements](docs/product/expressive-performance.md)
add typed beat modulation/categories, MPE/controllers, a lighting synthesizer,
musical Live patchbay, dual-host Link/AbleSet, rich device widgets and sharing.

## Workspace and ownership

- Tolglow owns authoring, registry/compiler, composition/policy, integrations and
  the new studio. The companion `tolgraven/afterglow` fork owns the shared
  runtime/control/render modules, device transports and legacy application.
  Moving-head calibration, GLSL, GPU dispatch and readback belong in Afterglow.
  Tolglow can measure a whole show through that API and owns the Studio scene.
  Keep independent repositories and preserve existing `tolmaster` work.
- The full project has JVM application source under `src/tolglow/`, with
  `resources/`, `test/`, `dev/`, `bin/`, and a root `project.clj` application
  manifest. `src/` is the JVM classpath source root; the repository root is not.
  The browser source is under `src/studio/`. `project.clj` is the canonical
  application/test manifest, including paired `../afterglow` sources (override
  with `AFTERGLOW_ROOT`). Both test
  lanes consume new fork APIs; the old default-master pin is not sufficient.
- The studio frontend is **ClojureScript with re-frame and Reagent**. The backend
  is **Ring**, with a separate Jetty server adapter. Keep domain transitions in
  the common model; re-frame app-db owns client projections/interaction state.
  Keep `src/` for Clojure, ClojureScript, shared CLJC code and local documentation.
  Author the Three.js engine in `js/preview/`. Public assets and executable device
  resources belong under `resources/`. Use `test/js/` for renderer and device
  JavaScript tests and `dev/` for executable QA and profiling tools.
- `core` and `live` now have headless require/reload tests. Read source before
  importing unfamiliar scratch/graphics namespaces. `live/start-demo!`, rig
  activation, physical device discovery and output are explicit operations,
  never ordinary test setup.

## Development contract

- Leave a blank line between adjacent top-level Clojure and ClojureScript definitions. Run `lein check-definition-spacing` to check tracked source.
- Run `lein test-pure` and applicable `lein test` against the paired
  fork. Frontend changes
  require the configured production build plus fresh browser interaction
  checks; desktop viewport emulation does not prove real iPad Safari behavior.
  See [testing](docs/testing.md) for exact scope and observed results.
- Characterize behavior before replacing it. Test real public functions and
  dependencies with capture sinks/injected transports at external boundaries.
  Add behavioral transition, validation and concurrency regressions; do not
  fake whole dependency namespaces to manufacture green tests.
- New effects take explicit context and return state/output data. IO, clocks,
  RNG ownership, subscriptions and mutable buffers belong at owned boundaries.
  A map containing a mutable object is not an immutable snapshot. Never invoke
  external callbacks or IO inside CAS retry functions such as `swap!`.
- Preserve deterministic priority/launch order. Freeze one frame snapshot,
  evaluate private instance state and merge results deterministically. Workers
  must not share mutable buffers or output ranges without explicit ownership.
- Controllers, web, automation and agents use the same validated commands and
  projections. Keep device note numbers outside domain logic. Distinguish
  definition/version, cue ID, instance ID, show epoch, command and frame revision.
- Musical authoring uses beat/bar/phrase units as its primary vocabulary. Keep
  milliseconds at explicit engine/audio/device boundaries; convert through the
  authoritative transport/tempo map rather than scattered UI arithmetic.
- Modulation and waveform state is explicit and seeded. Respect each parameter
  type, units, bounds, category overlap and human override policy. Normalize
  derivative/slope by its declared musical/physical interval; preview must not
  evaluate stateful signals a second time.
- MPE/per-note controls keep device/channel/note/activation identity. Verify
  Push 3, Seaboard and X-Touch modes/addresses from references or captures;
  generic MIDI support is not native display or hardware verification.
- Shared web/device widgets consume common model projections. Offscreen webview,
  Live/Link/AbleSet and afterglow-max need executable host evaluation before
  availability claims. Keep their work off the lighting deadline.
- Full local backups preserve complete state. Sharing exports are a separate
  explicit selection with dependencies/provenance and private data handling;
  do not weaken backup fidelity or publish as a side effect of saving.
- Record measured workloads and excluded costs. Neither a synthetic loop nor
  software WebGL execution establishes complete show throughput or GPU benefit.

## Parallel work and completion

- Record the owner, affected paths and prerequisite issues on the existing GitHub
  issue before editing. Coordinate shared contracts with the other owners.
- Link the pull request, tested revision, commands and observed results on the
  issue. Close it when its acceptance checks pass. For partial delivery, keep
  the issue open and identify the remaining work or hardware qualification.
- Hardware support needs hardware evidence. Push 3 generic MIDI bindings are
  not native display/standalone support. The robot duck remains later research
  until its identity/SDK are known. Do not place provider secrets in definitions,
  browser snapshots or persisted show documents.
- No physical output is implied by a code change request. Tests use captures;
  user instructions govern actual output when live control is requested.

Nested AGENTS.md files add local rules. Link central contracts instead of
copying schemas into multiple diverging documents.

Development workspace requirements are in
[the workspace specification](docs/product/development-workspace.md). Keep live show
authoring separate from branch/worktree application edits. A Quake/Bruvbox REPL,
Neovim repository/scratch workflow, agent diffs/watchers, target-specific reload
with last-good revert, and Docker/zsh terminal need actual execution and lifecycle
evidence. Capture portable locals explicitly; do not serialize opaque closures.
Keep manual, after-turn and preview reload policies visible and scoped.

Sensor and deadline contracts are in [sensors](docs/sensors.md) and
[output presentation](docs/output-presentation.md). Keep sensor model state separate
from owned credentials/sockets and require explicit fresh-session output arming.
Completed lighting packets retain original physical timestamps; presentation
never advances effect state. Fade only declared linear emission controls and
preserve discrete channels/coarse-fine semantics. Scene/fog visual work has one
owner; backend projection and shared frontend widgets have separate claims.
