# Working on tolglow

Read [docs/README.md](docs/README.md), the [v1 contract](docs/implementation-contract-v1.md)
and [roadmap](docs/roadmap.md) before architectural work. The complete requested
product scope is documented; proposed behavior is not automatically shipped.
The later [expressive performance requirements](docs/product/expressive-performance.md)
add typed beat modulation/categories, MPE/controllers, a lighting synthesizer,
musical Live patchbay, dual-host Link/AbleSet, rich device widgets and sharing.

## Workspace and ownership

- Tolglow owns authoring, registry/compiler, composition/policy, integrations and
  the new studio. The companion `tolgraven/afterglow` fork owns the shared
  runtime/control/render modules, device transports and legacy application.
  Keep independent repositories and preserve existing `tolmaster` work.
- JVM application source is under `tolglow/`; the repo root is on the classpath.
  The new browser source is under `src/studio/`. `deps.edn` now supports the
  application and tests, including a local `../afterglow` dependency. Both test
  lanes consume new fork APIs; the old default-master pin is not sufficient.
- The studio frontend is **ClojureScript with re-frame and Reagent**. The backend
  is **Ring**, with a separate Jetty server adapter. Keep domain transitions in
  the common model; re-frame app-db owns client projections/interaction state.
  Specialized WebGL/Neovim WASM JS engines belong behind explicit interop.
- `core` and `live` now have headless require/reload tests. Read source before
  importing unfamiliar scratch/graphics namespaces. `live/start-demo!`, rig
  activation, physical device discovery and output are explicit operations,
  never ordinary test setup.

## Development contract

- Run `./bin/test pure` and applicable `./bin/test parity` against the paired
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

- Use [GitHub Issues](https://github.com/tolgraven/tolglow/issues) for scope, ownership, prerequisites and evidence. Coordinate overlapping edits and shared public contracts with the other owners.
- Record actual commands, outcomes and limits before marking a milestone done.
  Completed v1 modules do not complete broader roadmap items. Keep source audit,
  focused test, integrated test, browser and physical device evidence distinct.
- Hardware support needs hardware evidence. Push 3 generic MIDI bindings are
  not native display/standalone support. The robot duck remains later research
  until its identity/SDK are known. Do not place provider secrets in definitions,
  browser snapshots or persisted show documents.
- No physical output is implied by a code change request. Tests use captures;
  user instructions govern actual output when live control is requested.

Nested AGENTS.md files add local rules. Link central contracts instead of
copying schemas into multiple diverging documents.
