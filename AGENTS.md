# Working on tolglow

Read [docs/README.md](docs/README.md), [the codebase survey](docs/codebase-survey.md),
and [the roadmap](docs/roadmap.md) before architectural work. The full requested
product scope is documented there; proposed behavior is not shipped behavior.

## Workspace and boundaries

* Tolglow owns show authoring, declarative definitions, composition, policy and
  user configuration. The companion `tolgraven/afterglow` fork owns the runtime,
  output, transport adapters and current web application. Keep independent Git
  repositories; do not vendor or silently substitute upstream Afterglow.
* Existing code is under `tolglow/`, not `src/`. `deps.edn` currently provides
  test tooling, not a complete interactive application dependency manifest.
* Never load `tolglow.core` or `tolglow.live` to discover functions: startup and
  scratch namespaces contain side effects and unresolved legacy dependencies.
  Start with source inspection and targeted tests. No real show output is needed
  for ordinary tests.

## Development contract

* Run `./bin/test pure` and, when touching fixtures or engine integration,
  `./bin/test legacy` with the sibling Afterglow checkout (or `AFTERGLOW_ROOT`).
  Run `clojure -M:check-roadmap` after tracking changes. See docs/testing.md.
* Preserve baseline behavior with characterization tests before replacing it.
  Add behavioral tests for state transitions, boundary values and concurrency;
  do not stub whole dependency namespaces to manufacture a green suite.
* New effect functions take explicit frame context and return updated context.
  Keep I/O, clocks, RNG ownership, subscriptions and mutable buffer ownership at
  the boundary. Do not imply that Clojure maps containing mutable objects are
  automatically immutable. Do not invoke callbacks or device I/O inside `swap!`.
* Threading must retain stable priority and launch order. Read one consistent
  frame snapshot; apply completed worker results deterministically. Do not share
  mutable effect state or output buffer ranges between workers.
* Controllers and web interactions use the same semantic commands and
  projections. Do not put Push-specific note numbers into domain operations.
* Stable registry IDs, definition revisions, instance IDs and show epochs have
  different purposes. Preserve these distinctions in future hot-reload work.
* Measure renderer performance with recorded workloads; a synthetic prototype
  result does not prove end-to-end improvement or GPU suitability.

## Parallel work and completion

* `docs/work-items.edn` is the authoritative implementation ledger. Claim an item
  as `:in-progress`, assign one owner and list exact write paths before editing.
  Coordinate shared contract changes first; avoid overlapping ownership.
* Write design decisions with alternatives, migration and acceptance criteria.
  Update the ledger and docs in the same change. Record actual test commands and
  outcomes; never mark a feature done because its specification is written.
* Stop short of hardware support claims until the device evidence exists. Keep
  Push 3 controller and standalone investigations separate. Keep the robot duck
  as later research; its product identity and SDK are unconfirmed.
* No hardware or production show activation is implied by a request for code
  changes. Transport tests use capture sinks. User instructions govern actual
  output when live control is requested.

Nested AGENTS.md files add guidance for their folder. Keep them concise and
link to the central documents instead of duplicating the specification.
