# Tolglow / Afterglow architecture program

This program starts by characterizing the existing system, then replaces its
mutable internals in stages. It preserves live coding, concurrent rendering and
human performance while opening the same operations to controllers, a mobile
web application, automation and agents.

**Current delivery:** a source survey, runnable test lanes in both repositories,
an additive pure runtime prototype, detailed specifications, folder guidance,
CI definitions and an implementation ledger. The live renderer, Push drivers
and web application have not been replaced. Most features below are planned.

| Document | Purpose |
| --- | --- |
| [Codebase survey](codebase-survey.md) | Existing behavior, entry points, coupling and gaps |
| [Branch compatibility](branch-compatibility.md) | Fork feature branches and the unresolved active application pair |
| [Testing](testing.md) | Exact commands, evidence, scope and remaining coverage |
| [Functional runtime](architecture/functional-runtime.md) | Explicit state, composition, concurrency, lifecycle and hot reload |
| [Rendering](architecture/rendering.md) | CPU baseline, packed data, SIMD/GPU feasibility and preview |
| [Benchmark baseline](benchmark-baseline.md) | Measured legacy assignment/packing stage with explicit limits |
| [Controls and web](architecture/control-and-web.md) | Shared operations, Push 2/3, touch UI and editor boundaries |
| [Show authoring](product/show-authoring.md) | Declarative definitions, sequences, envelopes, randomization and auto show |
| [Integrations](product/integrations.md) | Live, audio, agents, fixture sources, simulator and future robotics |
| [Roadmap](roadmap.md) | Dependency order, milestones and independent work lanes |
| [Work ledger](work-items.edn) | Machine-readable implementation state and acceptance criteria |
| [Work log](work-log.md) | Dated evidence and decisions |

The functional runtime and control documents define proposed shared contracts.
Freeze a small contract with tests before parallel feature implementation; do
not let every adapter invent its own cue lifecycle or parameter interpretation.
The prototype is intentionally smaller than the proposed production schema.

All user requirements are mapped to ledger items, including later research.
Performance and visual fidelity targets are hypotheses to measure, not delivery
claims. The robot duck's identity/SDK and Push 3 standalone integration remain
unverified. The Neovim reference was inspected in `tolgraven/tolgrAVen`.

Contributors start with repository and folder `AGENTS.md` files, claim a ledger
item, work within its paths, and update evidence when releasing the claim.
Specifications can be complete while the corresponding feature remains planned.
