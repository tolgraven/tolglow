# Tolglow / Afterglow architecture program

The working tree now contains executable v1 modules: an explicit-state runtime,
shared commands and device sessions, declarative authoring and composition,
CPU fixture rendering, integration primitives, and a web studio. The existing
Afterglow `tolmaster` work has been merged and Tolglow's headless application
compatibility repaired. Old source is retained only as a feature/functional parity reference. The new
runtime has no compatibility layer or legacy effect execution bridge; complete
production workflows remain in progress.

The working studio frontend uses **ClojureScript, re-frame and Reagent**; the
backend uses **Ring with Jetty**. Its optimized build and actual browser flows
now cover cue/parameter performance, multitouch holds, macro/sequence authoring,
fixture search/import/patching, rig editing, agent chat/history and full-show
save/load. Neovim WASM and WebGL remain specialized engines behind explicit
interop. Physical iPad Safari, real controller/Live/audio hardware and sustained
production-show behavior remain separate acceptance gates.

Schema2 files include running effect state, captured versions, musical timers,
composition/automation ownership, committed output, settings, agent history and
credential references, fixture cache, and durable reactive state. Resource
restoration stages validation before committing model/ownership together;
cleanup and connection resumption occur outside the frame-owner lock.

| Document | Purpose |
| --- | --- |
| [Codebase survey](codebase-survey.md) | Historical entry points, behavior and coupling |
| [Branch compatibility](branch-compatibility.md) | Original fork audit and current merged source baseline |
| [Compatibility repairs](compatibility-progress.md) | Actual namespace and legacy API repairs |
| [V1 implementation contract](implementation-contract-v1.md) | Shared model/command/runtime boundaries |
| [Testing](testing.md) | Commands, observed checkpoints and verification limits |
| [Functional runtime](architecture/functional-runtime.md) | State, concurrency, lifecycle, reload and migration |
| [Rendering implementation](rendering-progress.md) | Working typed/batch pipeline, projection and measured limits |
| [Rendering](architecture/rendering.md) | CPU backend, typed expressions and remaining acceleration work |
| [Benchmark evidence](benchmarks/README.md) | Retained CPU and software WebGL reports with provenance |
| [Original stage baseline](benchmark-baseline.md) | Legacy assignment/packing measurement and exclusions |
| [GPU experiment](gpu-experiment.md) | Executed WebGL2 shaders/readback, tolerances and limits |
| [Controls and web](architecture/control-and-web.md) | Shared semantics, devices, touch UI and editors |
| [Audio analysis research](audio-analysis.md) | Verified reusable libraries, live/offline distinctions and implementation choice |
| [Show files](show-files.md) | Full schema2 model, committed output and validated restoration |
| [Reactive REPL](reactive.md) | Durable subscriptions, ratoms, watchers and queued actions |
| [Fixture library](fixture-library.md) | Verified format coverage, search, validation and rig workflow |
| [Studio UI](studio-ui.md) | Frontend implementation and browser verification |
| [Expressive performance](product/expressive-performance.md) | Modulation/categories, MPE/controllers, lighting synth, musical patchbay and sharing |
| [Show authoring](product/show-authoring.md) | Definitions, sequences, envelopes, randomization and auto show |
| [Integrations](product/integrations.md) | Live, audio, agents, fixture sources, simulator and robotics |
| [Roadmap](roadmap.md) | Completed v1 milestones and remaining dependency order |
| [GitHub Issues](https://github.com/tolgraven/tolglow/issues) | Current work, owners, acceptance criteria and evidence |
| [Work log](work-log.md) | Dated evidence and decisions |

Tests run actual production functions and dependencies, using capture sinks and
injected transports at external boundaries. A passed narrow milestone does not
complete its broader roadmap item. Source-derived Push 2 tests are not physical
device verification. Push 3 currently has only explicitly configured generic
MIDI bindings; native display and standalone support are unverified. A Live-side bridge asset package and sync resource boundary are implemented,
while execution inside Max/Live, real provider accounts, physical audio routing
and AAA simulator fidelity remain separate work. The trainable robot duck is retained as later research;
its exact product and SDK are still unconfirmed.

Contributors read repository and folder `AGENTS.md`, identify their scope in the corresponding issue, and coordinate changes to shared contracts before editing consumers.
Record evidence before releasing a claim; keep implemented, proposed and
research behavior distinct. [Testing](testing.md) separates focused agent results
from integrated runs so overlapping suites are never added twice.
