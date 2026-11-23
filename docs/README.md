# Tolglow / Afterglow architecture program

Use the [repository map](../README.md#repository-map) to find the Clojure show
runtime, re-frame Studio, JavaScript renderer, packaged device scripts, tests
and native companions.

The canonical `project.clj` owns dependencies and Leiningen build profiles.

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
| [Measure room fog with iPhone](iphone-fog-calibration.md) | Capture clear air, fog delivery and clearance using a fixed contrast target |
| [Relative fixture brightness](relative-fixture-brightness.md) | Measure full-white output with a locked camera and save per-type calibration |
| [Venue fog](venue-fog.md) | Rated fogger output, elapsed DMX time, mixing and venue ventilation |
| [Starting a show](application-startup.md) | Config module order, headless and Studio entry points, REPL and shutdown |
| [Fog machine and venue controls](studio-atmosphere.md) | Configure rated fog output, DMX response, nozzle direction, ventilation and decay |
| [Spatial cue actions](spatial-show.md) | Select live fixture targets, observe volume contacts and run owned cues |
| [Headless show modules](headless-modules.md) | Use the show APIs from code and controllers |
| [Feature parity inventory](feature-parity-inventory.md) | Source-linked operator and authoring behaviors from Afterglow, Tolglow effects and tolglow-web, with acceptance cases |
| [Studio Debug](studio-debug.md) | Live event and DMX inspection, subscriber editing and musical action sequences |
| [Event subscribers and musical actions](event-subscribers.md) | Filter registered cues, launch effects on audio or transport events, sequence musical delays and save custom callbacks |
| [Event and DMX inspection](studio-event-stream.md) | Committed typed events, actual handler delivery and raw channel data for a selected universe |
| [Studio state stream](state-stream.md) | Typed show snapshots, acknowledged deltas, backpressure and reconnect |
| [Codebase survey](codebase-survey.md) | Historical entry points, behavior and coupling |
| [Branch compatibility](branch-compatibility.md) | Original fork audit and current merged source baseline |
| [Compatibility repairs](compatibility-progress.md) | Actual namespace and legacy API repairs |
| [V1 implementation contract](implementation-contract-v1.md) | Shared model/command/runtime boundaries |
| [Paired repository review, November 2026](reviews/2026-11-22.md) | Review history, security boundaries, source findings and compositor timings |
| [Testing](testing.md) | Commands, observed checkpoints and verification limits |
| [Functional runtime](architecture/functional-runtime.md) | State, concurrency, lifecycle, reload and migration |
| [Effect error recovery](effect-fault-isolation.md) | Per-effect isolation, last-good reloads, Retry, reactive events and asynchronous logs |
| [Musical effect keyframes](effect-keyframes.md) | Project effect output, shape transitions and inspect a head while a cue runs |
| [Rendering implementation](rendering-progress.md) | Working typed/batch pipeline, projection and measured limits |
| [Rendering](architecture/rendering.md) | CPU rendering, Afterglow GPU ownership and remaining acceleration work |
| [Benchmark evidence](benchmarks/README.md) | Retained CPU and software WebGL reports with provenance |
| [Original stage baseline](benchmark-baseline.md) | Legacy assignment/packing measurement and exclusions |
| [GPU experiment](gpu-experiment.md) | Executed WebGL2 shaders/readback, tolerances and limits |
| [Controls and web](architecture/control-and-web.md) | Shared semantics, devices, touch UI and editors |
| [Preview navigation](preview-navigation.md) | WASD, drag look, touch movement and active-fixture camera tours |
| [Push venue display](push-display-packaging.md#venue-and-rig-views) | Full-rig framing, cylindrical panorama and fixture aim on the 960 × 160 display |
| [Output deadlines](output-presentation.md) | Immutable completed frames, fixture-aware fade/interpolation and measured policy costs |
| [Phone and Watch sensors](sensors.md) | Maintained browser library, supported Apple APIs, pure middleware and owned OSC service |
| [Web REPL](web-repl.md) | Actual JVM sessions, streaming completion and captured locals |
| [Isolated workspaces](dev-workspaces.md) | Real paired Git worktrees, confined file editing and checks |
| [Docker terminal](terminal.md) | Actual PTY/container lifecycle and tested limits |
| [Restart ownership](show-restart.md) | Execution reset and in-place input callback renewal |
| [Scene contract](architecture/preview-scene-contract.md) | Shared fixture/rig optics and active visual acceptance |
| [Live audio sends and returns](live-audio-returns.md) | Single-track and Drum Rack routing, processed return cues, patch sources and retained echo voices |
| [3D rig editor](rig-editor.md) | Draft fixture/truss placement, direct musical aim paths, explicit cue creation and saved-path editing |
| [Energy autopilot](energy-autopilot.md) | Steer brightness and visual activity over musical time, admit and retire cues, and overdrive or whiten nearby fixtures |
| [Group lighting scenes](group-lighting-scenes.md) | Match unequal fixtures to a common brightness and cap the number and wattage of active sources |
| [Output response](output-response.md) | Set attack and release for fixture light and for the conductor’s changing targets |
| [Lighting capacity](lighting-capacity.md) | Rig-wide output metering, audio-energy limits and selected fog pulses |
| [Audio analysis research](audio-analysis.md) | Verified reusable libraries, live/offline distinctions and implementation choice |
| [Show files](show-files.md) | Full schema2 model, committed output and validated restoration |
| [Reactive REPL](reactive.md) | Durable subscriptions, ratoms, watchers and queued actions |
| [Fixture library](fixture-library.md) | Verified format coverage, search, validation and rig workflow |
| [RDM fixtures](rdm.md) | Explicit OLA discovery, device configuration, readback and show associations |
| [Aim paths](aim-paths.md) | Musical world-space path effects, guarded cue publication and persistence |
| [Studio UI](studio-ui.md) | Frontend implementation and browser verification |
| [Expressive performance](product/expressive-performance.md) | Modulation/categories, MPE/controllers, lighting synth, musical patchbay and sharing |
| [System controls](system-controls.md) | Instance/category pause, opacity and independent musical AR |
| [Modulation](modulation.md) | Seeded typed modulation, targeting and holds |
| [Compound cues and native layers](native-composition.md) | Ordered color, aim and musical transforms, reusable modifier effects and layer opacity |
| [Effect recipes](effect-recipes.md) | Declarative musical/spatial construction helpers |
| [Shared variable chains](global-variable-blocks.md) | Authoring and tracing global material chains in Studio, the REPL and Ring |
| [Effect building blocks](effect-blocks.md) | Shader materials, parameter and variable bindings, stage inspection |
| [Registered signals](signals.md) | Waveform versions, selectors and derivative units |
| [Push page overview](push-page-overview.md) | Browse cue layouts, authored swatches and type colors |
| [Live waveform projections](waveform-projections.md) | Current signal parameters, captured PCM history and bounded Push data |
| [Dependency upgrades](dependency-upgrades.md) | Registry-verified versions, migrations and updated build/test evidence |
| [X-Touch](xtouch.md) | Verified MCU/extender protocol, adapter and color research |
| [Sync](sync-progress.md) | Selected clock and explicit MIDI, Pro DJ Link and Live bridge sessions |
| [Ableton Link and paused lighting](ableton-link.md) | Follow transport, match tempo and phase, set a pause look and restore the pairing |
| [Runtime replay](runtime-replay.md) | Checked-in command/frame recording and deterministic output gate |
| [Live routing](live-routing.md) | Typed musical patch graph, source research and host limits |
| [Development workspace](product/development-workspace.md) | Popup REPL, source/scratch editing, agent branches, reload and Docker terminal |
| [Show authoring](product/show-authoring.md) | Definitions, sequences, envelopes, randomization and auto show |
| [Integrations](product/integrations.md) | Live, audio, agents, fixture sources, simulator and robotics |
| [Roadmap](roadmap.md) | Completed v1 milestones and remaining dependency order |
| [Work ledger](work-items.edn) | Authoritative state, exclusive paths and acceptance criteria |
| [Work log](work-log.md) | Dated evidence and decisions |

Tests run actual production functions and dependencies, using capture sinks and
injected transports at external boundaries. A passed narrow milestone does not
complete its broader roadmap item. Source-derived Push 2 tests are not physical
device verification. Push 3 has an explicitly captured User Mode subset and standard MPE profiles;
native display, standalone and physical verification remain separate. A Live-side bridge asset package and sync resource boundary are implemented,
while execution inside Max/Live, real provider accounts, physical audio routing
and AAA simulator fidelity remain separate work. The trainable robot duck is retained as later research;
its exact product and SDK are still unconfirmed.

Contributors read repository and folder `AGENTS.md`, claim disjoint paths in the
ledger, and coordinate changes to shared contracts before editing consumers.
Record evidence before releasing a claim; keep implemented, proposed and
research behavior distinct. [Testing](testing.md) separates focused agent results
from integrated runs so overlapping suites are never added twice.
