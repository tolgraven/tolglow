# Delivery roadmap and parallel work

`work-items.edn` is authoritative. `:done` means the named milestone's acceptance
criteria have evidence, not that every related product requirement is finished.
Broad roadmap items stay open when a smaller v1 module is delivered. Active
items need an owner, finished prerequisites and disjoint repository-prefixed
paths. The validator checks dependency cycles, missing evidence and overlapping
active claims; directory claims include all descendants and globs are forbidden.

## Delivered v1 modules

The source survey and genuine characterization lanes led to the `tolmaster`
integration, explicit dependency pins and headless Tolglow compatibility repairs.
The initial default-master baseline is retained as historical evidence. New
modules now implement the following narrower milestones:

| Milestone | Working behavior | Remaining broader gate |
| --- | --- | --- |
| Control model | Typed commands, source identity, bounded duplicate receipts, epochs/revisions, captured definitions, pure bindings/views and codecs | Complete adapter and human-performance parity |
| Runtime | Explicit instance state, deterministic frame reduction, timed release/reload policies, bounded executor | Whole-show recorded replay, complete effect-family migration and production I/O publication |
| Device sessions | Composable Push 2 adapter, injected MIDI/display IO, lifecycle/hold cleanup, generic MIDI/OSC/sensor/REPL sessions | Physical Push 2 checks; native Push 3 protocol/display work |
| Authoring | Validated registry/compiler, tweens/envelopes, macro/sequence primitives, response middleware, seeded constrained random and emission-aware policy | Complete editors, stepped fades, richer seek/catch-up and creative show planning |
| CPU rendering | Ordered attributes, normalized RGBW/dimmer, basic calibrated aiming, patch validation/coarse-fine packing, typed CPU/GLSL expressions | Broader fixture photometry/calibration and end-to-end deadline evidence |
| Integrations | Fixture normalization, Live/audio primitives, streaming provider parsing, loopback HTTP, scripted agent tool dispatch | Actual Live-side bridge, audio routing, external providers and full fixture format coverage |
| Studio | Re-frame/Reagent frontend, Ring/Jetty API and SSE, Neovim WASM authoring, touch/keyboard controls, fixture/rig editors | Physical mobile/device validation, richer editors and production sessions |
| Show files/resources | Complete model/output persistence, staged resource commit/retirement, agent history/credential refs, fixture cache, reactive state, guarded audio/sync intent | Real provider/peer reconnection and complete pairing UX |
| GPU experiment | Actual float shader dispatch, asynchronous PBO/fence readback, CPU oracle comparison | Physical GPU measurements, JVM transport and complete output-pipeline integration |

Exact commands and checkpoint counts are in [testing.md](testing.md). CPU and
software WebGL workloads differ; their timings must not be presented as a GPU
speedup comparison.

## Active studio integration

Musical authoring uses beat/bar/phrase units first, with explicit meter and
phrase anchors. Milliseconds/sample counters belong at engine/audio/device
boundaries. Reusable audio analysis is selected in [audio-analysis.md](audio-analysis.md);
full state and connection metadata must persist through [schema2 show files](show-files.md).

The product stack is **re-frame/Reagent on ClojureScript and Ring on the JVM**.
Use Ring request/response middleware independently of the server adapter; Jetty
currently provides streaming response delivery. Keep authoritative show state
in the shared host. Browser app-db contains projections and interaction state,
not a second cue lifecycle implementation. JS is appropriate for specialized
Neovim WASM and WebGL engines behind owned interop lifecycles.

The re-frame application now has a clean optimized production build and **28
frontend tests / 124 assertions**. Actual restarted-server browser checks passed
nine session and six authoring workflows without console/page errors. They
include real WASM Neovim edits/publish, typed macro slot overrides, pointer and
keyboard rig manipulation, fixture creation/import/patch/removal, and truss
settings reaching WebGL uniforms. These are browser/emulated-device checks,
not physical iPad Safari or hardware throughput claims.

The Ring host now commits model and resource restoration together, publishes
saved output without an extra effect step, rebases logical time, and disposes
stale/pending imports. Reactive watcher actions enter the next frame through the
shared reducer. Old controller sessions close outside the owner lock; audio
capture and sync callbacks are bounded and tied to the active show/resource
identity. Beat/response composition and bounded agent sessions have focused
passing suites; final combined source/manifest counts remain the root's gate.

| Active lane | Exclusive write boundary | Review gate |
| --- | --- | --- |
| Root studio host | Exact host/handler/output/demo/test paths in ledger | Sync clock/record callbacks, routes, restored connection outcomes, final integrated runs |
| Connections UI | Named re-frame state/views and specialized UI files | Sync/audio configuration, selected-clock status, fresh browser interactions |
| Sync boundary | `studio/sync.clj`, `afterglow/control/clock.clj` and tests | Explicit selected source, stale clock behavior, injected MIDI/Beat Link/Live lifecycle |
| Runtime replay | `afterglow/runtime/replay.clj`, tests and replay document | Captured initial state/clock/commands, deterministic serial/worker output verification |
| Push 3 research | New source-grounded profile/codecs only if verified | Official capabilities and captured protocol; no inferred Push 2 compatibility |
| Documentation | Central docs, guidance and retained benchmark evidence | Accurate scoped status, valid ledger and local links |

The ledger contains exact paths and current owners. Completed agents are not
background services; update the ledger when assigning further work. Coordinate
schema changes across owners before editing shared boundaries.

## Remaining functional migration and performance gates

Capture representative whole-frame traces and command histories for RGBW,
movement, fine channels, kind ordering, fades and compound effects. Run the
new engine against these old-source parity oracles. There is no runtime
compatibility layer: old effect closures are never executed through the new engine. Migrate effect
families gradually, preserving stable priority/launch order, private worker
state, snapshot consistency and deterministic merging. Verify failure,
deadline, restart and reload behavior at actual publication boundaries; pure
shape validation cannot prove arbitrary callbacks are side-effect-free.

Retain the CPU oracle while optimizing measured hot paths. Profile allocation,
packing, fixture conversion, scheduler latency and transport separately and
together. Explore JVM vector/native SIMD only with executable comparisons.
GPU assessment must include upload, synchronization/readback, JVM/browser
transfer and packing. The real lighting clock must not depend on an attached
browser, and preview load must not delay real output.

## Remaining authoring and performance workflows

Complete reusable macro slots and step-sequence editing, curved stepped fades,
loop/ping-pong/random order, retriggerable global/selected response envelopes and
one-shot/duration semantics across cue types. Improve semantic randomization
and autonomous scheduling while respecting darkness, emission dependencies and
human overrides. Registry-generated pages must preserve deliberate layouts.

Complete the Push 2 rewrite through capture replay and physical tests. Verify
Push 3 controller capabilities independently; do not infer its display/USB
protocol from Push 2. Validate iPad Safari and phones beyond desktop viewport
emulation: multitouch holds, radial long-press actions with accessible menu
alternatives, scrolling, reconnects, keyboard use and sustained frame latency.

The same scene and fixture model must drive rig editing, runtime targeting and
preview. Extend fixture creation/import/search with explicit format coverage and
provenance. Neovim WASM authoring needs diagnostics, validated revisions and
clear live-reload status. Agent-created definitions use that same activation
path and coexist with human performance.

## Production integrations and research

Finish and verify the implemented Live bridge assets and clip/drum-rack mapping, recording feedback, colors, origin/echo
suppression and Push mode ownership against a real Live-side bridge. Validate
audio pitch/level/residual analysis, effect-chain routing and tails after source
release against actual audio devices. External provider configuration and
streaming chat need live service verification; Codex session embedding remains
a feasibility item, while the shared REPL/control API is already usable.

Realistic shaped fog injection, advection/diffusion/dissipation, fixture optics
and mobile quality tiers remain simulator work beyond the rudimentary preview.
Club environment presets need licensed/provenanced geometry. Phone/LiDAR/splat
capture needs scale and geometry reconstruction. The trainable dancing robot
duck remains later research: confirm its identity/SDK, read timestamped movement
through the sensor boundary, and keep robot control independent of light-frame
deadlines.

## Expanded expressive performance program

The [expressive performance specification](product/expressive-performance.md)
adds beat-random modulation across parameter types and cue/category selections,
category colors/pause/release/opacity, iPad gestures, expressive Push 3 and
Seaboard M mappings, X-Touch/extenders, rich shared device widgets, registered
lighting waveforms with derivative/jagginess/audio signals, a musical Live
patchbay, Link/AbleSet on two laptops, an optional afterglow-max host evaluation
and a sharing website for artifacts/shows/Live arrangements. These requirements
supplement all earlier goals. Typed modulation, waveform/signal work, expressive
protocols and verified X-Touch implementation have separate owners and evidence;
the broader workflows remain planned until their acceptance criteria pass.
