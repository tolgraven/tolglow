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
| Runtime | Explicit state, bounded workers, deterministic replay corpus, private-clock pause, independent AR and opacity | Representative complete-show replay, full effect-family migration and production I/O qualification |
| Device sessions | Composable Push 2 adapter, injected MIDI/display IO, lifecycle/hold cleanup, generic MIDI/OSC/sensor/REPL sessions | Physical Push 2 checks; native Push 3 protocol/display work |
| Authoring | Validated registry/compiler, tweens/envelopes, macro/sequence primitives, response middleware, seeded constrained random and emission-aware policy | Complete editors, stepped fades, richer seek/catch-up and creative show planning |
| CPU rendering | Ordered attributes, normalized RGBW/dimmer, basic calibrated aiming, patch validation/coarse-fine packing, typed CPU/GLSL expressions | Broader fixture photometry/calibration and end-to-end deadline evidence |
| Integrations | Fixture library, sync/session boundary, Live bridge source assets, typed patch kernel, provider sessions | Actual Max/Live execution, audio/peer qualification, external providers and broader fixture formats |
| Studio | Re-frame/Reagent frontend, Ring/Jetty API and SSE, Neovim WASM authoring, touch/keyboard controls, fixture/rig editors | Physical mobile/device validation, richer editors and production sessions |
| Show files/resources | Complete model/output persistence, staged resource commit/retirement, agent history/credential refs, fixture cache, reactive state, guarded audio/sync intent | Real provider/peer reconnection and complete pairing UX |
| GPU rendering | Afterglow’s headless JVM and WebGL moving-head shader, explicit pan history, mixed targets and directions, and named GLSL fields for color, aim or focus | Physical GPU throughput, full-show deadline measurements and fixture verification |

Exact commands and checkpoint counts are in [testing.md](testing.md). CPU and
software WebGL workloads differ; their timings must not be presented as a GPU
speedup comparison.

## Mixed human and automatic performance

The energy autopilot can conduct the whole rig, a group, a cue page or an explicit set of sources. Brightness measures delivered light against calibrated capacity. Visual activity measures changes in the visible look. Both targets can follow audio energy, cue parameters and musical signals. The controller samples delivered output each advancing frame and makes decisions on the configured musical interval. [Operator guide](energy-autopilot.md)

| Milestone | Implemented and exercised |
| --- | --- |
| Measure and chase targets | Duration-weighted rolling means, volatility and time in range. Targets can rise or fall over a musical interval or by a fraction per frame. Capture tests cover dimming, standby, restored control and hard output limits. [Response settings](output-response.md) |
| Mix with a performer | Stepwise or immediate correction, optional source rotation, reversible gain, scoped cue selection and transient look generation. Tests cover manual takeover, an explicitly shut fixture and stale author replies. [Action preferences](energy-autopilot.md#start-with-a-two-bar-look) |
| Overdrive and spatial highlights | Afterglow carries excess emission through native composition. Independent controls whiten the source and nearby receivers according to distance. A real OpenGL capture matched the CPU packet and isolated an invalid focus value. [Highlight controls](energy-autopilot.md#overdrive-and-whiten-highlights) |

The first full conductor benchmark on a shared host exceeded the 25 ms frame budget on all 160 measured ticks with 2,048 RGBW pixels and 64 moving heads. Median wall time was 41.77 ms on ordinary ticks and 59.43 ms on decision ticks. Repeat the same workload to assess changes. Physical lamp measurements, sustained performance on the venue computer, iPad Safari and a live external authoring provider remain field checks. [Profiling the lighting controls](performance/lighting-controls.md)

See [spatial cue actions](spatial-show.md) for moving fixture targets, HDR contact actions and demand-driven signal sampling.

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

The re-frame application has a clean optimized production build. The latest
combined frontend source checkpoint passed **100 tests / 538 assertions**; the
last reported release before subsequent modules compiled 142 files with zero
warnings. Actual browser gates include nine session and six authoring workflows,
six connection configuration flows, six modulation flows, nine actual REPL,
four patchbay and three waveform flows, with no console errors in those runs. Physical iPad Safari and newer workspace modules remain
separate gates; do not infer their verification from desktop emulation.

The Ring host now commits model and resource restoration together, publishes
saved output without an extra effect step, rebases logical time, and disposes
stale/pending imports. Reactive watcher actions enter the next frame through the
shared reducer. Old controller sessions close outside the owner lock; audio
capture and sync callbacks are bounded and tied to the active show/resource
identity. Beat/response composition and bounded agent sessions have focused
passing suites; final combined source/manifest counts remain the root's gate.

| Active lane | Exclusive write boundary | Review gate |
| --- | --- | --- |
| Root studio integration | Host, Ring handlers and Afterglow output scheduling | Parallel effect workers, deterministic model commits and independent serial physical output |
| `room_renderer_fog_fixture_optics` | Entire Three scene, fixture optics and fog pipeline | Room-wide volume, finite lens aperture, depth/shadows and actual visual/mobile review |
| `studio_push_touch_interface` | Re-frame Studio/Push widgets and browser checks | Live waveform parity, captured audio, compact concurrent effects and sensor/touch flows |
| `audio_capture_waveform_projections` | Read-only waveform projection, PCM summaries and central tracking | Actual effective parameters and measured bounded audio, off the lighting deadline |
| `application_agents_isolated_code_changes` | Fresh branch agent service and its tests | Bounded CLI streaming, exact diffs/checks, explicit reload policy |
| `development_terminal_reliability_feature_parity` | Docker lifecycle, workspace recovery and control parity audit | Delayed-create cleanup, durable catalog and identified original feature gaps |
| `dependency_upgrades_clojure_jvm_frontend` | Verified dependency pins and migration changes | Current stable Clojure/JVM/frontend versions, paired tests and production build |

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

The [Ableton Link adapter and pause lighting](ableton-link.md) support shared playback, tempo and beat phase through Carabiner. The remaining hardware gate exercises cross-computer Live playback, tempo changes and reconnects.

Finish and verify the implemented Live bridge assets and clip/drum-rack mapping, recording feedback, colors, origin/echo
suppression and Push mode ownership against a real Live-side bridge. Validate
audio pitch/level/residual analysis, effect-chain routing and tails after source
release against actual audio devices. External provider configuration and
streaming chat need live service verification; Codex session embedding remains
a feasibility item, while the shared REPL/control API is already usable.

The Studio preview renders optically sized beams, local fog plumes, diffuse room haze and reflected light. Fogger output, elapsed physical time, room volume and measured clearance determine the atmosphere. A stationary iPhone scan can fit the room and fluid from contrast loss across a measured sightline. A metered scan can also fit nozzle mixing and optical extinction. Calibration and mobile frame rates still need comparison in a real venue. See [venue fog and calibration](venue-fog.md) and [the captured room preview](studio-atmosphere.md#reading-the-room-preview).

The lighting-capacity meter and per-group scenes operate on the patched rig. A music-energy crossing can request a different look at a chosen percentage on a fixture group. The show chooses the sources and parameter values and reports the resulting measured output. Fog has its own calibrated setpoint and band. See [group lighting scenes](group-lighting-scenes.md) and [lighting capacity](lighting-capacity.md).

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

## Integrated development workspace

The [development-workspace specification](product/development-workspace.md) adds
a Quake-style Bruvbox REPL, completions/documentation, repository-wide Neovim and
scratch-form evaluation with explicit captured locals. Show-authoring agents and
branch-isolated Codex/Claude application work use separate contexts, with actual
diffs, watcher summaries and task status. Manual/after-turn/preview-instance
reload policies must cover each supported target and preserve a last-good revert.
A real Docker/zsh terminal is an independent lifecycle milestone. Narrow REPL
backend tests do not complete these browser/workspace workflows.

The completed pure [output policy](output-presentation.md) retains original compute
timestamps, rejects stale frames and offers direct, emission fade and one-buffer
interpolation modes. Its measured costs do not establish scheduler/network deadlines.
[Sensors](sensors.md) documents the implemented context middleware, owned OSC input
and supported browser/Apple APIs; native source remains pending Apple build/hardware.
