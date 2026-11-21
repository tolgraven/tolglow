# Delivery roadmap and parallel work

`work-items.edn` is authoritative for state; this document explains ordering.
Statuses are `:planned`, `:in-progress`, `:blocked`, `:done`. A blocked item records
a concrete blocker. A done item records evidence and has satisfied its acceptance
criteria. Documentation completion does not complete a feature implementation.

Each item has a stable ID, kind, owner, exclusive write paths, prerequisites,
acceptance and evidence. Unclaimed work uses owner `nil`. Claim before editing,
release after recording evidence, and coordinate contract changes across repos.
The validator checks IDs, dependencies, cycles, status/evidence and overlapping
active path claims. `*` and glob ownership are intentionally unsupported: use
exact file or directory paths prefixed with `tolglow/` or `afterglow/`.

## Phase 0 — Evidence and contracts

Delivered: survey, regression tests, test tooling/CI definitions, documentation,
folder guidance and an experimental pure frame evaluator. Next, restore a
reproducible complete application dependency manifest, capture deterministic
legacy render traces and measure the actual hot paths. Freeze command identity,
frame snapshot and contribution schemas before adapter teams build on them.

The current prototype is not a production engine. It omits registry lookup,
commands, lifecycle transitions beyond `:done?`, target blending, output packing,
deadlines, buffering, hardware dispatch and legacy adaptation.

## Phase 1 — Functional engine and shared controls

Implement a coordinator with one state owner and immutable frame snapshots;
define per-instance state, ordered worker merge and all-or-nothing publication.
Build an explicit legacy adapter, then migrate effect families behind parity
tests. Introduce stable registry revisions and transactional hot reload with
show epochs to reject stale instances/results after restart.

In parallel, implement pure semantic control operations and projections. Push,
web, OSC, MIDI, REPL and agents must share launch/end/parameter semantics. Keep
transport decoding, interaction state and device painting separate. Begin with
fake/capture transports and common command conformance tests.

Exit: recorded legacy scenes reproduce within declared tolerances/byte equality,
worker-order variations are deterministic, restart removes stale activity, one
control action has consistent behavior through every initial adapter, and the
legacy live path can still be selected while migrating.

## Phase 2 — Authoring, interfaces and throughput

Build the declarative parameter/effect/cue registry and interpreters, then reusable
macro-slot and step-sequence editors, tween curves and ADSR response modifiers.
Add semantic randomization and an auto-show planner that understands fixture
emission/movement dependencies, one-shot duration and human override.

Rewrite Push 2 against the control layer. Implement Push 3 after protocol and
hardware captures establish its capabilities. Redesign the browser around
responsive cue interaction, parameter editing and streaming state updates, with
long-press actions and accessible alternatives. Validate iPad first, phones too.

Optimize measured CPU bottlenecks with compiled fixture indices, stable plans,
packed primitive buffers and partition ownership. Preserve a CPU oracle. Explore
JVM vector operations/native SIMD and GPU computation only against end-to-end
numbers, including synchronization/readback. No GPU dependency on a web client
should be required to keep the real light show running.

## Phase 3 — Integrated production workflows

Implement shared rig editing and simulator models, progressively better lighting
and persistent fog fields with quality tiers. Add fixture library/edit/import
workflows and registry-derived page creation. Integrate Neovim WASM with validated
effect evaluation, diagnostics, hot reload and clear running revision status.

Add Ableton Live's clip/drum-rack bridge and recording feedback with origin tags,
clock alignment and mode ownership. Prototype parameter audio transport and
analysis independently, retaining tails after cue release. Add server-side
provider configuration, streaming agent chat and a common REPL/tool control API.
Agent-created definitions use the same validation and registry activation path
as human edits; human performance remains concurrent and authoritative.

## Phase 4 — Research and content

Pursue realistic volumetric transport and environment presets beyond the initial
preview, documented fixture-format coverage, phone/LiDAR/Gaussian-splat imports,
and the trainable dancing robot duck. Verify product identity, SDK, permissions
and measurement quality before committing to implementations. Robotics consumes
the sensor/control API rather than adding a privileged show mutation path.

## Independent work lanes

| Lane | Owns | Waits for | Review gate |
| --- | --- | --- | --- |
| Runtime | context/coordinator, lifecycle, legacy adapter | replay fixtures, schema agreement | serial/parallel parity and failure publication |
| Performance | benchmark harness, storage/backends | CPU oracle and immutable snapshot | end-to-end p99, allocations, bytes |
| Control/devices | commands/projections, device adapters | stable command schema | common conformance + device capture |
| Authoring | registry, parameters, sequences, auto policy | lifecycle and command schema | deterministic replay and semantic validation |
| Web/simulator | responsive UI, rig/preview/editor | projections + rig schema | touch/accessibility + device performance |
| Integrations | Live/audio/agents/fixture import | shared commands/registry | echo suppression, latency, provenance |

Use separate files/modules within these lanes, not separate conflicting copies
of shared schemas. The integrator owns dependency pins, root manifests, CI and
the ledger. A lane proposes a contract update first, includes consumers' tests,
then coordinates adoption. Do not add hidden code paths just to avoid review.

The initial agents worked on distinct test trees, architecture documents,
product documents and the isolated runtime kernel. See work-log.md for their
completed contributions; these agents are not background services.
