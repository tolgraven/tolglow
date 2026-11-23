# Shared controls, device adapters, and web workspace

This document combines the original source audit with the target architecture.
The shared command model, pure bindings/views/codecs, Push 2/generic sessions and
re-frame/Reagent/Ring studio are now implemented v1 modules; their observed scope
is in [studio UI](../studio-ui.md), [show files](../show-files.md) and
[testing](../testing.md). The [tethered Push 3 screen](../push-display-packaging.md#tethered-push-3) has an explicit
USB transport and native-size recording tests. Standalone operation and the broader
performance workflows remain open. Expanded MPE/Seaboard M, X-Touch/extenders,
automapping, category widgets, offscreen CSS/webview rendering and Live patchbay
requirements are specified in [expressive performance](../product/expressive-performance.md).

Runtime contracts are in [functional-runtime](functional-runtime.md), rendering
in [rendering](rendering.md), authoring in [show authoring](../product/show-authoring.md),
and external integrations in [integrations](../product/integrations.md). New code
uses the shared pure model and owned IO boundaries; there is no old-effect
execution compatibility layer.

## Original source audit

The following table describes the inspected historical Afterglow revision
`16b0b4cac70433e4118038dfbfd304e22d1d9018` and Tolglow
`01146ece7731ec06aa1ee6409342373096c8e0dc`, not the current implementation status.

| Source | Current behavior | Consequence for migration |
| --- | --- | --- |
| `afterglow/src/afterglow/controllers.clj` | Shared `IGridController` and `IOverlay` protocols, cue-grid helpers, velocity mappings, recognizers, controller lifecycle and scheduling | Use preserved behavior as a parity oracle while separating domain actions from transport and presentation. There are existing abstractions to build on. |
| `afterglow/src/afterglow/controllers/ableton_push_2.clj` | 3,029 lines combine MIDI/SysEx palette handling, Wayang/native display frames, mutable UI buffers, cue launching, parameter editing, overlays and device lifecycle | Replace with a new adapter over common controls; do not reproduce this structure for Push 3. |
| `afterglow/src/afterglow/web/routes/show_control.clj` | 974 lines include direct show mutation, per-page client state in an atom, cue views, macro creation and update diffs | Use as a feature oracle for new Ring routes and the shared command path; do not add a runtime compatibility layer. |
| `afterglow/resources/public/js/show_updates.js` | jQuery/Bootstrap, polling 50 ms after successful responses, mouse-down/up cue interaction, context menu with delete | Mobile input, local editing state, reconnect behavior and rendering cadence need explicit redesign. |
| `afterglow/src/afterglow/web/routes/visualizer.clj` | Four-light cap, movement-history-derived color/aim, comments noting omitted dimmer behavior | A new simulator must consume actual resolved output and a shared rig model; the current view is not a photometric reference. |
| `afterglow/src/afterglow/web/routes/web_repl.clj` | Session-bound Clojure evaluation via `do-eval` | Existing REPL functionality is an administrative execution surface, not a declarative command schema. Keep the distinction explicit. |
| `tolglow/src/tolglow/osc.clj` | Direct show changes and callback registration; `receiver` currently prints and calls its function with `1 2`, while the actual handler registration is reader-discarded | Characterize and repair this separately; do not describe OSC receiving as a verified working generic adapter. |
| `tolglow/src/tolglow/sequencer.clj` | `secue` and `secudfx` are empty experimental functions | The proposed editor and sequence execution model are new work. |
| `tolglow/src/tolglow/live.clj` | Experiments in live effect code | This is not Ableton Live integration. |

## Ownership and public boundaries

Afterglow owns device-independent show commands, authoritative show state, effect instances, transport time and output. Tolglow owns authored show content, effect/cue registries and higher-level workflows. The web application and every device adapter consume the same semantic controls.

Boundary responsibilities below are architectural. The implemented names and exact callable contracts are recorded in the [v1 contract](../implementation-contract-v1.md); future namespaces in this table are not availability claims:

| Boundary | Inputs and outputs | Must not own |
| --- | --- | --- |
| `afterglow.control.commands` | Serializable command validation and normalization | MIDI bytes, Ring requests, renderer workers |
| `afterglow.control.model` | Pure `(model, command) -> {model, events}` | Network sends, clocks read during reduction, hidden show bindings |
| `afterglow.control.views` | Pure projection of a committed model plus explicit presentation time | Device handles or mutable fixture state |
| `afterglow.control.bindings` | Composable event transforms, scaling, relative encoders, pickup, mapping | Cue semantics duplicated per adapter |
| Controller-specific codecs | Raw messages to normalized input; view diffs to output messages | Global show mutation |
| Controller session shell | Ports, threads, bounded queues, display buffers, disconnect cleanup | Business rules embedded in callbacks |
| Web transport shell | Authentication, schema decoding, snapshots/deltas and acknowledgements | Separate definitions of launch/hold/end/parameter behavior |
| Tolglow authoring services | Validated registry, cue/page/sequence and rig transactions | Native device protocol knowledge |

No universal physical control hierarchy is required. A browser slider, an encoder and a sensor can map to the same `:param/set` command while retaining different input mechanics. Share semantics and small functions, not a framework requiring every device to resemble an 8×8 grid.

## Commands and ordered reduction

The following EDN is a contract sketch. Final names must be reconciled with the functional runtime before production callers depend on them.

```clojure
{:schema-version 1
 :command-id "session-8:1042"
 :show-id :main
 :epoch 7
 :source {:adapter :push-2 :session-id "session-8"}
 :op :cue/launch
 :cue-id :red-bloom
 :launch {:mode :one-shot :velocity 0.72}
 :gesture-id "pad-42-press-16"}
```

The initial `afterglow.runtime/render-frame` kernel remains a smaller pure API.
The implemented `afterglow.control.model/apply-command` and
`afterglow.runtime.engine/advance` now provide command admission, bounded
receipts, epochs, captured definitions and instance lifecycle. The studio owns
ordered ingress, frame publication, staged resource restoration and output.
Consult the executable v1 contract rather than treating the initial kernel as
the full current runtime.

The shell authenticates the source, validates bounds, deduplicates the command ID and assigns the authoritative sequence. Device clocks and arrival timestamps may be retained as metadata, but cannot determine total command order. Reduction produces a new immutable model and domain events; the runtime commits a frame from one captured model. Publishing events, MIDI feedback and persistence occur outside the reducer.

The shell returns an acknowledgement containing command ID, accepted/rejected status, show epoch, sequence and resulting revision. Rejection carries a stable code and actionable data such as the current cue version. Replayed acknowledgements must not relaunch a cue. Deduplication has a bounded retention window and explicit session expiry; clients must not retry commands indefinitely after that window.

Commands include:

- Cue launch, request release/end, force end, and explicit parameter set/reset.
- Gesture begin/update/end, with releases associated with the instance started by that gesture.
- Page navigation and optional viewport linking, scoped to a session or link group.
- Cue create/update/copy/move/delete, saved parameter snapshots and macro/sequence edits.
- Transport play/stop, tempo and synchronization requests; global master and blackout.
- Registry publish/reload, rig edit/patch and authoring transaction commit.

A cue ID identifies authored content, an effect definition ID/version identifies its implementation, and an instance ID identifies one launch. An old note-off must not stop a newer instance of the same cue. A page coordinate is a placement, not durable identity. Commands from an earlier show epoch must fail visibly after restart; a restarted browser requests a fresh snapshot rather than replaying a backlog of lighting gestures.

Concurrent edits use expected object revisions. Reject stale cue moves, pastes into occupied cells and incompatible parameter edits with the current revision available for reconciliation. Support explicit overwrite as a separate user action. Do not silently convert a failed copy into a launch or erase another client's edit.

For high-frequency input, coalesce only explicitly replaceable absolute values for the same parameter and gesture within a queue boundary. Never coalesce launches, releases, relative encoder deltas, transport changes or authoring transactions. Preserve ordering across the coalesced value and its gesture end. Bound per-source ingress so an OSC stream cannot starve a physical stop command. Queue policies and rejection metrics are part of the API, not an implementation accident.

## Composable controls

Keep transformations small and independently testable:

```clojure
;; Conceptual composition, not current API.
(def brightness-control
  (comp (emit-param :brightness)
        (quantize-to-spec brightness-spec)
        (clamp-to-spec brightness-spec)
        (scale-input [0 127] [0.0 1.0])
        midi-value))
```

Transforms receive explicit state where needed. Relative encoder accumulation, pickup/soft takeover, smoothing and press duration return updated control state with emitted commands. They must not close over atoms or read wall-clock time. Parameter metadata supplies units, finite numeric bounds, resolution, curve, default and whether randomization is meaningful. Color, vector and enum inputs retain their type rather than masquerading as scalar knobs.

Separate four kinds of state:

1. Authoritative show state: cue definitions, active instances, parameter values and transport.
2. Shared session state: selected page/instance and linked viewport, only when intentionally shared.
3. Local presentation state: open menus, focus, draft text, touch capture and temporary selections.
4. Adapter resources: MIDI ports, USB/native display handles, timers and bounded transport buffers.

An overlay becomes explicit local control state and a projection, with composable input routing. Priority and consumption are data. The transport shell owns mutable resource handles; using atoms there is appropriate. The requirement for functional effects does not imply allocating persistent vectors for every native display pixel.

## Push 2 replacement and Push 3

Begin with recorded device messages and current pure helper behavior. Preserve palette restoration, startup negotiation, identity matching, relative encoder signs, hold/release semantics, user-mode handoff and disconnect cleanup. Preserve explicit display/LED rate limiting and acknowledgements where the existing protocol needs them.

Split the implementation into:

- A capability description: pad geometry, pressure/velocity modes, encoder modes, display geometry/transport, LED formats and supported buttons.
- A codec with no show access: parse messages, validate lengths/ranges, encode exact bytes.
- A pure interaction model: selection, overlays, paging and gesture state.
- A shared view projection: cue colors/states, labels, parameter gauges, transport indicators.
- Device layout/drawing functions and an output diff planner.
- A lifecycle shell which opens resources, negotiates, runs bounded I/O and releases everything on shutdown or disconnection.

Render from immutable snapshots. Parallel display preparation is allowed only with owned buffers and frame IDs. Discard stale prepared frames; never let a late palette acknowledgement commit a previous session's state. Avoid a global singleton transfer callback when multiple devices can connect.

Push 3 is a separate profile and adapter. Determine the exact unit's modes, firmware, USB/MIDI ports, identity replies, display transport, pad feedback and pressure semantics from specifications and captured hardware traces. Do not infer Push 2 SysEx or Wayang compatibility from appearance. Host-connected and standalone operation are separate compatibility questions; mark each as supported, unsupported or unverified. A device emulator validates protocol logic but cannot certify real hardware.

Live and lighting modes share an explicit ownership state machine: active owner, pending handoff, release/restore, acquisition and failure. Flush held gestures before handoff and restore ownership-specific palette/display state. A reconnect must not cause both applications to claim a port. [Integrations](../product/integrations.md) defines clip/rack synchronization; this layer owns the physical handoff.

## Web workspace and mobile interaction

The web UI should have a stable performance surface: cue grid, transport/master, active effects and a selected-item inspector. Desktop and landscape iPad can show these together. Portrait iPad and phones use a full-size grid with an inspector sheet and persistent compact transport. Keep tap targets comfortably touchable (target at least 44 CSS pixels), account for safe-area insets and virtual keyboards, and provide labels/states independently of color.

Use Pointer Events and individual pointer IDs. Multiple fingers may hold different cues or move different parameters. Pointer cancellation, lost capture, visibility changes and session disconnection release the gestures owned by that session according to a defined server lease policy. A browser's `pagehide` send is best effort and cannot be the sole release guarantee. Distinguish latching cues, held cues and explicit one-shots in the interaction model.

Long-press opens a radial menu with launch mode, inspect/edit, add to macro, add to sequence, copy, move and delete when applicable; empty cells offer paste/create. Clamp the menu inside the visible viewport, account for the finger covering its center, and provide an equivalent ordinary action list for keyboard and assistive technology. A visible actions button makes these operations discoverable.

There is an interaction conflict between immediate held-cue launch and long-press actions. Resolve it explicitly: in performance mode pointer-down launches held cues immediately and the action affordance opens editing; in edit mode long-press opens actions without launching. Latching-cue short taps can commit on release, with long-press suppressing that launch. Do not add an invisible delay to every performance press. Show the current mode and test crossing the long-press threshold, dragging away and cancellation.

Macros and sequences share a reusable ordered-slot editor with a domain-configured mode:

| Mode | Slot meaning | Controls |
| --- | --- | --- |
| Macro/scene | Chosen effects launched together with parameter snapshots/overrides | Add/remove/reorder/duplicate, inspect parameters; no implied clock |
| Sequence | Timed or beat-relative step with cue/effect references | Duration, transition curve, gate, loop/ping-pong/random policy, playback position |

A slot has a stable ID separate from its index. Dragging and touch reordering produce one transaction on drop; keyboard move buttons offer the same action. Running playback reads a committed revision. Edits can be applied at a documented next-step boundary without corrupting the current step; retain the old revision if validation fails. The playback engine belongs to the runtime, not a browser timer. Detailed fade, randomization, envelope and auto-mode semantics live in [show authoring](../product/show-authoring.md).

## Browser state and transport

Use immutable client state and pure reducers; Reagent/re-frame is a natural fit with the requested Clojure direction, but choose the concrete client build after a small prototype verifies touch input, stream handling and bundle/startup costs. Retain server-side Clojure validation regardless of client framework.

The server publishes a versioned initial snapshot followed by ordered deltas and command acknowledgements. Include show epoch, from/to revision and schema version. A revision gap or epoch change triggers resynchronization; the client must not guess how to merge an incomplete stream. Reconnect restores committed selections and drafts deliberately, and never silently replays cue launches. Presentation may optimistically display a pending gesture, but confirmed state comes from the server.

WebSocket supports duplex control; SSE plus HTTP commands is also viable. Make transport replaceable and measure before choosing. Preserve the legacy routes through a translation adapter during migration. Feedback must reach the originating controller too, with correlation IDs preventing feedback loops rather than suppressing all self-origin updates.

Batch UI changes at animation frames and subscribe only to visible/needed projections. A 60 Hz screen need not receive or rerender every lighting output frame. Put heavy preview work in an owned render loop and worker where supported. Stop or throttle hidden preview rendering while maintaining explicit connection/gesture cleanup. Slow clients get a bounded latest snapshot, not an ever-growing list of intermediate states. Parameter text inputs remain usable while background values update; display editing/pickup conflicts instead of moving the caret.

Set measurable budgets during implementation: input acknowledgement latency p50/p95/p99, frame time on specified iPad/phone models, dropped animation frames, startup bytes/time, reconnect time and retained memory after repeated navigation. Measure with an active large rig and preview, not just an empty grid. The lighting renderer's deadline takes precedence over visual preview quality.

## Rig, fixture authoring and simulator

Use one versioned rig document for patching, setup editing and preview. It contains stable fixture/head IDs, fixture definition/version, world position/orientation, parenting to trusses, universe/channel allocation, calibration and optical metadata. Define units and coordinate conventions once. World transforms for output must agree with the graphical transform gizmos; a camera coordinate conversion is a renderer concern.

Rig editing supports fixture/truss library placement, numeric and graphical transforms, grouping, snapping, duplication and undo/redo. Draft changes preview locally; a validated rig transaction checks address overlap, universe limits, missing modes and unsupported channels before commit. Expensive compilation occurs off the render thread, with an atomic rig revision switch. A moved or deleted fixture must invalidate relevant geometry caches.

Fixture type editing and import normalize into the same declarative schema. Show source attribution, unsupported capabilities and conversion diagnostics. Search/download results are candidate definitions until parsed and validated. Never execute imported definition text as Clojure. [Integrations](../product/integrations.md) specifies import formats, library search and licensing/provenance requirements.

The simulator consumes resolved intensity, RGBW/color channels, shutter, aim, focus, zoom/gobo and fixture calibration, not only high-level effect intent. The preview also owns room geometry, material response, fog emitters, spatial fog density, advection/diffusion and quality settings. Empty air should not acquire bright beams merely because a fixture points through it; fog supplies the scattering volume. Progressive desktop/mobile quality levels, prefab environments and scan/splat ingestion are product work, not assumptions of photorealism. Keep the output reference renderer independent of preview GPU success. [Rendering](rendering.md) owns the physical model, output-buffer contracts and performance experiments.

## Agent and code authoring workspace

The agent uses the same commands and registry services as a human controller. Expose structured show/fixture/cue/parameter snapshots, dry-run validation, apply/revert transactions, run-once/duration launch and event subscriptions. The REPL provides a documented facade over these operations so Codex can inspect and drive shows with ordinary Clojure data. Keep trusted code evaluation separate from the declarative operation vocabulary.

A chat panel streams text and tool activity without blocking the cue grid. Display the concrete effect/cue/rig edits, validation result, execution status and affected running instances. Keep conversation identity distinct from show epoch; reconnect and cancellation must not duplicate a tool action. Agent proposals and manual gestures can coexist through the same revision/conflict rules. Provider configuration supports OpenAI, Anthropic, OpenRouter and self-hosted endpoints through server adapters; keys remain server-side and the UI shows configured status, never round-trips stored secrets in show snapshots. See [integrations](../product/integrations.md) for provider behavior and whole-show planning.

Codex session control from the app is a separately verified integration: establish the current supported MCP/session API and permission model before promising embedded session management. A generic process shell or arbitrary nREPL exposure is not a substitute for that contract.

### Verified Neovim reference

The read-only reference checkout `tolgraven/tolgrAVen` at `9daaea09577d9c7b80519e5c309583faf789c855` contains a real Neovim WASM integration:

- `src/frontend/website/modules/neovim/runtime.cljs` owns worker lifecycle and buffer RPC.
- `transport.js` implements the Worker/SharedArrayBuffer/MessagePack boundary, bounded input and cancellation.
- `model`/view code translates `ext_linegrid` redraws to browser rendering.
- `policy.cljc` and `doc/neovim.md` specify secure-context, COOP/COEP isolation, browser capability checks and full-document navigation across isolation boundaries.
- The editor supports `:initial-text`/`:initial-file`, `:on-save` and a lifecycle-owned `BufWritePost` `:on-write` callback. Browser files are a virtual workspace snapshot, not direct server filesystem access.

Reference: [Neovim integration documentation](https://github.com/tolgraven/tolgrAVen/blob/9daaea09577d9c7b80519e5c309583faf789c855/doc/neovim.md).

Extract a small reusable adapter after checking licensing and dependencies; do not copy the whole site's module/SSR framework. Mount an actual Neovim editor for code-defined effects/cues, lazy-load its assets, retain pinned artifact provenance and terminate workers/listeners on disposal. Its shared-memory requirement affects the top-level document and iframe ancestors; an iframe alone cannot make a non-isolated control page compatible. Prototype an isolated control workspace or dedicated editor document with explicit navigation and preserved show connection semantics. Provide a clear unsupported-browser state without claiming a text area is Neovim.

Buffer edits flow through parse, lint, schema/type checks, bounded preview compile and registry publish. Live evaluation is opt-in per editing session and only applies valid revisions. A syntax error leaves the last valid running effect unchanged. Correlate asynchronous validation to buffer revision; stale results cannot publish. WASM editing does not itself provide JVM Clojure evaluation or process-backed language servers. Choose a browser/server lint and language-service path separately. Trusted user code needs a defined execution/resource boundary; renderer workers cannot become arbitrary evaluation hosts.

## Migration slices and acceptance evidence

1. **Characterize current behavior.** Pure grid/velocity/coordinate helpers; cue gesture lifecycle and stale releases; recorded Push 2 codec fixtures; HTTP-to-command examples. Keep hardware acquisition out of namespace loading in new modules.
2. **Introduce shared command/model API.** Initially translate to the legacy runtime shell. Prove identical cue/parameter outcomes from MIDI, OSC, web and REPL fixtures. Test invalid values, duplicate commands, stale revisions/epochs, concurrent producers and bounded queues.
3. **Replace Push 2 internals.** Migrate codec, projection and session lifecycle independently behind the existing public binding entry point. Compare recorded input/output traces, then test physical palette/display restoration, unplug/replug and multiple controllers.
4. **Add Push 3 profile.** Capture the actual hardware's protocol and modes. Require a compatibility matrix and hardware evidence; do not mark complete from emulator tests alone.
5. **Ship responsive control workspace.** Verify multi-touch hold/release, radial/list actions, keyboard use, empty-cell paste conflicts, iPad landscape/portrait and phone keyboard/safe-area behavior. Exercise reconnect, offline interaction and two concurrent clients.
6. **Ship authoring surfaces.** Macro and sequence slots, parameter/effect/cue forms, rig/fixture editing and registry pages. Test schema round-trips, undo/redo, concurrent edits and effect version changes while running.
7. **Integrate advanced surfaces.** Neovim actual WASM lifecycle/isolation tests; streaming agent retries/cancellation; preview quality/latency budgets under large rig load; Live ownership handoff with hardware traces.

For automated UI checks, assert user-observable behavior and authoritative domain results. For protocol checks, assert exact decoded semantics and encoded bytes against captured/documented examples. Keep physical-device tests opt-in and report model, firmware and operating mode. No documentation-only milestone or mocked transport test establishes hardware support.
