# Local studio UI

The browser application lives in `resources/studio/` and is served by the local
`tolglow.studio` Ring application at `/`. It is a working first studio iteration, separate
from the historical Afterglow web UI. It uses ClojureScript, Reagent and re-frame,
with a local WebGL2 preview and an optional real Neovim WebAssembly
editor. It is not a claim of full product completion or AAA rendering quality.

## Performing

The Perform view presents one 8×8 page at a time, a transport/BPM control, master
intensity, blackout, active-instance inspector, and a live rig preview. Page cells
are coordinate records and refer to stable cue IDs. Active pads are projections
of actual instances; releasing instances remain visually distinct until the
engine retires them.

* **Toggle** launches a persistent instance or releases active instances for that
  cue. **One-shot** uses the cue's declared duration. **Hold** launches on pointer
  down and releases its exact instance on pointer up/cancel.
* Touch pointers have independent ownership. Cancellation, lost capture, window
  blur, hidden visibility and page exit release held instances. If release
  precedes acknowledgement, the arriving instance ID is immediately ended with
  the original epoch. The session heartbeat supports the shell's final lease
  timeout if a browser disappears completely.
* A 500 ms long press opens cue actions in toggle/one-shot mode; Edit grid also
  makes ordinary taps open actions. Users can copy a cue,
  paste a new cue into an empty cell, assign an existing library cue, clear a
  cell, inspect/launch it, edit its definition or append it to the composition.
  Clearing a cell preserves the definition. Copy/paste creates a new stable cue
  ID referencing the same effect, with independent cue parameter defaults.
* Shift+F10 or the context menu key opens cell actions. Arrow keys navigate the
  grid. Enter/Space activate a focused pad, including keyboard-held release.
  Outside editors, **B** toggles blackout, **Space** transport, and **1–4** switch
  views (**5** opens Assistant). All inputs have labels; a skip link reaches the main controls.
* Inspector controls follow captured effect parameter schemas: bounded numbers
  use sliders, integers use integral steps, booleans checkboxes, enums selects,
  RGB(A) colors a color input that retains alpha and vectors component inputs.
  Parameter writes are coalesced at the I/O boundary. Optimistic values remain
  visible until the corresponding acknowledged revision arrives, and incomplete
  numeric edits (such as a leading minus sign) survive frame updates.

Auto mode is a disclosure beneath the inspector. It configures cue interval,
count, page/all scope, launch mode and duration through `/api/auto`; the server's
policy supplies cue selection and emission dependencies. Persistent mode leaves
instances active until explicitly released. This is a policy-driven local mode,
not an LLM that autonomously invents new definitions.

Response envelopes have a separate disclosure. An attack/release gate multiplies
named numeric parameters globally, across the selected effect type, or on the
selected instance. Both stages share a musical unit (or explicit milliseconds),
with linear, smooth, ease-in and ease-out curves. Starting/retriggering and releasing
invoke the response API. Release reaches zero and stays there until the response
is removed; removal restores the original, unmodified parameter values.

## Compositions and definitions

Compose reuses one ordered slot model for macros and sequences. Slots contain a
cue ID, parameter overrides and a duration. Duration controls default to musical
units: fractional beats, bars and phrases, with milliseconds as an advanced
compatibility option. New sequence slots default to one beat. The UI exposes cue
selection, duration, reorder and removal. Each slot expands into the same typed
parameter controls used by live effects. Overrides compose with cue/effect
defaults, survive save/reopen, and can reset to defaults without changing a
running effect. Macros launch together; sequences use once,
loop, ping-pong or random traversal. Save, save-and-play, stop and reopening saved
compositions all invoke the corresponding local API.

Definitions lists effect/cue registry entries and their versions. EDN source is
loaded from the server, validated, compiled and published. The visual builder
creates RGB constant color effects with an adjustable intensity parameter,
duration and attack/release envelope, optionally with a matching cue. This is a
small working code-free subset; it is not yet a general visual expression graph.
Fixture types have their own editor in Rig & space. Empty grid cells can place
the resulting cue.
“Build registry page” reconciles an 8×8 page with registered effects, preserves
existing occupied cells, and reports new placements, conflicts and overflow.

Existing edits send `expected-version` for optimistic publication. The shell
owns actual version advancement. “Publish valid edits” debounces publication;
validation errors remain visible and do not replace the last valid definition.
This option is explicit and off when loading another definition. Arbitrary
Clojure source is not evaluated by the definition endpoint. Validation runs at
publish time; the textarea itself does not claim a live Clojure linter.

The default editor is a labeled accessible textarea with Tab indentation and
Ctrl/Cmd+Enter publication. Selecting Neovim loads the actual pinned WASM runtime
reused from Tolgraven. Its Worker owns the editable buffer; the UI consumes
`ext_linegrid` redraws, sends native input/paste/mouse RPC and reads real buffer
lines before publication or return to the textarea. Escape, Insert, Tab and Undo
buttons support touch use. Worker and ResizeObserver teardown are explicit.

Neovim requires a secure cross-origin-isolated context with SharedArrayBuffer
(localhost or HTTPS with COOP/COEP). The accessible editor remains available when
that is unavailable. Assets are local, approximately 30 MB; initial startup is
therefore heavier than the rest of the studio. The studio does not include
Tolgraven's plugins, host filesystem access, external-process language servers,
or a durable filesystem. `:w` writes the browser's temporary buffer file, not the
host checkout or lighting registry. See
[`vendor/neovim/README.md`](../resources/studio/vendor/neovim/README.md) for provenance
and bundled licenses.

## Rig and preview

Rig & space edits room dimensions and fixture names, XYZ positions in metres,
universes and addresses. The existing channel maps and fixture metadata survive
position edits. Quick add creates a 12-channel RGBW moving-head template; saving
submits the complete rig for validation. The runtime is responsible for rejecting
overlapping or invalid channel mappings. A top-down SVG plan edits fixture and
truss X/Z positions by dragging, or by keyboard arrows in 0.1 m steps (Shift for
1 m). Height is retained. Truss forms edit name, height, length and Y rotation;
these are saved in the same rig as fixture positions. The plan shows up to 128
fixtures. These are draft edits until Save rig, after which the performance
preview uses the saved geometry. This is a plan editor, not yet a 3D transform
gizmo or fixture attachment/structural engineering system.

The fixture library searches the saved catalog or explicitly searches Open
Fixture Library, downloads mode information, and imports a chosen mode for
review. Local uploads accept OFL JSON, QLC+ QXF and GDTF. Import does not publish
a definition or patch a fixture automatically. Provenance, license information
and diagnostics stay visible. Unsupported channel semantics require review
rather than hidden guesses.

A code-free type form edits labels, footprint, channel coarse/fine offsets and
inversion, with an advanced EDN definition for full metadata. It can start from a
custom RGBW PAR template. Publication validates the type; a separate patch step
sets the instance address, XYZ position, orientation, RGBW mixing and explicit
moving-axis calibration. Unmapped channels or invalid physical mappings block
patching. Existing unsaved rig edits must be saved before adding an instance.
The UI supports these three import families, not every fixture format.

The preview consumes `output.values[fixture-id].emitted-color`, positions and
pan/tilt values. It never invents light from an active cue indicator; blackout and
master therefore affect the preview through resolved output. A single WebGL2
fragment shader ray marches a finite room with floor/wall/ceiling surfaces,
fixture bodies, cone illumination, attenuation and volumetric scattering. Colors
are tone mapped for display, not photometrically calibrated fixture predictions.

A persistent CPU 32×16×32 density field starts with uneven low haze and accepts
shaped plumes. Bounded drift, diffusion and decay evolve it independently of
effect frames; a 3D texture supplies the ray marcher. Reducing atmosphere to zero
removes visible volume while surfaces still receive light. The “Atmosphere” and
“+ Plume” controls affect local preview fog only; they never trigger a physical fog
machine. This is a coarse visual approximation, not fluid simulation.

The first sixteen fixtures and eight trusses are rendered, with a visible notice
above those limits. Trusses are yaw-oriented metal box beams with analytic ray
intersections. They stop fog integration at the nearest beam and obscure fixture
bodies behind them; they are not detailed lattice meshes or a general shadow map.
There are 40 ray steps on phone widths and 64 otherwise, bounded device-pixel
ratios, one RAF loop capped at 30 fps, and a 10 Hz fog update. Hidden tabs/views pause graphics
work. Drag or arrow keys orbit, wheel/pinch/+/- zoom, and Home or reset restores
the camera. Graphics context loss has a visible recovery state. The preview
currently lacks comprehensive light/shadow occlusion between scene objects,
realistic gobos, fixture-specific optics, reflections, detailed truss meshes, architectural imports,
Gaussian splats and accurate fog fluid dynamics. Those remain renderer work,
not hidden simulated functionality.

## API and lifecycle

`src/studio/tolglow/studio/events.cljs` has one outstanding `/api/state?session=…` request at a time, normally
every 100 ms, with a request timeout and reconnection notice. Hidden tabs poll
less frequently. Polls include the last accepted runtime identity, epoch and
revision. Unchanged revisions return a partial projection; the browser merges
dynamic fields into existing authoring data only when all three identifiers
match. A mismatched partial response triggers a full fetch. The HTTP projection
bounds preview output to sixteen fixtures; the internal runtime snapshot retains
all fixture output. This reduces idle traffic but does not establish a measured
large-rig mobile performance guarantee.

Commands carry unique command IDs, `{adapter:"web",session-id}` source, runtime
identity and the current show epoch. Delayed held releases retain their original
identity, so they cannot address a replacement server process. The commands use
the shared reducer operations, not
private widget mutation functions. JSON page cells use `{x,y,cue-id}` records
because vector map keys do not round trip through JSON objects.

Definitions, rig, compositions and import/export use their documented studio
endpoints. Error responses appear in a toast or the editor validation area.
Export produces EDN; import accepts show files up to 16 MiB and returns to the new epoch. No physical output is
enabled by opening the studio. Output mode is reported by the shell.

The selected view, cue page and launch mode are portable UI preferences saved
through `/api/settings` and restored on startup. The browser restores only this
explicit whitelist, never provider credentials or editor/chat drafts. These are
server-owned show settings rather than browser local-storage state.

Application state is the re-frame app-db. `events.cljs` owns explicit event
transitions and effect descriptions; `subs.cljs` defines projections;
`views.cljs` renders Reagent components; `chat.cljs` supplies provider/chat events
and views. `interop.cljs` owns lifecycle acquisition/cleanup for two specialized
JavaScript engines: WebGL rendering and Neovim's WASM/MessagePack canvas. These
engines neither mutate application controls nor own show commands. The HTML is
only a mount point. There is no parallel vanilla JavaScript application.

Controlled form updates dispatch synchronously from input handlers, keeping
React's value/selection restoration consistent. Network operations stay in
effects. Authoring views receive bounded projections so renderer clock updates
do not redraw unrelated editors. Standard command/gesture events remain queued.

Responsive layout keeps the full 8×8 grid on phones, uses a bottom navigation bar
and moves import/export into the footer when the desktop sidebar disappears.
iPad-sized layouts retain larger cue cells and place preview/inspector alongside
one another when space permits. There are no remote fonts or UI/CDN assets.

## Assistant and provider connection

Assistant offers a streaming conversation and an inline provider form for OpenAI,
Anthropic, OpenRouter and self-hosted compatible endpoints. Model IDs/endpoints
are explicit configuration. Credentials are cleared from the form when submitted.
The server uses its separately configured local credential store when available,
or session memory otherwise. Only opaque references enter portable show data;
the actual keys are excluded from public configuration and exports. Saving
configuration does not call a model.

Chat uses the actual `/api/chat` SSE response. Text deltas accumulate into the
assistant message; tool starts/results appear as expandable activities. The
server owns conversation history: `/api/chat/history` restores saved conversations,
the picker selects one through `/api/chat/select`, and continued requests supply
its conversation ID rather than a client copy of previous messages. Late stream
or history responses cannot replace a newer conversation. Streaming follows the
latest message until the user scrolls up. The server supplies rig/show
context and validates every semantic tool operation. Stop aborts the stream,
which cancels future generation; it does not undo already accepted show commands.
No synthetic assistant replies or simulated model sessions are used. Provider
availability and a configured model determine whether a real conversation can run.
Restoring a show/history does not resume a generation automatically. Interrupted
conversations show their status and can be continued by sending another message.

## Build, verification and remaining evidence

Install pinned npm dependencies with `npm ci`, then run `npm run studio:build`
for the production Shadow CLJS bundle or `npm run studio:dev` for live compilation.
The `:cljs` Clojure CLI alias supplies Reagent/re-frame/Shadow dependencies. The
compiled bundle is served at `/studio/js/main.js`; generated files are not source.

`npm run studio:test` currently runs 28 tests and 124 assertions against the actual
registered re-frame handlers and interceptors, with effects left unapplied.
Coverage includes held acknowledgement races, independent pointers, page-exit
cleanup, optimistic parameter writes, musical durations, expected versions,
runtime identity, partial projections, incomplete numeric edits, saved settings,
stale chat/history responses, typed fixture mapping and obsolete fixture-service
responses after show replacement, composition overrides, and graphical rig
pointer ownership and save acknowledgements that arrive after newer rig edits.
The last test build had zero warnings.

Optimized-bundle browser checks observed real cue launches and parameter changes
(including an End→ArrowLeft slider change from 1.0 to 0.999), held releases,
definition publication v1→v2→v3 with actual Neovim WASM editing, composition
playback, rig save/reload, WebGL rendering, fog plume/reset controls, grid
arrow/context keyboard access, long-press actions and no horizontal overflow at
820 px iPad and 390 px phone viewport sizes. Musical one-shots remained paused
with the transport and completed after resume. Chromium CDP touch events verified
two independent simultaneous holds and touch cancellation. A focused numeric
edit test verified typing a negative fixture position. These are Chromium checks,
not physical Safari or iPad hardware evidence.

A subsequent production-bundle pass against the restarted server verified nine
session workflows: conditional snapshots retaining all 64 cells; preference
restore after reload; the real history endpoint; generated registry pages;
code-free fixture publication, patching and removal; an uploaded OFL file with
explicit mode selection; musical response release/removal; and the 390 px fixture
layout. Six additional authoring checks verified slot overrides in actual macro
playback, fixture drag/truss keyboard placement, saved truss geometry reaching
WebGL uniforms without GL errors, UI restoration of the original rig, and the
phone plan layout. Both passes captured no console or page errors. On the eight
fixture demo, one observed full response was 14,567 bytes and idle partial
responses were 1,511 bytes; these are payload observations, not a large-rig
throughput benchmark.

The JavaScript engines, transport and worker wrapper pass `node --check` under
Node 24.22.3. A live captured-output API smoke test launched and ended a real
held Ember instance and verified the returned instance definition/parameter
schema. Assistant provider loading, input and responsive layout were checked
against the real application API; no external provider conversation was run
without configured credentials. Physical iPad Safari, MIDI devices, multi-touch
hardware and production Art-Net have not been validated by these checks.

Manual regression coverage should include: toggle and one-shot behavior;
two-pointer holds with reversed acknowledgement order; pointer cancel and tab
hide; keyboard holds; master/blackout output; copy/paste and empty-cell placement;
sequence save/run/stop; invalid definitions and conflicting versions; rig address
rejection; import epoch changes; editor switch preserving the WASM buffer;
worker failure; resize/rotation; preview context loss and zero-density fog.
