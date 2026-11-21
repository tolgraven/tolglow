# Devices, music, agents, fixture libraries, and simulation

This document records target behavior and research boundaries as of 2026-11-21.
It does not claim that the integrations are implemented. Shared domain behavior
lives in [show authoring](show-authoring.md), adapter architecture in
[control and web](../architecture/control-and-web.md), and numerical/rendering
contracts in [rendering](../architecture/rendering.md).

## Existing foundations and gaps

| Integration | Evidence in the inspected repositories | Remaining scope |
| --- | --- | --- |
| MIDI and OSC | Afterglow has MIDI routing/clock and controller implementations; Tolglow has `osc.clj`, cue helpers, and configuration. | Normalize these through one control service rather than duplicating launch/parameter logic. |
| Push 2 | Afterglow `controllers/ableton_push_2.clj` implements the device with display, pad, encoder, and show behavior coupled in one namespace. | Replace it with capability, transport, layout, and binding layers while preserving tested behavior. |
| Push 3 | No Push 3 adapter was established by this review. | Verify supported connection modes and protocol differences, then implement and test on the user's device. |
| Ableton Live | `tolglow/live.clj` contains live-coding experiments, not a Live clip/Drum Rack integration. MIDI support is not evidence of clip, color, or automation access. | Build a Live-side bridge and a versioned mapping/session contract. |
| Audio parameters | `tolglow/audio.clj` contains only a commented Overtone require and a note about future analysis. | Encoding, routing, analysis, feedback semantics, and tails are new work. |
| Agent access | Afterglow has `web/routes/web_repl.clj` and optional nREPL startup configuration. The web REPL evaluates forms; it is not an isolated agent sandbox. | Add a documented control/read API, project workspace boundaries, provider adapters, and a streaming UI. |
| Fixture import | Afterglow `fixtures/qxf.clj` parses QLC+ definitions and emits Clojure fixture source; its namespace notes missing geometric information. | Normalize imports to data, retain provenance, validate gaps, and add more formats/search. |
| Preview | Tolglow `viz.cljc` and `graphics/` contain Quil/Processing/PixelFlow experiments; Afterglow has a web visualizer route. | Build a shared rig model and a modern WebGL simulator; existing code does not establish realistic volumetric fog. |

## Generic controls and Push migration

Push 2, Push 3, generic MIDI, OSC, browser gestures, agents, and sensors are
adapters around the same commands and read models. A pad press emits a launch or
selection intent; it does not implement cue lifecycle. Parameter normalization,
ownership, macro editing, sequencing, and undo belong to the shared service.

Describe a device's capabilities separately from its identity: grid dimensions,
RGB/palette pads, pressure, encoder type/resolution/touch, buttons, text/display
surfaces, transport, and supported feedback rates. Composable helpers bind a
control to a semantic action with scaling, pickup/fine adjustment, feedback, and
mode-specific layout. A browser or custom microcontroller can reuse those
bindings without pretending to be an Ableton device.

The Push 2 rewrite needs recorded MIDI/USB fixtures where legally available,
headless mapping/render-model tests, then hardware verification of pad colors,
pressure, encoder direction, text/display updates, reconnect, and held releases.
Keep the old adapter available until the replacement meets that parity gate.
Push 3 must have its own capability/protocol profile. Do not infer that a Push 2
SysEx identifier, display transport, or ownership handshake works on Push 3.
Distinguish tethered controller operation from standalone behavior until both
are investigated. Record firmware and Live versions used in hardware evidence.

Switching between lighting and Live mode is an ownership transition: release
held controls, stop stale feedback, negotiate the active mappings, and redraw
the new mode. Cue playback can continue while the surface changes mode. Returning
to lighting shows authoritative current state, not cached values from before
the switch. A disconnect/reconnect must not leave a stuck note or held cue.

Acceptance: the same cue/parameter commands behave alike from a web gesture,
Push pad, MIDI message, and OSC message; code can compose a new control binding
without changing cue logic; switching mode while holding a pad releases the
correct owner; unsupported Push 3 features are reported as unavailable rather
than silently emulated incorrectly.

## Ableton Live composition and recording

### Bridge and mapping model

MIDI provides notes, controllers, and clock, but not arbitrary Live clip edits,
Drum Rack metadata, pad colors, or complete recording state. Investigate a
Max for Live device and/or a supported Control Surface/Remote Script bridge,
document version constraints, and prototype the smallest round trip before
selecting the implementation. Ableton Link-style synchronization alone does not
supply clip editing or automation recording. Do not promise access to every
Live API on every edition or release.

Store stable mappings from show/cue/parameter IDs to Live set, track, rack, pad,
clip, and automation-lane identifiers. Names and grid coordinates are display
metadata. A mapping profile includes note/channel assignments, parameter value
scales, transport/clock authority, record behavior, color conversion, and bridge
protocol version. Reopening a set validates/reconciles mappings and reports
missing objects rather than assigning by whichever track is in the same slot.

The primary performance layout is a 64-pad Drum Rack page aligned with the
lighting cue grid. Note-on launches, velocity initializes mapped parameters,
pressure/controllers update them when supported, and note-off follows held or
explicit gate semantics. Persistent/timed/one-shot modes retain their domain
behavior. Parameter controls can be exposed as rack macros or a bridge-specific
parameter surface; the number and automation behavior of available macros must
be measured against supported Live versions.

Pad names and colors mirror the authoritative cue presentation, including page
changes and definition updates. Live's available palette/display constraints
may prevent exact RGB equality; use a deterministic documented conversion and
show the same logical cue identity everywhere. Live pad selections must not
implicitly launch a cue unless that gesture is configured to do so.

### Record every human action once

When the mapped Live destination is recording, accepted cue launches, releases,
parameter adjustments, and relevant macro actions are written to the appropriate
clip or automation lane. Explicitly distinguish session clip recording,
arrangement recording, overdub, automation arm, and track arm. The bridge reports
which destination is actually writable; the lighting UI must not display a
recording confirmation for a rejected or unmapped event.

Every action carries a `command-id`, `source`, show `epoch`, authoritative
sequence, and musical/monotonic timestamps. The recorder observes accepted
commands, not merely incoming knob packets. Correlation IDs suppress feedback
loops: a Live playback event reflected back to a pad is not recorded as a new
human performance. Parameter automation needs a declared sampling/coalescing
policy that preserves meaningful extrema and transitions while limiting traffic.
Launch/release events are not dropped by continuous-value coalescing.

Clock authority is explicit per session. Handle transport start/stop, seek,
tempo changes, loop boundaries, count-in, and reconnect. Convert timestamp domains
using measured offsets/latency; quantization is configured and never applied
twice. A cue already playing when recording begins needs a defined initial-state
capture so clip playback can reproduce it.

Conflict policy is visible: manual performance can temporarily override clip
automation, automation can remain authoritative, or recording can capture the
manual takeover. A reconnect uses the selected policy to resynchronize without
replaying old launches. Versioned exports include cue definitions/presets or
explicit missing-dependency reports so a light-show arrangement can be moved.

### Delivery and acceptance

1. Prove one note-to-cue launch, one parameter round trip, recording-state
   detection, and one color update on the target Live version.
2. Implement the 64-pad page and stable persisted mappings; test session reload,
   disconnected bridge, missing tracks, and mode handoff.
3. Capture accepted lighting commands into clips/automation with feedback-loop
   suppression; replay against a deterministic show snapshot.
4. Add arrangement workflows, page/rack generation, and richer parameter editor
   feedback after the bridge contract is proven.

Acceptance: recording a launch, two parameter changes, and release reproduces the
same logical sequence; playing that clip does not recursively record itself;
colors follow cue edits; a lost connection cannot launch buffered stale notes;
switching to Live mode stops lighting control feedback on the claimed surface;
timing error and jitter are measured, with supported tolerance documented rather
than described only as "in sync".

## Parameters as audio and audio-derived modulation

### Routing modes

Support an audio channel carrying one cue parameter and a Drum Rack arrangement
with a separate pad/effect chain per parameter. Preserve isolation between
parameters unless an explicit mix/analyzer design can separate them. A single
polyphonic mixed signal cannot reliably recover every original parameter with
a monophonic pitch estimator.

The encoder generates a sine whose frequency represents a normalized parameter:
for example `f(x) = f-min * (f-max / f-min)^x` for `x` in `[0,1]`. Frequency limits,
sample rate, amplitude, ramping, and latency are configuration, not hardcoded
musical guesses. The reverse mapping is explicit and monotonic within the
valid range. Signed or wider-domain parameters normalize using their schema.
The carrier amplitude can represent a separate gate/envelope, while returned
volume supplies a modulation signal.

Route processed audio back through a timestamped analysis adapter. Emit a feature
frame with level, estimated pitch, confidence, spectral features, baseline
reference, latency estimate, and continuity status. The control/effect layer
converts features into bounded parameter inputs using the same mapping and
constraint machinery as other inputs. The render loop consumes the newest
valid feature snapshot; it does not run audio callbacks or wait for FFT work.

### Pitch, level, and texture

Start with RMS/peak envelope and monophonic pitch with confidence tracking.
Estimate a time-aligned baseline sine using the known emitted frequency and
level. "Texture difference" should become explicit measurable channels, such
as harmonic-to-fundamental energy, spectral centroid change, spectral flatness,
transient flux, and residual energy after a fitted reference. Normalize these
against signal level and the baseline so a gain change does not masquerade as
every possible texture change.

Echo, chorus, distortion, pitch shift, and reverb can create multiple pitches or
low-confidence signals. Define confidence thresholds, hold/decay behavior,
hysteresis, noise floor, and fallbacks. Never turn an undefined pitch estimate
into a full-scale parameter jump. Analysis window size trades latency for low-
frequency accuracy; publish that latency and allow alignment compensation.

Texture channels drive selectable parameter fluctuations, not a claim to infer
the exact audio plugin or its settings. Map them through curves, smoothing,
bounded gain, and optional seeded variation. Record feature frames when exact
replay matters, because external audio plugins may not be deterministic.

### Echo/reverb tails and ownership

Stopping or looping the original cue must not immediately destroy the routed
audio return. The bridge retains a tail instance with the captured target and
mapping until signal energy stays below a threshold for a hold period, or a
configured maximum tail duration elapses. A delayed echo can then contribute a
new pulse after the source gate ends; reverb can cause a decaying response.

An audio tail's output may modulate a retained effect snapshot or an explicitly
declared target parameter. It must not accidentally alter a newly launched cue
that reused the same key. Live feedback loops require declared direction and
bounded gain: analyzed return values must not recursively regenerate the carrier
unless that mode is intentionally configured. Immediate show stop has an
explicit policy for clearing tails; ordinary cue release honors them.

Acceptance: encode/decode an unprocessed sweep within a documented error;
silence produces no random pitch jumps; gain change affects level predictably;
known delayed pulses remain after source release; reverb tails expire; two
isolated parameter chains stay independent; distorted/ambiguous audio remains
within parameter bounds; analysis overload cannot delay fixture output.

This integration is experimental until the carrier frequency range, audible
monitoring policy, supported routing, plugin behavior, and end-to-end latency
have been tested with the user's actual audio setup.

## Fixture definition editor, import, and search

Extend Tolglow's existing fixture maps into a canonical versioned fixture type:
manufacturer/model/mode, physical dimensions, emitters and spectra/color roles,
channel and fine-channel mapping, function ranges, calibrated pan/tilt/zoom/focus,
pixel/head geometry, default values, and physical/simulator metadata. A fixture
instance adds a stable identity, type/version reference, patch, position,
orientation, groups, and per-unit calibration. Fixture types and placed instances
are separate objects.

The web editor supports channel tables, range visualization, coarse/fine pairing,
reordered color layouts, pixel layout/density, and a small isolated preview.
Validation catches overlaps, invalid ranges, missing channels, inconsistent fine
mapping, address overflow, and incompatible mode selection. Do not automatically
treat reset/lamp/control ranges as ordinary dimmable channels. Patch preview
reports universe use and collisions before applying a rig revision.

Import adapters produce canonical data plus diagnostics, unsupported fields,
source metadata, and confidence for inferred values. Initial candidates are the
existing QLC+ QXF path, Open Fixture Library JSON, and GDTF; additional common
console formats should be added through an explicit support matrix. "All common
formats" is a product goal, not an assertion that proprietary formats can be
parsed losslessly. MVR is a rig exchange candidate, including placement and
GDTF references, rather than another interchangeable fixture schema.

For each format record supported versions, modes, geometry, fine channels,
functions, calibration, known omissions, and a round-trip or golden fixture
corpus. Imported unknown fields remain available as provenance/extension data.
A fixture missing geometry can still be patched with visible unverified
simulation/calibration fields; guessed geometry must not look like measured
manufacturer data.

Search accepts manufacturer/model/mode, searches configured sources, and returns
candidate definitions with source, version, license, and exact mode/footprint.
Download into a cache with a checksum and provenance record, validate, then add
the selected definition to the user's library. Failed searches should offer
manual import/editor entry. An optional agent can find candidates or translate
a manual into a draft, but imported descriptions are data, not instructions, and
agent-inferred values remain marked for review. Downloading a fixture must not
evaluate arbitrary Clojure from a remote definition.

Acceptance: known QXF fixtures preserve channel semantics; a GDTF/OFL import
reports unsupported fields rather than discarding them silently; source/cache
loss does not remove saved definitions; mode selection changes footprint
correctly; a multi-universe pixel fixture produces valid explicit patches;
search results distinguish identically named fixtures/modes; physical calibration
can be corrected without rewriting every cue.

## Agent control and code authoring

### Provider and session configuration

Offer adapters for OpenAI, Anthropic, OpenRouter, and self-hosted endpoints.
Store provider configuration separately from show files: provider kind, endpoint,
model, supported capabilities, connection settings, and references to credentials.
Credentials stay server-side and are omitted from shared show exports and logs.
Connection tests report authentication/model availability and capability limits
without claiming identical tool/streaming behavior across all providers.

The chat UI streams text/tool progress, shows proposed changes and errors, and
can cancel an active generation. It reconnects using session/event cursors and
does not duplicate completed tool actions. Context includes current rig,
capabilities, available effect registry, active cues, selected page, palette,
musical clock, and the operator's intent. Large pixel rigs are summarized by
groups/capabilities rather than dumping thousands of repeated objects into every
prompt.

Use capability-scoped tools: inspect show, inspect definitions, propose/register
a definition, place/edit a cue, launch/release, change parameters, build a plan,
and save/revert a session. Their implementations call the same validated command
layer as controllers. Requests carry source/session IDs and idempotency keys;
the resulting command log explains what the agent actually changed.

### Code boundary and feedback loop

Give agents a documented user-authoring area for effect definitions, cues,
palettes, fixture library additions, and show plans. Engine/compiler/controller
source changes use a normal development workflow with tests and reviewable diffs.
This distinguishes creative live coding from changing the runtime underneath a
performance. The user can configure automatic application of valid authoring
changes; this should not force a confirmation dialog for every cue launch.

Plain-language authoring produces a draft definition and parameter schema,
validates/compiles it off-thread, previews it against the actual rig, and can
create a cue or run it once/for a duration. Invalid edits retain the last working
version. Persist source, normalized definition, model/session attribution, and
the resulting version so edits can be understood and reverted.

Neovim should be an actual embedded WASM instance where code is entered, following
the [verified tolgrAVen reference](https://github.com/tolgraven/tolgrAVen/blob/9daaea09577d9c7b80519e5c309583faf789c855/doc/neovim.md).
Its Worker/SharedArrayBuffer transport requires a compatible secure, isolated
document; an iframe alone cannot supply isolation to an incompatible parent.
The [web architecture](../architecture/control-and-web.md) records the source
and deployment constraints. Syntax highlighting,
linting, schema/type diagnostics, and runtime validation are distinct stages.
Optional "run valid edits" debounces changes and publishes one validated revision
at a boundary. A syntax-valid infinite loop is not made safe by linting; arbitrary
Clojure code needs the same trusted-code boundary as the REPL, with expensive
compilation/evaluation separated from fixture rendering. A restricted declarative
editor offers predictable bounds where arbitrary code cannot.

### REPL and Codex integration

Provide a small documented REPL facade over the registry and command API, with
readable result data and dry-run/preview functions. Codex can then inspect the
rig, create an effect, run a test/preview, launch a one-shot, and observe the
result without knowing controller internals. Keep namespace loading free of
implicit fixture-output startup in this authoring path.

An MCP server is a useful optional transport for these same tools. Controlling a
Codex session from inside the web app is a separate feasibility task: verify
the supported Codex app/SDK/MCP capabilities, authentication, lifecycle, streaming,
and cancellation. The existence of an MCP tool server does not establish that
it can create or drive a Codex UI session. If session control is unavailable,
the app can still offer provider-backed chat and an external Codex REPL/MCP
connection using the same show service.

Afterglow's existing web REPL calls Clojure `eval`. Binding `*read-eval*` false
disables reader evaluation, not arbitrary evaluation or I/O by the form. Do not
present it as an untrusted-code sandbox or expose it as an unrestricted remote
agent endpoint. Authentication, local/remote access policy, and trusted code
capabilities need an explicit deployment design when integrating it.

### Prompt-to-show and human collaboration

A full-show prompt becomes a versioned creative plan: fixture selections,
palette constraints, effect definitions, pages, transition patterns, beat accents,
auto-mode rules, and a reproducible seed. The agent can evolve definitions and
plans over time through the same hot reload mechanism. It does not emit raw
DMX or require an LLM response on each beat/frame.

Humans continue to jam from Push/web while the plan runs. Ownership, pinning,
priority, and temporary override rules are visible; the planner consumes updated
state and avoids immediately undoing a manual choice. Cancelling agent planning
stops future proposals but does not ambiguously stop every human-owned cue.

Acceptance: a new agent session can discover supported commands and create/run
a one-shot from a prompt; invalid generated code leaves the current show intact;
provider disconnect keeps the last validated plan running; cancellation stops
pending proposals; duplicate tool retries do not duplicate cues; secrets never
appear in exported show data; an external Codex session can complete the same
workflow through the documented REPL facade.

## Simulator, rig editor, and environments

The long-term visual goal is a convincing club-scale preview with calibrated
fixture beams, surfaces, occlusion, color, and volumetric fog. "AAA game level"
is a quality direction; acceptance must use concrete reference scenes and device
budgets. Desktop, iPad, and phone quality tiers share a rig/output model while
varying render resolution, fog grid/sample count, shadows, and update rate.
The lighting engine continues at its own rate if the preview is closed,
backgrounded, disconnected, or overloaded.

Use one scene graph for live preview and rig setup: fixtures, trusses, groups,
mounting points, room geometry, transforms, and fog sources. Add fixtures from
the same library used for patching, drag/snap to truss positions, edit orientation
numerically, aim at a target, duplicate groups, and inspect resulting channels.
Define units, axes, handedness, and conversion at every imported-model boundary;
the current Processing coordinate adjustments are evidence that this cannot be
left implicit. Authoring a rig revision is separate from applying it to an
active show, with patch collision and calibration validation.

Fog sources produce localized dense plumes with position, direction, emission
rate, temperature/buoyancy where modeled, and turbulence. Over time the plume
advects/diffuses and the room gains ambient haze. Light scatters through that
density, is attenuated, and still illuminates surfaces. With fog density set to
zero, the model shows illuminated surfaces without a fake opaque beam volume.
Fog-machine cue output can drive emission, but the preview is an approximation
unless physically calibrated. Engineering choices and performance experiments
belong in [rendering](../architecture/rendering.md).

Prefab environments start with useful generic rooms and truss layouts. Real
club-inspired layouts require attributable/licensed geometry and a clear claim
about accuracy; do not label an invented room as a scanned famous venue. Keep
environment assets versioned, separately licensed, and optional downloads.

Phone camera/LiDAR scanning and Gaussian splatting are exploratory import paths.
A splat can provide appearance without watertight collision/occlusion geometry
or reliable scale. Preserve a separate calibrated room/truss/fixture model,
require scale/origin alignment, and supply simplified geometry for beam/shadow
intersections. Test relevant iOS capture/export availability and file sizes
before committing to an in-browser capture flow. Rig setup must remain usable
with manually entered dimensions and ordinary meshes.

Acceptance: an aimed real-world reference fixture and preview agree within
documented calibration tolerance; moving a truss moves attached fixtures and
their aim geometry coherently; zero fog removes volumetric beams; a finite fog
burst evolves from a localized plume to diffuse haze; backgrounding an iPad does
not interrupt Art-Net output; phone controls remain usable while the preview
automatically reduces quality; imported environments retain scale and provenance.

## Robot duck and other future sensors

Record the user's robot mini-duck idea as a later integration; "microduck" is an
unconfirmed product name, not a verified protocol or vendor. The intended loop
is procedural dance progression, optional learned movement policies, telemetry
of actual motion, and lighting driven by that observed motion.

First identify the device, SDK, transport, sensor/pose data, control rates,
latency, and whether learned policies run on-device or elsewhere. A generic
adapter can emit timestamped pose, joint angle, velocity, motion-phase,
confidence, and contact/beat events. Normalize these into the shared input model
so the same mappings work with another robot, wearable, microcontroller, or
sensor installation. Learn/execute movement outside the render loop.

Recorded telemetry must drive a simulated input source before connecting real
hardware. Define stale-data timeout, outlier handling, neutral/hold behavior,
coordinate calibration, and ownership handoff. Robot actuation and lighting
input are separate capabilities: receiving telemetry must not implicitly grant
an agent motor control. This is a future research item after the control layer,
deterministic playback, and sensor timestamps are established.

## Integration evidence and release gates

Every adapter needs a replayable protocol fixture or simulator, a compatibility
matrix, lifecycle/disconnect tests, and an explicit hardware acceptance record
where hardware is involved. Store supported versions and measured timing, not
only "works on my machine". Pure reducer tests do not prove USB display output,
Live recording, audio routing, physical photometry, or iPad frame rate.

Implement integrations incrementally behind the shared contracts. A connector
marked experimental remains usable with honest capability/error feedback, while
unsupported behavior stays visibly unavailable. Keep the project tracker linked
to these acceptance cases so documentation does not drift into a list of
unverified promises.
