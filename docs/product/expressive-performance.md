# Expressive performance, modulation and musical integration

This document records the expanded requirements of 2026-11-21. It supplements
[show authoring](show-authoring.md), [integrations](integrations.md),
[controls/web](../architecture/control-and-web.md), and the shared
[implementation contract](../implementation-contract-v1.md). It does not replace
the existing functional-runtime, GPU/CPU, simulator/fog, fixture, automation,
agent, environment-capture or robot-duck goals. Requirements below are proposed
unless a named implementation and its evidence are explicitly stated.

The current baseline has actual shared commands, typed parameter schemas,
musical durations, response envelopes, seeded cue-selection policy, a re-frame
studio and composable device sessions. It does not yet provide the complete
modulation synthesizer, expressive controller mappings, category performance
tools, musical patchbay or sharing service described here.

## Beat-authored modulation and categories

Every parameter type needs a defined modulation policy. Numbers and vectors can
interpolate within validated bounds; integer/discrete outputs quantize at an
explicit boundary. Enum and boolean modulation uses deliberate selection/gating
rules. Colors use the declared color space and RGBW fixture semantics. Random
values must come from explicit seeded state, respect reasonable schema ranges,
and support maximum change, weighting, smoothing and repeatable sequences.
“All parameter types” does not mean interpolating arbitrary strings or inventing
values outside an enum. Unsupported policies must fail validation visibly.

Rates, holds, change intervals and response envelopes use fractional beats,
bars or phrases by default. Pause, tempo/meter changes and restart must preserve
the specified musical phase. Milliseconds remain an explicit physical-time
option. Modulation can target one cue/instance, an explicit selection, all cues,
one category or a chosen subset of categories. Fixture/effect capabilities and
parameter schemas determine what a target accepts. A human override has an
explicit hold/release policy instead of being immediately overwritten by the
next automatic sample. Registry defaults and running-instance values remain
separate; modulation does not silently rewrite saved cue definitions.

Categories need stable identities, descriptive names and consistent colors
across the web, Push widgets, generic controls and Live pad mappings. Category
operations include pause/resume, removal from the active performance, opacity
and attack/release shaping. Proposed semantics distinguish pausing progression
from reducing intensity: pause freezes that category's logical progression;
opacity scales its contribution; release ends owned activations through their
envelopes. Removing active category contributions is not deleting the library.
These policies need explicit overlap rules when one instance belongs to several
categories, and must avoid multiplying attenuation twice by accident.

Acceptance:

- Deterministic seeded replay covers each supported type, bounded outputs,
  category/subset targeting and human overrides.
- Beat/bar/phrase phase, pause/resume and captured meter are tested against the
  actual runtime; disabling modulation restores the declared base value.
- Category color and selection metadata project consistently to every adapter.
- Pausing/releasing one category cannot stop unrelated human or automatic
  instances. Layer priority and blending remain deterministic under workers.
- Parameter/category editors expose supported choices and current modulation
  state, including attack/release and opacity, without requiring source edits.

## iPad performance and expressive controllers

The iPad interface should make live manipulation fluid with multiple touches,
independent holds, drags and grouped adjustments. Long press opens the radial
actions already requested; keyboard/context-menu and ordinary accessible
controls remain equivalent entry points. New gesture proposals must define
ownership, cancellation, scroll conflict and acknowledgement behavior. Actual
Safari/touch hardware is a separate gate from desktop viewport emulation.

Push 3 and Seaboard M should expose expressive inputs where their selected modes
actually provide them: per-note pressure, pitch and timbre/slide, touch position,
velocity and release information. Verify the exact device/mode and MIDI/MPE
specification before assigning numeric addresses. Per-note dimensions belong to
the originating activation; channel reuse, note-off, disconnect and mode changes
must not steer a later unrelated cue. Preserve useful continuous resolution and
timestamps before a domain binding chooses quantization or smoothing.

Automatic mapping should use semantic parameter metadata: type, range, unit,
category, preferred gesture, polarity and display label. Offer a reviewable map,
MIDI learn, explicit overrides and saved device profiles. Do not infer every
controller's protocol from Push 2 or replace user mappings on reconnect. Tethered Push 3 has a separate [USB screen qualification](../push-display-packaging.md#tethered-push-3).
Standalone hosting requires separate implementation and device measurements.

X-Touch and multiple extenders need a separately verified profile. Inspect actual
Ableton remote-script/MCU references, document the selected mode and port
ordering, then map faders, encoders, buttons, touch and feedback through the
common controls. Extender banking must have stable device/channel identity.
Investigate the requested color/PWM possibilities as research: distinguish
documented display/color commands, software refresh effects and unsupported
hardware assumptions. Tests derived from verified byte captures establish codec
behavior, not motor/fader/color behavior on physical devices.

Acceptance:

- Expressive state is isolated by device, zone/channel, note and activation;
  sustain/release/retrigger and late events have defined cleanup.
- Automatic maps are schema-valid, editable, persistable and visible before use.
- Multiple surfaces/extenders operate without sharing mutable gesture state.
- Protocol fixtures cite source revision and mode; unsupported features are
  explicit capabilities. Physical calibration/capture results are recorded
  separately.
- iPad tests exercise simultaneous expressive gestures, scrolling, cancellation,
  reconnect and keyboard accessibility while output continues independently.

## Rich device widgets and shared rendering

Push displays should show composable parameter/category widgets rather than a
second hardcoded application. Explore reusing re-frame components and CSS in an
owned offscreen webview, with pixels transferred to the device display through
its transport. The webview is a rendering client of shared projections; it must
not own a separate show clock or cue state. Inputs still produce the same domain
commands. Capability descriptors choose widget density, encoders, touch targets,
colors and refresh limits.

Before choosing an embedded webview, execute a small host prototype and measure
capture/transfer latency, allocation, text rendering, reconnect and teardown on
the intended platforms. A working Push 2 frame transport says nothing about the
Push 3 display protocol. Provide an explicit fallback when a webview/display is
unavailable; device rendering must never stall the lighting deadline. Browser
component reuse does not require duplicating the entire page into a tiny screen.

## A lighting synthesizer and reusable signals

Effects should be able to compose registered waveforms/signals into a lighting
synthesizer. A waveform registry needs stable identity/version, typed inputs,
units/ranges, phase/reset behavior, metadata for editors and a pure evaluation
contract. Include familiar periodic and stepped shapes and a way to register
custom declarative or explicit code implementations. Modulation, envelopes,
tweens and effects reuse these signals rather than each defining its own clock.

Derivative/slope and jagginess measures are requested controls. Define their
sampling interval, units, smoothing/window and edge behavior: differentiating
per frame without elapsed-time normalization changes the result with frame rate.
Noise/stochastic signals have explicit RNG state. Audio-derived signals include
validated level/energy, pitch, onsets, spectral/texture changes and the analyzed
parameter-audio feedback already requested. Confidence and availability accompany
estimates; a missing input cannot become an arbitrary jump to a misleading value.

Signal graphs need typed ports, deterministic ordering, cycle/delay semantics,
bounded state and explicit ownership. Preview/UI evaluation must not advance a
stateful lighting signal a second time. CPU batching/GLSL lowering can support
the proven portable subset, while unsupported nodes execute through a declared
CPU path or fail validation. Do not claim GPU parity for a node merely because
its source can be printed as GLSL.

Acceptance includes known-waveform samples, normalized derivative tests across
different frame cadences, deterministic noise, parameter bounds, stateful-delay
behavior, audio confidence/dropout handling and scalar/batch agreement for the
implemented subset. Editors must reveal the active waveform and its modulation
connections without obscuring the underlying saved data.

## Musical Live patchbay, Link, AbleSet and two laptops

The Live integration should expose actual musical sources: clip identity and
position, notes/velocity/gates, audio activity/features and automation values.
Arrange these and lighting destinations in a Reason-style patchbay with visible
typed connections. Mapping nodes support curves, source/destination ranges,
inversion, quantization, smoothing, attack/release and other declared envelopes.
One musical source can drive several cue/parameter/category destinations, and
several sources need explicit combination/priority rules. Audio channels and
per-parameter drum-rack chains remain supported goals, including pitch/level/
texture analysis and effect tails.

The existing Node/Max bridge assets and Clojure sync boundary are the first
protocol pieces, not proof of a complete Live device. MIDI note/CC delivery alone
does not confirm a clip write, recording state or audio activity. Track actual
LiveAPI/bridge acknowledgements and distinguish rejected, executed and uncertain
outcomes. Preserve origin metadata and echo suppression; restoring a show must
not replay historical chat tools, clip writes or held callbacks. Clearly label
bridge operations not yet supported, such as CC editing where the chosen bridge
does not implement it.

Evaluate Ableton Link and AbleSet integration for a two-MacBook-Pro performance
setup. Link supplies a tempo/phase/session relationship, not the entire Live
object model. AbleSet pairing/control must use verified APIs and declared
versions. Define which host is authoritative, explicit source selection,
disconnect/stale policy, transport/phase reconciliation and handoff. Do not
automatically switch to an unrelated discovered peer or let two laptops emit
competing lighting output. Session View/drum-rack workflows should preserve the
64-pad cue layout, colors, parameter organization and easy Live/lighting mode
switching, while allowing the human to jam alongside arranged clips and agents.

Evaluate `afterglow-max` as an optional embedded-host or integration reference.
Inspect its actual repository, dependencies, license, process model and supported
Max/Live versions before deciding what to reuse. Any chosen host runs the new
functional model and owned IO boundaries; it does not reintroduce a legacy
effect execution compatibility layer. Compare installation, process isolation,
timing, discovery, REPL and save/load behavior with the separate Ring/JVM host.

Acceptance includes fake/loopback protocol tests followed by actual Max/Live,
Link/AbleSet and dual-laptop checks: clip/session/rack mappings, clock selection,
record/playback/seek, reconnect, handoff, audio tails, stale-event rejection and
single-authority output. Captured logs identify exact software versions and
exclude secrets. Persist logical pairings/settings and desired connection state;
resources only open through the explicit post-restore host lifecycle.

## Sharing effects, rigs, shows and arrangements

A sharing website should publish discoverable, versioned artifacts: waveforms,
effect/fixture definitions, cues, pages, macros/sequences, rigs, complete show
files, mappings and linked Live arrangements. Packages need a manifest,
dependencies, preview/description, authorship/license/provenance, schema versions
and supported runtime/device versions. Distinguish portable declarative data,
trusted executable source, third-party assets and project-specific local links.

Import presents validation diagnostics, missing fixtures/devices/assets and a
reviewable merge/replace plan. Existing running definitions remain valid if an
import fails. Provider secrets, local credential material and live resource
handles never enter shared packages. Local credential references may need to be
removed or rebound for another user. A public artifact should not accidentally
publish private chat history or peer addresses just because a full local backup
can contain them; sharing export is an explicit product operation distinct from
lossless local save.

Acceptance covers portable roundtrip and dependency resolution, meaningful
preview, missing-resource behavior, version conflicts, attribution and explicit
publication. No website deployment, public upload or external message is implied
by recording this requirement. A full local backup remains lossless; sharing is
a deliberate selection/export flow rather than a weakened backup format.

## Implementation order and evidence

Keep independent lanes for typed modulation, signal registry, expressive
protocols, verified X-Touch work, musical integration and sharing. Claim exact
paths in [the work ledger](../work-items.edn), coordinate shared command/schema
changes and add behavioral tests before marking a narrow milestone done. The
initial implementation should demonstrate real end-to-end slices with injected
transports; later physical or external-service checks are separately selected.
