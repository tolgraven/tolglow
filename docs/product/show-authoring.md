# Show authoring, composition, and autonomous playback

This is a proposed product and domain contract, based on the source inspected on
2026-11-21. Examples describe the target data model; they are not executable APIs
in this checkout. Implementation status belongs in the project tracker, and a
feature is complete only when its acceptance cases run against production code.

The source observations below initially use Tolglow `effects` at
`01146ece7731ec06aa1ee6409342373096c8e0dc` and Afterglow `master` at
`16b0b4cac70433e4118038dfbfd304e22d1d9018`. This pairing is not a verified full
application baseline: Tolglow imports fork-specific parameter/color definitions
absent from that Afterglow branch. The fork also has a `tolmaster` branch that
must be evaluated before selecting the production migration baseline. Passing
isolated helper tests does not establish compatibility of every namespace.

The architectural prerequisites are the [functional runtime](../architecture/functional-runtime.md)
and the shared [control layer](../architecture/control-and-web.md). External
connections, fixture import, agent authoring, and the simulator are specified in
[integrations](integrations.md).

## What exists today

| Area | Observed implementation | Migration implication |
| --- | --- | --- |
| Cue launch | `tolglow/cue.clj` contains `launch!`, group helpers, and cue construction; Afterglow `show/add-effect-from-cue-grid!` creates temporary variables, respects priorities and `:end-keys`, and applies saved values, velocity, then explicit overrides. | Preserve this precedence and lifecycle in characterization tests before replacing the launch path. |
| Parameters | `tolglow/vars.clj` builds cue variable maps; `tolglow/param.clj` wraps dynamic parameters, formulas, oscillators, random values, and color transforms. Defaults and metadata are distributed between these files and `config.clj`. | Consolidate schemas without discarding units, bounds, velocity behavior, or existing values. |
| Fixtures | `tolglow/fixtures.clj` already accepts declarative channel, calibration, and pixel-strip maps. `create-heads` is a placeholder; the deprecated `pixel-strip` path differs from `create-pixel-heads`. | Extend the existing declarative approach, with explicit validation and versioned migration. Do not label arbitrary multi-head creation complete. |
| Pages | `tolglow/page.clj` constructs 8×8 pages with builder functions selected by `resolve`; `reload-all` rebuilds pages and contains comments requesting transparent reload. | Page position is presentation, not identity. Regeneration must preserve manually placed entries and active instances. |
| Composition | Afterglow `effects/scene`, `effects/chase`, and `effects.cues/compound-cues-effect` exist. Chase has `:blank`, `:loop`, and `:bounce` modes. | Reuse their tested semantics where useful; define ownership and timing explicitly before exposing an editor. |
| Macro editor | `tolglow/macro.clj` contains proposed APIs, empty functions, and copied/commented web macro logic. Afterglow's web route has a macro creation path. | The shared macro editor and controller-independent macro service remain to be implemented. |
| Sequencer | `tolglow/sequencer.clj` has empty `secue` and `secudfx` bodies. | This is a new scheduler and editor, not an extension of a working sequencer. |
| Random show control | `tolglow/random.clj` has cue variable randomization, a random launch function, and an `auto-running` atom. Several arguments and functions are incomplete. | Characterize useful behavior, then replace the scheduling/selection state with pure transitions. |
| Envelopes | Afterglow `effects/wrap-fade-in-out` supports fade-in/out using an atom; step parameters and fades already exist. | Build reusable envelope/tween descriptions and adapt the old wrapper before migrating it. |
| Live editing | `tolglow/live.clj` experiments with regenerating effect graphs during rendering. `page/reload-all` does not implement its advertised relaunch option. | Hot reload needs stable definitions and an explicit migration protocol. Reloading namespaces alone cannot supply that guarantee. |

The source paths above refer to this repository's `tolglow/` folder and the sibling
Afterglow fork's `src/afterglow/` folder. A source comment or empty definition is
evidence of intent, not shipped behavior.

## One authoring model

All authoring interfaces produce the same versioned data: EDN files, web forms,
code helpers, controller gestures, and agents. Serialization must not depend on
Clojure closures, Java object identities, mutable references, or a particular
browser session. JSON may be a transport representation, with an explicit mapping
for keyword identifiers, exact time values, and tagged values.

The entities have different identities:

- A `definition-id` identifies an effect, parameter expression, envelope, fixture
  type, or composition definition. An immutable `version` selects its content.
- A `cue-id` identifies a launch configuration and presentation metadata. A cue
  refers to a definition; its grid position can change independently.
- An `instance-id` identifies one execution, including its parameter values,
  local state, launch time, child ownership, and definition version.
- A `show-id` and `epoch` scope runtime work. Restart increments the epoch so old
  callbacks, held buttons, scheduled stops, and delayed packets cannot affect the
  new run.
- A layout revision identifies a particular page arrangement. A stale edit is
  resolved explicitly instead of overwriting unrelated changes.

Names and colors are editable metadata, never identifiers. A renamed effect
continues to resolve from saved cues and Live mappings. Copying a cue creates a
new `cue-id`, while linked references remain explicit. Deleting a definition
reports dependent cues and compositions; a running instance either keeps its
pinned definition or follows the explicitly selected retirement policy.

The runtime owns mutable infrastructure at its boundary. Authoring and playback
reducers consume values and return new values and ordered intents. Disk saves,
MIDI messages, network requests, and UI publication execute outside these reducers.

### Parameter definitions

Provide short helpers for common parameters, but normalize every helper to one
schema. A parameter definition records:

| Field | Purpose |
| --- | --- |
| Identifier and label | Stable machine key plus user-facing text. |
| Type and shape | Number, integer, boolean, enum, color, vector, range, fixture selection, or reference to a typed expression. |
| Unit and domain | Seconds, beats, degrees, meters, hertz, normalized intensity, or a fixture-specific physical domain; raw DMX is an explicit alternative. |
| Default and bounds | Default value, hard validity limits, recommended operating range, and allowed discrete values when relevant. |
| Scale and resolution | Linear/logarithmic/wrapped scaling, display precision, controller step size, fine adjustment. |
| Modulation | Supported input kinds, transform order, interpolation behavior, and whether the value may vary per fixture/head. |
| Randomization | Opt-in eligibility, distribution, recommended range, rate limit, maximum change, correlation group, and constraints. |
| Control metadata | Preferred editor, relative encoder behavior, velocity/pressure mapping, and whether the parameter is exposed. |
| Lifecycle | Per-instance or explicit show-wide scope; default/restore behavior and hot reload migration rules. |

Do not infer color units from the backing library: the current code mixes
`com.evocomputing.colors` and `thi.ng.color`. Normalize conversion at adapter
boundaries and document hue wrapping, linear/sRGB transfer, RGBW extraction,
alpha, and saturation/lightness ranges. Treat RGBW white contribution as a
fixture-specific calibrated operation, not universally `min(r,g,b)`.

Illustrative declarative parameter:

```clojure
{:id :level
 :type :number
 :unit :normalized-intensity
 :default 0.6
 :bounds [0.0 1.0]
 :recommended [0.15 0.75]
 :resolution 0.01
 :random {:enabled? true
          :distribution :uniform
          :max-change 0.15}
 :control {:velocity {:range [0.1 0.8]}
           :editor :slider}}
```

Validation returns paths and actionable messages, for example an inverted
`:recommended` range or an enum default absent from its options. Reject NaN and
infinity where they cannot have a defined output. Range parameters validate
jointly (`low ≤ high`), and expression references undergo cycle/type checking.
Explicit parameter overrides retain precedence over velocity, which retains
precedence over saved values, with definition defaults filling missing values.

### Declarative effects and code effects

The code-free effect format is a typed expression graph made from registered
operations: constants, fixture position/normal, time/phase, parameter input,
arithmetic, curves, noise, color operations, selection, and output assignments.
The first evaluator is a CPU reference implementation. The GPU compiler consumes
the same normalized graph; unsupported nodes remain visible CPU work or make
that backend unavailable for the graph. Never reinterpret arbitrary Clojure code
as though it were a safely compilable shader.

High-level Clojure helpers should produce the same descriptions where possible.
Custom Clojure effects can implement the pure runtime interface directly with
explicit context and state. The registry reports which effects have serializable
definitions, GPU support, sampleable previews, migratable state, and finite
duration. A callable function is not proof that it has all these properties.

Each effect declares the outputs it can affect and the capabilities it requires:
color/intensity, shutter, movement, focus/zoom, pixel position, fog, or other
fixture attributes. This metadata drives selection, page generation, control
labels, and auto-show reasoning. For code effects, undeclared capabilities mean
"unknown", not "safe to assume every fixture and parameter".

### Cue launch modes

Every cue exposes a common launch envelope. Its effect determines how that
envelope can end; the launch mode must not be hidden inside a controller handler.

| Mode | Contract |
| --- | --- |
| Toggle | Launch when inactive; request end of the addressed instance when active. |
| Held | Launch on press and release that same instance on matching release. Disconnect/cancel also releases it. |
| Timed | Launch for a specified logical duration, then request release. |
| One-shot | Execute one declared iteration; if no finite iteration exists, require a configured duration or terminal envelope. |
| Persistent | Continue until a stop/replacement/show lifecycle command. |

One-shot must be available through every control path. An indefinitely running
oscillator can use one cycle as its iteration only when that period is defined;
an arbitrary code effect needs an explicit duration contract. A code cue executes
its command once, and a repeat/retry policy cannot execute it twice accidentally.

Release permits an envelope or audio tail to finish. Immediate stop bypasses the
tail when requested. Instance ownership prevents a timed stop for yesterday's
launch from ending a newly launched cue with the same key. Repeated commands with
the same `command-id` are idempotent. `:end-keys` and priority/LTP/HTP behavior
remain compatible until a separately documented migration changes them.

Acceptance cases: explicit override beats velocity and saved value; a held cue
survives another unrelated finger lifting; a delayed release for instance A does
not stop replacement B; an infinite effect refuses an unspecified one-shot
duration with a useful error; a duplicate launch command creates one instance.

## Registry, page generation, and live reload

The registry exposes definitions, versions, parameter schemas, tags, fixture
requirements, previews, and lifecycle capabilities. Registration validates first
and publishes one complete revision. Failed compilation leaves the last valid
version available and reports the error without partially replacing the show.

Generated pages support all registered effects or a filtered subset, grouped by
category, fixture compatibility, tag, or preferred performance role. Sort order
and slot assignment are deterministic. Generate a draft layout, report collisions,
then apply its revision; never overwrite manual pages implicitly. An update can
add new entries and retain existing cue identities and user customizations.
Removing a registry item leaves a recoverable unresolved reference rather than
silently putting an unrelated cue in its place.

Manual and generated layouts coexist. The UI can drag, duplicate, link, cut,
paste into an empty slot, or swap with explicit intent. Page membership determines
auto-mode scope; selection and launch address stable cues independently of page
coordinates. Device viewports are projections of these layouts.

Hot reload operates on a validated definition revision at a frame boundary:

1. Compile and validate a candidate off the render thread. Compare schemas and
   dependencies with the current version.
2. For each live instance, select its declared policy: migrate state and retain
   compatible parameters, restart from the current logical time/phase policy, or
   defer the update until the next launch. Present disruptive choices clearly.
3. Apply one registry revision and instance transition atomically for a frame.
   A frame cannot use half the old graph and half the new graph.
4. Publish success with affected instance IDs, or leave the prior version intact
   and publish diagnostics. Keep a revertable version history.

State migration is a pure function of old state, old/new versions, parameters,
and explicit time. Removed parameters follow a declared discard/mapping policy;
new ones use defaults. A parameter type change cannot silently reinterpret old
values. Persist definitions and user parameter presets separately from running
instances. A clean restart starts with no stale active cues; an explicit resume
operation restores a saved session into a new epoch and new instance identities.

Acceptance cases: moving a cue leaves a macro/Live mapping intact; editing a
running color formula changes its output on one boundary while preserving the
level knob; an invalid edit keeps the previous output; an incompatible reload
reports its restart policy; an old worker completion cannot resurrect a stopped
show; generating a page twice creates no duplicates or manual-layout loss.

## Shared slot editor and composition semantics

Build one reusable slot editor with different modes. It can be used from the
web, Push, or an agent; the editor is a view over domain data.

**Macro/compound mode** is a collection of chosen cue/effect references and
parameter overrides. Slots describe membership and optional presentation order;
there is no playback cursor or implied clock. Launch can either create owned
children together or explicitly launch independent cues. Owned children are
ended with their parent, while unrelated replacements are left alone. The UI
must explain that independent launch mode has no parent lifetime.

**Scene mode** combines effect outputs into one instance with documented mixing
and priority rules. This differs from launching several independently adjustable
cues. Converting a macro to a scene reports controls or lifecycle behavior that
cannot be retained.

**Sequence mode** adds a time axis and playback cursor to the same slots. Each
step specifies a reference, duration, optional rest, parameter overrides,
transition, probability, and trigger style. Compound sequences can address cues
or effect definitions, but must not ambiguously change ownership between them.
Nested compositions validate dependency cycles and bound recursion.

All modes support selecting an unlaunched cue, adding it to a collection,
reordering, copying, pasting, removing, and inspecting references. Adding a cue
to a macro must not launch it. A long-press radial menu may expose these actions,
with explicit buttons/menus and keyboard equivalents available as well. Touch
dragging and scrolling must not inadvertently trigger cues.

### Sequence time and transitions

Use an explicit musical or absolute timebase per sequence. Define what a tempo
change does to an in-progress beat-based step; it follows musical phase while a
seconds-based duration follows monotonic elapsed time. Quantization can target
beat/bar/phrase boundaries and is applied once to the launch intent. Seeking,
transport rewind, pause/resume, and missed-frame recovery need explicit behavior.

Playback modes include once, loop, ping-pong, seeded random with replacement,
and seeded shuffle without replacement. Ping-pong does not duplicate endpoint
steps unless configured. A one-slot sequence is valid. Empty sequences report a
validation error or act as an explicit rest, according to the chosen construct.
Do not use unbounded retries to choose a non-repeating step from a singleton.

Transitions support a hold interval plus a fade interval, with linear, smooth,
sine, exponential, and editable bounded curves. Define the curve domain `[0,1]`,
endpoint behavior, overshoot policy, and interpolation by value type. Hue uses a
declared wrap direction; fixture selection and enums use discrete switching;
pan/tilt transitions respect calibrated movement paths and mechanical limits.
An adjustable fade can occupy a fraction of a step or an explicit duration.
Changing step length must not silently stretch an explicitly fixed duration.

The scheduler emits ordered launch/update/release intents. It does not invoke
I/O from a frame callback. Work is bounded when the transport jumps across many
steps: continuous values can sample the destination directly; skipped one-shots
follow an explicit skip/catch-up policy and a bounded event budget.

Acceptance cases: macro slots have no unintended timing; simultaneous children
launch at one logical boundary; a three-step ping-pong produces `1,2,3,2,1`;
shuffle visits each enabled step once per cycle; random playback is reproducible
from seed and command log; tempo changes and transport seeks have deterministic
results; stopping a parent releases its children exactly once.

## Tween and response envelopes

A tween is a data description of interpolation over logical time, not a timer
that mutates a variable. It declares targets, keyframes, units, easing, duration,
repeat mode/count, delay, and completion behavior. Evaluation receives an
explicit time and returns a value plus any lifecycle transition. Effects can be
written from tween descriptions instead of a per-frame imperative callback.

ADSR envelopes share the same curve machinery. Attack and release are first-class
controls; decay and sustain may be omitted for a transient response. Zero-duration
stages have defined boundary behavior. Release starts from the current envelope
value, including during attack, and retrigger behavior is selectable between
restart and continuation from current level. Envelope state lives in the
instance context so worker scheduling cannot change its result.

An envelope cue can target all eligible effects, selected instances, a group, or
specified parameter roles. It does not recreate the underlying effects. Targets
can be captured at launch or dynamically follow a selection; the UI labels this
choice. Targeting `:intensity` must not accidentally scale movement coordinates.
Multiple envelopes compose using declared multiply, replace, max, or add-with-
clamp semantics. Processing order with parameter modulation, effect mixing,
master intensity, and fixture encoding is part of the runtime contract.

Attack/release time constants and a trigger envelope are distinct: a response
smoother follows a changing input; a triggered ADSR follows a gate. The UI offers
both with truthful labels, and neither secretly depends on render frame count.

Acceptance cases: sampling at 30 or 120 frames per second gives equal values at
equal logical times; releasing halfway through attack is continuous; a zero-time
attack avoids division by zero; an intensity-only envelope leaves aim/focus
unchanged; nested envelopes have stable order; a finished one-shot releases its
owned state and contributes no unintended assignments.

## Controlled randomization

Randomization is an explicit operation over eligible parameters and cues, with
an immutable seed/state. Every decision records the seed lineage and resulting
values so previews, replay, regression tests, and agents can explain it.

The operator chooses targets, parameter roles, recommended or custom ranges,
distribution, change magnitude, correlation, and frequency. Hard schema/fixture
bounds still apply. By default, unmarked parameters are ineligible; reset,
calibration, addressing, and transport controls must not enter a lighting
randomizer because they happen to be numeric.

Constraint examples include lower ≤ upper bounds; legal beat divisions; positive
periods; movement inside calibrated limits; compatible saturation/white levels;
and palette rules such as saturated reds but desaturated non-reds. Generate
jointly valid tuples rather than independently sampling each field and repairing
them unpredictably. Infeasible constraints produce a visible no-op/error, never
an unbounded rejection loop. Volatility zero preserves values exactly.

Random choices may be per show, instance, fixture, or pixel. Define this scope
and derive stable substreams from identities; adding a pixel or evaluating on a
different worker must not change unrelated choices. Repeated evaluation of the
same logical sample gives the same value. Avoid `rand` inside pure render nodes.

Temporary randomization can restore prior values on expiry, but only when the
value is still owned by that operation. A human change after randomization must
not be overwritten by a stale restore callback. "Randomize again" and "revert"
are explicit commands with undo history.

Acceptance cases: bounds and pair constraints always hold; identical seeds and
inputs reproduce output across worker counts; colors and booleans do not pass
through numeric-only sampling; impossible constraints return promptly; selection
excludes locked parameters; a human edit survives an expired temporary change.

## Auto-show playback

Auto mode is a deterministic planner plus scheduler over the registry, rig,
running state, operator constraints, and explicit clock/seed. It selects one or
more cues at configurable intervals and launches them persistently, for a bounded
duration, or as one-shots. Planning never blocks rendering.

Scope can be the current page, named pages, selected cue sets, or all available
pages. "Current page" must identify the controlling session and whether scope
tracks that session's navigation or captures its page at start. Device-local
viewports cannot silently fight over the same auto-show scope.

Capability metadata gives the planner enough information to avoid invisible
movement. A movement cue declares that it requires emission on matching fixtures;
an intensity/shutter/color cue can satisfy that requirement. The planner checks
the resulting combined state, including global dimmer, shutter, black colors,
and conflicts. It chooses a compatible light-producing companion when needed,
or declines the choice with an explanation. Prerequisites may be declared as
explicit dependencies or satisfied by already-running human/auto effects.

This rule allows intentional darkness, movement in blackout for repositioning,
and a single spotlight. Such choices are explicit scene intents, not mistakes
that the planner "fixes" by lighting everything. Maximum active fixtures, group
weights, density, contrast, palette, beat accents, transition pacing, and strobe
eligibility are configurable creative constraints. Human-owned cues can be
pinned so the planner does not replace them.

Ownership matters: auto mode only stops instances it launched unless explicitly
configured otherwise. Turning it off can release its instances or leave them
running according to a selected policy. It cannot stop a replacement launched by
the operator under the same cue key. Dependency groups start and end coherently,
without briefly removing the only light source for a retained movement effect.

Use fast deterministic rules for beat-level scheduling. An optional model can
propose longer-term scenes, new definitions, or revised plans asynchronously;
those proposals pass the same validation and installation path as human edits.
A slow model, provider outage, or invalid suggestion leaves the last validated
plan running. The UI exposes current intent, upcoming actions, ownership, and a
concise reason for each choice.

The user's example brief becomes a useful end-to-end acceptance scenario:
mostly red with occasional desaturated purple/other colors, beat-driven
one-shots, evolving definitions and selection, deliberate alternation between
smooth/chill sections and strobing, darkness punctuated by flashes, and sparse
individual/group use. Tests should verify the declared constraints and timing;
an operator evaluates artistic quality in the simulator and then with fixtures.

Acceptance cases: page-only mode cannot launch outside its scope; one-shots
finish while timed cues respect their durations; movement gains a compatible
emission cue; explicit blackout remains dark; a human-pinned spotlight is
retained; duplicate ticks do not duplicate launches; a model outage does not
stall the show; show restart discards every old scheduled action.

## Delivery order and evidence

1. Characterize current cue precedence, composition, fixture construction, and
   parameter behavior. Record broken or untested paths separately from green
   tests; avoid importing startup namespaces merely to test a pure helper.
2. Introduce versioned domain data, validation, registry identities, and the
   shared command reducer behind legacy adapters. Test serialization and
   deterministic replay without controllers or a network.
3. Implement launch modes, envelopes/tweens, and owned composition. Test exact
   lifecycle boundaries and stale-instance handling before adding the editor.
4. Build the shared slot editor and generated pages using those commands; test
   iPad/phone gestures and an accessible non-gesture path.
5. Add constrained randomization and deterministic auto mode, followed by
   asynchronous agent planning. Keep hardware acceptance distinct from unit
   and simulator evidence.

Each feature entry in the tracker should link its code, tests, current owner,
dependencies, and known limitation. These acceptance cases are specifications;
listing one here does not mean it has been implemented or verified.
