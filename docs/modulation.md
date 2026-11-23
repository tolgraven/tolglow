# Musical parameter modulation

`tolglow.modulation` implements recurring parameter randomization and curved
transitions over running effects. It advances an explicit seed and beat schedule
once in the show owner, then returns immutable per-instance parameter overlays
for effect workers. Canonical parameter values do not drift as frames run.

This is distinct from launch-time randomization: a controller can choose new
values every fraction of a beat, beat, bar or phrase for as long as it runs. The
pure module, actual compiler/engine integration tests, cue reconciliation and
restore helpers are implemented. Host/UI integration is coordinated separately;
these APIs do not open devices or install a scheduler when required.

## Configuration

```clojure
{:id :movement-randomizer
 :seed 42
 :every {:beats 1/4}
 :select {:categories [:movement]}
 :params :all
 :mode :glide
 :curve :smoothstep
 :priority 5
 :ranges {:speed [0.1 0.8] :pan [0.25 0.75]}
 :distribution :triangular
 :max-change 0.2}
```

`validate-config` returns path/message errors. Defaults are seed `0`, period one
beat, selection of all instances, all parameters, `:sample-hold`, linear curve,
priority `0`, owner exclusion and a maximum catch-up budget of 1024 samples per
controller per frame. An ID is required by `start`; an effect's embedded config
can omit it because cue reconciliation derives an activation ID.

| Setting | Contract |
| --- | --- |
| `:every` | One positive `:beats`, `:bars` or `:phrases` value; ratios and decimals are accepted. Milliseconds are not a modulation period. |
| `:select` | Optional vectors `:instance-ids`, `:effect-ids`, `:cue-ids`, `:categories`. Supplied selectors intersect; values within one vector are alternatives. An empty vector matches nothing. |
| `:params` | `:all` or distinct parameter keywords. Explicit keys absent from an effect are skipped, allowing heterogeneous selections. |
| `:mode` | `:sample-hold` changes at each boundary. `:glide` transitions from the prior value to the new random destination over that period. |
| `:curve` | Linear, smoothstep, sine, ease-in, ease-out, step, or `{:type :power :exponent positive-number}`. |
| `:ranges` | Optional parameter-key to ordered numeric pair; intersected with that parameter's hard bounds. |
| `:distribution` | Uniform or triangular; omitted means use the parameter's random metadata/default. |
| `:max-change` | Optional nonnegative componentwise change limit; otherwise use parameter metadata. |
| `:priority` | Finite precedence. Later activation wins at equal priority, independently for each overlapping parameter. |
| `:exclude-owner?` | Defaults true for a modulator cue; explicitly false permits targeting its own effect instance. |
| `:max-catch-up` | Integer 1–100000; exceeding it fails the whole advance instead of skipping random draws. |

Categories include the registry's union derived from controlled output attributes
(such as dimmer → intensity and aim/pan/tilt → movement), plus `:category` or
`:categories` on the captured effect, instance or cue. Selection uses the definition version that the engine will
evaluate after an opted-in reload. Pinned effects retain their captured schema.

The meter is captured at activation. In quarter-note transport units a 7/8 bar
is 3.5 beats; two bars per phrase therefore make a seven-beat phrase. Later meter
or BPM changes do not reinterpret an existing controller's period. A paused
transport with unchanged beat does not consume RNG or advance interpolation,
even if wall time advances.

## Parameter domains and interpolation

All seven current parameter schema types are supported: number, integer,
boolean, enum, color, vector and range. Explicitly enabling a controller is the
opt-in; it can modulate parameters whose separate launch-randomization metadata
is disabled.

Numeric domains come from the controller's parameter range, then schema
`:random :range`, `:recommended`, or hard `:bounds`. Colors have the intrinsic
linear RGB domain `[0,1]`. Hard bounds always constrain the result; maximum
change is measured from the previous selected value, componentwise for vectors.
The existing explicit seeded random primitive supplies reproducible draws.

An unbounded numeric parameter without a configured meaningful range is reported
in `:diagnostics` as `:modulation/skipped`. It gets no guessed range and does not
block other parameters. Setting its range makes it eligible at the next sample.
An empty intersection or unsupported distribution also produces a diagnostic.
Enums choose declared values; booleans choose true/false. Range pairs stay sorted.

Glide interpolates numeric values and equally shaped numeric vectors. Integers
are rounded and constrained to valid integers. Boolean/enum values remain
discrete, switching at sample boundaries even in glide mode. The first glide
starts from the current canonical parameter value, and each following glide
starts at the preceding destination. Curves can make the transition smooth or
stepped without depending on how many rendering frames were sampled.

## Owner and worker boundary

```clojure
(require '[tolglow.modulation :as modulation]
         '[tolglow.compiler :as compiler]
         '[afterglow.runtime.engine :as engine])

;; Explicit activation, usually dispatched by a control action.
(def state
  (modulation/start (modulation/new-state)
                    {:id :level-toggle :seed 42 :every {:beats 1/2}
                     :select {:instance-ids ["0:1"]} :params [:level]}
                    {:beat 0.0 :meter {:beats-per-bar 4 :bars-per-phrase 4}}))

;; Once per captured owner frame, before dispatching effect work.
(defn advance-frame [show frame]
  (let [{:keys [state overrides]} (modulation/advance (:modulation show) show frame)
        show (assoc show :modulation state)
        overlay (modulation/middleware overrides)]
    (engine/advance show frame
                    (fn [definition]
                      (comp (compiler/compile-effect definition) overlay)))))
```

The example's `frame` includes fixtures for the compiler, an explicit time and
beat (or model transport). A production host can reuse its compiled step cache
and owned mapper. With response envelopes, the composition order is
`(comp compiled-effect response-middleware modulation-middleware)`: modulation
selects parameter values and the response then shapes them for that frame.

`advance [state model frame]` returns:

```clojure
{:state ...
 :overrides {"0:1" {:level 0.6}}
 :events [{:type :modulation/sampled :controller-id :level-toggle
           :samples 1 :ticks 3 :next-beat 1.5}]
 :diagnostics []}
```

The state includes controller order, seed, captured meter, anchor/next beat,
sample count, and numeric interpolation endpoints. It contains no atoms,
functions or arrays. Advancing the same input gives the same result; calling it
again with its returned state at the same beat consumes no new RNG. Effects
share the returned overlays as read-only data and do not advance controllers.

`middleware [overrides]` changes only `[:instance :params]` in a frame context.
The engine retains canonical parameters from its captured instance descriptor.
It validates overlays against that effect's schema and fails before evaluation
if they do not fit. Target reloads use new defaults and retained compatible
parameters, so removing or adding a schema key does not leave obsolete overlays
that prevent the first reloaded frame from completing.

Controllers sample in priority/activation order. Within one controller, instance
IDs and parameter keys have stable ordering (`:all` sorts keys; an explicit key
vector keeps its order). A late frame samples every missed boundary with the
**supplied model**, retaining RNG and next-beat continuity. With unchanged target
membership/parameters, sparse and dense frame sampling agree. It cannot invent
historical edits or sensor inputs that were not captured. New instances join at
the next sample boundary; removed/unselected instances lose stale cells.

## Human control and toggles

The owner applies a human parameter edit through the normal control command,
then calls the pure `hold` helper:

```clojure
(modulation/hold state {:instance-id "0:1" :params [:level]
                        :until-beat 8.0})
(modulation/hold state {:instance-id "0:1" :params [:level]}) ; until resumed
(modulation/release-hold state "0:1" [:level])
(modulation/stop state :level-toggle)
```

A hold suppresses every controller's overlay for those instance parameters.
It clears their interpolation cells and consumes no random draws for them while
held. At the next eligible sample after resuming, glide starts from the new human
value. A timed hold expires exactly at its deadline; an indefinite hold requires
release. Ending a controller exposes the existing canonical value immediately.
These functions are pure despite describing control actions.

The UI should distinguish canonical value, current modulated value, period/mode,
and held status. Per-parameter toggles and global panels use the same controller
config, with selectors defining scope. Configuration updates use a deliberate
new `start`, resetting that activation's seed/schedule; stop removes it. They
should not rewrite current canonical parameter values every frame.

## Launchable modulator cues and lifecycle

An ordinary effect definition can contain a `:modulation` map alongside lighting
outputs, or alone for a control-only effect. The usual cue references its
`effect-id`; it therefore uses the same launch/held/one-shot/release/reload
lifecycle as other cues. Registry/compiler support and host dispatch belong to
the shared integration boundary, not a second independent cue system.

`reconcile-cues [state model frame]` derives controllers with IDs
`[:cue instance-id]` and records their `:owner-instance-id`. `advance` already
calls it, so ordinary hosts need not invoke it separately. Owners are excluded
from their own target selection by default. Removal, entering release and exact
duration expiry stop a controller before it can influence that boundary's frame,
even if the model has not yet been advanced by the engine.

Reconciliation follows engine reload-before-expiry ordering. Pinned owners keep
their captured config. With preserve-state reload, unchanged config retains
RNG/schedule; changed modulation config restarts at the current beat using its
declared seed. A restart reload restarts the controller even when config is
unchanged. These rules are explicit and tested rather than treating every
registry refresh as a new random activation.

## Persistence and restart

`validate-state` checks saved data without sampling. Ordinary EDN serialization
preserves exact integer seeds, ratios, next-beat and interpolation endpoints.
An actual show/epoch change clears stale controllers and holds. Restore is a
different operation:

```clojure
(modulation/restore-state saved-state
  {:show-id :main :epoch 7
   :instance-ids {"0:1" "7:4" "0:2" "7:5"}})
```

The helper remaps cue owner identities, instance selectors, cells and manual
holds while preserving exact seed/schedule. Missing owners are removed; missing
instance-specific bindings/values are dropped. The ID map must be injective.
Restored beat coordinates must use the saved transport timeline; moving to an
earlier beat is rejected. Owner config provenance is retained so remapping an
instance selector does not look like a user edit and restart RNG on the next
reconciliation. The host must call this helper during an epoch-changing import
before the first new frame.

An external Live loop, seek or clock-source epoch is a clock discontinuity, not
ordinary scheduling backlog. The host explicitly calls
`(rebase-clock state {:beat new-beat})` before advancing that frame. This shifts
anchors, next sample beats, interpolation deadlines and finite manual-hold
deadlines by `new-beat - last-beat`, preserving RNG, tick counts, intervals and
the current glide phase. Before any sampled frame, supply `:from-beat` too.
Large forward seeks use the same explicit operation and do not consume thousands
of invented random samples. Requiring that signal distinguishes a transport jump
from an actual missed-frame backlog; ordinary backward input remains an error.

The host must apply the same timeline policy to the engine's instance timers and
last-frame beat. This helper only owns modulation state. A catch-up-budget error
contains controller ID, current/next beat, minimum samples and explicit recovery
choices; it returns no partially advanced RNG state. It is not silently retried
as if the transport had continued normally after a known seek.

## Verification and limits

The focused `tolglow.modulation-test` suite passed **16 tests / 124 assertions**
with reflection warnings enabled and none emitted. It covers musical fractions and captured
meters, pause and catch-up equivalence, schema types/bounds, missing ranges,
curved glides, categories and precedence, manual holds, cue timing/reload,
portable state restoration, explicit loop/seek rebasing, target-schema reload,
and actual compiler/engine worker equivalence.
Canonical parameters remain unchanged across those frames. The replay lane in
[runtime-replay.md](runtime-replay.md) supplies complementary explicit-state
recording infrastructure; a complete host trace must also capture modulation
activation, holds, state updates and external inputs.

This module does not yet implement correlated perceptual-color sampling,
constraint solving across parameters, audio-reactive modulation, or learned
movement policies. It does not guarantee that every mathematically valid range
looks good on a particular rig. Numeric domains and category metadata are
authoring data that the UI can expose for deliberate control.

## Waveforms on a running instance

Use the instance ID from a launch receipt to attach a waveform to one parameter. Two launches of the same cue can use different waveforms and ranges. The base parameter stays editable. Removing the waveform reveals the underlying input again, including another random or patch source if one is active.

```clojure
(show/submit! runtime :modulation-bind
  {:instance-id instance-id :param :level
   :binding {:type :lfo :waveform :pulse :period {:beats 1/2}
             :phase 0.25 :inputs {:duty 0.35} :from 0.15 :to 0.9}})
(show/submit! runtime :modulation-hold {:instance-id instance-id :params [:level]})
(show/submit! runtime :modulation-resume {:instance-id instance-id :params [:level]})
(show/submit! runtime :modulation-unbind {:instance-id instance-id :param :level})
```

Studio uses the same operations through typed Transit POSTs to `/api/modulation/bind`, `/hold`, `/resume` and `/unbind` under the modulation route. Binding again edits the current source. Musical periods accept beats, bars or phrases. Phase runs in cycles on the activation's private beat clock and captured meter. Holding the parameter exposes its base while the phase continues. Pausing the effect freezes its private clock. Ending the activation releases its bindings and the compiled plan is retired on the next frame.

Registered waveform values are normalized from their declared range, clamped to 0–1, and interpolated between the two typed endpoints. Colors, coordinates and ranges interpolate component-wise. Integers round to the nearest value and boolean or enum inputs select the second endpoint at halfway. Live revisions of the selected waveform recompile its plan. A composite authored with `signals/sample` captures the nested expression at publication. Republishing the composite picks up a new constituent waveform. An incompatible destination or waveform input is reported on that source while the underlying parameter remains usable. Show files retain bindings, original schema, waveform registry and manual holds across instance-ID changes.

Committed Studio snapshots include the registered waveform choices in `:parameter-waveforms`. Each bound entry under `[:instances n :parameter-sources param]` exposes its base value, effective input and sources, including held and constant-valued sources. A random controller waiting for its next legal sample is marked pending. A rejected sample carries a diagnostic on that frame. The input pipeline applies the random controller, instance waveform, spatial overrides and response middleware in order. Parameter blocks inside an effect can yield a different result per fixture. Live destinations list the MIDI controller and current ports while the lighting bridge owns them. Handing the device back to Live or stopping the input removes that source. Patch destinations include their captured route and source IDs, including audio pitch or level. A spatial field reports its configured variable, cue parameter or volume control. Their authored block source contains the path for `:runtime/block-trace`, which returns the committed result for the requested fixture.
