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
| `:every` | One positive `:beats`, `:bars` or `:phrases` value between 1/1024 and 2^40 beats; ratios and decimals are accepted. Milliseconds are not a modulation period. See [period range](#period-range). |
| `:select` | Optional vectors `:instance-ids`, `:effect-ids`, `:cue-ids`, `:categories`. Supplied selectors intersect; values within one vector are alternatives. An empty vector matches nothing. |
| `:params` | `:all` or distinct parameter keywords. Explicit keys absent from an effect are skipped, allowing heterogeneous selections. |
| `:mode` | `:sample-hold` changes at each boundary. `:glide` transitions from the prior value to the new random destination over that period. |
| `:curve` | Linear, smoothstep, sine, ease-in, ease-out, step, or `{:type :power :exponent positive-number}`. |
| `:ranges` | Optional parameter-key to ordered numeric pair; intersected with that parameter's hard bounds. |
| `:distribution` | Uniform or triangular; omitted means use the parameter's random metadata/default. |
| `:max-change` | Optional nonnegative componentwise change limit; otherwise use parameter metadata. |
| `:priority` | Finite precedence. Later activation wins at equal priority, independently for each overlapping parameter. |
| `:exclude-owner?` | Defaults true for a modulator cue; explicitly false permits targeting its own effect instance. |
| `:max-catch-up` | Integer 1–100000. A longer backlog skips to the current boundary and reports `:modulation/rebased`; see [catch-up budget](#catch-up-budget). |

Categories include the registry's union derived from controlled output attributes
(such as dimmer → intensity and aim/pan/tilt → movement), plus `:category` or
`:categories` on the captured effect, instance or cue. Selection uses the definition version that the engine will
evaluate after an opted-in reload. Pinned effects retain their captured schema.

The meter is captured at activation. In quarter-note transport units a 7/8 bar
is 3.5 beats; two bars per phrase therefore make a seven-beat phrase. Later meter
or BPM changes do not reinterpret an existing controller's period. A paused
transport with unchanged beat does not consume RNG or advance interpolation,
even if wall time advances.

### Period range

A period must be between `min-period-beats` (1/1024 beat) and
`max-period-beats` (2^40 = 1,099,511,627,776 beats).

- **Shortest.** 1/1024 beat is 0.33 ms at 180 BPM and 0.84 ms at 70 BPM, which
  is shorter than any lighting frame. A faster period would spend the catch-up
  budget on samples that no output can show. With the default budget of 1024,
  a frame may span just under one beat at this period before the controller
  skips its backlog (see [catch-up budget](#catch-up-budget)).
- **Longest.** The schedule adds a period to a beat coordinate. With an anchor
  and a period both at most 2^40 beats, the next boundary stays below 2^41
  beats. A double still resolves 2^-11 beat there, half the shortest period,
  so every period in the range advances its schedule. A longer period, such as
  `{:bars 1e308}`, overflows the boundary or rounds it back onto its anchor. It
  cannot start or schedule. 2^40 beats is about 11,600 years at 180 BPM.

`validate-config` refuses a period outside the range in the default meter
(four beats per bar, four bars per phrase). Effect publication
(`compiler/definition-errors` with `{:publication? true}`, which Tolglow's
publish gate runs), `:modulation-cue` and `:modulation-start` use it. The error has the path
`[:every <unit>]`, or `[:modulation :every <unit>]` in an effect, and a message
that names the period, its length in beats and the range:

```clojure
(modulation/validate-config {:every {:bars 1e308}})
;; => [{:path [:every :bars]
;;      :message "Modulation period :every {:bars 1.0E308} is beyond the finite beat range in the default meter (4 beats per bar, 4 bars per phrase); a period must be between 1/1024 and 2^40 (1099511627776) beats"}]
```

A bar or phrase period is checked again against the meter that the controller
captures when it starts: the owner's launch meter on a first launch, and the
frame's meter on a reload. A controller is refused alone when:

- a cue owner's period falls outside the range in that meter, or its config
  does not validate;
- a saved controller's `:interval-beats` is outside the range;
- its schedule stops advancing at a very large beat coordinate.

The owner effect still renders, and every other controller and effect keeps
its frame. A `:modulation/refused` diagnostic carries the controller ID, owner
instance, path, meter and message. It appears in the `[:modulation :diagnostics]`
projection, which the Studio modulation panel lists.

An owner's refusal is recorded in the state under `:refused`, keyed by its
controller ID, with the owner version, modulation config and priority it was
judged on. While those are unchanged the owner is not attempted again and
reports the recorded diagnostic each frame. A new revision, config or priority
starts the controller like a reload, at that frame's beat and meter, not like a
first launch that would catch up over the cue's whole life. The record follows
`restore-state` and goes when the owner ends.

A refusal on reload is judged in the frame's transport meter, and a first
launch in the owner's captured launch meter. The refusal then holds until the
owner's revision, modulation config or priority changes. A later change of the
transport meter alone does not retry it; publish a new revision to try again.

A refused global controller (one started with `:modulation-start`) leaves its
schedule and keeps a record under its own ID. It reports the same diagnostic
every frame until `:modulation-stop` with that ID clears it, or a new
`:modulation-start` with that ID replaces it. `restore-state` keeps it.
`:modulation-stop` on a refused cue owner ends the owner, as it does for a
running one.

Saved shows load without the range check: registry and running definitions
(`compiler/definition-errors` without `:publication?`), saved controller
states and saved waveform bindings. A backup that an older owner wrote keeps
its contents, and its frames refuse the affected controller alone.

A new waveform binding (`:modulation-bind`) uses the same range for its
`:period`, measured in the meter its phase follows: the transport meter for
`:phase-reference :transport`, otherwise the cue's captured meter. Binding
sampling has no schedule, so this is shared vocabulary rather than a precision
limit.

### Catch-up budget

A late frame samples every missed boundary, up to `:max-catch-up` samples per
controller per frame. A longer backlog never fails the frame. The controller
skips the missed boundaries: its schedule moves to the last boundary at or
before the frame's beat, its tick count advances by the skipped boundaries,
and it samples that boundary once. It keeps its own grid and consumes one
random draw, as a stopped transport resumes without catch-up. The frame
reports `:modulation/rebased` with the controller ID, the number of skipped
steps, the budget, the next beat and the current beat. The skip is
deterministic for a given state and beat.

Within the budget, a frame that jumps to a beat reaches the same state as
frames that step there. A skip breaks that equivalence: the controller draws
once at the last boundary instead of once per boundary, so two timelines that
reach the same beat with different frame timing can diverge in seed and
values. A deterministic replay must therefore record each frame's beat, not
only the transport timeline. Seeding each draw from the controller's seed and
tick count, rather than from the previous draw, could remove this divergence;
that is a possible follow-up, not implemented here.

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

### A frame with a pending hot reload

`reload-candidate [model instance]` is the newer definition the engine attempts
for an instance on the next frame, and `owner-definition` is that candidate or
the captured definition. A pinned or paused instance has no candidate, and
neither does one whose retained parameters the newer schemas reject, because
the engine refuses that revision at preparation. During a parameter ramp the
show owner reads candidates through `(modulation/with-frame-params model
ramp-params ramp-targets)`: a retained parameter must then fit with its
committed value, the ramp's frame value and the ramp's target. Where a revision
refuses the committed value or the target, `ramp-preparation` gives the engine
those values for that instance, so the engine also refuses the revision instead
of adopting one that cannot hold the value the model keeps or the ramp commits.
See [scheduler](scheduler.md#ramp-semantics).

A controller follows candidates in both roles. Its owner's candidate supplies
the `:modulation` config, and each target's candidate supplies the parameter
schemas. Only the engine decides whether a candidate is accepted, and it decides
during the frame those samples feed. So the owner samples both outcomes from the
same prior state and lets the engine read one of them:

```clojure
(defn advance-frame [show frame]
  (let [candidate (modulation/advance (:modulation show) show frame)
        last-good (when (modulation/reload-pending? show)
                    (modulation/advance (:modulation show) (modulation/last-good-view show) frame))
        overlay (if last-good
                  (modulation/reload-middleware (:overrides candidate) (:overrides last-good))
                  (modulation/middleware (:overrides candidate)))
        result (engine/advance (assoc show :modulation (:state candidate))
                               (cond-> frame
                                 last-good (assoc ::engine/reload-fallback modulation/last-good-frame))
                               (fn [definition] (comp (compiler/compile-effect definition) overlay)))]
    (cond-> result
      (= :fallback (:reload-inputs result))
      (assoc-in [:model :modulation] (:state last-good)))))
```

`last-good-view` is the model as a show reads it when the revision was never
published: every instance keeps its captured definition. Sampling it is a pure
call on the same prior state, so no seed, schedule or cell advances twice.
With `::engine/reload-fallback` the engine attempts the candidates first. If it
accepts them all, the whole frame reads the candidate overlays and
`:reload-inputs` is `:frame`. If it rejects one at preparation, compilation,
migration, evaluation or renderer output, every instance renders its captured
revision with the last-good overlays and `:reload-inputs` is `:fallback`; a
healthy revision published for that same frame waits one frame, without a
fault. The owner commits the state behind the inputs that were read. The frames
and the controller's seed, ticks, anchor and cells are then those of a show
that never published the rejected revision. An accepted revision still
switches its controller once, on the takeover frame. In a scheduled
show a revision waits while its programs compile off the frame; modulation
reads the captured definition, as the engine does, until the frame that
adopts it ([effect failure isolation](effect-fault-isolation.md)). The show owner supplies
the fallback only when the two outcomes differ, so an ordinary hot reload keeps
the engine's single pass. Spatial cue fields are the other input that can
differ between the outcomes, and readers that commit before the engine
(spatial cues, autopilot) take the last-good samples while a reload is
pending ([effect failure isolation](effect-fault-isolation.md)). See Afterglow's
[effect failure isolation](../../afterglow/doc/architecture/effect-fault-isolation.md#frame-inputs-that-follow-a-candidate).

Beat-clock [ramp overlays](scheduler.md#ramp-semantics) do not follow a
candidate. Both outcomes rest on the same ramp overlays, under the modulation
and waveform overrides as usual. `candidate-overrides [model overrides]`
restricts them for an instance with a pending candidate to the parameters the
candidate's schemas accept, while the last-good revision reads them unchanged.
After a renderer rejection the owner removes the ramp overlay from the rebuilt
engine result, so the rejected frame does not commit a ramp's frame value.

A [Room audition](repl-studio-parity.md#audition-a-scene-in-the-room) renders
through the same frame function on its private timeline. A draft that publishes
a revision of a running cue meets a pending reload there, and samples both
outcomes the same way. The private timeline keeps the reload fault and the
controller state behind the inputs the engine read; the live show's model,
controllers and packets are not touched. A draft whose revision the engine
rejects therefore auditions exactly the live frames.

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
registry refresh as a new random activation. They apply to a reload the engine
accepts. A revision it rejects leaves the controller exactly as it was, under
`:preserve-state`, `:restart` and `:migrate` alike; see
[a frame with a pending hot reload](#a-frame-with-a-pending-hot-reload).

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
last-frame beat. This helper only owns modulation state. Without a rebase, a
backlog beyond the catch-up budget skips to the current boundary as described
under [catch-up budget](#catch-up-budget); a rebase keeps the glide phase and
every tick instead.

## Verification and limits

The focused `tolglow.modulation-test` suite covers musical fractions and captured
meters, pause and catch-up equivalence within the catch-up budget (beyond it,
see [catch-up budget](#catch-up-budget)), backlog skips, period refusals and
their records, schema types/bounds, missing ranges,
curved glides, categories and precedence, manual holds, cue timing/reload,
portable state restoration, explicit loop/seek rebasing, target-schema reload,
reload candidates and both reload outcomes, and actual compiler/engine worker
equivalence. `tolglow.effects.modulation-reload-test` runs the show owner beside
a twin that never published the revision and compares captured DMX and
controller state frame by frame, for each reload policy and rejection stage.
Canonical parameters remain unchanged across those frames. The replay lane in
[runtime-replay.md](runtime-replay.md) supplies complementary explicit-state
recording infrastructure; a complete host trace must also capture modulation
activation, holds, state updates, external inputs and each frame's beat.

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
             :phase-reference :transport :phase 0.25
             :inputs {:duty 0.35} :from 0.15 :to 0.9}})
(show/submit! runtime :modulation-hold {:instance-id instance-id :params [:level]})
(show/submit! runtime :modulation-resume {:instance-id instance-id :params [:level]})
(show/submit! runtime :modulation-unbind {:instance-id instance-id :param :level})
```

Studio uses the same operations through typed Transit POSTs to `/api/modulation/bind`, `/hold`, `/resume` and `/unbind` under the modulation route. Binding again edits the current source. Musical periods accept beats, bars or phrases. The **Phase (cycles)** offset adds the same fraction of a cycle for either setting of **Phase reference**. Choose the phase reference independently of cue launch quantization.

- **Cue launch** (`:launch`, the default for existing bindings) measures elapsed beats since the activation started. Bars and phrases use the meter captured at launch. An instance pause freezes this phase, and a transport seek preserves its elapsed phase.
- **Show transport** (`:transport`) measures beats from transport beat zero. Equal periods and offsets align across cue launches. A transport seek changes the phase immediately. Bars and phrases use the shared meter of the current frame, so a meter change can also change their phase. An instance pause leaves the source following the shared beat. When the shared beat stops advancing, the waveform holds its phase.

Holding the parameter exposes its base while the waveform keeps its clock reference. Ending the activation releases its bindings and the compiled plan is retired on the next frame.

A bound waveform is sampled against the destination schema of the revision the engine evaluates. While a hot reload of its instance is pending, the owner samples it for both outcomes, as it does for [random controllers](#a-frame-with-a-pending-hot-reload). A rejected revision therefore leaves the waveform's value and a modulated layer priority's held tier as they were.

To switch a running waveform to another parameter, open its **Source** editor, choose a **Destination**, set that parameter's **From** and **To** values, and select **Move LFO**. Studio lists parameters on the same activation that can accept a waveform. The move takes effect together at the next owner update. The original parameter returns to its underlying input, and the waveform keeps its shape, period, inputs, and clock phase. Manual holds stay with their parameters.

The same operation is available through the typed bind endpoint or the show API. Pass `:from-param` to identify the currently bound parameter, its projected LFO source `:identity` as `:expected-source`, and the destination's typed endpoints in `:binding`.

```clojure
(let [{:keys [instances runtime-id epoch]} (show/snapshot runtime)
      instance (some #(when (= instance-id (:id %)) %) instances)
      lfo-source (some #(when (= :lfo (:type %)) %)
                       (get-in instance [:parameter-sources :level :sources]))]
  (show/submit! runtime :modulation-bind
    {:runtime-id runtime-id :expected-epoch epoch
     :instance-id instance-id :from-param :level :param :step
     :expected-source (:identity lfo-source)
     :binding {:from 1 :to 5}}))
```

An occupied destination, invalid endpoints, or an intervening source or effect revision change reject the move and retain both parameters' previous sources. Studio waits for the committed projection before releasing a pending edit. For clients that supply `:request-id`, the projection's bounded `:parameter-source-outcomes` also exposes terminal results for requests whose HTTP reply is pending.

### Layer priority as a destination

A waveform can also drive the launch's render layer priority through the
reserved destination `:layer/priority`. Its typed schema is
`tolglow.effects.parameter-sources/priority-schema`: an integer from −1000 to
1000 in the `:priority` category. Both endpoints must be integer tiers. The
destination is not an effect parameter, so it never enters effect parameter
overlays or random-controller selection.

```clojure
(show/submit! runtime :modulation-bind
  {:instance-id instance-id :param :layer/priority
   :binding {:type :lfo :waveform :triangle :period {:beats 4}
             :phase-reference :transport :from -1 :to 1
             :hysteresis {:band 1/4 :dwell {:beats 1/2}}}})
```

The owner samples the waveform once per frame as a continuous value between the
endpoints. `quantize-priority` then chooses a tier:

- `:band` (default `1/4`, 0–8 tiers) is a Schmitt band. The held tier changes
  only after the sample passes the boundary by at least the band. With a quarter
  band, rising from tier 0 switches at 0.75 and falling back switches at 0.25.
- `:dwell` (optional beats, bars or phrases, using the captured meter) keeps a
  new tier for at least that interval on the launch's own clock. A transport
  seek backwards re-arms it.

A large jump can change several tiers at once; the result is clamped to the
bounds. Hysteresis state is a tier and the beat it started, carried with the
previous frame's samples and keyed by the source identity. Rebinding, a show
epoch change, a manual hold or a restored show starts it again from the next
sample. Because the band follows the sampled path, sparse and dense frame
sampling can differ at a boundary.

The frame passes the tiers to `afterglow.runtime.engine/advance` as
`::engine/priority-overrides`. The engine resolves positions once from that
frozen frame input and uses them to order evaluation and merge contributions. A tier equal to
the launch's effective priority keeps its committed place. A different tier
places the launch in that tier after any explicitly reordered peers, then in
launch order. The committed `:priority`, `:render-priority`, `:render-order`
and `:active-instance-order` stay unchanged. The snapshot adds
`:effective-instance-order`, and the launch's
`[:parameter-sources :layer/priority]` reading has the committed priority as
`:base-value` and the current tier as `:effective-value`. Each source also
records `:priority` and `:raw-priority`. Holding the destination restores the
committed order for that launch. An accepted `:instance/set-priority` or
`:instance/move-after` on a launch with a bound priority waveform holds that
destination with `:reason :manual-priority` (`modulation/hold` accepts an
optional keyword `:reason`), so the operator's priority is lit until the
waveform is resumed. Only the `:layer/priority` destination accepts
`:hysteresis`. A definition parameter that shares the `:priority` category
does not. A waveform cannot target a member of a
compound native layer, because its members share one priority. A move between
`:layer/priority` and an effect parameter drops the hysteresis on the way out
and uses the default band on the way in.

Registered waveform values are normalized from their declared range, clamped to 0–1, and interpolated between the two typed endpoints. Colors, coordinates and ranges interpolate component-wise. Integers round to the nearest value and boolean or enum inputs select the second endpoint at halfway. Live revisions of the selected waveform recompile its plan. A composite authored with `signals/sample` captures the nested expression at publication. Republishing the composite picks up a new constituent waveform. An incompatible destination or waveform input is reported on that source while the underlying parameter remains usable. Show files retain bindings, original schema, waveform registry and manual holds across instance-ID changes.

Committed Studio snapshots include the registered waveform choices in `:parameter-waveforms`. Each bound entry under `[:instances n :parameter-sources param]` exposes its base value, effective input and sources, including held and constant-valued sources. A random controller waiting for its next legal sample is marked pending. A rejected sample carries a diagnostic on that frame. The input pipeline applies the random controller, instance waveform, spatial overrides and response middleware in order. Parameter blocks inside an effect can yield a different result per fixture. Live destinations list the MIDI controller and current ports while the lighting bridge owns them. Handing the device back to Live or stopping the input removes that source. Patch destinations include their captured route and source IDs, including audio pitch or level. A spatial field reports its configured variable, cue parameter or volume control. Their authored block source contains the path for `:runtime/block-trace`, which returns the committed result for the requested fixture.

Spatial links such as `{:cue-id :solo :param :position}` read the selected activation’s typed value, including its instance waveform and response envelope. A position vector or RGB color uses the same lookup as a scalar. The current instance waveform takes precedence over the last completed random sample. When a linked input is itself authored by another spatial field, its previous completed value is used. Two fields can therefore feed each other with one frame of delay per link.

Patch sources stay pending until their first contribution reaches the destination. An open gate or a continuing smoothing or release tail keeps the source active. After expiry, its configuration remains available to edit. An imported show displays its saved random sample and glide position immediately, with the controller’s random seed and next musical boundary preserved. If response middleware fails, the affected input identifies the response and reports its error. Other inputs retain their own values and source information.
