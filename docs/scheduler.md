# Beat-clock scheduling and parameter ramps

An agent, REPL user or controller can post show changes ahead of time on the
beat clock and describe a gradual change as one event: a target value, a
musical duration and an easing curve. The show owner fires the events and runs
the animation, so a client does not need to send a command every frame. Issue
[#645](https://github.com/tolgraven/tolglow/issues/645) tracks this work. The
timeline in [#646](https://github.com/tolgraven/tolglow/issues/646) reads the
[projection](#projection) described below.

[Event subscribers](event-subscribers.md) · [Reactive actions](reactive.md) ·
[REPL and Studio parity](repl-studio-parity.md) · [Source](../src/tolglow/event_subscribers.clj)

## How it fits the existing machinery

There is no separate scheduler. A scheduled event is a one-step run on the
[event subscriber](event-subscribers.md) action queue. That queue already plays
ordered actions on the show clock, with `:delay`, provenance, validated
commands and handler faults. Event subscriber actions and scheduled events use
the same action vocabulary. Each step accepts one timing key:

| Timing | Meaning | On seek, loop or clock change |
| --- | --- | --- |
| `:delay {:beats 2}` | After the preceding step of the same sequence | Keeps its remaining musical wait |
| `:in {:bars 4}` | After the triggering event, or after the posting beat for a scheduled event | Keeps its remaining musical wait |
| `:at {:bar 64}` | At an absolute transport position: `:beat`, `:bar` and/or `:phrase`, added together. A position already behind the transport is rejected when posted | Reported as missed if the new position is past it. It is then dropped and never fires twice. A forward correction of at most 1/4 beat, such as a Link phase nudge, leaves it due now instead |
| `:grid {:phrases 1}` | At the next boundary at or after the preceding step. An exact boundary is due now, as in `cue_timing` | Re-quantized to the next boundary from the new position |

Positions are **zero-based** transport coordinates, matching
`[:clock/bar]` in [reactive clock subscriptions](reactive.md#musical-subscriptions-and-effect-watchers):
`{:bar 64}` is beat 256 in 4/4, and `{:bar 64 :beat 2}` is beat 258. In
one-based counting, as on Live's arrangement ruler, `{:bar 64}` is bar 65 and
`{:bar 0}` is the first bar. Each post receipt lists the resolved `:due-beats` so
you can check the conversion. Bars and
phrases convert through the transport meter when the event is posted, moved or
re-quantized. A later meter change does not move a pending position or wait.
Tempo is not used to decide when an event is due, so a tempo change stretches
every remaining musical wait in wall-clock time.

The owner fires due runs during each frame, after it applies queued commands and
before rendering. Runs fire in due-beat order, then in admission order. The
frame that first reaches or crosses the due beat fires the event. A paused
transport holds both events and beat ramps.

`afterglow.reactive` effect watchers queue semantic actions that the next owner
cycle applies through the common command reducer. A watcher can return
`{:op :schedule/post :events [...]}` or a direct `:param/ramp` action like any
other command. Cue launches with `:grid` still use the separate quantized queue
in `cue_timing`. A cue launch with `:at` or `:in` uses this queue.

## Actions

| Action | Fields | Effect |
| --- | --- | --- |
| `:cue/launch` | `:cue-id` or `:select`, `:launch` | Launch a cue |
| `:effect/launch` | `:effect-id` or `:select`, `:launch` | Launch an effect directly |
| `:instance/end`, `:instance/kill` | `:instance-id` or `:select` | Release or remove running instances |
| `:instance/opacity` | `:select`, `:value` 0–1 | Set system opacity |
| `:param/set` | destination, `:value` | Set a value now |
| `:param/ramp` | destination, `:to`, `:over`, `:curve` | Move from the current value to `:to` |
| `:master/set` | `:value` 0–1 | Set the master level |
| `:blackout/set` | `:value` boolean | Blackout on or off |
| `:variable/set` | `:variable`, `:value` | Write a show variable (`[:variables k]`) |
| `:macro/run`, `:macro/stop` | `:macro-id` | Start or stop a saved macro or sequence |
| `:auto/configure` | `:request` (an [auto](auto-show.md) request) | Configure auto mode on its line, before auto decides on that frame. Usually posted by an auto request with `:at` |
| `:lighting/scene`, `:callback` | As in [event subscribers](event-subscribers.md) | Subscriber-only. A scheduled event uses built-in actions |

`:cue-id`, `:effect-id` and `:instance-id` are shorthand for `:select {:ids
[id]}`. A launch uses `:pick :first`. An action on running instances, such as a
ramp, set, opacity or end, uses `:pick :all`, so a cue ID addresses every running
instance of that cue. An instance selector matches running instances by
instance, cue or effect ID. This lets you ramp a cue that an earlier event in the same batch
launches. Use `:select {:categories [:intensity] :pick :all}` to address a group
of running cues by category.

A destination for `:param/set` and `:param/ramp` is chosen with `:target`:

| `:target` | Also needs | Value type |
| --- | --- | --- |
| `:param` (default) | instance selector, `:param` | The effect parameter's own schema: `:number`, `:integer`, `:color`, `:vector`, `:range` or `:gradient` |
| `:opacity` | instance selector | 0–1. A category selector gives group alpha |
| `:master` | — | 0–1 |
| `:variable` | `:variable` | A number or a numeric vector (any portable value for a set). Runtime-owned variables are reserved: `:runtime`, `:spatial`, `:sensors`, `:sync`, `:audio`, `:audience`, `:surface-aim` and the `audio/`, `fog/`, `lighting/`, `sensor/`, `sync/` and `spatial/` namespaces |

`:over` takes one positive `{:beats n}`, `{:bars n}` or `{:phrases n}`. `{:ms n}`
is an explicit boundary unit. It runs on the owner's millisecond clock and,
like a beat ramp, stands still while the transport is stopped: play resumes it
with the time it had left ([transport stop](clock-domains.md#transport-stop)). `:curve` takes any
`afterglow.render.easing` curve: `:linear` (default), `:ease-in`, `:ease-out`,
`:ease-in-out`, `:sine`, `:smoothstep`, `:step`, `{:type :power :exponent 3}`,
`{:type :cubic-bezier :points [x1 y1 x2 y2]}` or `{:type :spring ...}`. A
`:param/ramp` may also use `:start {:in ...}`, `{:at ...}` or `{:grid ...}` in
place of a top-level timing key.

### Ramp semantics

- The ramp starts on the frame that fires it, at that frame's transport beat
  (or logical milliseconds for `:ms`). It captures `:from` from the
  destination's current value. Replay gives the same frames, so the start is
  deterministic.
- **The per-frame value is a frame overlay.** It is never written to the
  committed show. Each frame, the owner samples `(curve progress)` at the frozen
  frame transport. Progress is `(beat − start) / (end − start)`. A parameter
  reaches the effects through the same parameter-overlay middleware that
  modulation uses. The stages that read the model directly see a frame-only
  model with the ramped master, opacity and parameters: spatial cues (their
  volume opacity and parameters), the engine and the output stage. Committed
  values are restored after each stage, unless the stage itself changed that
  location to a different value (compared numerically, so re-boxing an equal
  value is not a write). A variable reaches the frame's resolved variables.
  Override detection also compares committed values numerically, so `1` and
  `1.0` are the same value. Category and cue selectors skip scene-owner,
  show-control and code-cue instances, such as a spatial cue. A human operator
  surface may still address one by exact `:instance-id`. An agent, reactive
  action or subscriber is refused, at post if the instance exists and again
  when the event is due. The
  committed value, backups, saved cue parameters and revision-keyed consumers
  keep the start value until the ramp commits. There are no timers, per-frame
  client commands or extra RNG. Numeric values are constrained to their bounds,
  and integers stay integers. Gradients with equal stop counts move every stop.
  Other gradients switch at the halfway point.
- When progress reaches 1, the owner commits the exact target through the
  shared reducer: `:param/set`, `:instance/opacity-set`, `:master/set` or the
  validated `:variable/set` command. A receipt and a committed event follow, with
  the poster's source.
- **Modulation precedence.** An active waveform binding or random modulation
  controller on the same parameter wins over the ramp overlay. The ramp still
  advances and commits its target, which becomes the base value under the
  modulation.
- **Pending hot reload.** A ramp overlay does not follow a reload candidate.
  On a frame with a pending reload, both the candidate inputs and the
  last-good inputs (see
  [modulation](modulation.md#a-frame-with-a-pending-hot-reload)) rest on the
  same ramp overlays. The candidate reads only the ramped parameters its
  schemas still accept, so a healthy revision that removes or retypes a ramped
  parameter is not rejected because of the ramp. A renderer rejection rebuilds
  the engine result from its frame model; the owner takes the ramp overlay out
  of that result too, so the rejection does not commit the ramp's frame value
  and the ramp keeps running. A revision that keeps a ramped parameter must
  accept its committed value, the ramp's frame value and the ramp's target. If
  it refuses any of them, it is rejected at preparation (`:stage :reload`, one
  `:effect/reload-failed` event) under every reload policy, and the ramp keeps
  running on the captured revision and commits its target. The committed value
  stays in the model until the ramp commits the target, so a revision that
  refused either could not hold the value the model keeps or receives. The
  owner therefore prepares such an instance on the refused values, and the ramp
  reaches that frame through the parameter overrides.
- **Supersede.** A new ramp, or a scheduled set on the same destination,
  replaces the running ramp. A new ramp starts from the old ramp's value at its
  start, so there is no jump. `:recent` records
  `{:status :cancelled :reason :superseded :by event-id}`.
- **Back to back.** Whether a ramp was cut short is decided musically, not by
  where frames happen to land. A ramp keeps the instant it was due as well as
  the frame it started at. For a beat ramp that is `:due-start` next to
  `:start`, and for a millisecond ramp `:due-start-ms` next to `:start`.
  Sampling uses `:start`. When the next ramp or scheduled set on its
  destination fires, the running ramp has finished if both of these hold:
  - its planned span, counted from when it was due, ends at or before the
    newcomer's due instant;
  - on the clock it samples, it is within one frame of its end.

  One frame is measured from the engine's record of the last rendered frame
  (`:last-frame-beat`, `:last-frame-time-ms`). Commands never write those, so
  unrelated traffic earlier in the same frame does not change the outcome. A
  finished ramp commits its exact target first and records `:completed`, as it
  would have later in the same owner cycle, and the next ramp starts from that
  committed value. A ramp over beats 4–8 followed by one due at beat 8 records
  the first as `:completed` at any frame rate. Frame progress within 1e-9 of 1
  also counts as the end, so accumulated floating-point error in the transport
  beat cannot hold a ramp open for an extra frame. A ramp that fired late, for
  example after a forward phase correction or a step that waited on a
  callback, is still more than a frame from its end. Committing its target
  would jump, so it records `:superseded` and the newcomer takes over from
  where it actually is.
- **Velocity at a takeover**
  ([#896](https://github.com/tolgraven/tolglow/issues/896)). A superseding
  ramp continues from the old ramp's position and velocity; it never restarts
  from rest. It keeps its authored easing `e(p)` and adds a decaying Hermite
  term:

  `v(p) = from + (to − from)·e(p) + (m0 − (to − from)·e′(0))·p(1 − p)²`

  The term is zero at both ends, so the ramp still reaches its exact target at
  its planned end, with its curve's own end slope. `m0` is stored on the ramp
  as `:handover`: the old ramp's slope at the handover frame, per beat (or per
  millisecond for an `:ms` ramp), converted through the transport tempo when
  the two ramps use different units, times the new ramp's span. It is a
  velocity per unit of the new ramp's progress, so a tempo change, a clock
  rebase or a stopped transport leaves it valid, and a backup restores it.
  Numbers, colours, vectors, ranges and gradients with equal stop counts carry
  one velocity per component. An eased integer ramp carries the velocity of its
  unrounded track and still rounds. A linear integer ramp is a stepped move,
  where only the step crossings show, so it takes over from the position alone
  and keeps a linear move's crossings. A component its bounds hold carries no velocity,
  and the destination's bounds still clamp an overshoot. No velocity is
  carried when the new curve cannot start with a finite slope (`:step`,
  `:power` below 1, a bezier with a vertical start), into or out of a gradient
  whose stop counts differ, or across beat and millisecond ramps without a
  transport tempo. Gesture takeover, modulation precedence and motion speed
  are unchanged: nothing is clamped or refused for how fast a destination
  moves. `takeover` in `event_subscribers.clj` decides what a newcomer
  inherits; the term itself is `tolglow.composition.handover`.
- **Override at the domain layer.** Any committed write to the destination
  cancels the ramp. This covers a gesture, a Studio control, a macro, routing,
  automation or another agent. The command hook catches `:gesture/begin` and
  direct commands immediately. The owner also compares the committed value with
  the value the ramp started from, each frame after macros, automation and
  routing have run, so a writer that bypasses the hook is caught too. The
  destination keeps the written value. A `:gesture/begin` writes nothing, so the
  owner first commits the ramp's current frame value: the gesture takes over
  from where the ramp is, and cancelling the gesture returns to that value. A
  refused gesture leaves the ramp running. A gesture, trusted operator
  authority, or a Studio, web, Push, X-Touch (including the Extender),
  Seaboard, MPE, MIDI, OSC, sensor or REPL source records
  `:reason :human-override`. Other sources, such as agents, reactive actions
  and macro or automation commands, record `:reason :superseded`. `:by` holds
  the writer's op, command ID and source from the shared receipt history.
- **Cancel.** `:schedule/cancel` stops a running ramp and commits its current
  frame value through the shared reducer, with the ramp's source. The
  destination holds where the ramp left it and does not return to the start
  value. `:recent` records `{:status :cancelled :reason :operator-cancel}`.
- A removed instance cancels its ramps with `:reason :destination-removed`.
- A ramp whose target the destination refuses when it completes, for example
  because the destination's schema changed under it, is not recorded as
  completed. The committed value stays, and `:recent` records
  `{:status :cancelled :reason :commit-refused :message ...}` with the
  reducer's message.
- A seek or loop shifts beat ramps with the clock, so their elapsed musical
  progress is preserved.

### Who may schedule, cancel and move

The show decides who posted a request from how it arrived, never from the
`:source` in the request:

| Transport | Source |
| --- | --- |
| Studio HTTP (`/api/command`, `/api/auto`, `/api/settings`, `/api/lighting-scene`), including the iPhone client | `{:adapter :web :session-id <tab>}`. A client's own adapter and other fields are ignored; its session ID only labels the tab. |
| Agent tool calls (`show/dispatch-tool!`, Studio chat) | `{:adapter :agent :session-id <conversation> :run-id <run>}`. Tool arguments cannot name a source. |
| REPL helpers (`show/post-events!`, `show/cancel-events!`, surface helpers) | `{:adapter :repl :session-id "repl"}`, or what in-process code passes to `show/submit!` |
| Push, X-Touch and other connected devices | The device session, assigned when it connects |

Studio's WebSocket streams carry only subscriptions, acknowledgements and
observations, never commands. An HTTP client is always the operator; binding
an external agent over HTTP is
[#903](https://github.com/tolgraven/tolglow/issues/903). A held launch from
any web client, the iPhone included, is released 10 s after its tab's
heartbeat stops. Edit history uses the listener's operator
authority, not the payload.

These are operator defaults:

- Human master and blackout stay authoritative. An agent (`:adapter :agent`)
  cannot schedule `:master/set`, `:blackout/set` or a master ramp. The batch is
  rejected with a reason.
- Studio, web, Push, X-Touch, Seaboard, MPE, MIDI, OSC, sensor and REPL
  requests may cancel or move any event. Any other requester, such as an agent
  or a reactive action, matches only events and ramps it posted itself (same
  adapter and session), whatever criteria it supplies. A requester whose source
  has no adapter owns nothing. An agent therefore cannot cancel a subscriber's
  ramp or another agent's plan. `cancel_scheduled` with `mine?` makes this
  explicit.
- A scheduled `:macro/run` or `:macro/stop` runs as a domain command carrying the
  poster's source, so the receipt records who started the macro.
- A scheduled `:auto/configure` is checked against the show when it is posted
  and again when it is due, when its first decision is committed as that
  frame's decision. The show owner applies it through the same path as an
  immediate auto request. This
  namespace only carries it, so without the owner's handler it is rejected. A
  request the show refuses when due is recorded as `:rejected` with its
  message, and auto mode and its looks stay as they were. One posted less
  than a bar after its position fires at once, and its outcome carries
  `:late? true :late-beats`; a bar or more is refused. Only that policy sets
  the marker. It is scheduler-only: a subscriber definition
  with it is refused. An
  immediate **Stop auto** withdraws the pending auto changes its sender may
  cancel (`:reason :auto-stopped`), so a change waiting on its line cannot
  start auto again.

## Commands

These are owner commands with the usual receipts. A command ID,
epoch and show ID are checked, and a repeated command ID returns its first
receipt.

```clojure
;; Atomic batch: one invalid event rejects all of them.
{:op :schedule/post
 :events [{:id "drop" :label "Drop" :op :cue/launch :cue-id :wash :at {:bar 64}}
          {:id "swell" :op :param/ramp :cue-id :wash :param :level
           :to 0.8 :over {:bars 8} :curve :ease-in-out :at {:bar 64}}]}
;; => {:status :accepted :event-ids ["drop" "swell"] :due-beats [256.0 256.0] ...}

;; Direct form for one event: any vocabulary action carrying :at, :in or :grid,
;; or any :param/ramp. :event-id and :label are optional.
{:op :param/ramp :instance-id "1:3" :param :level :to 0.8 :over {:beats 8}
 :curve :ease-in-out :start {:in {:bars 1}} :event-id "swell"}

;; Cancel pending events and running ramps. All supplied criteria must match.
;; A cancelled ramp holds its current value.
{:op :schedule/cancel :event-ids ["swell"]}
{:op :schedule/cancel :posted-by {:adapter :agent :session-id "run-7"}}
{:op :schedule/cancel :label "Drop"}
{:op :schedule/cancel :destination {:target :param :instance-id "1:3" :param :level}}

;; Reschedule one pending event. The receipt has its new :due-beats.
{:op :schedule/move :event-id "drop" :at {:bar 72}}

;; Validated show-variable write and macro control as plain commands.
{:op :variable/set :variable :section :value 3}
{:op :macro/run :macro-id :build-up}
```

An event ID is text. A keyword or a JSON string gives the same ID. If you omit
it, the ID is `"<command-id>/<index>"`. An ID cannot be reused while its event
is pending, its ramp is running or its outcome is still in `:recent`. At most 64 events can be posted in one batch,
with 256 scheduled events and 256 active ramps. Validation covers action shape,
timing, curve, ramp duration, cue, effect and macro IDs. For exact running
instance IDs, it also checks the parameter, value type and whether the
parameter can ramp. A cue or category selector is resolved when the event fires.
If nothing matches then, the event records `:rejected` with the fault message,
like any handler fault.

## Projection

`(show/schedule runtime)` and `(:schedule (show/snapshot runtime))` return the
same map. Studio receives it in the show state stream as `[:show :schedule]`.
Push, the REPL and agents read this one projection. Keep the shape stable: the
[#646](https://github.com/tolgraven/tolglow/issues/646) timeline consumes it.

```clojure
{:beat 130.25 :bpm 124.0 :playing? true
 :meter {:beats-per-bar 4 :bars-per-phrase 4}
 :upcoming   ; due-beat order, then admission order
 [{:event-id "drop" :origin :scheduled          ; or :subscriber
   :op :cue/launch :category :cue :label "Drop"
   :due-beat 256.0 :anchor :absolute            ; :relative moves with a seek
   :posted-beat 130.0 :step 0 :steps 1
   :source {:adapter :agent :session-id "run-7"}
   :select {:ids [:wash] :pick :first} :at {:bar 64}}
  {:event-id "swell" :origin :scheduled :op :param/ramp :category :ramp
   :due-beat 256.0 :anchor :absolute :target :param :param :level
   :to 0.8 :over {:bars 8} :curve :ease-in-out ...}
  {:event-id "subscriber/kick-accents/41" :origin :subscriber
   :subscriber-id :kick-accents :step 1 :steps 2 ...}]
 :ramps
 [{:event-id "swell" :label "Swell" :category :ramp
   :destination {:target :param :instance-id "1:3" :param :level}
   :from 0.2 :to 0.8 :curve :ease-in-out
   :unit :beats :over {:bars 8} :start 256.0 :end 288.0
   :source {:adapter :agent :session-id "run-7" :scheduled-event-id "swell"}}]
 :recent     ; newest last, at most 64
 [{:event-id "drop" :op :cue/launch :category :cue :status :fired
   :due-beat 256.0 :at-beat 256.02 :time-ms 123456.0 :source {...} :label "Drop"}
  {:event-id "outro" :status :missed :reason :clock-discontinuity ...}
  {:event-id "swell" :status :cancelled :reason :human-override
   :by {:op :gesture/begin :command-id "..." :source {:adapter :push-3 ...}} ...}
  {:event-id "fade" :status :completed :category :ramp :value 0.0 :to 0.0 ...}]}
```

`:status` is one of `:fired`, `:rejected` (with `:message`), `:missed`,
`:cancelled` or `:completed` (ramps). `:at-beat` is the transport beat and
`:time-ms` the owner time of the frame or command that committed the outcome.
Both come from the same frame, so an outcome fired on the frame at beat 8.0
carries that frame's time, not the previous frame's.

A seek, loop or clock-source change moves each recent outcome's beat
coordinates (`:at-beat`, `:due-beat`, and a beat ramp's `:start` and `:end`)
by the same offset as the transport, as the
[clock rebase](clock-domains.md#host-integration-and-recovery) does for every
other historical anchor. An outcome therefore stays as many beats behind the
transport as it was, and a consumer that places outcomes by beat still finds
them in the past after a loop. `:rebased-beats` holds the total offset an entry
has taken, so `(- due-beat rebased-beats)` is the beat the event was posted
for (an `:at {:beat 4}` event fired before a loop of −6 beats reads
`:due-beat -2.0 :rebased-beats -6.0`). The timeline lists that as
`:posted-due-beat`, and Studio's **Was due** row shows it. `:time-ms`, a
ramp's `:started-ms` and a millisecond ramp's span keep their original
instants, and the timeline keys outcomes and ramp bars by them, so a loop
does not replace their elements. A `:missed` outcome recorded by the rebase
itself happens at the new position.

Every discontinuity rewrites all recent entries, including a tiny backward
step from a jittery external clock. There is deliberately no threshold. Any
step left unshifted would leave the newest outcomes ahead of the transport,
and the timeline would hide them until the transport caught up. The rewrite
touches at most 64 small maps and runs only on a discontinuity, which already
rebases every instance and routing anchor. With stable keys it does not
remount anything in Studio. `:category` is one of `:cue`,
`:instance`, `:param`, `:ramp`, `:master`, `:variable`, `:macro`, `:scene` or
`:callback`. Values are not rounded. A ramp outcome (`:completed` or
`:cancelled`) also keeps the ramp's `:from`, `:curve`, `:unit`, `:over`,
`:start` and `:end`, so the [timeline](timeline.md) can still draw it as a bar.

The projection changes only when the schedule changes. It does not publish a
ramp's live value. A consumer derives progress from `:start`, `:end` and the
snapshot's transport `:beat`, or `:time-ms` for an `:ms` ramp, and samples
`:curve` from `:from` to `:to`. On the JVM, `subscribers/ramp-progress` returns
`[progress value]` for a ramp and model. `:upcoming` also lists pending event
subscriber steps, so a timeline shows what subscribers will do next. Studio's
cue panel shows scheduled events and running ramps, with **Cancel** and **Stop**
by event ID. Subscriber steps stay in **Debug**.

`(show/schedule runtime :compact)` and the `inspect_schedule` agent tool return
a low-token view:

```clojure
{:legend {:upcoming [:due-beat :event-id :op :label]
          :ramps [:event-id :destination :progress :value :to :end]
          :recent [:event-id :status :reason]}
 :beat 130.25 :bar 32 :beats-per-bar 4 :bpm 124.0
 :upcoming [[256.0 "drop" :cue/launch "Drop"] [256.0 "swell" :param/ramp]]
 :ramps [["swell" "1:3/level" 0.313 0.41 0.8 288.0]]
 :recent [["outro" :missed :clock-discontinuity]]}
```

## Agent example: plan the next phrase

An agent reads the clock once, posts a phrase of events and checks back later.
It does not stream parameter values.

```clojure
(require '[tolglow.show :as show])

(show/schedule runtime :compact)      ; => {:beat 118.6 :bar 29 ...}

@(show/post-events! runtime
   [{:id "p8-wash"  :label "Wash in"   :op :cue/launch :cue-id :wash :grid {:phrases 1}}
    {:id "p8-swell" :label "Swell"     :op :param/ramp :cue-id :wash :param :level
     :to 0.9 :over {:bars 8} :curve :ease-in-out :grid {:phrases 1}}
    {:id "p8-dim"   :label "Pull back" :op :param/ramp :target :opacity
     :select {:categories [:intensity] :pick :all}
     :to 0.6 :over {:bars 2} :curve :ease-out :at {:bar 44}}
    {:id "p8-drop"  :label "Drop"      :op :cue/launch :cue-id :strobe
     :launch {:mode :one-shot :duration {:beats 2}} :at {:bar 48}}]
   {:source {:adapter :agent :session-id "phrase-planner"}})
;; => {:status :accepted :event-ids ["p8-wash" "p8-swell" "p8-dim" "p8-drop"] ...}

;; Changed its mind about the drop:
@(show/submit! runtime {:op :schedule/move :event-id "p8-drop" :at {:bar 52}})
;; Or withdraw everything it posted:
@(show/cancel-events! runtime {:posted-by {:adapter :agent :session-id "phrase-planner"}})
;; An agent posting master or blackout changes is refused: the human keeps them.
```

Through the provider tool loop, the same plan is one `schedule_events` call
with JSON arguments. Ops and enums may be strings:

```json
{"events": [
  {"id": "p8-wash", "op": "cue/launch", "cue-id": "wash", "grid": {"phrases": 1}},
  {"id": "p8-swell", "op": "param/ramp", "cue-id": "wash", "param": "level",
   "to": 0.9, "over": {"bars": 8}, "curve": "ease-in-out", "grid": {"phrases": 1}},
  {"id": "p8-dim", "op": "param/ramp", "target": "opacity",
   "select": {"categories": ["intensity"], "pick": "all"}, "to": 0.6,
   "over": {"bars": 2}, "curve": "ease-out", "at": {"bar": 44}}]}
```

`cancel_scheduled` with `{"mine?": true}` withdraws everything that run posted.
`inspect_schedule` returns the compact view. See
[application agents](application-agents.md#show-authoring-agents-and-the-beat-clock).

## Persistence and restart

A full show save keeps pending scheduled runs, their anchors, active ramps (with
`:from`, `:to`, `:start` and `:end`) and recent outcomes. Because ramp values
are frame overlays, the saved destinations hold their committed values, not a
mid-ramp sample. A restore remaps instance IDs in ramp destinations and
selectors. The restored show resumes on its saved transport and logical time,
so waits, beat ramps and `:ms` ramps continue where they were. If the
restored show then follows a clock that is elsewhere, the usual clock rebase
applies. Relative waits and ramps keep their remaining time, and absolute
events behind the new beat are reported missed. `:show/restart` clears runs,
ramps and recent outcomes, as it does for subscriber runs.

## Tests

[`beat_scheduler_test.clj`](../test/tolglow/beat_scheduler_test.clj) drives the
real owner tick and captures ArtDmx packets from the output sink:

- A ramp from 0 to 1 over 8 beats with `:ease-in-out` matches the curve at every
  frame within one DMX step. The tempo halves at the midpoint, and the remaining
  4 beats take 4 seconds instead of 2. A replay is byte-identical.
- A launch at `{:bar 2}` fires on the frame that crosses beat 8. A forward seek
  past `{:bar 4}` reports it missed. Looping back fires neither event again. A
  replay is byte-identical.
- A superseding ramp continues from 0.5 without a jump. A Push gesture cancels
  the running ramp with `:human-override`. Mid-ramp, the committed parameter,
  master and variable keep their start values while the output follows the
  overlay.
- A committed write from a macro-like source cancels the ramp with `:superseded`
  and the writer in `:by`. A Push-sourced write records `:human-override`.
- A batch of 16 agent events with one bad curve is rejected as a whole. The
  valid batch projects in order with categories. Agents cannot schedule master
  or blackout, cannot cancel a human's event and cannot move it. Cancelling by
  source withdraws all 16.
- A position already behind the transport is rejected with a zero-based hint.
  A small phase correction leaves a crossed bar due now, and `:grid` steps
  re-quantize after a seek.
- A show restored into a runtime whose clock then sits at beat 100 keeps its
  relative wait and beat ramp progress, reports the passed bar as missed and
  keeps the remaining milliseconds of an `:ms` ramp.
- Full backup round trip, millisecond and variable ramps, reserved variables,
  and subscriber `:at`, `:in` and `:grid` timing.
- A cancelled ramp holds its current value in the output and the committed
  parameter. A variable ramp from a vector to a number is refused when due and
  later frames still render. A scheduled set rejected when due leaves the
  running ramp alone.
- A gesture that begins on a ramped parameter takes over from the ramp's
  current value, and a refused gesture leaves the ramp running.
- Two ramps back to back on one parameter: the first, which reached its end
  on the beat the second fires, records `:completed` and commits its target,
  and so does the second when a scheduled set lands on its end beat. A ramp cut
  short still records `:superseded`. The same outcomes hold at every frame
  period from 11 to 40 ms, and at 16.667, 33.3 and 125 ms, starting off the
  beat grid. Every outcome's `:time-ms` is the time of the frame at its
  `:at-beat`, and a replay gives identical output and outcomes. A 2000 ms ramp
  that ends within a frame of the next ramp's due beat also records
  `:completed`, also when an unrelated set lands on the same frame. A ramp that
  fired 0.15 beats late after a MIDI clock phase correction is superseded at
  its planned end and hands over its actual position.
- After a loop on a MIDI clock source, fired and completed outcomes keep their
  distance behind the transport and their original `:time-ms`, and the
  timeline still lists them behind now, with unchanged keys and the posted
  beat, also when no event or ramp is pending. A seek's missed outcome, a
  gesture override and an auto stop's withdrawal carry their own frame or
  command time.
- Same-beat events fire in the projection's order after a reschedule. A
  requester whose source has no adapter cannot move or cancel anything. A group
  gain change leaves an opacity ramp running, and a group opacity change
  cancels it.

[`ramp_handover_test.clj`](../test/tolglow/ramp_handover_test.clj) captures
16-bit ArtDmx while an ease-in-out ramp is superseded at its fastest point,
for beat/beat, beat/ms, ms/beat and ms/ms pairs. The change in first
difference around the handover stays within one 8-bit DMX step (about 0.4
measured), against 6 to 8 steps with the position-only takeover. The new ramp
reaches its exact target at its planned end, the timeline `:shape` matches the
captured output at all 17 samples, a replay is byte-identical and a backup
keeps `:handover`. Colours carry one velocity per component, an eased integer its
unrounded velocity (a linear one none), a level held at its bound none, and a `:power 0.5` curve
keeps its authored start.
[`handover_test.clj`](../test/tolglow/composition/handover_test.clj) checks the
term itself on every curve type.

[`modulation_reload_test.clj`](../test/tolglow/effects/modulation_reload_test.clj)
covers ramps on a frame with a pending hot reload: a running ramp composes
with both reload outcomes byte-identically to a show that never published the
rejected revision, a renderer-rejected reload does not commit the ramp's frame
value, and an accepted revision that drops a ramped parameter is not faulted by
the ramp. A revision that refuses the ramped parameter's committed value, its
frame value or its target is rejected at preparation and matches the twin byte
for byte, for number, integer and colour parameters, under `:preserve-state`,
`:restart` and `:migrate`, with and without a modulation controller. A revision
that accepts all three takes over mid-ramp and the ramp commits its target, and
a completion the destination refuses is recorded as `:commit-refused`.

## Remaining gaps

- Modulation depth (waveform binding endpoints) is not yet a ramp destination.
- Push 3 does not yet display the projection. Its pages read the same snapshot
  when one is added.
- Hardware-timed output and physical device evidence are outside these tests.
- Studio's Timeline clips a ramp curve to a tenth beyond its box, so the
  overshoot of a large carried velocity (#896) is cut off on the bar. The
  projection's `:shape` holds the whole curve.
