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
| `:at {:bar 64}` | At an absolute transport position: `:beat`, `:bar` and/or `:phrase`, added together | Reported as missed if the new position is past it. It is then dropped and never fires twice |
| `:grid {:phrases 1}` | At the next boundary at or after the preceding step. An exact boundary is due now, as in `cue_timing` | Same as `:at` |

Positions are **zero-based** transport coordinates, matching
`[:clock/bar]` in [reactive clock subscriptions](reactive.md#musical-subscriptions-and-effect-watchers):
`{:bar 64}` is beat 256 in 4/4, and `{:bar 64 :beat 2}` is beat 258. Bars and
phrases convert through the transport meter. Tempo is not used to decide when an
event is due, so a tempo change stretches every remaining musical wait in
wall-clock time.

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
| `:lighting/scene`, `:callback` | As in [event subscribers](event-subscribers.md) | Subscriber-only. A scheduled event uses built-in actions |

`:cue-id`, `:effect-id` and `:instance-id` are shorthand for
`:select {:ids [id] :pick :first}` (cues and effects) or `:pick :all`
(instances). An instance selector also matches running instances by cue or
effect ID. This lets you ramp a cue that an earlier event in the same batch
launches. Use `:select {:categories [:intensity] :pick :all}` to address a group
of running cues by category.

A destination for `:param/set` and `:param/ramp` is chosen with `:target`:

| `:target` | Also needs | Value type |
| --- | --- | --- |
| `:param` (default) | instance selector, `:param` | The effect parameter's own schema: `:number`, `:integer`, `:color`, `:vector`, `:range` or `:gradient` |
| `:opacity` | instance selector | 0–1. A category selector gives group alpha |
| `:master` | — | 0–1 |
| `:variable` | `:variable` | A number or a numeric vector |

`:over` takes one positive `{:beats n}`, `{:bars n}` or `{:phrases n}`. `{:ms n}`
is an explicit boundary unit. It runs on the owner's logical millisecond clock,
including while the transport is paused. `:curve` takes any
`afterglow.render.easing` curve: `:linear` (default), `:ease-in`, `:ease-out`,
`:ease-in-out`, `:sine`, `:smoothstep`, `:step`, `{:type :power :exponent 3}`,
`{:type :cubic-bezier :points [x1 y1 x2 y2]}` or `{:type :spring ...}`. A
`:param/ramp` may also use `:start {:in ...}`, `{:at ...}` or `{:grid ...}` in
place of a top-level timing key.

### Ramp semantics

- The ramp captures `:from` from the destination's current value at its start.
  A beat ramp starts at its due beat, even when a frame crosses that beat late.
  Its musical position therefore does not depend on frame timing.
- Each frame, the owner samples `(curve progress)` from the frozen frame
  transport and writes the destination before rendering. Progress is
  `(beat − start) / (end − start)`. There are no timers, per-frame client
  commands or extra RNG. Replaying the same command and clock log gives the same
  output. Numeric values are constrained to their bounds, and integers stay
  integers. Gradients with equal stop counts move every stop. Other gradients
  switch at the halfway point.
- When progress reaches 1, the owner writes the exact target through the shared
  reducer (`:param/set`, `:instance/opacity-set` or `:master/set`), so a
  receipt and a committed event exist. A variable ramp ends with a revision.
- **Supersede.** A new ramp, or a scheduled set on the same destination,
  replaces the running ramp. A new ramp starts from the old ramp's value at its
  start, so there is no jump. `:recent` records
  `{:status :cancelled :reason :superseded :by event-id}`.
- **Human override.** A committed write outside the scheduler cancels a running
  ramp on the same destination: `:param/changed`, `:master/changed`, an opacity
  change or `:gesture/begin`. The destination keeps the human value.
  Gestures, trusted operator authority and Studio, web, Push, X-Touch, MIDI, OSC
  or sensor sources record `:reason :human-override`. Other sources, such as
  agents, the REPL and reactive actions, record `:reason :superseded`. `:by`
  holds the overriding op, command ID and source.
- A removed instance cancels its ramps with `:reason :destination-removed`.
- A seek or loop shifts beat ramps with the clock, so their elapsed musical
  progress is preserved.

## Commands

All three commands are owner commands with the usual receipts. A command ID,
epoch and show ID are checked, and a repeated command ID returns its first
receipt.

```clojure
;; Atomic batch: one invalid event rejects all of them.
{:op :schedule/post
 :events [{:id "drop" :label "Drop" :op :cue/launch :cue-id :wash :at {:bar 64}}
          {:id "swell" :op :param/ramp :cue-id :wash :param :level
           :to 0.8 :over {:bars 8} :curve :ease-in-out :at {:bar 64}}]}
;; => {:status :accepted :event-ids ["drop" "swell"] ...}

;; Direct form for one event: any vocabulary action carrying :at, :in or :grid,
;; or any :param/ramp. :event-id and :label are optional.
{:op :param/ramp :instance-id "1:3" :param :level :to 0.8 :over {:beats 8}
 :curve :ease-in-out :start {:in {:bars 1}} :event-id "swell"}

;; Cancel pending events and running ramps. All supplied criteria must match.
{:op :schedule/cancel :event-ids ["swell"]}
{:op :schedule/cancel :posted-by {:adapter :agent :session-id "run-7"}}
{:op :schedule/cancel :label "Drop"}
{:op :schedule/cancel :destination {:target :param :instance-id "1:3" :param :level}}

;; Reschedule one pending event.
{:op :schedule/move :event-id "drop" :at {:bar 72}}
```

An event ID is text. A keyword or a JSON string gives the same ID. If you omit
it, the ID is `"<command-id>/<index>"`. An ID cannot be reused while its event
is pending or its ramp is running. At most 64 events can be posted in one batch,
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
   :from 0.2 :to 0.8 :value 0.41 :curve :ease-in-out
   :unit :beats :over {:bars 8} :start 256.0 :end 288.0 :progress 0.3125
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
`:cancelled` or `:completed` (ramps). `:category` is one of `:cue`,
`:instance`, `:param`, `:ramp`, `:master`, `:variable`, `:macro`, `:scene` or
`:callback`. Values are not rounded. `:upcoming` also lists pending event
subscriber steps, so a timeline shows what subscribers will do next. Studio's
cue panel shows scheduled events and running ramps, with **Cancel** and **Stop**
by event ID. Subscriber steps stay in **Debug**.

`(show/schedule runtime :compact)` and the `inspect_schedule` agent tool return
a low-token view:

```clojure
{:legend {:upcoming [:due-beat :event-id :op :label]
          :ramps [:event-id :destination :progress :value :to :end-beat]
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
    {:id "p8-dim"   :label "Pull back" :op :param/ramp :target :master
     :to 0.6 :over {:bars 2} :curve :ease-out :at {:bar 44}}
    {:id "p8-drop"  :label "Drop"      :op :cue/launch :cue-id :strobe
     :launch {:mode :one-shot :duration {:beats 2}} :at {:bar 48}}]
   {:source {:adapter :agent :session-id "phrase-planner"}})
;; => {:status :accepted :event-ids ["p8-wash" "p8-swell" "p8-dim" "p8-drop"] ...}

;; Changed its mind about the drop:
@(show/submit! runtime {:op :schedule/move :event-id "p8-drop" :at {:bar 52}})
;; Or withdraw everything it posted:
@(show/cancel-events! runtime {:posted-by {:adapter :agent :session-id "phrase-planner"}})
```

Through the provider tool loop, the same plan is one `schedule_events` call
with JSON arguments. Ops and enums may be strings:

```json
{"events": [
  {"id": "p8-wash", "op": "cue/launch", "cue-id": "wash", "grid": {"phrases": 1}},
  {"id": "p8-swell", "op": "param/ramp", "cue-id": "wash", "param": "level",
   "to": 0.9, "over": {"bars": 8}, "curve": "ease-in-out", "grid": {"phrases": 1}},
  {"id": "p8-dim", "op": "param/ramp", "target": "master", "to": 0.6,
   "over": {"bars": 2}, "curve": "ease-out", "at": {"bar": 44}}]}
```

`cancel_scheduled` with `{"mine?": true}` withdraws everything that run posted.
`inspect_schedule` returns the compact view. See
[application agents](application-agents.md#show-authoring-agents-and-the-beat-clock).

## Persistence and restart

A full show save keeps pending scheduled runs, their anchors, active ramps (with
`:from`, `:to`, `:start` and `:end`) and recent outcomes. A restore remaps
instance IDs in ramp destinations and selectors. `:show/restart` clears runs,
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
  the running ramp with `:human-override`.
- A batch of 16 agent events with one bad curve is rejected as a whole. The
  valid batch projects in order with categories. Cancelling by source withdraws
  all 16.
- Full backup round trip, millisecond and variable ramps, subscriber `:at`,
  `:in` and `:grid` timing, and rebase semantics.

## Remaining gaps

- Modulation depth (waveform binding endpoints) is not yet a ramp destination.
- Push 3 does not yet display the projection. Its pages read the same snapshot
  when one is added.
- Hardware-timed output and physical device evidence are outside these tests.
