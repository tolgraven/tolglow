# Auto show

Auto mode lets the show choose and launch cues on the beat while you keep
playing. It is a deterministic planner, not a model: the same seed, show and
inputs give the same launches. It lives in `tolglow.automation.policy`; the
show calls it once per frame and dispatches its commands through the ordinary
command path. Design contract: [show authoring](product/show-authoring.md#auto-show-playback).
Issue: [#217](https://github.com/tolgraven/tolglow/issues/217).

## Start it

From the REPL, with a running show `runtime`:

```clojure
@(show/submit! runtime :auto {:enabled? true :seed 42
                              :interval {:beats 1} :count 1
                              :scope {:page-id :main}
                              :launch {:mode :one-shot :duration {:beats 1}}})
@(show/submit! runtime :auto {:enabled? false})
```

In Studio, open **Perform → Tools → Let it wander**. **Every**, **Cues per
step**, **Choose from** (current page or every page), **Launch as** and **Cue
duration** fill the same fields. **Start auto** and **Stop auto** send the same
`:auto` request through `POST /api/auto`. An agent uses the `configure_auto`
tool. A request applies when it arrives, or on a musical line with `:at` (see
[on a musical line](#on-a-musical-line)). A config the planner refuses comes back as `:status :rejected` with the
first problem in the message, for example `Invalid auto config: Count must be
an integer from 1 to 64 at [:count]`; the committed config is left as it was.

The request is checked before anything is committed: settings, scope shapes
(`:all`, `{:page-id id}`, `{:cue-ids [...]}`), launch modes and durations, and
that every scope (including each section's) names a saved page or cue. A cue or
page deleted later never blocks a save: the planner leaves it out and reports
`:scope-missing`.

Intervals and durations are beats, bars or phrases read from the transport.
Milliseconds are accepted as an explicit physical option. Musical timers,
decision intervals and section plans alike, sit on the grid of their unit (a
beat, bar or phrase line; an interval shorter than its unit, on its own
length) by one rule
([`tolglow.automation.grid`](../src/tolglow/automation/grid.clj)). A timer that
follows another starts where the previous one fell due, so a decision that
fires on the first frame after its line does not push later decisions off the
beat. A timer with nothing on the grid to follow (a start, a changed interval,
a gap, or the end of an interval that a seek or loop shifted off the grid)
ends on the first grid line at least its length less half a step away: its
first run is within half a step of its length and never falls due on the next
frame. A seek or loop moves the running interval with the clock, so that
interval finishes once at the progress it kept, off the grid; the next one
re-anchors. A decision still fires on
the first frame at or after its line, so it is up to one frame late. A stopped transport
makes no decision and holds every timer, so play resumes where the plan was,
without a burst of catch-up launches.

## On a musical line

A request applies when it arrives unless it carries `:at`. With `:at` it waits
on the [beat scheduler](scheduler.md) as an `:auto/configure` event and the show
owner applies it on the first frame at or past its line, before auto decides
on that frame. A mood change for a section therefore lands on the downbeat:
the previous settings make no decision on the line and the new ones none
before it ([#767](https://github.com/tolgraven/tolglow/issues/767)).

```clojure
@(show/submit! runtime :auto (assoc (red-show/auto-config 218) :at {:bar 16}))
;; => {:status :accepted :scheduled? true :event-ids ["auto/…/0"] :due-beats [64.0] …}
@(show/submit! runtime :auto {:enabled? false :at :next-phrase})
(get-in (show/snapshot runtime) [:auto :pending])
;; => [{:event-id "auto/…/0" :label "Auto configure" :due-beat 64.0 :enabled? true :source {…}} …]
@(show/submit! runtime :command {:op :schedule/cancel :event-ids ["auto/…/0"]})
```

`:at` takes a zero-based position (`{:bar n}`, `{:beat n}`, `{:phrase n}` or a
sum of them) or one of `:next-bar`, `:next-phrase` and `:next-section`, the end
of the auto section now playing (refused when no section plays). JSON clients
send the words as text. `:event-id` and `:label` name the event.

A request is checked when it is posted, against the config it would install
(with `:keep-plan?`, the plan saved then). It is checked again when it
applies, against the plan saved at that point, with one trial decision as an
immediate configure has. A request refused then is recorded as `:rejected` in
the schedule's recent outcomes, and auto mode and its looks stay as they were.

**Late changes** (operator policy). A position that has already passed by less
than one bar applies at once, before auto decides on that frame. Its receipt
and its recent outcome carry `:late? true` and `:late-beats`. A position a bar
or more behind is refused as already passed.

A pending change behaves like any scheduled event:

- It holds while the transport is stopped and fires at its beat after play
  resumes, never caught up.
- A seek or loop moves a `:next-section` change with the section timer, so it
  stays on the section line. A seek past a position reports the change missed.
  A next-bar or next-phrase line re-quantizes from the new position.
- It is saved with the show and listed by `show/schedule`, Studio's timeline
  and `inspect_schedule`. A show saved, or a recovery checkpoint written, while
  a change is pending does not restore on a build that predates
  `:auto/configure`.
- It is cancelled by event ID. An agent cancels what its conversation posted,
  in any turn: an agent source is `{:adapter :agent :session-id
  <conversation-id> :run-id <run-id>}`.
- A subscriber cannot run `:auto/configure`. A subscriber would restart auto
  behind an operator's Stop on every trigger.

Each auto session is named deterministically: `sched/<epoch>/<run>/0` for a
scheduled change, `auto/<epoch>/<revision>` for an immediate one. A replay
gives the same names. Several changes due on the same frame apply in the order
they were posted.

An immediate **Stop auto** withdraws the pending auto changes its sender may
cancel: from Studio or the REPL, all of them; from an agent, its
conversation's. The receipt lists them in `:withdrawn-event-ids` and any that
still wait in `:pending-event-ids`, so nothing waiting on a line starts auto
again unannounced. An immediate start leaves pending changes waiting.

In Studio, **Start or stop** chooses **Now**, **Next bar**, **Next phrase** or
**Next section line** (offered while a plan section plays; the projection gives
its end as `:section-end-beat`; chosen while none plays, the show refuses it, as
it does for the REPL). Pending changes are listed under the buttons
with their position and a **Cancel** button. An agent passes `configure_auto`
an `at` beside its `configuration`, for example `{"configuration": {...},
"at": {"bar": 16}}` or `"at": "next-bar"`.

## Settings

| Key | Meaning |
| --- | --- |
| `:interval` | Time between decisions, e.g. `{:beats 1}`, `{:bars 2}`. |
| `:count` | Cues chosen per decision, 1–64. Emission companions are extra. |
| `:scope` | `:all`, `{:page-id id}` or `{:cue-ids [...]}`. Companions come from the same scope. |
| `:launch` | `{:mode :one-shot\|:timed\|:persistent :duration {...} :params {...}}`. Fixed `:params` are never randomized. |
| `:intent` | `:darkness` lets movement run unlit (repositioning, a dark passage). |
| `:randomize` | Seeded launch parameters; see below. |
| `:categories` | `{:include [..] :exclude [..]}` over each cue's output-derived and declared categories (`:color`, `:intensity`, `:movement`, `:beam-shape`, `:strobe`, `:atmosphere`, `:modulation`). |
| `:max-lit` | At most this many fixtures emit after the decision, counting light already on stage. See the note on the power budget below. |
| `:yield-to-performers?` | Default `true`: a cue someone else is running is not launched again on top of it. |
| `:activity-target` | A number in [0,1], or a signal binding (a waveform LFO, an audio-energy variable, nested arithmetic) sampled at each decision. Cues whose predicted visual activity is closest go first. |
| `:sections` | A musical section plan; see below. |

## Parameters within their domains

`:randomize` chooses launch values for the cues auto mode picks.
`tolglow.automation.launch-params/choose` reads each parameter's schema:

- **Which parameters.** `:params :marked` (default) takes parameters whose
  schema has `:random {:enabled? true}`, a key listed in `:ranges`, and colors
  when a `:palette` is given. `:params :all` takes every parameter with a
  known domain; a vector names the keys.
- **Never randomized.** A value the performer saved on the cue (`:saved-params`,
  unless `:saved :randomize`), a schema with `:random {:locked? true}` or
  `{:enabled? false}`, a seed parameter, and a value the launch fixes.
- **Domains.** Numbers and integers stay inside hard bounds and the narrowest
  of the authored range (`:ranges`, `:random :range`) and `:recommended`.
  Musical periods (`:unit :beats/:bars/:phrases` or `:semantic/type
  :beat-ratio`) pick a power-of-two division from 1/16 to 64 inside that range.
  Colors come whole from a weighted `:palette`, never componentwise. Enums
  honor `:random {:weights {value w}}`. `:max-change` limits the step from the
  cue's current value.
- **Categories.** `:randomize {:categories [:color]}` randomizes only cues in
  one of those categories. A cue in several selected categories is drawn once.
- **Infeasible.** A range with no legal value leaves the parameter at its cue
  value and reports `:infeasible`; it never loops.

Each parameter draws from its own stream keyed by seed, decision and cue, so
adding a cue or a parameter does not change the other choices. The decision's
reason lists what was chosen and why anything was left alone:

```clojure
(get-in (show/snapshot runtime) [:auto :reasons])
;; [{:reason :added-emission :section :red
;;   :randomized {:wash-color {:params {:color [1.0 0.0 0.0] :level 0.82} :excluded {}}}}]
```

The emission forecast uses the chosen values, so a randomized level of zero
cannot pass for light under a moving head.

## Section plans

A plan alternates moods on the bar grid. The first section plays from the
start until the first grid line at least its length less half a grid step
away (a bar line for bar lengths, a phrase line for phrase lengths), so it
runs within half a step of its length; every later boundary is on the grid. Each section has a musical `:length`
(beats, bars or phrases, never milliseconds), a `:weight`, an optional
`:max-run` (how many times in a row it may repeat) and a `:config` that
overrides the settings above while it plays. `:randomize` and `:launch` merge
one level deep: a section palette keeps the base ranges, and a section launch
keeps the base launch `:params` (merged), taking its duration from the section
when it gives one. A section `:interval` or `:interval-ms` replaces both base
forms. `:transition` says what happens to
auto's own looks when the section changes: `:overlap` (default) lets them
finish, `:release` ends them through their release envelopes, `:cut` removes
them at once for an abrupt contrast. A performer's looks are never ended.

`tolglow.automation.red-show` is the R1o example on the 2023 RGBW gig rig:
mostly red, occasionally muted purple, beat one-shots, a quiet section with at
most one lit fixture (movement continues in the dark), and short strobe
sections that cut in and out.

```clojure
(require '[tolglow.automation.red-show :as red-show])
(def runtime (show/start! {:model (red-show/model)}))
@(show/submit! runtime :auto (red-show/auto-config 217))
(get-in (show/snapshot runtime) [:auto :section])
```

Studio shows the plan's section names next to **Keep the section plan** and the
current section and last decision under the buttons. With the box ticked,
**Start auto** and **Stop auto** send `:keep-plan? true`, which keeps the
saved sections, category filters, lit limit, a signal-bound activity target
and the randomize domains (palette, ranges, maximum change, categories). Stop
keeps the whole randomize selection. **Vary marked parameters within their
ranges** starts out ticked when the saved config varies parameters; a start
with it unticked keeps the domains with `:params :none`, so ticking it later
brings the palette back. The REPL can send `:keep-plan? true` too.

## Playing alongside it

Auto mode owns only the instances it launched, and only while nobody else has
touched them. When a performer changes a parameter or a manual control (opacity,
pause, attack, release, gain) of an auto look, from Push, Studio, a knob or the
REPL, that instance is handed over: the reasons record `:performer-took-over`,
and neither a section cut nor **Stop auto** ends it. A cue the performer is
running is not stacked again. Quiet sections count the performer's light
toward `:max-lit` instead of darkening it.

A cue the show refuses (for example a shutter write to heads without a
shutter), whether while predicting light or when the launch is dispatched, is
dropped and reported as `:launch-refused`. A refusal that depends only on the
definition and the rig (`:incapable-target`, `:invalid-targets`,
`:invalid-params`, `:invalid-definition`, `:stale-calibration`, unknown cue or
effect; invalid parameters only when the launch brought none of its own) is
remembered in `[:auto :state :refusals]` and skipped until its cue
or effect version or the patched rig changes, so it is not tried again on every
decision. A refusal that depends on the moment, such as an aim path's
`:start-wrap-mismatch` while a head sits on another pan wrap, or one caused by a
section's launch options, counts for that decision only and is tried again. On the red
show with ten refused cues, remembering them brought decision ticks from p95
2.4 ms / max 3.0 ms to p95 0.6 ms / max 1.2 ms (captured output, 125 ms ticks,
this M4 Pro under load from other work), and auto actually launched its two
valid cues instead of picking refused ones. If the planner itself fails, auto
mode switches off with a `:planner-failed` reason, ends the looks it still owns
(`:released` counts them) and the show keeps rendering.

**Power budget.** With `:max-lit` set, the lit-limit planner chooses instead of
the budget planner: it does not rank cues by unmet watts or release older auto
looks to make room. The budget still caps delivered output, and the emission
forecast is taken under that cap, so a cue the budget would dim to darkness
does not count as lit. Use `:max-lit` for quiet sections and leave it out where
the budget should choose.

## Saving and restart

Auto config and state (seed, decision count, section, owned instances and their
launch records, remembered refusals) are saved with the show and rekeyed on
restore. A show saved by another build may carry settings this planner does not know.
On load they are set aside verbatim under `[:auto :unrecognized]` and listed in
the reasons as `:restored-without-settings`; the next save writes them back
into the config unchanged (restore never rekeys instance IDs inside them).
**Stop auto** and other requests with `:keep-plan?` keep them; any other
configure request replaces the config and drops them.
If what remains still cannot run, the show loads with auto mode off and a
`:restored-disabled` reason, keeping its ownership so **Stop auto** still ends
the looks it launched, instead of refusing the whole file. Configure requests
stay strict. A show restart
discards ownership and the section, and the next decision starts a new one.

## Verification and limits

- `lein test tolglow.automation.launch-params-test tolglow.automation.sections-test tolglow.automation.policy-test tolglow.automation.r1o-scenario-test`
  covers domains, seeded replay, takeover, yielding, categories, lit limits,
  section boundaries and the R1o scenario through the real show owner with
  captured DMX at 120 BPM, including a Push-sourced performer and a transport stop.
- `lein test tolglow.automation.configure-at-test` runs auto changes with `:at`
  through the real show owner: a change on bar 2 lands on the first frame past
  it with no launch of the previous mood from the line and none of the new one
  before it, a pending change holds across a stop and fires once on its beat
  after resume, positions and next bar, phrase and section lines resolve,
  refusals leave nothing scheduled, an immediate stop withdraws a pending change
  and reports what still waits, the agent tool posts, lists and withdraws its
  own (across turns of one conversation, not another conversation's), a
  change arriving a beat after its line applies at once marked late while one
  a bar late is refused, a `:next-section` change stays on the section line
  across seeks of ±2 beats, and a subscriber cannot carry an auto change.
- `lein with-profile +dev run -m tolglow.qa.auto-show-browser` runs the
  optimized Studio in headless Chrome against the red-show example: a REPL plan
  reaching the panel, a Studio start that keeps it, refusals on both sides and a
  REPL stop reaching the page. Headless Chrome is not iPad Safari evidence.
- Not covered: saved compositions (macros and sequences) as auto candidates
  wait on [#216](https://github.com/tolgraven/tolglow/issues/216); new
  definitions invented while playing ("evolving definitions") belong to
  generated material and [#670](https://github.com/tolgraven/tolglow/issues/670),
  whose first slice is the recorded [rehearsal](rehearsal.md) of this planner;
  Studio has no editor for section plans or palettes yet, so author them at the
  REPL or in the show file. Whether a plan feels musical is for the operator
  in the Room and then on fixtures.
