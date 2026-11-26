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
tool. A config the planner refuses comes back as `:status :rejected` with the
first problem in the message, for example `Invalid auto config: Count must be
an integer from 1 to 64 at [:count]`; the committed config is left as it was.

Intervals and durations are beats, bars or phrases read from the transport.
Milliseconds are accepted as an explicit physical option. A stopped transport
makes no decision and holds every timer, so play resumes where the plan was,
without a burst of catch-up launches.

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
| `:max-lit` | At most this many fixtures emit after the decision, counting light already on stage. |
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

A plan alternates moods on the bar grid. Each section has a musical `:length`
(beats, bars or phrases, never milliseconds), a `:weight`, an optional
`:max-run` (how many times in a row it may repeat) and a `:config` that
overrides the settings above while it plays. `:transition` says what happens to
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
and, when **Vary marked parameters within their ranges** is ticked, the
palette and ranges. The REPL can send `:keep-plan? true` too.

## Playing alongside it

Auto mode owns only the instances it launched, and only while nobody else has
touched them. When a performer changes a parameter or a manual control (opacity,
pause, attack, release, gain) of an auto look, from Push, Studio, a knob or the
REPL, that instance is handed over: the reasons record `:performer-took-over`,
and neither a section cut nor **Stop auto** ends it. A cue the performer is
running is not stacked again. Quiet sections count the performer's light
toward `:max-lit` instead of darkening it.

A cue the show would refuse (for example a shutter write to heads without a
shutter) is dropped from that decision and reported as `:launch-refused`; the
rest of the plan continues. If the planner itself fails, auto mode switches off
with a `:planner-failed` reason and the show keeps rendering.

## Saving and restart

Auto config and state (seed, decision count, section, owned instances and their
launch records) are saved with the show and rekeyed on restore. A show restart
discards ownership and the section, and the next decision starts a new one.

## Verification and limits

- `lein test tolglow.automation.launch-params-test tolglow.automation.sections-test tolglow.automation.policy-test tolglow.automation.r1o-scenario-test`
  covers domains, seeded replay, takeover, yielding, categories, lit limits,
  section boundaries and the R1o scenario through the real show owner with
  captured DMX at 120 BPM, including a Push-sourced performer and a transport stop.
- `lein with-profile +dev run -m tolglow.qa.auto-show-browser` runs the
  optimized Studio in headless Chrome against the red-show example: a REPL plan
  reaching the panel, a Studio start that keeps it, refusals on both sides and a
  REPL stop reaching the page. Headless Chrome is not iPad Safari evidence.
- Not covered: saved compositions (macros and sequences) as auto candidates
  wait on [#216](https://github.com/tolgraven/tolglow/issues/216); new
  definitions invented while playing ("evolving definitions") belong to
  generated material and the rehearsal in [#670](https://github.com/tolgraven/tolglow/issues/670);
  Studio has no editor for section plans or palettes yet, so author them at the
  REPL or in the show file. Whether a plan feels musical is for the operator
  in the Room and then on fixtures.
