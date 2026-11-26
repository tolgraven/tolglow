# Linked knobs

A linked knob is one normalized control, 0 to 1, that moves several typed
destinations at once: a colour's saturation on one cue, a head's pan on
another, a waveform's period in exact beat ratios, and the duty cycle and gain
of a square LFO on a dimmer. Each destination keeps its own identity and stays
editable on its own. The knob stores only links. Issue
[#666](https://github.com/tolgraven/tolglow/issues/666) tracks this work.

[Source](../src/tolglow/knobs.clj) · [Scheduler and ramps](scheduler.md) ·
[Waveform destinations](modulation.md#waveforms-on-a-running-instance) ·
[REPL and Studio parity](repl-studio-parity.md#turn-one-knob-across-several-cues)

These are called *knobs*, not macros. A saved macro (`:macro/run`,
[#216](https://github.com/tolgraven/tolglow/issues/216) and
[#546](https://github.com/tolgraven/tolglow/issues/546)) composes cue slots.
A knob owns no cues and launches nothing.

## Data

The show model keeps portable, versioned data under `:knobs`. It contains no
functions, so a full backup saves it and a restore validates it.

```clojure
{:schema-version 1 :next-order 1
 :knobs {:look {:id :look :name "Look" :value 0.5 :order 0
                :links [{:id "sat" :destination {:kind :param :cue-id :tint :param :saturation}
                         :range [0.2 1.0] :direction :up :curve :linear :input [0 1]}]}}}
```

A link has these fields:

| Field | Meaning |
| --- | --- |
| `:id` | Stable ID within the knob. Linking the same ID again edits that link |
| `:destination` | What the link drives (table below) |
| `:range` | `[from to]` in the destination's own units and type |
| `:direction` | `:up` (default) or `:down`, which runs the range from `to` back to `from` |
| `:curve` | Any `afterglow.render.easing` curve, as in scheduler ramps. Default `:linear` |
| `:input` | `[low high]` part of the knob that moves this link. Default `[0 1]`. Below `low` the link holds `from`, above `high` it holds `to` |
| `:quantize` | Optional. `{:ratios :preferred}` or `{:ratios :free}` for beat ratios, `:integer` for whole numbers |
| `:unit` | Optional unit for a show variable, which has no schema |

Destinations:

| `:kind` | Fields | Type comes from |
| --- | --- | --- |
| `:param` | `:cue-id` or `:instance-id`, `:param` | The effect parameter's schema |
| `:lfo` | `:cue-id` or `:instance-id`, `:param`, and `:field` (`:from`, `:to`, `:period`) or `:input` (a waveform input such as `:duty`) | The bound parameter's schema for `:from`/`:to`; exact beats for `:period`; the waveform's registered input schema for `:input` |
| `:variable` | `:variable` | A number, with the link's `:unit`. Runtime variables are reserved, as for `:variable/set` |

A `:cue-id` addresses every running launch of that cue, as scheduler and agent
actions do, and keeps working across relaunches and restores. An
`:instance-id` addresses one launch. The waveform `:to` endpoint is the pulse's
high level, so it acts as the LFO's gain.

Supported destination types are number, integer, colour, vector, enum and
boolean. Gradient and range parameters are refused with a reason.

## Commands

All surfaces send the same owner commands with the usual receipts, show ID,
epoch and command-ID replay.

```clojure
{:op :knob/create :knob-id :look :name "Look" :links [...]}   ; :value defaults to 0
{:op :knob/rename :knob-id :look :name "Wash look"}
{:op :knob/link   :knob-id :look :link {...}}                 ; add, or edit by link :id
{:op :knob/unlink :knob-id :look :link-id "sat"}
{:op :knob/set    :knob-id :look :value 0.75}
{:op :knob/nudge  :knob-id :look :delta 1/64}                 ; relative encoders; clamps to 0–1
{:op :knob/remove :knob-id :look}
```

Linking validates the destination against the running show. An unknown cue,
missing parameter, endpoint outside the parameter's bounds, ratio steps on a
parameter that is not a beat ratio, or a waveform link with no waveform bound is
refused. A cue that is not running yet can be linked. Its links are skipped
until a launch exists.

## Resolving a knob value

For each link the knob position goes through the input window, then the curve,
then the direction, giving a fraction `u` from 0 to 1. A continuous destination
takes `from + u × (to − from)`, component-wise for colours and vectors, clamped
to the parameter's bounds and rounded for integers.

A stepped destination (beat ratios, enums, booleans) divides `u` into equal
steps: step `floor(u × n)` of the `n` values from `from` to `to`.

- **Preferred ratios** are the exact fractions `n/d` with `n` and `d` from 1, 2,
  3, 4, 6 and 8, the same set as cue variations and the Push ratio pad (#529,
  #547), inside the link range and the parameter's bounds. A period range of
  `[2 1/2]` steps through 2, 3/2, 4/3, 1, 3/4, 2/3 and 1/2 beats.
- **Free ratios** use every fraction with a denominator up to 16 in the range.
- An **enum** steps through its declared values between the two endpoints.
  A **boolean** switches at the halfway point.

**No hysteresis.** A knob position exactly on a boundary selects the step that
a slightly higher position selects, in either sweep direction. A position a
hair below selects the previous step. Boundaries are placed before the
direction is applied, so `:direction :down` over `[a b]` steps exactly like
`:up` over `[b a]`, boundaries included. Positions within 10⁻⁹ below a boundary
count as on it, so a decimal `3/7` behaves like the exact fraction. The knob is the operator's own position,
so the same value must give the same step on Studio, Push, the REPL and after a
reload. Hysteresis would make the step depend on the path taken. Layer priority
uses hysteresis because it follows a free-running waveform; a knob does not.

Ratios stay exact. `[:knobs ...]` in the snapshot reports `:resolved {:value 3/2
:text "3/2 beats" :unit :beats :ratio [3 2]}`, and Transit carries the fraction
to Studio unchanged.

## How the value reaches destinations

`:knob/set` commits the knob value and every waveform-field link in one owner
command. The owner then sends each parameter and show-variable link as a
`:param/set` or `:variable/set` through the same command reducer that a Studio
control uses. Each write gets the derived command ID `<command-id>/<n>` and the
knob command's source. The receipt lists every link with `:status`:

| Status | Meaning |
| --- | --- |
| `:applied` | Written |
| `:skipped` | Not written, with `:reason`: `:not-running`, `:gesture-open`, or a dangling reason |
| `:rejected` | The destination refused the value, with its `:message`. Other links still apply |

Replaying a knob command ID replays its writes from their receipts, so nothing
is written twice and a later edit survives.

Resolution reads only the knob value and link data. It samples no waveform and
advances no random controller, so a frame never evaluates a stateful signal a
second time for a knob.

## Overrides and conflicts

Knobs reuse the existing override contract. They add no parallel one.

- **Manual edit or a finished gesture on a destination.** The destination
  takes the human value and keeps it. The knob keeps its own position. The
  projection marks the link `:diverged? true`. The next knob move writes the
  resolved value again: last committed writer wins, with no pickup or
  catch-up mode.
- **An open human gesture on a destination.** While a Push, Studio or other
  operator gesture holds a parameter (`:gesture/begin` until `:gesture/end` or
  `:gesture/cancel`), knob moves skip that destination with `:reason
  :gesture-open`. The other links still follow the knob.
- **A scheduler ramp on a destination.** A knob write is a committed write, so
  it cancels the ramp as [scheduler.md](scheduler.md#ramp-semantics) describes.
  From Studio, Push or the REPL `:recent` records `:reason :human-override`; from
  an agent or reactive source it records `:reason :superseded`. A ramp posted
  after the knob moved runs, commits its target, and the link then shows
  `:diverged?`.
- **A waveform or random controller on the same parameter.** The knob writes the
  base value. Under the existing manual-edit rule, an operator write to a
  parameter with an active waveform or random controller holds that source until
  it is resumed. Link the waveform's own fields (`:lfo` destinations) to shape
  the modulation instead of fighting it.
- **Two knobs on one destination.** Both are allowed. Each link lists the other
  knobs in `:shared-with`. The last committed knob write wins, in owner command
  order. There is no averaging or priority between knobs.
- **Editing a link** (range, direction, curve, window) changes how the knob
  resolves but writes nothing. The link shows `:diverged?` until the knob next
  moves, so reshaping a range while authoring never jumps a destination.
- **Removing a knob** deletes only its links. Destinations keep their
  definitions and current values, including any waveform period, duty or gain
  the knob last set.

## Dangling and incompatible targets

The projection resolves each link against the current show. A link's
`:status` is `:ok`, `:inactive` (its cue has no running launch) or `:dangling`
with a `:reason` and `:message`:

| Reason | When |
| --- | --- |
| `:cue-missing` | The cue no longer exists |
| `:instance-missing` | The addressed launch ended, or did not survive a restore |
| `:param-missing` | The effect no longer has that parameter |
| `:lfo-missing`, `:input-missing` | The waveform was unbound, or it has no such input |
| `:incompatible` | The range no longer fits the parameter, for example after a revision changed its bounds or type |

Dangling links are never dropped and never crash a knob. Setting the knob
skips them and applies the rest. Linking the same link ID to a live destination
clears the state.

A full backup saves knobs as they are. Restoring rebinds `:instance-id` links to
the new launch IDs. A link whose launch did not survive keeps its old ID with a
saved `:dangling {:reason :instance-missing}` marker, so it cannot attach to an
unrelated launch. Other reasons are derived again from the restored show.

## Using it from the REPL

```clojure
(require '[tolglow.show :as show] '[tolglow.knobs :as knobs])

@(show/submit! runtime :command
   {:op :knob/create :knob-id :look :name "Look"
    :links [{:id "sat" :destination {:kind :param :cue-id :tint :param :saturation} :range [0.2 1.0]}
            {:id "pan" :destination {:kind :param :cue-id :aim :param :pan} :range [0.25 0.75] :curve :ease-in-out}
            {:id "rate" :destination {:kind :lfo :cue-id :pulse :param :level :field :period}
             :range [2 1/2] :quantize {:ratios :preferred}}
            {:id "gain" :destination {:kind :lfo :cue-id :pulse :param :level :field :to}
             :range [0.5 1.0] :input [0.5 1.0]}
            {:id "duty" :destination {:kind :lfo :cue-id :pulse :param :level :input :duty}
             :range [0.125 0.5] :direction :down :input [0.5 1.0]}]})

(:links @(show/submit! runtime :command {:op :knob/set :knob-id :look :value 1.0}))
(->> (show/snapshot runtime) :knobs :knobs first :links (map (juxt :id (comp :text :resolved))))
;; => (["sat" "1.000"] ["pan" "0.750"] ["rate" "1/2 beats"] ["gain" "1.000"] ["duty" "0.125"])
```

The waveform must be bound first, for example with `:modulation-bind` as in
[modulation.md](modulation.md#waveforms-on-a-running-instance).

## Using it from Studio

**Perform → Live → Knobs** lists every knob with a slider, and under it each
link's destination, resolved value with units (for example **1/2 beats**), its
range, direction and curve. Type a name and select **Create knob**. Open **Add
destination**, choose a running cue's numeric parameter, a bound waveform's
low, gain (high), period or input, or a show variable, then set **From**,
**To**, **Direction**, **Curve**, **Steps** (continuous, whole numbers,
preferred or free beat ratios) and the **Knob start**/**Knob end** window, and
select **Link**. Fractions such as `1/2` are accepted. Studio sends the same
`:knob/*` commands and redraws from the committed `:knobs` projection, so a
REPL `:knob/set` moves the open slider and values. While the slider is dragged
one `:knob/set` per knob is in flight; turns made meanwhile collapse into the
newest position, sent when the receipt arrives, so the owner admits them in
order and ends on the last position. The slider shows the local position until
a snapshot at or past the receipt's revision arrives. A link that differs from the
knob, is shared with another knob, inactive or dangling says so under its
name. The picker offers numeric destinations only; colour, enum and boolean
links made from the REPL are shown and can be re-shaped, but their endpoints
are edited from the REPL.

`dev/tolglow/qa/knobs_browser.clj` drives this in headless Chrome against the
preview-only demo show on port 4300: it builds a four-link knob in the panel,
turns it, edits a direction, checks the projection and destination values
through `/api/state`, checks a refusal, then sets the knob from the JVM and
checks the open page follows.

```
CHROMIUM_PATH=... lein with-profile +dev run -m tolglow.qa.knobs-browser target/knobs-browser 4300
```

## Push

A Push encoder turns a knob by sending `knobs/encoder-command`: `:knob/nudge`
with 1/100 of the range per detent, or 1/1000 with Shift, the same steps as the
master encoder. Push is an operator surface, so a turn takes over a ramped
destination with `:reason :human-override`. `knobs/encoder-display` gives the
display the knob value and up to eight link columns with the same resolved text
and exact ratios Studio shows. `tolglow.knob-sweep-test` drives both with a Push
3 source.

**Not yet bindable on the device.** The Push adapters live in the Afterglow fork
and only edit the selected launch's parameters (Push 2) or captured
`:encoder-bindings` of `{:param k}` or `{:target :master}` (Push 3). Binding an
encoder to a knob needs this adapter change in `tolgraven/afterglow`:

- `push_3.clj`: accept `{:target :knob :knob-id id :step s}` in
  `:encoder-bindings` validation (`user-profile`). In `encoder`, emit
  `{:op :knob/nudge :knob-id id :delta (* delta (or step 1/100) (if fine? 1/10 1))}`
  for that target, as the `:master` branch does. In `touch-encoder`, start no
  parameter gesture for it and record `:touched-control {:target :knob :knob-id
  id}` for the display view.
- `push_2.clj`: an optional host `:encoder-page` option, `{:active? (fn [state])
  :turn (fn [model encoder-index delta fine?] -> commands-or-nil)}`, consulted in
  `encoder` before the selected-parameter path, so the host can place up to
  eight knobs on the encoders while a page button is held or toggled.
- Both display views: pass `:touched-control` through so the host projector can
  call `knobs/encoder-display` and draw the knob's columns.

The Tolglow side then passes the bindings in `show/connect-device!` options. The
virtual Push 2/3 replay harness would prove the turn and the display; that and
physical Push hardware are not verified here.

## Verification

`tolglow.knob-sweep-test` drives the real owner tick and captures ArtDmx. One
knob sweep up and back down checks saturation (green channel), eased pan, the
preferred-ratio period, and the square LFO's gain and duty on the dimmer at
each frame's beat, including exactly on and just below each ratio boundary in
both directions. Halfway the pulse is half a beat long at half level. At the
bright end it is a 1/16-beat pulse at full level. Other tests cover manual
edits, an open gesture, ramp cancellation, two knobs on one destination,
removal, command replay, a partial receipt when one derived write is refused,
a full backup round trip and every dangling reason before and after reload.
`tolglow.knobs-test` covers validation and every supported type without a
running show, including `:direction :down` stepping exactly like a swapped
range.

## Room audition

A Room draft (#531) cannot stage `:knob/*` commands, and the audition's private
model carries no knobs, so an audition frame never commits knob or destination
state. A knob turned while a draft is staged is a live edit after staging: it
writes the live show, and the Room keeps the values the draft was staged with,
as for any other live parameter edit. Applying the draft replays its steps on
the live show, which keeps the knob and the values it last wrote.
`tolglow.knob-sweep-test/a-room-audition-never-commits-knob-or-destination-state`
checks this.

## Knob looks

A knob look (#547) saves the current position of every knob in one of eight
slots, so a performer can return to a whole look in one press. A slot stores
knob positions only, never destination values: recall moves the knobs and each
link resolves through its current range, curve, direction and steps, exactly
as a turn does. The bank lives in `src/tolglow/knob_presets.clj` and is saved
under the show's `:extensions`, so show saves and full backups keep it.

| Command | Fields | Effect |
| --- | --- | --- |
| `:knob-preset/capture` | `:slot` 0–7, optional `:overwrite? true` | Store every knob's position. A saved slot is refused unless `:overwrite?` is set. |
| `:knob-preset/recall` | `:slot` | Move every stored knob that still exists, then fan out its links. |
| `:knob-preset/clear` | `:slot` | Empty the slot. |

Each command accepts `:expected`, the slot contents the surface showed (`nil`
for an empty slot). A slot changed elsewhere since is refused with
`:stale-knob-preset` instead of being recalled or replaced. Push and Studio
always send it.

Recall commits every stored knob value and waveform-field link in one owner
command, then sends the parameter and variable writes through the shared
reducer in the same turn (the `knobs/set-value` path), so the next frame shows
every destination together and ramps, open gestures and manual holds behave as
for a knob turn. The receipt lists every link as `[knob-id link-id]`. Knobs
created after the look was saved, and parameters no knob links to, are not
written. A knob removed since is skipped and listed under `:missing`; when none
of a look's knobs remain, recall is refused with `:knob-preset-unavailable`.

The `:knob-presets` projection in the show snapshot gives eight slots. Each has
`:state` `:empty`, `:stored` or `:selected` (the look last saved or recalled),
`:matches?` (every stored knob still sits at its saved position) and `:knobs`
with each stored position, the knob's current position and `:missing?` for a
removed knob.

```clojure
(show/submit! runtime :command {:op :knob-preset/capture :slot 0})
(show/submit! runtime :command {:op :knob/set :knob-id :look :value 0.9})
(:links @(show/submit! runtime :command {:op :knob-preset/recall :slot 0}))
(get-in (show/snapshot runtime) [:knob-presets :slots 0 :state]) ;; => :selected
```

**Studio.** Under **Perform → Live → Knobs**, **Knob looks** shows the eight
slots with each stored knob position. An empty slot offers **Save**; a saved
one **Recall**, **Overwrite** and **Clear**. The selected look is outlined, and
outlined in blue once its knobs have moved. A refusal shows its reason.

**Push.** With the variation overlay enabled (`:cue-variations? true` on Push 2,
`:cue-variation-pads? true` on Push 3 with a captured `:variation/cycle`
button), the cycle button opens knob looks after a selected cue's full states and
parameter rows, or first when no cue is selected. The top pad row holds the
eight slots: an empty pad saves the knobs into it, a saved pad recalls it, and
Record (Push 2) or the captured `:variation/record` button (Push 3) held with a
saved pad overwrites it. Cycle closes the overlay. Pads are dim when empty,
green when saved, white when selected and blue when selected but moved; the
display names each slot and its knob positions. As for cue variations, a press
counts only once the pads (Push 2 LED acknowledgement) or the display frame
(Push 3 presentation receipt) showing that slot have arrived. On Push 3 only the
top-row pads in the captured layout are shown.

`tolglow.knob-presets-test` covers capture, recall and refusals on captured
ArtDmx, Studio's command route and `/api/state`, a virtual Push 2 and a captured
Push 3 layout, command replay and save/restore.
