# Generated knobs

Building a cross-effect knob by hand interrupts a jam. A *knob candidate* is a
complete [linked knob](linked-knobs.md) mapping that the show proposes from the
cues that are running: which parameters move, over what range, in which
direction, along which curve and in which musical steps. The operator sweeps it
in the Room, rerolls until one feels right, edits it, and keeps it as an
ordinary knob, or discards it. Issue
[#667](https://github.com/tolgraven/tolglow/issues/667) tracks this work.

[Generator](../src/tolglow/knob_candidates/generate.clj) ·
[Owner commands](../src/tolglow/knob_candidates.clj) ·
[Room audition](repl-studio-parity.md#audition-a-scene-in-the-room) ·
[Track feel](personalities.md) ·
[REPL and Studio parity](repl-studio-parity.md#generate-a-knob-and-keep-it)

A candidate is a draft. It lives under `:knob-candidates` in the show model,
it is never written to a show file, and it never writes a destination. The live
show and the saved knobs stay as they were until the operator keeps it.

## What the generator does

The generator is pure data to data. `generate/eligibility` reads the running
show and `generate/generate` turns that, an optional track-feel personality and
an integer seed into one candidate. All randomness comes from
`tolglow.automation.random` and the seed, so the same show, feel, seed and
options always give the same candidate.

### Eligible destinations

Every running launch is scanned, or only the cues in `:cue-ids`. A destination
is addressed by cue ID, so it follows relaunches, and listed once even when a
cue runs twice. A namespaced cue is named by its full text (`"show/wash"`); a
bare name selects a cue only when one cue has it. Labels and link IDs keep the
namespace too (`show/wash-level`), so cues that differ only by namespace never
share a link ID.

| Offered | Left out, with a reason |
| --- | --- |
| Number, integer and colour parameters with bounds and a known role | `:unsupported-type` (vector, enum, boolean, gradient, range: linked by hand) |
| A bound waveform's gain (`:to`), period and registered inputs such as `:duty` | `:modulated`: the base value of a parameter a waveform drives (writing it would hold the waveform; its waveform fields are offered instead) |
| | `:knob-driven`: an existing knob already drives it (see below) |
| | `:unbounded`, `:seed-parameter`, `:no-role`, `:no-parameters`, `:not-running`, `:cue-missing` |

#### Destinations an existing knob drives

A destination a saved knob already drives is left out with reason
`:knob-driven`, naming the knob, so a kept candidate does not fight an older
knob ("last committed write wins", [linked knobs](linked-knobs.md)). A knob link
addressed to a running launch by instance ID counts for that launch's cue.
`:include-knob-driven? true` on generate offers those destinations anyway; each
such link's explanation then carries `:shared-with` and the knob IDs.
`generate/knob-driven` lists them from a model.

The recipe records the destinations a generation left out (`:avoid`), and a
repeat avoids exactly those whatever knobs exist now. Without that, the knob a
candidate is kept as would drive its own destinations and the recipe could not
repeat.

A **role** says what a destination does musically. It comes from the schema
first (a colour type, a beat-ratio or beat-unit period) and otherwise from the
parameter name: `level`, `dimmer`, `gain` → intensity; `duty`, `width`, `fade`
→ pulse width; `rate`, `speed`, `decay` → rate; `beats`, `bars`, `period` →
period; `hue`, `saturation`, `lightness`; `spread`, `fan`, `radius` → spread;
`pan`, `tilt`, `x`, `height` → position. `x-bars` is a period, not a position:
earlier roles in the table in the source win.

### Characters

A candidate has one character, which says what turning the knob up means:

| Character | Anchor roles (one required) | Knob up |
| --- | --- | --- |
| Energy | intensity, rate, period | brighter, busier, shorter periods and pulses, more saturated, wider |
| Colour | hue, colour, saturation | the colour turns and deepens, lightness falls |
| Motion | position, spread | positions travel, spread widens, periods shorten |

Each link's direction follows its role in the character, so every link on a
knob agrees about what "more" means. The character is drawn by seed, weighted
by the track feel: a fast feel leans to Energy, a slow smooth one to Colour, a
jittery asymmetric one to Motion. `:character` pins one.

### Correlated choices

Some roles on one cue move together, and then share a knob window (the whole
knob, or its upper half) so they change in the same stretch of the turn:

- intensity with pulse width: **brighter but shorter pulses**;
- intensity with period or rate: brighter and quicker, or busier;
- spread with period: wider and quicker;
- hue with saturation: the colour turns and deepens together.

Picking one makes its partner three times as likely to be picked next.

### Ranges, curves and steps

- A continuous range spreads from the parameter's authored default towards both
  bounds by a seeded width, so the default always lies inside it. Position and
  hue centre on the default instead. Ranges come from schemas, not live values,
  so a recipe repeats after the show has been played.
- A waveform period steps through the **preferred beat ratios** (n/d with n
  and d from 1, 2, 3, 4, 6 and 8), centred on one beat, earlier for a fast feel.
  A period or per-beat rate parameter without a beat unit gets endpoints on
  those ratios but moves continuously; declare `:unit :beats` on the schema to
  make it step.
- A colour runs from its default to a palette colour of the track feel that is
  clearly different from it, or to a fallback colour when no feel is selected.
- Curves follow the feel: a smooth feel eases (`:ease-in-out`, `:sine`,
  `:smoothstep`), a nervous one moves straight (`:linear`, `:ease-in`).
  Intensity favours `:ease-in`. Ratio steps are linear.

### Constraints and score

Each generation draws four drafts and keeps the best-scoring one that meets
every hard constraint (`generate/constraint-errors`):

- two to eight links (eight encoder display columns), no destination twice;
- at least one anchor role of the character;
- every link valid for its destination schema under the knob API's own rules,
  and its two knob ends resolve to different values;
- every direction agrees with the character.

The score (`generate/score`, 0–1) favours variety of roles and of cues, range
widths near 60 % of the bounds, at least one correlated pair, and three to six
links. It is shown with the candidate.

### Too few parameters

When the selection has fewer than two eligible destinations, or none that fits
the pinned character, the result is `:status :insufficient` with a message, the
eligible labels and every excluded parameter with its reason:

```clojure
{:status :insufficient :eligible 0 :needed 2
 :message "No parameter in the selection can be generated; a knob needs at least 2. ..."
 :excluded [{:label "gather-path" :reason :no-parameters :message "The cue has no parameters"} ...]}
```

## Commands

All surfaces send the same owner commands with the usual receipts.

```clojure
{:op :knob-candidate/generate :seed 11}                      ; also :character, :cue-ids, :size [min max], :name,
                                                             ; :include-knob-driven? true
{:op :knob-candidate/generate :from-knob "generated-1"}      ; repeat a kept knob's recipe, or :recipe {...}
{:op :knob-candidate/reroll}                                 ; next seed with a different mapping
{:op :knob-candidate/audition :value 0.75}                   ; sweep in the Room
{:op :knob-candidate/link :link {...}}                       ; replace one link by :id (knob link format)
{:op :knob-candidate/unlink :link-id "strobe-duty"}
{:op :knob-candidate/keep :name "Lift" :value 0.75}          ; optional :knob-id
{:op :knob-candidate/discard}
```

- **Generate** without `:seed` takes one from the command ID and reports it, so
  a replayed command gives the same candidate. The receipt has
  `:candidate-status` `:ready` or `:insufficient`. `:size` is the two link-count
  bounds (2–8) in either order and is kept as `[min max]`.
- **Refusals** carry a `:code`: `:invalid-seed`, `:unknown-character`,
  `:invalid-size`, or `:invalid-knob-candidate` for other malformed input.
- **Reroll** tries the next seeds until the character or destination set
  differs from every candidate of this session, up to sixteen seeds. It stops
  at a seed with no playable mapping. When the candidate it returns repeats an
  earlier one (every seed repeated, or it stopped after a repeat), it carries
  `:repeated? true`.
- **Edits** are validated against the running show by the knob API, recorded
  in the recipe and replayed when it is regenerated. A candidate keeps at least
  two links.
- **Keep** sends `:knob/create` through `tolglow.knobs` with the candidate's
  links and its recipe. The new knob writes nothing until it moves, like any
  knob created at a value.
- **Discard** drops the draft.
- **`:show/restart`** drops the candidate and its reroll session, as it drops
  the launches it was drafted from, the Room draft, surface sessions and
  composition runs. The candidate ID counter continues, so a later candidate
  never reuses an earlier ID. Regenerating instead would find nothing running
  right after the restart.

### Recipes

A kept knob stores `:recipe`:

```clojure
{:generator 1 :seed 13 :character :colour
 :size [3 6] :cue-ids [...]              ; when given
 :avoid [{:kind :param :cue-id :strobe :param :rate}] ; knob-driven destinations left out
 :personality {:id :squircle :version 1} ; when a feel shaped it
 :edits [{:op :unlink :link-id "..."} {:op :link :link {...}}]}
```

`{:op :knob-candidate/generate :from-knob id}` regenerates the same links
against the same running cues and definitions, also after a save and reload.
The recipe names its personality, which is looked up in the show's library at
that version whatever feel is active now. A recipe from a different generator
version is refused rather than silently resolving to other links.

## Room audition

`:knob-candidate/audition` stages an **audition-only Room draft** (#643) with
the draft ID `"knob-candidate"`. The owner takes a fresh private copy of the
show, resolves the candidate's links at the knob value through the knob API,
and writes them into that copy through the shared command path. The audition
worker renders it like any draft: the Room shows the candidate, live output,
the knobs and every destination are untouched.

- Such a draft has no staged operations and carries a `:preview` descriptor
  (`{:kind :knob-candidate :candidate-id :value :name}`), so
  `:scene-draft/apply` refuses it with `:preview-draft`. Keep or discard the
  candidate instead.
- A reroll or an edit restages it with a fresh private copy, so the Room
  follows the current candidate at the last swept value. Keeping or discarding
  ends it, as does `:scene-draft/discard` with that draft ID.
- **Sweeps coalesce.** While the candidate and the committed show are unchanged
  since staging, a new audition value bumps the draft revision and queues a
  *retune*: the audition worker writes the links at the new value into its
  running private model before its next frame, keeping the private compile
  caches and timeline. The receipt says `:retuned? true`. Any committed live
  change (a new show revision), another candidate, an edit, or a stale or failed
  audition stages a fresh copy instead, as before.
- An audition is refused with `:no-running-cues` when none of the candidate's
  cues is running (after their launches were released); there is nothing of
  the knob to show. A restart drops the candidate, so an audition after it is
  refused with `:no-candidate`.
- One Room draft is staged at a time. A candidate audition waits for another
  draft to be applied or discarded (`:draft-active`), and the other way round.
- The snapshot's `:knob-candidates :audition` gives the swept value, the Room
  status and the draft revision; `show/scene-draft` gives the same draft.

## Using it from the REPL

```clojure
(require '[tolglow.show :as show])

@(show/submit! runtime :command {:op :knob-candidate/generate :seed 11})
(-> (show/snapshot runtime) :knob-candidates :candidate
    (select-keys [:seed :character-name :score :explain]))
@(show/submit! runtime :command {:op :knob-candidate/audition :value 1.0})
@(show/submit! runtime :command {:op :knob-candidate/reroll})
@(show/submit! runtime :command {:op :knob-candidate/keep :name "Lift" :value 1.0})
@(show/submit! runtime :command {:op :knob-candidate/generate :from-knob "generated-1"})
```

The generator also runs without a show owner:

```clojure
(require '[tolglow.knob-candidates.generate :as generate])
(generate/generate (generate/eligibility model {:cue-ids [:strobe]}) nil {:seed 3 :character :energy})
```

## Using it from Studio

**Perform → Live → Generate a knob**, under Knobs. Choose a **Character** (or
Any), type a **Seed** (or leave it blank), optionally press cue chips to limit
the selection, and select **Generate**. The candidate shows its seed,
character and score, and each link's label, role, value at the sweep position,
range in sweep order (with swatches for colours), curve, knob window and why it
was chosen, including its correlated partner. Move **Sweep in the Room** to
audition it; the Room banner then reads **Knob candidate** with **Stop
audition** instead of the scene Apply buttons. **Reroll**, **Drop** (a link),
**Keep as knob** (with the name field) and **Discard** send the commands above.
A kept knob with a recipe appears under **Repeat a kept knob**. A selection with
too few parameters shows the explanation and every excluded parameter.

While the sweep is dragged one audition is in flight; moves made meanwhile
collapse into the newest value, sent when the receipt arrives. A REPL or agent
generation or audition moves the open panel and slider.

## Agents

The `knob_candidate` tool takes `op` (`generate`, `reroll`, `audition`, `link`,
`unlink`, `keep`, `discard`) and the same fields. It dispatches the same owner
commands with the agent run as source. `inspect_show` returns the
`:knob-candidates` projection, so an agent reads the candidate, its
explanation or why too few parameters were eligible the same way.

## Verification

- `tolglow.knob-candidates.generate-test`: eligibility roles and every
  exclusion reason, a selection with missing cues, 60 seeds that each meet the
  hard constraints (bounds, defaults inside ranges, exact preferred ratios,
  directions), brighter-but-shorter pulses on one cue, waveform periods that
  only shorten, palette colours and curves following the feel, the
  too-few-parameters explanation, constraint and score unit cases, and three
  distinct rerolls reproducible from their seeds.
- `tolglow.knob-candidates-test` (real show owner, showcase rig): three
  distinct candidates each auditioned at the top of the knob with the Room
  model holding exactly the resolved values while the live parameters,
  waveform bindings, knobs and variables stay unchanged; apply refused; keep,
  turn, save, reload and regenerate from the recipe; edits replayed; too few
  parameters; discard; command replay; the agent tool.
- `dev/tolglow/bench_knob_candidate_sweep.clj` measures the owner tick and the
  Room frame while a candidate is swept on the showcase rig (below).
- `dev/tolglow/qa/knob_candidates_browser.clj` drives the production Studio
  build in headless Chrome against the showcase rig: generate, sweep, reroll
  twice, keep, too few parameters, phone width, then a REPL generation and
  audition followed by the open page.

```
CHROMIUM_PATH=... lein with-profile +dev run -m tolglow.qa.knob-candidates-browser target/knob-candidates-browser 4520
```

### Sweep cost (#737)

`bin/repl run tolglow.bench-knob-candidate-sweep 300` on the showcase rig with
five cues and their waveforms, seed 11: one new audition value every fourth
logical 23 ms owner tick, 300 values after 40 warm-up values, each audition
frame awaited with a spin. Apple M4 Pro (14 cores), JDK 25, pooled REPL, load
average 3.2 from other work. Software CPU frames only; no output, GPU or
display costs are included.

| Median (p90), ms | Restage every value (before) | Coalesced |
| --- | --- | --- |
| Owner tick that takes an audition value | 2.09 (2.46) | 0.39 (0.47) |
| Owner tick without one | 0.40 (0.56) | 0.38 (0.44) |
| Room frame right after a value | 6.57 (7.12) | 0.41 (0.48) |
| Room frame otherwise | 0.38 (0.45) | 0.37 (0.42) |

"Before" forces the previous path (a fresh private copy and compile caches per
value) through the same code. A sleeping wait for each Room frame instead of a
spin makes macOS move both threads to slower cores during a long audition, and
every number then rises over the run; that is a property of the harness, not of
the show.

## Not yet

- Push: a candidate cannot be swept from an encoder yet; the linked-knob Push
  binding it would reuse is itself waiting on an Afterglow adapter change
  ([linked knobs](linked-knobs.md#push)).
- Enum, boolean and vector destinations are not generated; they can be linked
  by hand after keeping.
- Musical subdivisions step only on beat-unit schemas; showcase periods such as
  `:bars` have plain number schemas and move continuously between musical
  endpoints.
- No preference learning (#677) or rehearsal policy (#217) chooses between
  candidates yet; the score is a fixed heuristic.
- Studio has no control for `:include-knob-driven?` and does not show a link's
  `:shared-with`; the REPL and the agent tool have both. Knob-driven exclusions
  appear in Studio only in the too-few-parameters explanation.
