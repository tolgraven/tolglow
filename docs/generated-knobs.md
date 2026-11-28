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
| Number, integer, colour, enum, boolean and vector parameters with bounds where they are numeric, and a known role (generator version 4; earlier recipes replay with numbers, integers and colours only) | `:unsupported-type` (gradient, range: linked by hand) |
| A bound waveform's gain (`:to`), period and registered inputs such as `:duty` | `:modulated`: the base value of a parameter a waveform drives (writing it would hold the waveform; its waveform fields are offered instead) |
| | `:knob-driven`: an existing knob already drives it (see below) |
| | `:unbounded`, `:seed-parameter`, `:no-role`, `:no-parameters`, `:not-running`, `:cue-missing` |
| | `:too-few-steps`: an integer whose range holds fewer than two whole values (generator version 3 and later), an enum with one value, or a number or vector whose range is narrower than its `:step` (version 4; `generate` lists it under `:excluded`) |
| | `:type-not-selected`: a `:types` selection leaves its type out |

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
repeat. `:avoid` only replays the recipe's own candidate: rerolls after a
repeat leave out the destinations the current knobs drive, like a fresh
generation, and share them only when the recipe was made with
`:include-knob-driven? true` (which it then records).

A **role** says what a destination does musically. It comes from the schema
first (a colour type, a beat-ratio or beat-unit period) and otherwise from the
parameter name: `level`, `dimmer`, `gain` → intensity; `duty`, `width`, `fade`
→ pulse width; `rate`, `speed`, `decay` → rate; `beats`, `bars`, `period` →
period; `hue`, `saturation`, `lightness`; `spread`, `fan`, `radius` → spread;
`pan`, `tilt`, `x`, `height` → position. `x-bars` is a period, not a position:
earlier roles in the table in the source win. An enum is a **choice** and a
boolean a **switch**, whatever their names. A vector is a position when its
schema says `:semantic/type :position` or its name reads as one (`centre`,
`point`, `offset`, `origin`, `target`, `aim`, or the number tokens), and a
spread when it reads as a size (`extent`, `scale`, `size`, `spread`, ...); a
vector with neither is left out with `:no-role`.

`:types` on generate (a list of `number`, `integer`, `colour`, `vector`,
`choice`, `switch`, or the schema type names) keeps only those destination
types; the recipe records it, so a repeat uses the same selection. The
character still needs one of its anchor roles, so a selection of choices and
switches alone is explained as too few.

### Characters

A candidate has one character, which says what turning the knob up means:

| Character | Anchor roles (one required) | Knob up |
| --- | --- | --- |
| Energy | intensity, rate, period | brighter, busier, shorter periods and pulses, more saturated, wider |
| Colour | hue, colour, saturation | the colour turns and deepens, lightness falls |
| Motion | position, spread | positions travel, spread widens, periods shorten |

Choices and switches anchor no character but join any of them with weight 1:
a choice steps up through its declared values and a switch turns on as the
knob rises.

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
- An integer parameter gets whole-number endpoints inside its bounds, at least
  one step apart, and `:quantize :integer`, so the knob steps through whole
  values. Its range is drawn like a number parameter's and then rounded; an
  integer with a beat unit steps through whole beats, not ratios, centred on
  its default or, when that is not positive, on a beat as a waveform period
  is. An integer whose range holds fewer than two whole values is left out
  with reason `:too-few-steps`.
- A colour runs from its default to a palette colour of the track feel that is
  clearly different from it, or to a fallback colour when no feel is selected.
- A **choice** (enum) steps through a contiguous span of two to five of its
  declared values that holds the default where it can, in declared order, on
  a linear curve so each value has an equal share of the knob window.
- A **switch** (boolean) runs off to on and switches at a seeded knob position
  (0.5, 0.625 or 0.75, through its input window), on a linear curve.
- A **vector** moves along a seeded curve through vector space. A position
  goes from the default by a seeded width of the range along a seeded
  direction, each component kept inside the bounds; a spread grows every
  component from below the default towards the upper bound. The curve follows
  the feel like a number's.
- A number, integer or vector whose schema declares `:step` keeps its
  endpoints (every component of a vector) on that grid, anchored at the lower
  bound, and gets `:quantize {:step size}`, so the knob steps through the grid.
- A colour target is kept inside the schema's colour bounds.
- Curves follow the feel: a smooth feel eases (`:ease-in-out`, `:sine`,
  `:smoothstep`), a nervous one moves straight (`:linear`, `:ease-in`).
  Intensity favours `:ease-in`. Ratio steps are linear.
- **Envelopes (version 4).** Every stepped link (choice, switch, integer,
  beat ratio, grid) gets `knobs/stepped-envelope`: the smoothed knob crosses
  the whole range in one beat either way and the step is read after it, so a
  brief spike or a noisy input does not change the step, and at rest the step
  is the plain step at the knob value ([linked knobs](linked-knobs.md#resolving-a-knob-value)).
  Continuous links (numbers, colours, vectors without a grid) follow the knob
  at once. Version 4 never emits `:hysteresis`.

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
                                                             ; :include-knob-driven? true, :types ["choice" "switch"]
{:op :knob-candidate/generate :from-knob "generated-1"}      ; repeat a kept knob's recipe, or :recipe {...}
{:op :knob-candidate/reroll}                                 ; next seed with a different mapping
{:op :knob-candidate/audition :value 0.75}                   ; sweep in the Room
{:op :knob-candidate/link :link {...}}                       ; replace one link by :id, or add one (knob link format)
{:op :knob-candidate/unlink :link-id "strobe-duty"}
{:op :knob-candidate/keep :name "Lift" :value 0.75}          ; optional :knob-id
{:op :knob-candidate/discard}
```

- **Generate** without `:seed` takes one from the command ID and reports it, so
  a replayed command gives the same candidate. The receipt has
  `:candidate-status` `:ready` or `:insufficient`. `:size` is the two link-count
  bounds (2–8) in either order and is kept as `[min max]`.
- **Refusals** carry a `:code`: `:invalid-seed`, `:unknown-character`,
  `:invalid-size`, `:invalid-types`, `:unknown-link`, `:duplicate-destination`,
  `:unsupported-generator`, or `:invalid-knob-candidate` for other malformed
  input.
- **Reroll** tries the next seeds until the character or destination set
  differs from every candidate of this session, up to sixteen seeds. It stops
  at a seed with no playable mapping. When the candidate it returns repeats an
  earlier one (every seed repeated, or it stopped after a repeat), it carries
  `:repeated? true`.
- **Edits** are validated against the running show by the knob API, recorded
  in the recipe and replayed when it is regenerated. A candidate keeps at least
  two links. An unlink names an existing link (`:unknown-link`). A link with a
  new ID adds one, but never a second link on a destination the candidate
  already drives (`:duplicate-destination`): replace that link by its ID. A
  replayed edit gets the same checks, so an edit recorded against other link
  IDs is refused instead of silently dropped or doubled. Replacing a link by
  its ID onto a destination another link drives is refused the same way. The
  knob API itself still allows two links on one destination; a candidate does
  not, because the later link would overwrite the earlier one on every turn. A
  recipe kept before #737 whose recorded edit added such a duplicate therefore
  no longer repeats: it is refused with `:duplicate-destination`.
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
{:generator 4 :seed 13 :character :colour
 :size [3 6] :cue-ids [...]              ; when given
 :types [:boolean :enum :number]         ; when given
 :avoid [{:kind :param :cue-id :strobe :param :rate}] ; knob-driven destinations left out
 :include-knob-driven? true              ; when made with sharing
 :personality {:id :squircle :version 1} ; when a feel shaped it
 :edits [{:op :unlink :link-id "..."} {:op :link :link {...}}]}
```

`{:op :knob-candidate/generate :from-knob id}` regenerates the same links
against the same running cues and definitions, also after a save and reload.
The recipe names its personality, which is looked up in the show's library at
that version whatever feel is active now. A recipe from an unknown generator
version is refused (`:unsupported-generator`) rather than silently resolving to
other links.

Version 2 (#737) keeps the cue's namespace in link IDs and may carry `:avoid`,
so a build that only knows version 1 refuses it. This build still replays
version 1 recipes as version 1 did: link IDs and owner grouping (for
correlated pairs and owner variety) use the cue's plain name (`feel-level` for
`:rig/feel`), so their links and recorded edits match.

Version 3 (#747) links integer parameters with whole-number endpoints; versions
1 and 2 drafted fractional endpoints that the knob API refused, so their
candidates never drove an integer parameter. A version 1 or 2 recipe still
replays that way, leaving integer parameters out (and keeping integers that
cannot step in the pool, whose size feeds the draws), and a show without
integer parameters generates the same links under versions 2 and 3.

Version 4 (#667) generates enum, boolean and vector destinations, keeps `:step`
grids and gives stepped links the stepped envelope. It draws nothing extra
for numbers and colours, so a show without the new types or `:step` generates
version 3's links with the envelope added to the stepped ones. A version 1–3
recipe replays with numbers, integers and colours only and no envelope;
`tolglow.knob-candidates.golden-test` checks 1080 version 1–3 candidates
drawn by master's generator before version 4 (`v3_golden.edn`), on a show
with every type and a number whose `:step` is wider than its range.

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
  audition stages a fresh copy instead, as before. The saving therefore applies
  only while the committed show is quiet: a routing patch writing changed
  values, audio returns, composition steps, auto launches or a second encoder
  all commit revisions, and with any of them running, every value still
  restages.
- Each Room frame records the draft revision it rendered. `:frame-revision`
  appears next to `:draft-revision` in `:knob-candidates :audition`, in
  `show/scene-draft` and on the snapshot's `:draft-output`. Until the worker has
  rendered a new value, the Room output is the previous value's frame, and the
  two revisions differ.
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

Type chips (**Numbers**, **Integers**, **Colours**, **Vectors**, **Choices**,
**Switches**) restrict the destination types, sent as `:types`; none pressed
means every type. **Add a destination to the candidate** under the links is
the knobs panel's typed picker: choose any running destination, its endpoints
in its own type (declared values for a choice, off/on for a switch, components
for a colour or vector), steps, attack and release, and select **Link**. It
sends `:knob-candidate/link`, so the link is validated by the knob API and
recorded in the recipe, and the sweep then auditions it in the Room with the
rest. A link row shows its envelope.

The audition shows where the candidate settles at the sweep value: links
resolve at once there, envelopes left out, which is the step a kept knob
reaches once its envelope has moved.

While the sweep is dragged one audition is in flight; moves made meanwhile
collapse into the newest value, sent when the receipt arrives. A REPL or agent
generation or audition moves the open panel and slider.

## Agents

The `knob_candidate` tool takes `op` (`generate`, `reroll`, `audition`, `link`,
`unlink`, `keep`, `discard`) and the same fields. It dispatches the same owner
commands with the agent run as source. `inspect_show` returns the
`:knob-candidates` projection, so an agent reads the candidate, its
explanation or why too few parameters were eligible the same way.

The `knob` tool (#667) covers the whole loop over every destination type in
one declaration: `propose` (generate, with `types` as well as the generate
fields), `reroll`, `audition`, `keep` and `discard` for the draft; `link` and
`unlink` on the draft, or on the kept knob that `knob-id` names; and `create`,
`rename`, `remove`, `set` and `nudge` (`delta`) on kept knobs, which change live
output like `set_parameter`. `remove`, and `unlink` on a kept knob, work only
on knobs the same agent conversation created (`:created-by`); the operator's
knobs can be linked, renamed and turned but not deleted. The show merges the
command envelope last, so tool fields never replace its source or command ID. `tolglow.knob-candidates/knob-tool-command`
translates a call into exactly one `:knob-candidate/*` or `:knob/*` command,
so it is validated and applied like the REPL and Studio. A knob ID sent as
text also finds a knob the REPL created with a keyword ID, and enum endpoints
sent as text become the declared values. `inspect_show` returns `:knobs` with
every link's resolved value.

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
  parameters; discard; command replay; the agent tool. For #737: retuned sweeps
  checked on the private model, the rendered packets and `:frame-revision`;
  two retunes behind a slowed in-flight frame (the last value wins, and the
  in-flight frame keeps its own revision); a reroll racing an in-flight frame;
  a stopped transport holding across retunes; a restart; and rerolls after a
  recipe repeat leaving out knob-driven destinations.
- `dev/tolglow/bench_knob_candidate_sweep.clj` measures the owner tick and the
  Room frame while a candidate is swept on the showcase rig (below).
- `tolglow.knob-typed-destinations-test` (#667), through the real generator,
  knob API and show owner: every type drafted within its schema over 300
  seeds, stepped links with the stepped envelope and no hysteresis; `:types`;
  older recipes without the new types or envelopes;
  - scheduled steps against per-frame evaluation: one enveloped knob turned up,
    retargeted down and up again, and the same links without envelopes turned
    every frame to the first run's smoothed level. Committed values and
    captured packets match on all 90 frames, and stepped links were evaluated
    161 times instead of 546;
  - a noisy input: twenty one-frame spikes to 0.95 from 0.3 change no stepped
    destination and commit nothing for them, while a sustained move does;
  - a sweep that is monotone, visits each step once and settles on the plain
    step at the knob value, by one turn and by a hand sweep;
  - retargets mid-move, up and down, by one step and by several: the level
    carries on from where it was, no step is skipped or committed twice;
  - Push coarse detents on a 64-step integer and a 12-value choice reach both
    endpoints; the same crossings at the same beats at 70 and 180 BPM; a
    stopped transport holds a move and resumes it without catching up;
  - a knob look recalls the captured step; envelope state saved, validated
    and dropped for links that went; bounded grids; a stepped `:param/ramp`
    whose frames look values up and whose takeover carries on from the exact
    position; the Room audition; and the `knob` agent tool, including the
    refusal to remove the operator's knobs.
- `dev/tolglow/qa/knob_typed_browser.clj` drives the production Studio build in
  headless Chrome on a cue with a parameter of every type: a candidate limited
  to choices, switches and numbers, a choice added from the picker with its
  envelope and swept in the Room, a switch linked to a kept knob and turned
  live until its envelope settles, phone width, then a REPL knob with a vector
  link followed by the open page.
  `CHROMIUM_PATH=... lein with-profile +dev run -m tolglow.qa.knob-typed-browser target/knob-typed-browser 19701`
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
average 3.9 from other work. Software CPU frames only; no output, GPU or
display costs are included.

This is the best case: the showcase has no live writer committing revisions
between values, so all 300 values retuned. In a show with routing, audio
returns, compositions or auto launches running, values restage and cost what
the "before" column shows.

| Median (p90), ms | Restage every value (before) | Coalesced |
| --- | --- | --- |
| Owner tick that takes an audition value | 2.26 (2.56) | 0.43 (0.52) |
| Owner tick without one | 0.45 (0.61) | 0.40 (0.48) |
| Room frame right after a value | 6.68 (7.36) | 0.43 (0.53) |
| Room frame otherwise | 0.40 (0.48) | 0.37 (0.43) |

"Before" forces the previous path (a fresh private copy and compile caches per
value) through the same code. A sleeping wait for each Room frame instead of a
spin makes macOS move both threads to slower cores during a long audition, and
every number then rises over the run; that is a property of the harness, not of
the show.

## Not yet

- Push: a candidate cannot be swept from an encoder yet; the linked-knob Push
  binding it would reuse is itself waiting on an Afterglow adapter change
  ([linked knobs](linked-knobs.md#push)).
- Vectors move along a straight segment (eased by the curve), not a path
  through several points, and per-component links to one vector parameter are
  not offered.
- Envelopes are linear slews; there is no exponential follower. The Push
  encoder path is tested only through the shared `:knob/nudge` command.
- A `:param/ramp` schedules its steps only for an integer destination with a
  linear curve; other ramps sample every frame as before. Enum and boolean
  parameters still cannot ramp.
- Musical subdivisions step only on beat-unit schemas; showcase periods such as
  `:bars` have plain number schemas and move continuously between musical
  endpoints.
- No preference learning (#677) or rehearsal policy (#217) chooses between
  candidates yet; the score is a fixed heuristic.
- Studio has no control for `:include-knob-driven?` and does not show a link's
  `:shared-with`; the REPL and the agent tool have both. Knob-driven exclusions
  appear in Studio only in the too-few-parameters explanation. Studio's Room
  view does not yet compare `:frame-revision` with `:draft-revision`.
