# Fixture roles, reference rigs and retargeting

A fixture role says what job a fixture does in a rig, in words no venue owns.
Cross-rig work reads roles where it would otherwise read a venue's group tags:
retargeting an agent decision onto a reference rig ([#858](https://github.com/tolgraven/tolglow/issues/858)),
and later the translation of a whole show ([#861](https://github.com/tolgraven/tolglow/issues/861), [cross-rig research](research/native-agent/cross-rig.md)).
Source: [`tolglow.rig.roles`](../src/tolglow/rig/roles.clj),
[`tolglow.rig.reference`](../src/tolglow/rig/reference.clj),
[`tolglow.rig.retarget`](../src/tolglow/rig/retarget.clj) and
[`tolglow.rehearsal.retarget`](../src/tolglow/rehearsal/retarget.clj).

## The vocabulary

A role is a zone, a function and an optional ordinal, written as one keyword:
`:back.beam`, `:front.wash`, `:floor.wash.2`. Version 1 of the vocabulary is
closed:

| Facet | Words |
| --- | --- |
| Zone | front, back, side, floor, overhead, audience, set |
| Function | wash, beam, blinder, strobe, pixel, key, atmosphere |
| Ordinal | 1 to 99, optional |

A later version may add words; it never changes what an existing word means.
Venue group tags stay as they are, beside the roles.

## Declaring roles

A rig declares roles under `[:rig :roles]`:

```clojure
{:version 1
 :assignment {:front.wash [:wash-1 :wash-2]   ; fixture or head IDs
              :back.pixel [:tube-1]}}         ; a pixel layout ID names all its heads
```

The rig validator checks the assignment with the rest of the rig: every role
is in the vocabulary, a fixture holds at most one role, and a role never reuses
a patch group's name. An ID the rig no longer has is not an error, so deleting
a fixture never blocks a rig edit; `roles/report` lists such IDs and the units
without a role. Roles are saved with the show.

A declared role resolves as a group through the ordinary target narrowing, so a
cue with `:selector {:group-id :front.wash}` (or `(roles/selector :front.wash)`)
launches on any rig that declares the role, with no fixture ID in the cue, and
is refused as an unknown group on a rig that does not. An ordinal role also
answers to its base role: `:floor.wash` takes `:floor.wash.1` and
`:floor.wash.2` together unless the rig declares `:floor.wash` itself.

```clojure
(require '[tolglow.rig.roles :as roles] '[tolglow.show :as show])
(def proposal (roles/propose (:rig (show/snapshot runtime))))
(:assignment proposal)                          ; correct it, then declare it:
@(show/submit! runtime :rig (roles/assign (:rig (show/snapshot runtime)) (:assignment proposal)))
(roles/members (:rig (show/snapshot runtime)) :front.wash)
```

## Proposals

`roles/propose` suggests a role for every unit (a fixture, or one pixel layout
with all its heads) from its kind, capabilities and position normalised to the
rig's bounding box (`npos`, −1 to 1 per axis, as in the agent observation).
The function comes from the heads (pixels, fog, pan and tilt with a beam of at
most 30°, a patch type of blinder or strobe, otherwise a wash). The zone comes
from height (below 1 m is floor), the lateral edge below three quarters of the
rig's top (side), depth in the rig box (front or back third), and otherwise
overhead or set. Every unit carries its reason. `key`, `audience` and ordinals
are never proposed. A proposal is a statement about the venue that data cannot
check, so the operator corrects it before declaring it.

Proposed today (not declared; the operator's corrections go on #858):

| 2023 gig rig | Fixtures |
| --- | --- |
| front.wash | the ten washes, `wash-1` to `wash-8`, `wash-2-1`, `wash-2-2` |
| front.beam | `moving-mini-1` to `moving-mini-3` |
| floor.beam | `moving-2`, `moving-mini-4` |
| set.beam | `moving-1` |

| Showcase rig | Fixtures |
| --- | --- |
| overhead.beam | `beam-1` to `beam-6` (the back truss sits mid-depth among the rig's heads) |
| floor.beam | `mover-1` to `mover-4` |
| side.wash | `wash-1`, `wash-2` (booms) |
| floor.wash | `wash-3`, `wash-4` |
| overhead.blinder | `blinder-1`, `blinder-2` |
| floor.atmosphere | `hazer-1` |
| back.pixel | `tube-1` to `tube-8` |
| front.pixel | `ribbon` |

## Reference rigs and rig.sig

The reference rigs are ordinary rig data with declared roles, built from fixture
types the showcase rig already publishes. Each is one class:

| Rig | Content | Roles |
| --- | --- | --- |
| `:pars` | 4 front and 2 back RGBW pars | front.wash, back.wash |
| `:pars-strip` | 4 front and 4 back pars, a 48-pixel strip upstage | adds back.pixel |
| `:movers-pars` | 4 front and 4 back pars, 4 moving heads upstage | adds back.beam |
| `:movers-pars-pixels` | pars, 4 narrow beams, 2 blinders and 6 pixel tubes upstage | back.beam, back.blinder, back.pixel |

`(reference/model id)` is a show model on one; the proposal on each agrees with
the roles it declares.

`rig.sig` version 1 (`reference/signature`) is a fixed-length descriptor of 16
numbers in 0…1: unit and pixel counts on a log scale, the share of movers,
static heads, pixel layouts and atmosphere, width, height and depth, median
spacing, height layers, left–right symmetry, and the shares of colour, white,
strobe and narrow-beam heads (`reference/signature-fields`). `reference/distance`
is a weighted root mean square difference that weighs what kinds of head a rig
has above its height or spacing. `reference/classify` gives the class of the
nearest reference rig. The learning lane may cluster by it later (#847).

| | gig | showcase | pars | pars-strip | movers-pars | movers-pars-pixels |
| --- | --- | --- | --- | --- | --- | --- |
| gig | 0.00 | 0.28 | 0.25 | 0.26 | **0.17** | 0.25 |
| showcase | 0.28 | 0.00 | 0.40 | 0.28 | 0.33 | **0.14** |
| pars | 0.25 | 0.40 | 0.00 | 0.20 | 0.16 | 0.30 |
| pars-strip | 0.26 | 0.28 | 0.20 | 0.00 | 0.24 | 0.17 |
| movers-pars | 0.17 | 0.33 | 0.16 | 0.24 | 0.00 | 0.25 |
| movers-pars-pixels | 0.25 | 0.14 | 0.30 | 0.17 | 0.25 | 0.00 |

## Retargeting agent decisions

`retarget/retarget` takes a source show model, a target (a reference rig ID or
any show model) and recorded decisions (`{:id :calls [{:id :name :arguments}]}`,
the tool calls a provider streams). It lifts the source show's cues to roles: a
cue whose fixtures are exactly one role on the source rig becomes a cue on that
role, and its effect loses its fixture targets. It then rewrites the calls so
they name only carried cues. The policy is report, do not substitute. Each of
these is listed with its reason, never guessed:

| Reason | Meaning |
| --- | --- |
| `:no-matching-role` | the cue's fixtures are not exactly one role (with the count and the roles they overlap) |
| `:world-space` | an aim point, position sampling or volume in source metres |
| `:role-absent` | the target does not declare the role; the cue stays dark |
| `:named-fixtures`, `:fixture-reference` | a call or definition names fixtures |
| `:instance-reference` | a call names a running instance |
| `:cue-not-carried`, `:no-carried-cue`, `:no-section-carried`, `:page-scope` | what is left of an auto configuration or schedule |
| `:unsupported-tool` | a tool retargeting does not handle yet |

The record (`tolglow-retarget/1`) holds the role-addressed definitions, the
rewritten decisions, the report, whether the source roles were declared or
proposed, the source and target classes, and a digest. It holds no fixture ID,
address or position of either rig.

```clojure
(require '[tolglow.rehearsal.retarget :as rr] '[tolglow.rig.retarget :as retarget]
         '[tolglow.rehearsal.passage :as passage])
(def club (passage/club 670))
(println (retarget/summary (rr/record club :movers-pars)))
(def take (rr/run-take club :movers-pars))   ; real show owner, captured output
(rr/output-digest take)
```

The #670 club take carries its wash and strobe cues (`front.wash` on every
reference rig). The movers' cues, the accent, the stem hits and the spotlight
use fixture sets no gig role matches and are reported, and the quiet section's
change has nothing left to carry. Replays on each reference rig light only the
front washes and give identical output digests run to run.

## Not yet

- Group-local frames, in-role fixture assignment and spatial scaling are the
  cross-rig pipeline's (#883). Retargeting maps whole roles only.
- Signals, stems and capability substitution are not retargeted (#884 to #886).
- Studio lists declared roles as groups in the cue target picker and keeps role
  members through a rig editor save, but cannot propose, edit or show roles,
  or run a retargeting. See [REPL and Studio parity](repl-studio-parity.md#declare-fixture-roles).
