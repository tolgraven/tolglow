# Template groups

A template group is a saved, reusable group of cues. Its member cards have
sizes and an order, and it has open *roles* that are bound to fixtures when
it is launched. The same template can run on the left washes and, at the same
time, on the right washes. A template can move to another rig by binding its
roles to that rig's groups. Shared controls that move several members at once
are planned for the second slice (below).
[#812](https://github.com/tolgraven/tolglow/issues/812) tracks this work.
It follows from #795, which gave cards custom sizes "for custom setups only:
macros and template groups".

[Source](../src/tolglow/template_groups.clj) ·
[Card arrangement](../src/shared/tolglow/authoring/card_layout.cljc) ·
[Macros and sequences](../src/tolglow/composition.clj) ·
[REPL and Studio parity](repl-studio-parity.md#launch-a-template-group-and-size-its-cards)

> **Status: proposal plus a first slice.** The operator has not yet agreed
> to this design. [Decisions](#decisions-for-the-operator) lists the open
> choices. The code uses the name *template group* throughout and stays apart
> from macros, so it can be renamed or reshaped.

## What it is and what it is not

Tolglow already has several features that group cues or controls. A template
group reuses them instead of copying them.

| Feature | What it groups | Launches cues? | Fixtures | Instances at once | Shared controls |
| --- | --- | --- | --- | --- | --- |
| **Macro** (composition `:mode :macro`, #216) | Fixed cue slots | Yes, together | Each cue's own targets | One run per macro ID | None of its own |
| **Sequence** (composition `:mode :sequence`) | Timed steps | Yes, one after another | Each cue's own targets | One run per ID | None |
| **Effect group channel** (#553) | Running instances, for mixer levels | No | None | Not applicable | One level per group |
| **Linked knob** (#666) | Parameter destinations | No | None | One knob, global | It is the control |
| **Cue variations** (#547) | Saved parameter values of one cue | No | None | Not applicable | None |
| **Fixture group / lighting-scene group** | Fixtures | No | It is the selection | Not applicable | None |
| **Template group** (this note) | Cue *members*, with roles | Yes, together | **Bound per launch** through roles | **Many placements** | Planned: template knobs, one set per placement |

In short:

- A **macro** is one fixed setup. A **template group** is the *shape* of a
  setup. Its roles are bound when it is launched, and it can be launched more
  than once.
- An **effect group channel** mixes instances that are already running. A
  template group launches them. A placement could later join a group channel
  as a whole (see [later slices](#later-slices)).
- A **linked knob** is the control format that template groups will reuse for
  their shared controls. Today a knob names a cue or an instance. A template
  knob would name a *member* and become a real linked knob for each placement.
- **Cue variations** stay per cue. A member could later name a saved variation
  row as its starting values.

## Comparable software (concepts only)

Lighting and performance tools solve similar problems in a few ways. This is
a summary from general knowledge of how these products work. No product
content or documentation is reproduced here.

- **Console presets and palettes** (grandMA, Hog, ChamSys). Stored attribute
  values can apply to any fixture of a compatible type. Cues *refer* to them,
  so editing a preset changes every cue that uses it.
- **Reference-based cue content** (grandMA3 "recipes" and similar). A cue part
  is built from a group, a preset and selection modifiers. Changing the group
  re-targets the cue. A template role is the same idea: the selection is a
  reference, chosen late.
- **Effect engines applied to a selection** (most consoles). An effect has no
  fixtures until it is applied to a selection. Tolglow cues already work this
  way through `:selector`.
- **Racks with macros** (Ableton Live). Several devices are grouped, a few
  macro knobs fan out to their parameters, and the rack is saved as a preset
  to drop onto any track. This is the closest match to "a reusable group of
  effects with shared controls".
- **Layer groups and dashboards** (Resolume). A group has a shared master
  level, and a small set of dashboard knobs links to parameters inside it.
- **Components with custom parameters and clones** (TouchDesigner). A master
  component defines exposed parameters, and its clones follow its definition
  while keeping their own parameter values.

What Tolglow takes from these:

- Roles are references that are bound late (recipes, effects on a selection).
- Shared controls are a small set of linked knobs scoped to one placement
  (racks, dashboards).
- Placements capture the definition version they started from, as Tolglow's
  macros and cue launches already do. Consoles instead update by reference.
  [Decision 2](#decisions-for-the-operator) asks which the operator wants.

## Data

A template group is portable data saved with the show under
`:extensions :tolglow.template-groups/groups`, a map from ID to definition.
It holds no functions, so a full backup saves it and a restore validates it.

```clojure
{:id :verse-look
 :name "Verse look"
 :version 3                                  ; set by the show owner
 :roles [{:id :wash :name "Washes" :selector {:group-id :wash}}
         {:id :beams :name "Beams"}]         ; no default: bind at launch
 :members [{:id :fill  :cue-id :warm-fill  :role :wash  :params {:level 0.6}}
           {:id :pulse :cue-id :beat-pulse :role :beams :launch {:mode :persistent}}
           {:id :haze  :cue-id :fog-breath}]  ; no role: the cue's own targets
 :cards [{:slot-id :pulse :size :medium} {:slot-id :fill :size :small}]}
```

| Field | Meaning |
| --- | --- |
| `:id` | Stable keyword or string ID |
| `:name` | Optional label |
| `:version` | Starts at 1. The show increases it on each save that changes roles or members. A card edit does not change it |
| `:roles` | Optional vector of `{:id :name :selector}`. `:selector` is the default binding: `{:group-id g}`, `{:fixture-ids [...]}`, `{:volume-id v}` or `:all`, exactly what a cue launch accepts |
| `:members` | Nonempty vector of macro slots (`:id`, `:cue-id`, optional `:params` and `:launch`), validated by the same function that validates macro slots, plus an optional `:role` |
| `:cards` | The member cards' sizes and order, with the same schema and validation as a macro's `:cards` (`tolglow.authoring.card-layout`), keyed by member ID. Pins (#816) apply once that lands |

Rules:

- Every member needs an explicit `:id`, because cards, placements and later
  template knobs name members by ID.
- A member's `:role` must name a role. A member with a role must not set
  `:launch :selector` or `:launch :targets`, because the role supplies them.
- The snapshot adds `:placements` for views. A definition read back from a
  snapshot and saved again drops that key, and drops a client-supplied
  `:version`.
- A save that omits `:cards` keeps each card whose member keeps its ID and cue,
  as a macro save does. An explicit `:cards nil` clears them.

### Placements

Launching a template group makes a *placement*: one running copy with its
roles bound.

```clojure
{:template-id :verse-look :placement :left
 :bindings {:wash {:group-id :left-wash} :beams {:fixture-ids [:head-1 :head-2]}}}
```

A placement runs as an ordinary composition run under the ID
`"template-group/verse-look/left"`. The run captures:

- a derived macro definition (the members as slots, each role's selector
  copied into its members' `:launch :selector`);
- `:template {:id :version :placement :bindings}`.

The rest of the show therefore treats a placement like a running macro:

- Members launch as one native layer, in member order, with the run's owner ID.
- Stop releases only the placement's own instances.
- Restore remaps the run's instance IDs and owners.
- A refused member launch is recorded as a composition fault, and the other
  members keep playing.
- Saved macros never list placements, so the macro library stays as the
  operator wrote it.

## Commands

The REPL and Studio use the same validated messages, as they do for macros.

| Message | Payload | Effect |
| --- | --- | --- |
| `:template-group` | the definition | Validate and save; return the new `:version` |
| `:template-group-remove` | `{:id}` | Remove a template group that has no running placement |
| `:template-group-launch` | `{:id :placement :bindings}` | Validate the bindings against the current rig, then start or replace that placement. `:placement` defaults to `:main`. `:bindings` overrides role defaults |
| `:template-group-stop` | `{:id :placement}` | Stop that placement, or every placement of the group when `:placement` is omitted |
| `:template-group-cards` | `{:id}` plus one of `:cards`, `:resize` or `:move` (and `:pin`/`:pins` after #816) | Change the arrangement. The change goes through `composition/set-cards`, so macros and templates share one code path |

HTTP: `/api/template-group`, `/api/template-group/remove`, `/launch`, `/stop`
and `/cards`.

Validation and refusals:

- A launch with an unknown role in `:bindings`, a role that has neither a
  binding nor a default, or a selector that does not resolve on the current rig
  (unknown group or fixture) is refused, with the path
  `[:bindings role]` or `[:roles i :selector]`. Nothing starts.
- A save is refused with paths in the template's own terms: `[:members i …]`,
  `[:roles i …]` or `[:cards i …]`.
- Removing a template group with a running placement is refused.
- A placement ID that equals a saved composition ID is refused, so the two can
  never share a run.

## Launch and edit semantics

- **Capture on launch.** A placement keeps the definition it started with,
  as macro runs and cue instances do. Editing the template does not change
  running placements. The projection marks a placement `:stale? true` when the
  template has a newer `:version`. Launching the same placement again applies
  the edit. This matches `composition/start`'s rule that "running executions
  retain their captured definition".
- **Arrangement is live.** Card sizes and order are presentation only. Studio
  reads them from the current template, so a resize shows at once on every
  placement. Cards naming a member that a running placement does not have are
  skipped. Members without a card take the default size.
- **Placements replace by ID.** Launching `:left` again replaces only `:left`.
  `:right` keeps running.
- **Transport stop** pauses members as it pauses every effect. Template groups
  add no clock of their own.
- **Output.** Nothing here touches devices. A launch is a set of
  `:cue/launch` commands through the shared reducer, with the same aim-path,
  capability and target checks as any other launch.

## Cards: sizing and arrangement

Template groups reuse #795 exactly:

- the `card_layout` schema (`{:slot-id :size}` in card order, keyed by member
  ID) and its validation, `arrange`, `resize` and `move`;
- the macro board in Perform → Live: each running placement gets its own
  section with the template's name, the placement and a **Template** tag, and
  **Arrange** shows S/M/L and ‹ ›;
- the same packing (`slot-grid/pack` with in-order placement and grow-down),
  so a template card looks exactly like a macro card of the same size.

Only macros and template groups take custom sizes. Every other card keeps its
standard slot.

## Persistence and sharing

- **Full local backup and show save** keep everything: definitions under
  `:extensions`, and running placements as composition runs with their
  captured definitions, bindings and versions. A restore validates both.
  A stale card or pin is dropped, never refused, by the same rule as for
  macros.
- **Sharing export** (not built yet; see
  [sharing](product/expressive-performance.md#sharing-effects-rigs-shows-and-arrangements))
  is a separate, explicit selection. A shared template group should carry:
  - its definition;
  - the cues and effects its members name, as dependencies with versions;
  - its roles' default selectors, as *hints* only. Another rig rebinds them
    on import or at launch.

  It should never carry running placements, instance IDs, bindings to a
  private rig or show-local knobs. This keeps a template portable without
  weakening the full backup.

## REPL and Studio parity

| Action | REPL | Studio (first slice) |
| --- | --- | --- |
| Save | `:template-group` | **Save as template group** on a running macro's heading. It copies the macro's slots and cards, with no roles |
| Launch | `:template-group-launch` with any bindings | **Launch** in the Template groups row, with default bindings at placement `:main` |
| Stop | `:template-group-stop` | **Stop** in the same row |
| Size and order cards | `:template-group-cards` | **Arrange** on the placement's section |
| Read | `(:template-groups (show/snapshot runtime))` | The row and the placement sections read the same projection |

Gaps recorded for later slices:

- Studio cannot yet author roles, choose bindings or name a placement.
- Studio cannot rename or remove a template group.

## Later slices

1. **Template knobs.** `:knobs` on the template, in the #666 link format, with
   destinations naming `{:member :fill :param :level}`. On launch each becomes
   a placement-scoped linked knob bound to that placement's instance IDs. It is
   removed on stop, and it is shown in the placement's section.
2. **Studio authoring.** Roles, bindings and placement names in Compose, and a
   binding picker reused from the fixture-group selector.
3. **Effect group channel per placement** (#553), so one mixer level fades a
   whole placement.
4. **Capability hints on roles** (for example "needs pan/tilt"), checked at
   binding time with the existing capability filter (#688).
5. **Sharing export and import** as described above, with a rebind step.
6. **Push and X-Touch:** launch placements from pads, and give template knobs
   encoders through the uniform surface layer (#706).

## Decisions for the operator

1. **Meaning.** Is a template group a reusable group of cues with roles bound
   at launch, several placements at once and (later) its own shared knobs? Or
   did #795 mean something else, such as a Live Group Track template (#683) or
   a Studio layout preset?
2. **Capture or follow.** Should a running placement keep the version it
   started with (proposed, and how macros behave), or follow template edits
   live, as console presets do?
3. **Where the arrangement lives.** On the template, shared by every
   placement (proposed), or per placement?
4. **Roles.** Are the existing selectors (fixture group, fixture IDs, volume,
   all) enough for binding, or should roles also declare required
   capabilities?
5. **Shared controls.** Should template knobs belong to one placement and be
   removed on stop (proposed), or be global knobs that drive every placement
   at once?
6. **Macros.** Should macros stay as they are (proposed), or should a macro
   eventually become a template group with no roles and one placement?
7. **Removal while running.** Refuse (proposed), or remove the template and
   let its placements finish as orphans?
