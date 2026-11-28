# Compound cues and native layers

Place cues in a macro to run them together. Each execution has its own lighting
layer. Slots run in authored order on each head, so one cue can rotate the
color or shift the 3D aim point produced by the previous cue. Afterglow resolves
the completed layer at the priority position of its highest ordered active
member. Independent cues above that position still control the result. Members
of a sequence share the same layer whenever their lifetimes overlap, including
release envelopes. The values a modifier reads are from the current frame.

In Studio, open **Compose**, add two cues and expand **Layer transforms** on the
second slot. Choose **Offset aim point**, enter the displacement in meters and
enable **Include earlier members’ heads** to apply it to heads touched by the
first slot. Lower **Layer opacity** to mix the final color and aim with cues
beneath the compound. **Save & play** starts a fresh execution with the edited
slots. An existing execution retains its slot program until restarted.

## Authoring in Clojure

The following macro turns an existing red wash green by rotating the hue a
third of a turn. Both slots refer to registered cues. The cue in the second slot
may contribute its own values as well. For each target head, the second slot
stores an exact beat fraction and reads it back for the rotation.

```clojure
{:id :turn-the-room :mode :macro :opacity 0.75
 :slots [{:cue-id :red-wash}
         {:cue-id :accent
          :targets :previous
          :transforms [{:attribute :beat-ratio :value 1/3}
                       {:attribute :color
                        :value {:op :rotate-hue
                                :args [{:op :previous :attribute :color :default [0 0 0]}
                                       {:op :field :attribute :beat-ratio}]}}]}]}
```

A registered effect can supply its own layer program. This definition moves
the aim point of earlier members one meter right and half a meter down. It can
be published as an ordinary effect, placed on a cue, and reused in several
compositions. An unpinned running cue using a reload policy that follows
registry changes picks up edits to its effect program.

```clojure
{:id :shift-aim :version 1
 :layer {:targets :previous
         :transforms [{:attribute :aim
                       :value {:op :add
                               :args [{:op :previous :attribute :aim}
                                      [1 -1/2 0]]}}]}}
```

By default, a slot transforms the heads to which its effect contributes.
`:targets :previous` also includes heads touched by earlier layer members.
For an individually launched modifier, set the effect’s ordinary top-level
`:targets` to specific fixture IDs. A standalone modifier with
`:layer {:targets :previous …}` and no explicit fixture IDs follows the heads
that lower-priority lighting has already touched. A modifier with direct
outputs can read those outputs and transform them in the same member.

A registered modifier gets the category tags for its output and transform
destinations. An aim filter that reads dimmer belongs to Movement. A modifier
that writes both an aim and a color also appears in Color. These tags feed
Studio, Push and the shared category selectors that pause or fade effects.

| Expression | Value for this head |
| --- | --- |
| `{:op :previous :attribute :aim}` | Vector as it stood before this member, including lower-priority lighting |
| `{:op :incoming :attribute :color}` | Color contributed by the member’s direct outputs |
| `{:op :field :attribute :color}` | Current value including the member’s earlier transform steps |
| `{:op :add :args [a b]}` | Componentwise sum, including scalar/vector combinations |
| `{:op :multiply :args [a b]}` | Componentwise product |
| `{:op :divide :args [a b]}` | Componentwise division, preserving fractions for integer operands |
| `{:op :rotate-y :args [xyz radians]}` | Vector rotated about the positive Y axis |
| `{:op :rotate-hue :args [color turns]}` | Hue rotated by a fraction of a full turn |
| `{:op :color-guard :args [color [min-saturation restriction center]]}` | Color kept in a palette: hue clamped to an arc, low saturation lifted ([post-effects](post-effects.md)) |
| `{:op :color-grade :args [color [dim desaturate hue]]}` | Color hue-turned, desaturated toward its luminance and dimmed ([post-effects](post-effects.md)) |

Use `:default` on any field reference when a previous value may be absent.
Colors accept normalized RGB triples or portable thi.ng colors. Aim points use
signed XYZ world coordinates in meters. Directions use XYZ vectors.
`:beat-ratio` is a musical value shared within the layer for the current head
and frame. Afterglow converts the resulting physical attributes to DMX after
composition. A scratch beat ratio never consumes a fixture channel.

Steps run in vector order, with the effect program followed by the slot program.
Use `:field` to chain the result of one operation into the next. Member opacity
mixes the changed fields with their prior values. Layer opacity then mixes the
finished compound with the underlying lighting once. Show files save the programs
and restore the identity and order of active members and release tails.

The [64-head compound benchmark](performance/compound-layers.md) compares
individual launches, an opaque macro and a translucent transformed macro.

## Edit sequence steps from code

`tolglow.composition.sequence-edit` takes and returns ordinary sequence definitions.
Select, mute, copy, duplicate, paste and delete steps by their IDs. Supply a fresh
ID whenever inserting a step. Copying retains the authored duration, cue and
parameters. The clipboard and edited definitions are immutable values.

```clojure
(require '[tolglow.composition.sequence-edit :as edit])

(def chase
  (-> {:id :chase :mode :sequence :slots []}
      (edit/add-step nil {:id :wash :cue-id :red-wash :duration {:bars 1}})
      (edit/add-empty-step :wash :pause)
      (edit/extend-duration :pause {:beats 1/2} {:beats-per-bar 4})
      (edit/duplicate-step :wash :wash-copy)
      (edit/toggle-muted :wash-copy)))
```

Use the existing `:composition` owner operation to save and `:composition-run`
to start the saved program. Empty and muted steps occupy their full duration.
On transition, the preceding sequence step releases its own cue instance through
its configured release envelope. Current-step playback data includes the step
ID, kind, mute state and remaining time. [#554](https://github.com/tolgraven/tolglow/issues/554)

The Push overlay needs a draft session that fences page and pad selections, then
routes the edits through these step IDs. Code-authored steps need declared typed
bindings with source provenance validated through the cue publication path before
the overlay exposes those values to performance controls.

## Planned blackout and master-scale steps

The available leases cover held web cues, automatic fog pump deadlines, and
photometry sessions. Each admits a different operation and uses a different
clock. In the surveyed show owner, `begin-presentation!` captures the independent
publisher's immediate darkness gate before `composition/advance`. Moving that
gate into composition would still apply a future render target's darkness as
soon as the producer starts. Render targets can lead the physical slot, and
buffered interpolation can still hold older completions. The publisher's
safety hook checks source expiry after selecting and interpolating a packet.
Timed composition overrides need an output control boundary that the publisher
can admit at a physical slot. [Tolglow frame owner](../src/tolglow/show.clj#L1080) · [Publisher gate](https://github.com/tolgraven/afterglow/blob/7a54b5d548ea5b6935d5683a9b412bc5376b3952/src/afterglow/output/scheduler.clj#L43-L71) · [Publisher safety hook](https://github.com/tolgraven/afterglow/blob/7a54b5d548ea5b6935d5683a9b412bc5376b3952/src/afterglow/output/scheduler.clj#L177-L192)

1. Add timed sequence step kinds `:blackout` and `:master-scale`. The latter
   carries a finite `:scale` in `[0,1]`. Use the existing sequence clock and mute
   flag. Identify each acquired override by show epoch, execution owner and
   step visit, with the step ID as provenance. Step end and truncation remove
   exactly that execution's overrides. Replacement runs and loop visits receive
   fresh identities. Repeated ticks and stale releases leave current leases in
   place. Steps skipped in an admitted seek produce no retrospective override.
2. Keep the owner-committed `:master` and `:blackout?` as operator settings.
   Derive effective blackout from operator and pause blackout plus active
   blackout leases. Combine overlapping scale factors multiplicatively in a
   deterministic order. Apply each factor once alongside the capacity ceiling.
   A `0.5` lease over master `0.8` yields `0.4`. If the operator changes master
   to `0.3` during that visit, it yields `0.15`, then returns to `0.3` on release.
   A release affects its lease key alone, so it cannot clear a newer operator
   blackout or change another run's attenuation. [Operator commands](https://github.com/tolgraven/afterglow/blob/7a54b5d548ea5b6935d5683a9b412bc5376b3952/src/afterglow/control/model.clj#L328-L334)
3. Commit sequence advancement and output control before the affected slot's
   effect work, preserving manual command ingress and sensor/subscriber order.
   The control commit survives a later render failure. Admit a bounded boundary
   record with the epoch, patch/control generation and physical target deadline
   from that commit. Present it only when the slot becomes due. Tag each completion
   with the same control generation. Once the boundary is due, the publisher
   should reject older incompatible packets, interpolate only within a matching
   control segment, and use the patch's safe idle packet while waiting for an
   eligible completion. Retain the immediate operator blackout and shutdown
   gates. The publisher consumes owner-prepared boundary data with its existing
   monotonic clock. [Presentation timing](output-presentation.md) · [Buffered sample selection](https://github.com/tolgraven/afterglow/blob/7a54b5d548ea5b6935d5683a9b412bc5376b3952/src/afterglow/render/presentation.clj#L255-L287)
4. Supply the same effective controls to the batch and scalar render options,
   response ceilings and hard-off checks, capacity/autopilot, fog demand, and the
   error fallback. Use the existing linear emission and fixture packing path
   for scale. Fence calibration receipts during an incompatible override.
   Compute the fields for the render context while preserving the operator
   settings in the model and snapshots. Reconcile these changes with #553's
   group attenuation before editing the shared frame owner. On restore, discard
   the pre-restore publisher boundaries and derive new ones from the restored
   run's state under the new epoch. Resume presentation with a fresh compatible
   frame. [Output response](output-response.md) · [Shared renderer entry](../src/tolglow/show.clj#L1282)

Use an injected clock and capture sink to exercise entry, exit, overlap, mute,
pause, catch-up, truncation, replacement, and restore. Change the manual master
and blackout while overrides are active and check both committed settings and
packet bytes after release. Test a renderer that runs ahead of the affected
slot and a completion that misses it under direct, hold/fade, and buffered
policies. Record the nominal slot and actual sink entry. The decisive packet
checks are no early darkening, no older brighter packet after an admitted
attenuation boundary, and no premature brightening on release. Use the existing
fixture patch and real scheduler with capture-only output.
