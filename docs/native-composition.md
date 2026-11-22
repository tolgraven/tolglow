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
