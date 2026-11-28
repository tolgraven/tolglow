# Colour guards, shading looks and held group blackouts

A post-effect is a cue that changes the lighting composed beneath its
priority instead of adding light of its own. Its priority is its layer
position: every cue below it is filtered, and every cue above it is not.
[`tolglow.effects.post`](../src/tolglow/effects/post.clj) builds four of them
as ordinary portable effect definitions. They publish, save, reload and edit
like any other effect. [#479](https://github.com/tolgraven/tolglow/issues/479)

| Recipe | What it does to the lighting beneath |
| --- | --- |
| `color-guard` | Keeps colours inside a palette: lifts muddy low-saturation mixes and clamps hue to an arc |
| `bloom-shade` | Inverse bloom: the bloom band dims, desaturates or hue-shifts what it passes over |
| `sparkle-shade` | Darkening sparkle: seeded sparks are holes in the light, not points of it |
| `group-blackout` | Zeroes the dimmer of the heads its cue selects, and optionally every emitter channel, for as long as the cue runs |

## How the filters work

The colour guard and the two shading looks are
[native layer modifiers](native-composition.md). Each one computes three
coefficients per head and per frame from its typed parameters, the head
position and the transport beat. It writes them as its colour output, and its
layer program passes them with the previous colour to an Afterglow colour
filter, which writes the filtered colour in their place:

```clojure
{:layer {:transforms [{:attribute :color
                       :value {:op :color-guard
                               :args [{:op :previous :attribute :color :default [0.0 0.0 0.0]}
                                      {:op :incoming :attribute :color}]}}]}}
```

- `:color-guard [min-saturation restriction center]` moves the hue to the
  nearest edge of the arc `1 - restriction` turns wide around `center`. It then
  lifts a saturation `s` below `min-saturation` (`f`) by
  `(f - s) * smoothstep(s / f)`. White and faint tints stay close to white,
  mid-saturation mixes reach the floor, and the mapping is continuous and
  monotonic.
- `:color-grade [dim desaturate hue]` turns the hue by `hue` turns at constant
  saturation and value. It then mixes the colour toward its own Rec. 709
  luminance by `desaturate` and scales it by `1 - dim`.

Both filters work in HSV on linear RGB and keep the value, the brightest
component, so HDR overrange light keeps its excess. A zero triple leaves the
colour unchanged. Settings outside [0,1] are clamped. The exact definitions are
in Afterglow's [native layer guide](https://github.com/tolgraven/afterglow/blob/master/doc/architecture/native-layers.md).

Nothing in these effects keeps state, reads a clock or draws a random number.
The sparkle hashes the pixel position, the sixteenth note and its `:seed`
parameter. A stopped transport holds every frame, play resumes without
catching up, and one seed always replays the same bytes.

### Parameters

| Recipe | Parameter | Unit and bounds | Default |
| --- | --- | --- | --- |
| `color-guard` | `:min-saturation` Saturation floor | fraction, 0–1 | 0.55 |
| | `:hue-center` Palette hue | turns, 0–1 | 0 |
| | `:hue-range` Palette width | turns, 0–1 (1 allows every hue) | 1 |
| | `:amount` | fraction, 0–1: scales both from no change to as set | 1 |
| `bloom-shade` | `:dim` Dampen, `:desaturate` | fraction, 0–1 | 0.6, 0 |
| | `:hue-shift` | turns, −0.5–0.5 | 0 |
| | `:beats`, `:min`, `:max`, `:width`, `:halo`, `:center-x`, `:center-y`, `:reach` | the legacy bloom band, in beats, fractions of reach and metres | as the showcase bloom |
| `sparkle-shade` | `:dim`, `:desaturate`, `:hue-shift` | as above | 0.85, 0, 0 |
| | `:chance`, `:fade`, `:seed` | fraction, beats, seed | 0.385, 0.75, 656 |

Pass `:defaults` to change a default when you build the effect, for example
`(post/color-guard :warm-guard {:defaults {:hue-center 0.08 :hue-range 0.3}})`.
A default outside its bounds is refused.

## Use them from the REPL

```clojure
(require '[tolglow.effects.post :as post])

(doseq [effect [(post/color-guard :guard {:defaults {:min-saturation 0.6}})
                (post/bloom-shade :drain {:defaults {:dim 0.0 :desaturate 1.0}})
                (post/sparkle-shade :holes)
                (post/group-blackout :blackout)]]
  @(show/submit! runtime :definition {:kind :effects :definition effect}))

;; A guard over the whole rig, high in the stack.
@(show/submit! runtime :definition
   {:kind :cues :definition {:id :guard :effect-id :guard :priority 900}})
;; A shading bloom on one group.
@(show/submit! runtime :definition
   {:kind :cues :definition {:id :drain :effect-id :drain :priority 40 :selector {:group-id :tubes}}})
;; A held, operator-owned blackout for one group.
@(show/submit! runtime :definition
   {:kind :cues :definition (post/blackout-cue :tubes-out :blackout {:group-id :tubes})})

@(show/submit! runtime {:op :cue/launch :cue-id :guard})
@(show/submit! runtime {:op :param/set :instance-id "0:7" :param :hue-range :value 0.35})
```

## Use them from Studio

Place the published cues on a page and play them as usual. The cue card's
parameter controls show the typed parameters with their names, units and
bounds. In **Compose**, **Layer transforms** offers
**Guard previous color to a palette** and **Shade previous color** with
editable settings. These use the same Afterglow filters with literal settings
in a compound slot.

## Hold a group blackout

`blackout-cue` gives the cue `:launch-mode :held`, `:role :show-control`, the
blackout priority 1000 and the group `:selector`. Push pads read a cue's
`:launch-mode`, and Studio pads now do too. A cue authored as held is momentary
whatever Studio's launch mode is. Hold the pad to black the group out, and
release it to restore the group.

The pad uses the existing held launch. The press sends
`{:op :cue/launch :cue-id :tubes-out :launch {:mode :held}}`. The release ends
the instance it launched, or cancels a quantized launch that has not fired yet.
From the REPL, hold and release explicitly:

```clojure
(def held (:instance-id @(show/submit! runtime {:op :cue/launch :cue-id :tubes-out
                                                :launch {:mode :held}})))
@(show/submit! runtime {:op :instance/end :instance-id held})
```

While it is held, the blackout writes zero to the dimmer of its heads. Every
patched head takes a dimmer, a physical channel or the virtual one that scales
its emitters, so every head goes dark and a group, a volume or an explicit
fixture list all admit it. A head with a physical dimmer keeps its colour and
white bytes behind the closed dimmer. `(post/group-blackout :blackout
{:emitters? true})` also zeros the colour and every emitter channel (white,
amber, UV and the rest). A group or volume skips heads that lack one, but an
explicit fixture list must implement each, as with any launch. Shutter, strobe
and other discrete channels keep the values the cues beneath set. The blackout
keeps no state and never pauses or advances the cues beneath. They run on
their own clocks, so release shows each one where it would have been anyway.
With the transport stopped they hold, and so does the blackout.

`:role :show-control` keeps the blackout with the operator. Agents, event
subscribers and automatic category selections cannot launch or end it. A
human surface addressing its exact instance can. This rule already applies to
the global master and blackout.

## Limits

- A filter's transform skips a selected head without RGB, as effect outputs
  do (Afterglow `e58f69e0`).
- The filters read only the `:color` field. Cues that drive individual
  emitter channels (white, amber, UV) bypass the colour guard and the shading
  looks.
- The guard's hue clamp jumps at the hue directly opposite its centre. One arc
  per guard: two separate palette hues need the arc centred between them, or
  a per-head centre computed by a custom effect.
- A filter over a large pixel rig costs as much as the existing keyhole
  bloom's native layer: about 25 ms per frame over 2,048 pixels on a loaded
  machine. [#908](https://github.com/tolgraven/tolglow/issues/908)
- HDR highlight spill is added after composition. A neighbour driven past
  full scale can still spill light onto a blacked-out head unless the show's
  HDR `:fixture-ids` scope excludes it.
- Studio honours a cue's authored `:held` launch mode only. A cue authored
  `:persistent` or `:one-shot` still follows Studio's selected launch mode.
