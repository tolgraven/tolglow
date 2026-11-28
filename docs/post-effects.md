# Colour guards, shading looks and held group blackouts

A post-effect is a cue that changes the lighting composed beneath its
priority instead of adding light of its own. Its priority is its layer
position: every cue below it is filtered. Cues above it, and cues at the same
priority launched after it, are not.
[`tolglow.effects.post`](../src/tolglow/effects/post.clj) builds four of them
as ordinary portable effect definitions. They publish, save, reload and edit
like any other effect. [#479](https://github.com/tolgraven/tolglow/issues/479)

| Recipe | What it does to the lighting beneath |
| --- | --- |
| `color-guard` | Keeps colours inside a palette: lifts muddy low-saturation mixes and clamps hue to an arc |
| `bloom-shade` | Inverse bloom: the bloom band dims, desaturates or hue-shifts what it passes over |
| `sparkle-shade` | Darkening sparkle: seeded sparks are holes in the light, not points of it |
| `group-blackout` | Masks the heads its cue selects after composition and HDR, so they stay dark whatever else runs, for as long as the cue runs |

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

Both filters work in HSV on linear RGB. The guard and the grade's hue turn
keep the value, the brightest component, so HDR overrange light keeps its
excess. The grade's desaturation keeps Rec. 709 luminance instead, so it spends
the excess. A zero triple leaves the colour unchanged. The floor, restriction,
dim and desaturation are clamped to [0,1]. Hue settings are turns and wrap. A
transform skips a selected head without RGB, such as a white-only par in a
guarded group. The exact definitions are
in Afterglow's [native layer guide](https://github.com/tolgraven/afterglow/blob/master/doc/architecture/native-layers.md).

Nothing in these effects keeps state, reads a clock or draws a random number.
The sparkle hashes the pixel position, the sixteenth note (modulo 4096, so the
pattern repeats every 1024 beats) and its `:seed` parameter. The hash uses only
IEEE-rounded add, multiply and fract, never `sin`, so one seed replays the same
bytes on any JVM and architecture. A float32 shader would not match it. A
stopped transport holds every frame, and play resumes without catching up.

### Fading a filter

Fade a filter through its instance opacity, or its attack and release. The
engine turns those into a crossfade between the colour before and after the
filter. Do not scale the filter's colour output. A colour envelope or an `:add`
blend would scale the hue settings too: a 50% envelope on a guard centred on
0.6 moves its arc to 0.3. The recipes refuse a colour envelope, and every
post-effect carries `:post-effect? true`. The energy autopilot never picks a
post-effect for a brightness gain, because member gain applies after the
transform and would scale every head the filter rewrote.

### Parameters

| Recipe | Parameter | Unit and bounds | Default |
| --- | --- | --- | --- |
| `color-guard` | `:min-saturation` Saturation floor | fraction, 0–1 | 0.55 |
| | `:hue-center` Palette hue | turns, 0–1 | 0 |
| | `:hue-restriction` Hue restriction | turns, 0–1 (0 allows every hue, 1 locks to the palette hue) | 0 |
| | `:amount` | fraction, 0–1: scales both from no change to as set | 1 |
| `bloom-shade` | `:dim` Dampen, `:desaturate` | fraction, 0–1 | 0.6, 0 |
| | `:hue-shift` | turns, −0.5–0.5 | 0 |
| | `:beats`, `:min`, `:max`, `:width`, `:halo`, `:center-x`, `:center-y`, `:reach` | the legacy bloom band, in beats, fractions of reach and metres | as the showcase bloom |
| `sparkle-shade` | `:dim`, `:desaturate`, `:hue-shift` | as above | 0.85, 0, 0 |
| | `:chance` Chance per sixteenth, `:fade`, `:seed` | probability, beats, seed | 0.15, 0.75, 656 |

Pass `:defaults` to change a default when you build the effect, for example
`(post/color-guard :warm-guard {:defaults {:hue-center 0.08 :hue-restriction 0.7}})`.
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
@(show/submit! runtime {:op :param/set :instance-id "0:7" :param :hue-restriction :value 0.65})
```

## Use them from Studio

Place the published cues on a page and play them as usual. The cue card's
parameter controls show the typed parameters with their names, units and
bounds. In **Compose**, **Layer transforms** offers
**Guard previous color to a palette** and **Shade previous color** with
editable settings. These use the same Afterglow filters with literal settings
in a compound slot. Their fields carry the recipe's parameter names:
**Saturation floor**, **Hue restriction**, **Palette hue (turns)**, **Dampen**,
**Desaturate** and **Hue shift (turns)**.

## Hold a group blackout

`blackout-cue` gives the cue `:launch-mode :held`, `:show-control? true` and
`:role :show-control`, priority 1000 and the group `:selector`. Push pads read
a cue's `:launch-mode`, and Studio pads now do too. A cue authored as held is
momentary whatever Studio's launch mode is. Hold the pad to black the group
out, and release it to restore the group. Pointer-cancel, a window blur and
page-hide release it as well.

The pad uses the existing held launch. The press sends
`{:op :cue/launch :cue-id :tubes-out :launch {:mode :held}}`. The release ends
the instance it launched, or cancels a quantized launch that has not fired yet.
From the REPL, hold and release explicitly:

```clojure
(def held (:instance-id @(show/submit! runtime {:op :cue/launch :cue-id :tubes-out
                                                :launch {:mode :held}})))
@(show/submit! runtime {:op :instance/end :instance-id held})
```

While an instance runs, the show passes its heads to Afterglow as
`:blackout-ids`. Packing then treats them as the global blackout treats every
head, after composition and HDR: their dimmer and every emitter drive (colour,
white, amber, UV and the rest) are zero. No cue of any priority and no
highlight spill from an overdriven neighbour can relight them, and every
fixture goes dark however its dimmer is wired. The effect itself writes only a
zero dimmer, which every patched head accepts, so a group, a volume and an
explicit fixture list all admit it. `(post/group-blackout :blackout
{:emitters? false})` is only that dimmer write at the cue's priority, with no
mask. Shutter, strobe and other discrete channels keep the values the cues
beneath set. The blackout keeps no state and never pauses or advances the cues
beneath. They run on their own clocks, so release shows each one where it
would have been anyway. With the transport stopped they hold, and a release
on the stopped transport restores the group at once.

The show owner keeps the blackout with the operator.
`tolglow.operator-owned/refusal` rejects, with `:code :operator-owned`, any
command from an agent, auto mode, the energy autopilot or an event subscriber
that would launch an operator-owned cue or address an operator-owned instance.
That covers the agent tools `launch_cue`, `stop_instance`, `set_parameter` and
`schedule_events`. Auto mode and the subscribers' automatic selections also
leave such cues out of their candidates. Operator-owned means `:show-control?`,
`:scene-owner`, `:code-cue?`, or a `:role`, `:kind` or tag of `:show-control`,
`:code` or `:code-cue`. A human surface, Studio, Push or the REPL, can still
end it.

## Limits

- The filters read only the `:color` field. Cues that drive individual
  emitter channels (white, amber, UV) bypass the colour guard and the shading
  looks.
- The guard's hue clamp jumps at the hue directly opposite its centre. One arc
  per guard: two separate palette hues need the arc centred between them, or
  a per-head centre computed by a custom effect.
- A filter over a large pixel rig costs as much as the existing keyhole
  bloom's native layer: about 25 ms per frame over 2,048 pixels on a loaded
  machine. [#908](https://github.com/tolgraven/tolglow/issues/908)
- Group operations such as an automated `:group/apply` over categories are not
  issued by agents or auto mode today. If a future automated path issues one,
  it needs the same operator-owned check.
- Studio honours a cue's authored `:held` launch mode only. A cue authored
  `:persistent` or `:one-shot` still follows Studio's selected launch mode.
