# Composable Clojure effect recipes

[tolglow.effects](../tolglow/effects.clj) is a small authoring API over the existing
portable expression DSL. Its functions build data; they do not read a clock,
mutate effect state, open devices or introduce another execution engine. The
returned maps use the same registry, compiler, frame loop, save files and live
reload behavior as definitions written directly in EDN or the studio builder.

## Start with a recipe

```clojure
(require '[tolglow.effects :as fx])

(def red-wash
  (fx/wash :user/red-wash
           {:name "Desaturated red"
            :color [0.65 0.10 0.08]
            :level 0.6
            :duration {:bars 4}}))

(def breathing-red
  (fx/breathing-wash :user/breathing-red
                    {:color [0.65 0.10 0.08]
                     :wave {:shape :sine :period {:bars 2}
                            :low 0.15 :high 1.0}
                     :envelope {:attack {:beats 1/2}
                                :release {:bars 1}}}))

(def travelling-wash
  (fx/breathing-wash :user/travelling
                    {:color [0.35 0.05 0.45]
                     :wave {:shape :triangle :period {:beats 2}
                            :axis :x :wavelength 4.0}}))

(def moving-target
  (fx/aim-sweep :user/moving-target
                {:center [0 0 2] :width 4 :zoom 0.2
                 :wave {:period {:phrases 1}}}))
```

`wash` exposes editable `:tint` and `:level` parameters. `breathing-wash` multiplies
that level by a temporal or spatial oscillator. `aim-sweep` moves a target in
world-space metres and exposes `:center`, `:width` and `:zoom`. It declares an
emission dependency and contributes no color or intensity, so a separate
emitting cue can provide the light.

Every recipe defaults to version 1 and a one-bar duration for timed/one-shot
launches. Persistent launches follow the usual runtime lifecycle. Supplying
`:duration {:beats 1/2}`, `{:bars 2}` or `{:phrases 1}` retains those musical
units in the definition.

## Compose your own outputs

```clojure
(def color-and-focus
  (fx/effect :user/color-and-focus
    {:name "Purple focus"
     :params {:level [0.7 0.0 1.0]
              :tint (fx/parameter [0.4 0.03 0.5])}
     :targets [:head-left :head-right]}
    (fx/color (fx/param :tint))
    (fx/intensity
      (fx/multiply (fx/param :level)
                   (fx/sine {:period {:bars 1} :low 0.1 :high 1.0})))
    (fx/focus (fx/triangle {:period {:phrases 1}}))
    (fx/zoom 0.25)))
```

`effect` accepts an option map and any number of output descriptors or vectors
of descriptors. `compose` combines reusable groups while retaining their exact
order. Repeated attributes remain distinct layers; they are not collapsed by a
map merge.

```clojure
(def base-layer
  (fx/compose (fx/color [1 0.1 0.02]) (fx/intensity 0.2)))

(def accented
  (fx/effect :user/accented {:targets [:head-left :head-right]}
    base-layer
    (fx/intensity 0.3 {:targets [:head-left] :blend :add})))
```

The effect-level target set and each output's targets intersect. Here the left
head receives a total dimmer contribution of 0.5 and the right receives 0.2.
Supported blend names are the compiler's `:replace`, `:add` and `:max`.

`color`, `intensity`, `zoom` and `focus` clamp their expression to the normalized
output range. `aim` retains finite world coordinates without clamping them.
The generic `output` constructor can address other supported scalar attributes:

```clojure
(fx/output :pan (fx/sine {:period {:bars 2} :low 0.25 :high 0.75}))
(fx/output :strobe (fx/pulse {:period {:beats 1} :duty 0.25}))
(fx/aim (fx/xyz (fx/sine {:low -2 :high 2}) 0 3))
```

Use `output` directly for an existing output-level tween expression. A tween
is sampled by the existing tween compiler and is not an arbitrary numeric node
that can be nested inside other arithmetic.

## Musical oscillators

All oscillators default to one elapsed bar per cycle. They accept `:period`,
`:clock`, `:phase`, `:low` and `:high`. Pulse also uses `:duty`, the fraction of
the cycle spent high.

| Helper | Phase 0 | Phase 1/4 | Phase 1/2 | Phase 3/4 | Phase 1 |
| --- | ---: | ---: | ---: | ---: | ---: |
| `sine` | 0.5 | 1 | 0.5 | 0 | 0.5 |
| `triangle` | 0 | 0.5 | 1 | 0.5 | 0 |
| `saw` | 0 | 0.25 | 0.5 | 0.75 | 0 |
| `pulse`, duty 0.5 | 1 | 1 | 0 | 0 | 1 |

`:phase` is an additive cycle offset and may itself be an expression.
`:clock :elapsed` starts the wave at effect launch; `:clock :absolute` follows
the supplied transport coordinate. Bars and phrases use the instance's captured
meter, so a later meter change does not silently alter an already-running
pattern. A paused beat clock freezes musical oscillation even if milliseconds
continue advancing. Tempo changes affect the transport's progress, without
converting a musical period into a fixed millisecond deadline.

```clojure
(fx/saw {:period {:beats 1/2} :phase 0.25})
(fx/triangle {:period {:phrases 2} :clock :absolute})
(fx/pulse {:period {:bars 1} :duty (fx/param :duty)})
```

`:period {:ms 1000}` is an explicit physical/wall-time option. It follows
supplied milliseconds and therefore continues while the musical transport is
paused. No helper fetches wall time itself.

`cycles` exposes the continuous phase expression for custom composition:

```clojure
(fx/mix [1 0 0] [0.2 0.02 0.4]
        (fx/sine {:period {:bars 4}}))
```

For spatial motion, `position-wave` adds position divided by wavelength to the
phase. Coordinates and wavelength use metres. A positive wavelength moves the
pattern toward the negative axis as time advances; a negative wavelength
reverses it. This is a per-position expression compiled by the same numeric IR.

```clojure
(fx/position-wave :saw {:axis :z :wavelength -3.0
                       :period {:beats 4} :low 0.1 :high 0.8})
```

## Parameter shorthand and semantic controls

Within `:params`, `[default low high]` is a bounded numeric shorthand, not an RGB
literal. Use `parameter` for color/vector defaults and richer metadata:

```clojure
{:level [0.7 0 1]
 :tint (fx/parameter [0.7 0.06 0.03])
 :center (fx/parameter [0 0 2] {:type :vector :size 3})
 :duty (fx/parameter 0.25 {:recommended [0.1 0.5]
                          :random {:enabled? true :max-change 0.1}})
 :count (fx/parameter 4 {:type :integer :bounds [1 16]})
 :enabled true
 :palette (fx/parameter :red {:values [:red :purple]})}
```

The helper validates defaults and hard/recommended ranges using the existing
registry schema. Boolean and enum schemas are useful authoring/control data;
referencing them directly inside the current numeric IR still requires a
supported lowering. They do not become executable conditionals automatically.

`params` adds shared controller annotations to common numeric names:

| Parameter | Default `:control/axis` |
| --- | --- |
| `:level`, `:dimmer` | `:pressure` |
| `:velocity` | `:strike` |
| `:pan` | `:glide` |
| `:tilt`, `:zoom`, `:focus` | `:slide` |

The aim-sweep width parameter explicitly uses `:glide`. Color and vector
parameters are not implicitly assigned to scalar axes. An explicitly supplied
`:control/axis` wins, including `nil` to opt out. These are the same annotations
consumed by the MPE/Seaboard mapping layer; helpers do not own a device mapping
implementation or open a controller.

Effects also expose `:categories` derived from the union of their output layers
(color, intensity, movement, beam shape, strobe or atmosphere). `:modifiers`
record recipe semantics such as oscillator shape and spatial variation. These
are classification tags, independent of a cue's grid swatch. UI/controller
consumers can color category tags without replacing manually chosen cue colors.

## Compilation, precision and live editing

Numeric helper constants are finite doubles. This keeps their arithmetic in the
same numerical domain as the typed batch compiler. Complete IR maps passed to
`expression` retain their authored numeric types; exact ratios or large integer
arithmetic can therefore select the existing scalar fallback. Numeric parameter
defaults generated by `parameter` are doubles unless the type is `:integer`.

`effect` validates through `compiler/compile-plan` and returns only definition
data. The compiled functions and typed buffers never enter that definition.
Color/intensity/zoom/focus recipes run through the typed path when its numeric
contract is satisfied. The current batch selector deliberately sends aim and
direction effects through the ordinary geometry compiler; this is an explicit
existing fallback, not a second implementation introduced by the recipes.

To revise a running effect, produce the same stable ID with a higher version
and publish it through the existing show owner. The instance's reload policy
controls restart/preserve/pinned behavior. Changing a parameter default does not
overwrite a running instance's explicit parameter values.

```clojure
;; `runtime` is an already-running studio owned by the caller.
(require '[tolglow.studio :as studio])
@(studio/submit! runtime :definition
   {:kind :effects
    :definition (fx/breathing-wash :user/breathing-red
                  {:version 2 :color [0.55 0.05 0.15]
                   :wave {:period {:bars 4}}})})
```

The focused tests render real scalar and typed contributions at quarter-cycle
positions, compare final DMX bytes, exercise fractional periods, captured meter
and pause, compose target-aware layers, resolve world-space aiming, round-trip
EDN and hot-reload an authored definition through the actual control/engine path.
See [effects_test.clj](../test/tolglow/effects_test.clj) for executable examples.
