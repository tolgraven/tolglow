# Composable Clojure effect recipes

[tolglow.effects](../src/tolglow/effects.clj) is a small authoring API over the existing
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

### Mixed rigs: heads without the attribute

Each output writes only to the heads that implement its attribute (#688).
`:color` needs red, green and blue channels, `:aim` and `:direction` need pan
and tilt, every patched head accepts `:dimmer`, and any other attribute needs
its own channel. An effect without targets therefore lights the RGB fixtures on
a rig that also has a hazer or a white-only par, and leaves those heads
untouched. A group, volume or `:all` selection is narrowed the same way.

The launch acknowledgement and the running instance name the heads that were
skipped:

```clojure
@(show/submit! runtime {:op :cue/launch :cue-id :wash})
;; => {:status :accepted :instance-id "0:1"
;;     :target-resolution {:skipped [{:attribute :color :heads [:par :hazer]
;;                                    :requires [:red :green :blue]}]}}
```

Naming such a head explicitly is a mistake, so the launch is refused instead.
Explicit means output `:targets`, effect `:targets`, launch `:targets` or a
`{:fixture-ids [...]}` selector:

```clojure
@(show/submit! runtime {:op :cue/launch :cue-id :wash :launch {:targets [:rgb :hazer]}})
;; => {:status :rejected :code :incapable-target
;;     :message "Explicit targets include heads that cannot take a written attribute: color on hazer (needs red, green, blue)"
;;     :details {:conflicts [{:attribute :color :heads [:hazer] :requires [:red :green :blue]}]}}
```

The compilers apply the same filter on every frame, so a later reload or patch
change cannot fault a frame through an incapable head. Studio launches send the
same command and receive the same acknowledgement. Studio does not yet show the
`:target-resolution` report.

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

### Sample a gradient parameter

A `:gradient` parameter holds ordered `{:position p :color [r g b]}` stops. An
output reads it with `{:op :gradient :key k :args [position]}`, or
`fx/gradient`, which gives a linear RGB colour. The position is any scalar
expression: a constant, a fixture coordinate, another parameter, an oscillator
phase or arithmetic on them. The colour nests in arithmetic like any other
`vec3` (#689).

```clojure
(def tide
  (fx/effect :user/tide
    {:params {:palette (fx/parameter [{:position 0.0 :color [0.02 0.05 0.55]}
                                      {:position 0.5 :color [0.28 0.03 0.75]}
                                      {:position 1.0 :color [0.75 0.02 0.42]}])
              :shift [0.0 -1.0 1.0]
              :level [0.8 0.0 1.0]}}
    (fx/color (fx/multiply (fx/gradient :palette (fx/add (fx/divide (fx/position :x) 8.0) (fx/param :shift))
                                        {:extend :mirror})
                           (fx/param :level)))))
```

- Colours interpolate component by component between neighbouring stops, on
  the stored linear values. Studio's gradient swatch draws the same stops with
  a CSS ramp, which interpolates the same numbers the same way.
- Before the first stop and after the last, the end colours hold. That is the
  default `:extend :clamp`, which matches the swatch. `:extend :repeat` wraps the
  position into [0,1), so 1.0 shows the first stop again. `:extend :mirror`
  reflects the position back over [1,2].
- One stop is a solid colour.
- `fx/parameter` infers `:gradient` from a vector of stops and makes the
  numbers doubles.

A block stage's colour input can hold the same sample. Studio's block builder
writes one through its **Gradient sample** input mode
([Studio controls](studio-ui.md#assigning-and-inspecting-effect-blocks)).

Reading the gradient with `{:op :param :key :palette}` is refused when the
definition is published, and the message names the `:gradient` form. A
`:gradient` sample of a numeric parameter, a vector position, a missing
position or an unknown `:extend` is refused at the expression's path.
Reactive outputs cannot sample a gradient, because they read only their declared
signal inputs.

The compiler lowers each sample to ordinary numeric IR: one clamped mix for each
segment between stops. A compiled effect expands only the gradients that one of
its outputs, blocks or shader uniforms samples. It does so once each time the
value changes (identical or equal values reuse the previous expansion), never
per fixture, and the fixtures only do arithmetic. A gradient parameter that
nothing samples costs no more than before this change. The scalar, batch and
GLSL backends all run the lowered sample. The batch compiler keeps its typed
path with no fallback reason, including on a rig with integer coordinates.
Live edits (`:param/set`, ramps) and definition revisions take effect on the
next frame without recompiling the effect.

The schema's `:max-stops` sets how many segments the IR contains, from 1 to 16.
The default is 8. A default with more stops than the capacity is refused when an
output samples it.

A live value with more stops stays valid. It is resampled at `:max-stops`
evenly spaced positions from its first stop to its last. This takes linear time
and moves continuously while the value moves, for example during a ramp. The
running instance reports the reduction as `:gradient-reductions`, for example
`{:palette {:from 10 :to 8}}`, in `show/snapshot` and Studio's state. Studio's
parameter row then says "Outputs sample 8 evenly spaced stops of 10". If two
stops are closer than float32 GLSL can represent, the gap is a hard step rather
than a fault.

Each fixture pays for every segment, and the position expression is repeated
once per segment, because the IR has no shared subexpressions. A smaller
capacity and a simple position are therefore cheaper. Measured on 2,048 pixels
on this M4 Pro under load from other agents, per frame, with warm JIT and
excluding output packing, network and UI:

| Position | `:max-stops` | Batch | Scalar fallback |
| --- | ---: | ---: | ---: |
| `(position :x)` | 3 | 0.6 ms | 12 ms |
| `(position :x)` | 8 (default) | 1.5 ms | 33 ms |
| `(position :x)` | 16 | 3.1 ms | 66 ms |
| `fract(sin 3x + cos 5x)` | 3 | 1.0 ms | 18 ms |
| `fract(sin 3x + cos 5x)` | 8 (default) | 3.0 ms | 52 ms |
| `fract(sin 3x + cos 5x)` | 16 | 8.0 ms | 108 ms |

The same `fract(sin 3x + cos 5x)` written once as a colour, without a
gradient, costs 0.34 ms on the batch path. When a position is expensive, use a
small capacity. The show renders through the batch compiler. The scalar path
runs only on fallback and for single-head cue-pad previews.

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

`:period {:ms 1000}` is an explicit millisecond period. It reads the
instance's private millisecond clock, which stands still while the transport is
stopped, so it pauses and resumes like a musical period. No helper fetches wall
time itself. See [transport stop](clock-domains.md#transport-stop) for the rule
and its exceptions.

`cycles` exposes the continuous phase expression for custom composition:

```clojure
(fx/mix [1 0 0] [0.2 0.02 0.4]
        (fx/sine {:period {:bars 4}}))
```

### Phase accumulators

A phase from `cycles` is elapsed beats divided by the period, so changing a
period parameter while the cue runs moves the phase by an amount that grows
with the cue's age. `phase-accumulator` keeps the phase instead: each frame it
adds the cue's elapsed beats since the last frame divided by the period now
set, and keeps the result in the instance's state under its ID.

```clojure
;; Cycles of a turn every :bars bars; turning :bars changes the speed from here.
(let [turns (fx/phase-accumulator :turn (fx/param :bars) {:unit :bars})]
  (fx/aim (fx/xyz (fx/multiply 1.5 {:op :cos :args [(fx/multiply (* 2 Math/PI) turns)]})
                  3.0
                  (fx/multiply 1.5 {:op :sin :args [(fx/multiply (* 2 Math/PI) turns)]}))))
```

The period is any scalar expression without position, in beats, bars or
phrases (`:unit`). With `:phase-reference :launch` (the default) the phase
starts at zero on launch and follows the cue's elapsed beats. With
`:phase-reference :transport` it starts at transport beat / period and follows
the transport beat, so a cue launched off the beat still steps on the beat and
bar grid; bars and phrases then use the current meter. Either way it adds
nothing while the transport is stopped and resumes from the held value. While
the period is zero, negative or not a number the phase holds instead of
faulting, and it advances again when the period is positive.

An effect that reads one is never forecast between keyframes, since its state
advances once per committed frame. Global variable chains refuse it, since
they have no cue state to keep it in. The IR node is
`{:op :phase-accumulator :id k :period expr :unit u :phase-reference r}`; IDs
are unique within an effect and keep the phase across a `:preserve-state`
reload.

The phase depends on the path the beat took. With a constant period it is the
plain reference beat / period. A backward seek or loop jump rewinds it by the
jump over the current period, so after an edit it does not return to where a
stateless phase would be. Under a changing period the result depends on frame
timing; it is the same for the same tick schedule, so a replay is
byte-identical.

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
contract is satisfied. Aim and direction use signed world-space coordinates.
Afterglow applies fixture calibration and pan history after the ordered fields
have been blended.

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

### Migrate running effect state

A cue or launch with `:reload-policy :migrate` adopts a newer revision like
`:preserve-state` and, once at the frame where the new revision takes over,
transforms the instance's private `:state`. The new definition declares the
transform as portable data, which publication and show loading validate:

```clojure
(assoc effect :version 4
       :state-migration {:from {1 [{:op :rename :from [:count] :to [:frames]}]
                                2 []
                                3 [{:op :default :path [:phase] :value 0}
                                   {:op :scale :path [:frames] :by 2}]}})
```

`:from` maps each previous version to the steps that bring its state up to the
next listed version, or to this definition's version for the highest key. An
instance on v1 runs the lists for 1, 2 and 3 in order; one on v3 runs only 3's.
A version without a key faults, so list `[]` when its state already fits. Keys
must be below the definition's own version, and validation names any that are
not.

Steps are `:assoc`, `:default`, `:dissoc`, `:rename` and `:scale`.
`:dissoc` and `:rename` only remove map keys and leave an absent or non-map
parent unchanged. A `:rename` whose source is absent does nothing, and one whose
target already holds a value fails rather than overwriting it. `:scale`
multiplies two whole numbers exactly and fails on 64-bit overflow. Any other
combination, including ratios and big decimals, becomes a finite double. Every
runtime failure, including `assoc` through a number or past the end of a
vector, names its step, for example `[:state-migration :from 3 1 :path]`. [`tolglow.effects.state-migration`](../src/tolglow/effects/state_migration.clj)
compiles them into Afterglow's pure `(old-state old-version new-version context)`
function. The show owner compiles each declaration once per definition version
and attaches it to the compiled step; automation prediction attaches it the
same way, so a pending migration renders identically there. Imported shows
never resolve vars or run code to migrate. Effects compiled from `:outputs` do not
read `:state` today; the migrated map is retained, persisted and visible to steps
that do.

If the migration throws, for example scaling a value that is absent, the
instance keeps its last good revision and state. It records a bounded
`:reload-fault` with `:stage :migrate`, `:effect-version`, `:from-version` and the
authored cause path, such as `[:state-migration :from 2 0 :path]`. Other cues keep
rendering the same bytes. Publish a newer revision or submit `:instance/retry`
to try again. Modulation owned by or aimed at the cue is sampled for both
outcomes, so a rejected revision leaves its controllers and overlays untouched;
see [modulation](modulation.md#a-frame-with-a-pending-hot-reload). The engine contract is in Afterglow's
[effect failure isolation](../../afterglow/doc/architecture/effect-fault-isolation.md#state-migration-on-reload).

The focused tests render real scalar and typed contributions at quarter-cycle
positions, compare final DMX bytes, exercise fractional periods, captured meter
and pause, compose target-aware layers, resolve world-space aiming, round-trip
EDN and hot-reload an authored definition through the actual control/engine path.
See [effects_test.clj](../test/tolglow/effects_test.clj) for executable examples.
