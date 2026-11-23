# Composing effect building blocks

A cue can expose a dimmer control while the effect applies the same travelling water pattern to brightness, color and optical focus. Changing the cue’s dry level still changes the whole look. Each stage can be replaced, reordered or given its own phase and wet amount.

```clojure
(def water {:block :optics/through-water
            :inputs {:frequency 0.8 :phase {:op :bar} :rate 1.0 :depth 0.55}})

(def pool
  {:id :pool :name "Pool light" :version 1
   :params {:level {:type :number :bounds [0.0 1.0] :default 0.8}}
   :outputs {:dimmer {:op :param :key :level}
             :color [0.9 0.75 0.4]
             :focus {:op :varying :key :lens}}
   :blocks {[:params :level] {:stages [water]}
            [:outputs :color] {:stages [water {:block :optics/gel
                                               :inputs {:transmission [0.7 0.95 1.0]}}]}
            [:varyings :lens] {:base 0.66 :stages [water]}}})
```

Publish the effect through the usual definition registry. Bind its ID to a cue. The numeric fields compile once per definition and rig, and the chosen scalar, typed CPU or native shader backend evaluates them using the effect’s captured clock. Effect definitions and their local prefab data round-trip in a Transit show file.

## Signals and stages

| Stage | Effect |
| --- | --- |
| `:optics/through-water` | Two intersecting wave trains in space and musical time. Depth controls the strength of the ripple. |
| `:optics/ribbed-glass` | A periodic transmission band along one coordinate. |
| `:optics/gel` | A linear RGB transmission factor for color fields. |
| `:optics/fog-transmission` | Beer–Lambert transmittance from extinction per metre, density and distance in metres. |
| `:lfo/sine`, `:lfo/triangle`, `:lfo/saw`, `:lfo/pulse` | A normalized oscillator. A cycle lasts one beat by default. Supply a bar or phrase expression, or scale the phase, for other musical divisions. |

Each stage specifies a `:block`, optional `:inputs`, `:mode`, `:amount` and `:coordinates`. Modes are `:multiply`, `:add` and `:replace`. For a wet amount from zero to one, multiply interpolates its factor from unity, add scales its offset and replace crossfades to the generated value. Omit the amount for the full effect. Scalar factors broadcast over numeric color and vector components. Output validation still follows the channel type and the show’s HDR settings.

A beat, bar or phrase tween can supply the dry signal, or feed a stage input. The host samples that tween once for the current effect clock and supplies its numeric result to every selected fixture. The chain result is available before the downstream envelope, native compositing, master and channel packing.

Use `[:outputs 2]` to attach a stage to the third assignment when an ordered output list writes the same channel more than once. A varying is private to an effect and needs a base expression. Its result can feed any number of outputs through `{:op :varying :key :lens}`. A cue parameter uses its effective value after upstream control mappings as the dry input. Cycles between named bindings fail during compilation.

## Planes, mesh surfaces and volumes

World coordinates are in venue metres. To project onto the floor, specify `:coordinates [{:op :position :axis :x} {:op :position :axis :z}]`. Two coordinates imply zero depth. Three expressions can rotate, stretch or animate the sampling space. On a mesh with its own UV layout, choose the `:surface-uv` fragment interface and supply the interpolated `v_uv` varying from the vertex shader. Fixture compute passes receive XYZ. To use baked fixture UVs, supply their two coordinate expressions.

An effect can extend `:prefabs` with a data-only template. For example, this material makes a soft band that travels across the surface.

```clojure
{:show/band {:name "Travelling band" :mode :multiply
             :inputs {:phase {:op :bar} :frequency 2.0}
             :glsl "0.7 + 0.3 * cos(6.283185307 * (uv.x * frequency - phase))"}}
```

A prefab accepts a numeric GLSL expression or a typed IR `:expression`. Supported GLSL includes arithmetic, coordinate components, input names, scalar and vector functions, and vector constructors. For procedural 3D materials, read `position.x`, `position.y` and `position.z`. The compiler checks types and arity and emits actual fragment or transform-feedback shader programs. Texture samplers, loops and arbitrary shader entry points require a separate rendering backend.

Use `tolglow.signals/waveform-block` to capture a registered waveform as a local prefab. Its selected version and parameter defaults travel with the effect. Use `{:channel :slope}`, `:angular-velocity` or `:jagginess` to drive a second field from the waveform’s motion.

## Reuse a material across the show

Store reusable definitions under `[:settings :prefabs :definitions]`. In Studio, open **Definitions → Assign blocks**, select an output, cue parameter or private input and choose the material by name. The same stage ID works in a global variable chain.

```clojure
{:prefabs
 {:definitions
  {:venue/copper-ripple
   {:name "Copper ripple" :mode :multiply
    :inputs {:phase {:op :bar} :depth 0.4}
    :glsl "mix(1.0, 0.5 + 0.5 * cos(6.2831853 * (position.x - phase)), depth)"}}}}
```

For example, place `{:block :venue/copper-ripple}` in the color stages of two independent effects. Each published revision captures the shared definitions it uses under `:imported-prefabs`. A later library edit takes effect in global variable chains on the next frame. Republish an effect to adopt the edit there. Pinned and archived revisions keep their original look across Transit saves and reloads. An effect can override a shared material by providing the same ID in its own `:prefabs` map.

## Shared variables

Attach a chain to `[:variables :weather :density]` in an effect and read it through `{:op :variable :path [:weather :density]}`. This resolves the input privately for that effect. To shape it for every running effect, configure the same path in show settings.

```clojure
{:variable-blocks
 {:bindings {[:variables :weather :density]
             {:stages [{:block :lfo/triangle :mode :multiply
                        :inputs {:phase {:op :bar :clock :absolute}}}]}}}
```

Global chains sample once per show frame. They start from current source values every time, so repeatedly applying a transmission factor does not accidentally decay a stationary sensor reading. The effect frame, reactive graph and `tolglow.show/variables` expose the resolved values. The saved show retains the source values and settings needed to reproduce them. Code effects can also compile `afterglow.render.blocks/middleware` for a numeric path in their own context.

## Inspecting a chain

Open a running effect’s block inspector and select its binding and fixture. The dry value, every signed stage change and the final result use the same captured parameters and private effect clock. With no fixture chosen the preview samples the venue origin. An instance's result is absent if the selected fixture is unknown or outside its assignment. The rolling history lives in the viewing surface. Closing the inspector releases the query.

If the runtime predicts keyframes, the inspector distinguishes values sampled at an endpoint from current interpolated native output. Opening an inspector never runs the effect a second time. A failing shared variable chain holds its last successful value in the same show epoch when available. Failure and recovery enter the runtime event stream.

Headless clients can acquire `[:runtime/block-trace instance-id binding-path target-id]`. For a global variable, pass nil as the instance ID and omit the target. `[:runtime/block-library effect-id]` lists the available stages and input defaults. The passive Push webview takes the selected trace from the display capture worker. Configure `[:settings :controllers :push-display]` with `:mode :block-trace`, `:binding-path` and optionally `:target-id`.
