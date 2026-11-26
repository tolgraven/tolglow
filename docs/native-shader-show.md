# Run effect expressions and moving-head calibration on a native GPU

A show can evaluate declarative color, intensity, focus, aim and direction
expressions with Afterglow's native OpenGL backend. Select the backend in the
ordinary show configuration. Independently select the native calibrated
movement solver with `:movement-backend :native-gpu`. This example starts the show without a web server
or physical output.

```clojure
{:hz 40
 :init {:components [:show] :modules [] :post []}
 :show {:enabled true
        :compute-backend :native-gpu
        :movement-backend :native-gpu
        :gpu-budget-ms 12
        :gpu-options {:max-programs 8 :queue-capacity 4 :timeout-ms 1000}}}
```

Save the map as `venue.edn` and run it with the paired Afterglow checkout.

```sh
lein with-profile +native-gpu run --config venue.edn
```

The `:native-gpu` Leiningen profile supplies the native libraries selected
by Afterglow. Linux requires an available graphics display, which can be a
software GL display on a headless machine. The default show uses `:cpu` and
runs with plain `lein run`. Both backend options default to `:cpu`.
Choose either GPU stage on its own, or enable both as shown above. Setting `:batch? false` selects scalar authoring
and is incompatible with `:compute-backend :native-gpu`.

## What runs in each frame

A pure effect step returns a named field request with its parameter and beat
snapshot, selected fixture IDs and envelope gains. The show evaluates ordered
requests through one Afterglow session shared by every active effect, including
effects launched or reloaded during playback. Afterglow's render compute API
owns native sessions, shader dispatch, readback, the shared deadline and CPU
recovery. Tolglow translates its show config and binds the compiler to the
fork's pure group preparation. It closes the render owner on show stop and
when startup fails after acquisition.
[Afterglow compute API](https://github.com/tolgraven/afterglow/blob/master/src/afterglow/render/compute.clj)

Afterglow keeps the reference CPU evaluator and full-precision positions for
each group. A failed or late draw takes that path immediately using the same
captured frame inputs. If the driver finishes later, the completed draw has
no path to a publication or a later frame. Successful requests are reused
across effect-isolation retries for that frame. The publication scheduler
continues to apply its configured presentation policy. Exact numeric inputs
and tweens use their existing CPU paths.

When `:movement-backend :native-gpu` is enabled, Afterglow first resolves the
ordered effects and their opacities into final world-space targets. It sends
eligible heads to its calibrated movement solver, using history from the last
accepted frame. Direct pan/tilt channels and fixtures without a world-space
target go through their ordinary packing path. A patch change rebuilds the
native fixture mounts. Exact coordinates, ambiguous results and unavailable or
late readbacks use the reference solver. A head whose pan or tilt lies within
the driver's rounding margin of a DMX code boundary is also ambiguous.
Afterglow chooses that margin from `GL_RENDERER`; see "Rounding guard by
renderer" in its `doc/architecture/moving-head-shaders.md`. On an Apple M-series
GPU the margin is a sixteenth of a 16-bit step. On Mesa llvmpipe it is a whole
step and on other software or unmeasured drivers four steps. A margin of half a
step or more reaches a code boundary from every level, so every 16-bit axis
uses the reference solver on those drivers and only 8-bit axes stay native.
`afterglow.render.gpu.movement/renderer` reports the renderer and its
`:rounding` row. A movement-only show uses the same facility with scalar or
batched CPU-authored effects.

The default GPU waiting budget is half a frame period, rounded down to a
minimum of one millisecond. It is 12 ms at 40 Hz. All requests in a frame share
one deadline measured from the start of the render tick, including the
subsequent calibrated movement solve. If expression evaluation already spent
that budget, Afterglow resolves movement on the CPU for this frame. The independent
`:timeout-ms` option bounds a driver's readback wait. Compilation and queue
wait both count against the show budget. CPU evaluation, fixture resolution
and output consume additional time.

## Inspect a running show

```clojure
(require '[tolglow.show :as show])
(get-in (show/snapshot runtime) [:status :compute])
(get-in (show/snapshot runtime) [:status :movement])
```

`:requested` and `:active` report the selected and available backends. A
startup failure records its cause in `:last-fallback` and the show starts on
the CPU. During rendering, `:native-dispatches` counts successful on-time
readbacks, `:cpu-fallbacks` counts recovered requests and `:deadline-fallbacks`
counts requests recovered because their frame budget expired. The latest
failure gives its `:stage`, `:type` and `:message`. The `:session` entry
includes the driver renderer and program cache occupancy.

The movement status describes the published frame. `:requested` shows the
configured backend. `:backend`, `:gpu-heads` and `:cpu-heads` report the
solver used and the number of final world targets assigned to each solver.
When available, `:reason` gives the fallback or idle reason, e.g. `:timeout`,
`:busy`, `:unavailable`, `:cpu-input`, `:no-geometry` or `:ambiguous` (the
native solver returned no head that it could publish). `:backend :idle`
means the output has no movement solver result, such as during startup.

## Verify with a capture sink

```sh
LIBGL_ALWAYS_SOFTWARE=1 xvfb-run -a lein with-profile +native-gpu test tolglow.native-show-test
```

This starts a real software GL context and compares captured DMX frames from
two concurrent effects against the CPU show. In the eight-fixture test with
fractional world positions, float32 rounding affected fine pan by one 16-bit
DMX step for three fixtures. The remaining channels matched, and frames
recovered on the CPU matched the full reference universe byte for byte. A
separate test withholds completion of a real draw until after the frame budget
and show restart and checks the captured output again. These are software-driver
correctness checks. Live hardware throughput requires separate measurement.

The calibrated movement test launches overlapping effects on two of eight heads,
sweeps the upper effect through zero, half and full opacity and compares all
DMX slots with the CPU show byte for byte. It runs once with CPU expressions
and again with both native stages sharing the same frame deadline. The demo
heads have 16-bit pan and tilt, so the test expects both heads native only when
the renderer's margin is under half a 16-bit step, and expects `:backend :cpu`
with `:reason :ambiguous` otherwise. A second run gives the same rig 8-bit pan
and tilt and expects both heads native on every renderer, so llvmpipe still
compares native movement bytes against the CPU show.
