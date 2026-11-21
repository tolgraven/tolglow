# Run effect expressions on a native GPU

A show can evaluate declarative color, intensity, focus, aim and direction
expressions with Afterglow's native OpenGL backend. Select the backend in the
ordinary show configuration. This example starts the show without a web server
or physical output.

```clojure
{:hz 40
 :init {:components [:show] :modules [] :post []}
 :show {:enabled true
        :compute-backend :native-gpu
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
runs with plain `lein run`. Setting `:batch? false` selects scalar authoring
and is incompatible with `:compute-backend :native-gpu`.

## What runs in each frame

A pure effect step returns a named field request with its parameter and beat
snapshot, selected fixture IDs and envelope gains. The show evaluates ordered
requests through one Afterglow session shared by every active effect, including
effects launched or reloaded during playback. The fork owns the GPU context,
shader cache, draws and readback. The show releases it on stop, including when
startup fails after acquiring the context.

Tolglow keeps the reference CPU evaluator and full-precision positions for
each group. A failed or late draw takes that path immediately using the same
captured frame inputs. If the driver finishes later, the completed draw has
no path to a publication or a later frame. Successful requests are reused
across effect-isolation retries for that frame. The publication scheduler
continues to apply its configured presentation policy. Exact numeric inputs
and tweens use their existing CPU paths. World-space aim and direction
expressions produce targets for Afterglow's subsequent fixture calibration.

The default GPU waiting budget is half a frame period, rounded down to a
minimum of one millisecond. It is 12 ms at 40 Hz. All requests in a frame share
one deadline measured from the start of the render tick. The independent
`:timeout-ms` option bounds a driver's readback wait. Compilation and queue
wait both count against the show budget. CPU evaluation, fixture resolution
and output consume additional time.

## Inspect a running show

```clojure
(require '[tolglow.show :as show])
(get-in (show/snapshot runtime) [:status :compute])
```

`:requested` and `:active` report the selected and available backends. A
startup failure records its cause in `:last-fallback` and the show starts on
the CPU. During rendering, `:native-dispatches` counts successful on-time
readbacks, `:cpu-fallbacks` counts recovered requests and `:deadline-fallbacks`
counts requests recovered because their frame budget expired. The latest
failure gives its `:stage`, `:type` and `:message`. The `:session` entry
includes the driver renderer and program cache occupancy.

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
