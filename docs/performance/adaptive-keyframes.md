# Measuring adaptive effect cadence

`show/start!` measures actual evaluator time on each release worker. The host
can extend the prediction interval for costly, predictable effects when its
frame cost repeatedly exceeds the available time. Cheap and ineligible effects
keep their authored cadence. To inspect the running instances in cost order,
call `(tolglow.effects.cadence/status runtime)`. Each entry identifies the
effect instance, exponentially smoothed evaluation cost, current stride, total
evaluations and number of frames with at least one real evaluation.

The profile below uses 4,000 RGBW pixels arranged in three dimensions. Two
independent red and blue effects each evaluate eight nested trigonometric
operations on one spatial axis and add a beat-driven ramp. A third effect sets
green on one pixel. Since the ramp is linear in beat, per-frame evaluation
provides an exact output reference for the interpolated lighting.

```sh
AFTERGLOW_ROOT=../afterglow lein with-profile +benchmark run -m \
  tolglow.perf.adaptive-keyframes 4000 24 80
```

Both passes use two release workers, a 25 ms output budget and a capture sink.
The first pass measures every effect every frame and limits the stride to one.
The second enables strides up to eight. Both use a 40% reserve for this stress
case and require three successive pressure frames to change cadence. The
production default reserves 15%. The comparison checks every ArtDMX byte,
model, intent and three preview rows for each warmup and measured frame.
Sampling counters exclude the 24 warmup frames.

| 80 measured frames | Stride capped at 1 | Adaptive stride up to 8 |
| --- | ---: | ---: |
| Red spatial evaluations | 80 | 10 |
| Blue spatial evaluations | 80 | 10 |
| Single-pixel green evaluations | 80 | 80 |
| Median complete frame, ms | 42.09 | 32.50 |
| p95 complete frame, ms | 97.98 | 63.64 |
| Slowest frame, ms | 122.99 | 154.70 |
| Frames over 25 ms | 62 | 55 |

The local capture used Tolglow host `3ae9f81` with Afterglow `adaf18c`,
Java 21, four JVM-visible processors and a 768 MiB heap. The slower tail and
55 remaining late frames matter when planning a 40 Hz show. The sequential
passes shared a development machine with another workload, so these timings
include scheduler and warmup variability. Physical I/O is outside this
measurement. The EDN report retains individual effect estimates and frame
duration distributions in [adaptive-keyframes.edn](../benchmarks/master-performance/adaptive-keyframes.edn).
