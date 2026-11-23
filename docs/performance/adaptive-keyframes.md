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

The master performance workflow runs this comparison and retains
`adaptive-keyframes.edn`. To write the same report locally, append an output
file path to the command. Both passes use two release workers, a 25 ms output budget and a capture sink.
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

## Recovery under sustained frame pressure

A repeat with Afterglow `40ff676` exposed a warmup spike in the one-pixel
effect. The show kept it at stride eight even after its smoothed cost fell to
0.29 ms, below the configured 0.5 ms floor. It received only 11 evaluations
in 80 frames. Afterglow `901e64f` requires two qualifying observations to
throttle and restores an effect's cadence after two cheap evaluations, including
while other effects keep the render loop busy. A tween-only frame supplies no
evidence about the cost of the underlying evaluation.

| 80 measured frames on the 4,000-pixel rig | Before recovery fix | After fix, heavier load | After fix, lighter load |
| --- | ---: | ---: | ---: |
| Green evaluations, per-frame → adaptive | 80 → 11 | 80 → 80 | 80 → 80 |
| Red evaluations, per-frame → adaptive | 80 → 10 | 80 → 10 | 80 → 20 |
| Blue evaluations, per-frame → adaptive | 80 → 10 | 80 → 10 | 80 → 80 |
| Median frame, ms | 19.13 → 19.75 | 33.05 → 10.90 | 12.81 → 8.25 |
| Frames over 25 ms | 27 → 31 | 52 → 13 | 19 → 6 |

These captures used the same workload and two sequential passes per run.
Every completed pass reproduced the full show state, intents, projected
fixtures and all ArtDMX bytes over 24 warmup and 80 measured frames. The
first two attempts after the fix reached the production 500 ms worker timeout
during the per-frame reference. The heavier-load diagnostic set that worker
timeout to five seconds for both passes. The lighter-load repeat kept the
ordinary 500 ms guard and set the starting and maximum JVM heap to 768 MiB.
The shared CPU quota and JVM warmup varied between these runs. The effect
counts show how sampling responds to observed costs and available headroom.

[Download the recovery captures and run conditions](../benchmarks/master-performance/adaptive-keyframe-recovery.zip).
