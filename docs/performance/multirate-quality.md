# Adaptive render quality

The show delivers DMX at 40 Hz. Presentation sampling can take a different
rate, and predictable effects may evaluate expensive endpoints less often than
their results are presented. A controller needs both time and output-error
measurements to choose a rate responsibly.

The ongoing quality lane owns replay, perceptual and discrete fidelity, timing
headroom, quality ceilings, and slow recovery after a pressured period. The
existing profiler owns spatial candidate pruning and GPU fusion. The viewport
renderer owns antialiasing. Shared live scheduling policy changes follow
measurements against a full-rate reference.

## Existing behavior

* Afterglow's keyframe runtime evaluates a future private clock, caches typed
  endpoints and interpolates native RGB, aim and other continuous attributes.
  Discrete assignments, emitted intents and stateful evaluations fall back to
  the live path. An artist can choose a musical interval and custom easing.
* The release host measures last-frame wall time and worker effect evaluation
  cost. Sustained pressure doubles one eligible effect's spacing. Quiet frames
  shrink spacing in steps. The current cadence cost estimates do not contain
  color, angular or transient-fidelity measurements.
* The cue-page sampler in Afterglow #71 and Tolglow #117 gives mounted visual
  sampling first claim on quality reduction. Window, page and device lifetime
  decide what presentation work exists. The lighting output cadence remains
  independent of pad presentation.
* The existing spatial-field adaptive benchmark verifies exact ArtDMX bytes for
  linear time terms. Its reference cannot reveal how sampled intervals alias a
  nonlinear pulse. The mover performance benchmark verifies layered transforms
  at full rate. This replay combines those two questions.

## Reproduce the measurement

Run both lanes against the same pinned Afterglow revision. The native run
requires a working OpenGL context. Each report includes the renderer, native
dispatches, deadline fallbacks, per-frame pulse and angular error, and wall
time. The command exits unsuccessfully on discrete shutter, cue lifecycle or
protected pulse divergence. It saves the EDN report before checking fidelity.

```sh
lein run -m tolglow.perf.multirate-quality target/quality/cpu.edn 1024 64 80 160 cpu
lein with-profile +native-gpu run -m tolglow.perf.multirate-quality target/quality/gl.edn 1024 64 80 160 native-gpu
```

The reference is an independent full-rate scalar CPU run. Each comparison uses
two show workers and the normal compact renderer, scheduler, ArtNet encoder and
capture sink. The patch contains 1,024 RGBW pixels and 64 moving heads. The
clock advances by 25 ms at 120 BPM. The first 80 frames warm up each runtime.
The next 160 span two 4/4 bars. They contain 16 one-frame flashes and 19
physical shutter transitions. A new single-shot cue launches every 17 frames.
The comparison reads final emitted RGB, the internal world-space target,
every universe byte, the shutter DMX byte, cue lifetime and effect intents.
A maximum raw byte error of 255 can come from rollover of the fine byte of a
16-bit movement channel. Angular and world-space errors quantify aiming.

### Results on the shared Linux host

Stride counts are frames between actual effect evaluations. They appear in
wash / aim / pulse order. The discrete shutter always runs at show rate.

| Policy | Strides | Missed / extra flash frames | Maximum OKLab distance | Maximum angle | Maximum aim-position error |
| --- | --- | ---: | ---: | ---: | ---: |
| Full CPU reference repeat | 1 / 1 / 1 | 0 / 0 | 0 | 0° | 0 m |
| Uniform | 4 / 4 / 4 | 11 / 5 | 1.000 | 0.192° | 0.0161 m |
| Selective | 4 / 2 / 1 | 0 / 0 | 0.00365 | 0.0482° | 0.00402 m |

Both CPU and Mesa preserved all shutter transitions and cue lifetimes. Full-rate
Mesa differed from the scalar oracle by at most 0.00000051 in OKLab and
0.000024° in aim. The uniform and selective fidelity numbers round to the same
values for CPU and Mesa.

| Policy | CPU p95 tick / late frames | Mesa p95 tick / late frames | Mean process CPU per tick, CPU / Mesa | Mesa native dispatches / deadline fallbacks |
| --- | ---: | ---: | ---: | ---: |
| Full | 29.42 ms / 13 | 40.62 ms / 32 | 4.25 / 15.31 ms | 365 / 434 |
| Uniform | 34.72 ms / 21 | 26.25 ms / 10 | 5.44 / 9.56 ms | 66 / 13 |
| Selective | 38.44 ms / 27 | 32.15 ms / 18 | 7.13 / 8.19 ms | 247 / 72 |
| Full repeat | 45.09 ms / 32 | 31.96 ms / 14 | 6.31 / 9.19 ms | 643 / 156 |

A late tick exceeded the 25 ms frame budget. Timings cover the real show tick
through capture, with projection and comparison outside that boundary. CPU time
sums process threads. The repeat shows substantial host/JIT/GC variance.
Mesa used software llvmpipe (LLVM 19.1.7) with a 12 ms dispatch budget.
A last-frame CPU fallback also occurred on the full and selective movement
lanes. These runs establish output fidelity. Physical GPU timings and
headroom measurements on a dedicated machine remain open.

Uniform interpolation erased short flashes and introduced flashes on dark
frames. Reserving full rate for the pulse recovered every flash, and halving
the mover interval reduced its peak error fourfold. On this scene the selective
CPU lane cost more than full rate. A live governor should protect measured
transients, slow only costly predictable inputs under sustained pressure, and
recover in steps when the delivery deadline allows it.

## Worker CPU cost of direct, endpoint and interpolated frames

```sh
lein run -m tolglow.perf.sampling-cost target/quality/sampling-cost.edn
```

The stage replay uses the JVM's per-thread CPU clock around each effect on
its rendering worker. An observer identifies actual endpoint evaluations.
Snapshot projection, DMX delivery, other threads and time spent descheduled
are outside this measurement. Enabling the same sampler observer in every
lane keeps its normal overhead in the measurement. The two workers render
a wash and 64-head aim program simultaneously. Frame output, endpoint
preparation and interpolation use the release implementations.

For each expression depth, the show runs at stride 1, 4, 4 and 1 in that order.
Each lane warms up for 40 frames and records 100. Both stride-four runs must
actually execute 25 endpoint frames and 75 interpolation frames per effect.
The report includes sample counts, medians, p95 and mean thread CPU for each
stage and the whole effect. Compare mean task CPU across the entire window
when predicting savings. Endpoint and interpolation frames have different costs.

In an eight-oscillator replay, the wash took 1.76–2.06 ms per frame
at full rate and 0.69–1.31 ms at stride four. The 64-head aim program took
0.12–0.16 ms directly and 0.29–0.82 ms with the same four-frame spacing.
Its interpolated frames were cheap, but its 25 endpoint frames outweighed
those savings. The existing 0.75 ms minimum cost already keeps these direct
aim evaluations below the automatic slowdown threshold on this machine.

## Continuous evidence

The existing master performance workflow retains `multirate-quality.edn` and
`transient-scheduling.edn` alongside the measured Tolglow and Afterglow commit IDs. It also stores the candidate’s worker-stage distributions in `sampling-cost.edn`.
A change to either replay or to the adaptive scheduler triggers the PR job. The
existing master push and weekly jobs use the same CPU replay. Manual revision
comparisons record both reports for each revision that contains their source.
Timing percentiles are recorded for comparison. The fidelity assertions use
frame-by-frame reference output and run independently of host speed.

[Master performance workflow](../../.github/workflows/performance.yml) ·
[Flash recovery experiment](../effect-keyframes.md#reproduce-the-transient-and-recovery-test)
