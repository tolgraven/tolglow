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

## Aim error relative to a projected spot

```sh
lein run -m tolglow.perf.beam-fidelity target/quality/beam-fidelity.edn 0.25
```

A direction error of one degree is easy to see in a pencil beam. The same
angular error in a broad wash can leave its projected spot mostly in place.
This replay quantifies the displacement on a planar floor. Sixteen groups of
four moving heads sit six metres above the receiver. Within each group the
fixtures follow the same lateral orbit and phase. Their targets are either
0.7 or 6 metres below the lens, and their full beam angles are either 5° or
40°. The lens radius is 5 mm. The show emits a frame every 25 ms at 120 BPM.

Before reading the show, the replay sends six known pan and tilt values through
the paired Afterglow packer, including a pan fine slot in the next universe.
It checks the actual bytes and verifies the decoded angle input to within half
a code step. The report records the encoding, raw probe bytes and Afterglow Git
revision. At half and full travel these two verified packers emit the following
coarse/fine bytes.

| Afterglow revision | Half | Full | Decode to normalized travel |
| --- | --- | --- | --- |
| [`3fd49ef`](https://github.com/tolgraven/afterglow/commit/3fd49ef6877cac57dbc5f0e185fedd2dc7607f39) | `7F 80` | `FF 00` | `(255 × coarse + fine) / 65025` |
| [`68f87b2`](https://github.com/tolgraven/afterglow/commit/68f87b2bd58e67ca695631b05e7258c4bffac89c) | `80 00` | `FF FF` | `(256 × coarse + fine) / 65535` |

Afterglow applies the mount calibration, reconstructs the world
direction and intersects the lens cone with the floor. For every fixture and
frame, compare aim angle, angle as a fraction of the beam's half-angle, floor
displacement in metres, and displacement in units of the original projected
ellipse. An ellipse-relative distance of one places the candidate spot centre
on the reference spot boundary along that direction.

The EDN report includes two bars of reference and four-frame footprint traces
for one representative fixture in each group. The optional positive radius
budget selects the longest interval whose worst observed first-bar displacement
fits the given fraction. The report then shows its worst second-bar error.
This offline comparison uses the projected optical footprint and DMX commands.
Actuator latency and physical response require separate hardware measurements.

Across 160 frames at four-frame intervals, both calibrated packers measured
3.438° near-target p95 angular error. Far-target p95 was 0.626° in
`3fd49ef` and 0.627° in `68f87b2`. Each p95 column below gives projected
spot radii in that same revision order.

| Target depth | Beam angle | Two-frame p95 | Four-frame p95 | Eight-frame p95 | Chosen stride at a 0.25-radius budget |
| --- | --- | --- | --- | --- | --- |
| 0.7 m | 5° | 0.335 / 0.335 | 1.299 / 1.300 | 4.801 / 4.801 | 1 |
| 0.7 m | 40° | 0.055 / 0.055 | 0.206 / 0.206 | 0.704 / 0.704 | 4 |
| 6 m | 5° | 0.065 / 0.065 | 0.246 / 0.246 | 0.899 / 0.899 | 2 |
| 6 m | 40° | 0.009 / 0.009 | 0.034 / 0.034 | 0.124 / 0.124 | 8 |

Admission uses the maximum,
so the narrow far beam chose stride two even though its stride-four p95 was
below the cap. The worst second-bar displacement for these choices was 0,
0.211, 0.068 and 0.138 radii respectively. A changing movement program or zoom
can invalidate a calibrated interval. An online policy would have to detect
that change or resample before relying on earlier measurements.

A second replay exercises a continuous sine orbit with a four-frame period.
For the 5° near beam, sampling only the midpoint of each four-frame window
would report less than 0.001 radii of error on all 40 windows. The full
command stream shows worst intermediate displacements of 10.818 radii on
`3fd49ef` and 10.817 on `68f87b2`.
All 40 midpoint checks would falsely admit the window at the example budget.
The report includes these false admissions for each optical configuration.

For the fixed-depth sine orbits in this rig, the report also evaluates an
analytic error bound from the cue expression and fixture optics. It admits
strides 1, 2, 2 and 8 before reading the measured frames. Every decoded
full-rate comparison stayed inside its predicted limit.
[Curvature assumptions and derivation](aim-curvature.md)
