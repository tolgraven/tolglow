# Weighted cue volumes on a dense venue

These runs launch ordinary grid cues on a rig of 10,000 or 20,000 RGBW pixels
and 64 differently mounted moving heads. Each sphere moves at every tick,
paints its own color and fades across a 1.5 m smoothstep band. The four-field
case measures real overlap. In beam mode a fixed pixel can illuminate a field
even if its lens is outside the sphere. The moving heads also change aim.
The 24 × 9 × 24 m room includes receiver walls, a floor and the stage.

```sh
AFTERGLOW_ROOT=../afterglow lein with-profile +benchmark run -m \
  tolglow.perf.weighted-venue 10000 3 5 all /tmp/weighted-10000.edn
```

Run again with `20000` for the larger rig. The positional arguments give pixel
count, warmup frames, measured frames, scope and an optional output file. The
available scopes are `position` for the indexed field calculation, `optics`
for the isolated optical calculation against committed light output,
`host-position`, `host-beam`, `host` for both host cases and `all` for the
position calculation and both host cases. The continuous performance workflow
retains both dense reports whenever the paired checkouts include volume cues.

Each host case boots the show, spawns the cues and changes their positions
through the command queue. The scalar reference renders rich fixture values
and captures direct ArtDMX. The typed candidate publishes independently with a
25 ms physical clock. Every reference tick checks the entire model, intents,
all universe bytes, and compact previews for 16 pixels and all moving heads.
An analytic full-rig sphere scan checks projected memberships. Beam-only
memberships are counted separately for fixed pixels and movers. A fresh
candidate then replays the timed series against the captured packets, intents
and previews. Scalar work stays outside the timed series.

The geometry assay compares every signed-distance weight to an independent
smoothstep sphere calculation within `1e-10`. Geometry times and allocations
are for the calling thread. Host allocations cover the coordinator and warmed
workers. The measured series excludes network output, a real device,
independent clock wakeup jitter and the browser.

## Whole-show comparison

Both sides used the same moving cue series, Java 21.0.13, four JVM-visible
processors, 768 MiB of heap and two release workers on an AMD EPYC 9V74. Each
row has three warmup frames followed by five measured frames. “Beam” includes
origin and emitted-beam contributions. The left side used the initial host cue
implementation (`2809efe`) and core field/optics (`b31c276`). The right side used
shared compact optical observations (`960e6e1`), typed native composition,
reusable receiver planes and analytic sphere witnesses (Afterglow `e3cdf71`).
Both host overlays include the performance harness and worker-deadline option
from Tolglow `9bec1b6`.

| RGBW pixels + 64 movers | Selection | Cues | Frame p50, ms | Frame p95, ms | Median allocation, MB |
| ---: | --- | ---: | ---: | ---: | ---: |
| 10,000 | position | 1 | 181 → 107 | 215 → 170 | 203.1 → 27.6 |
| 10,000 | position | 4 | 357 → 73 | 432 → 126 | 571.4 → 90.3 |
| 10,000 | beam | 1 | 4,196 → 382 | 5,701 → 1,058 | 2,288.7 → 153.9 |
| 10,000 | beam | 4 | 5,233 → 2,375 | 8,795 → 2,964 | 8,402.5 → 570.5 |
| 20,000 | position | 1 | 356 → 193 | 491 → 274 | 433.8 → 53.1 |
| 20,000 | position | 4 | 1,703 → 661 | 1,829 → 1,142 | 1,244.8 → 177.5 |
| 20,000 | beam | 1 | 3,154 → 2,707 | 4,300 → 3,206 | 4,588.5 → 304.1 |
| 20,000 | beam | 4 | 12,046 → 6,848 | 12,161 → 7,824 | 16,810.4 → 1,155.6 |

Every measured frame in this matrix exceeded the 25 ms lighting deadline. The
20,000-pixel beam cases consumed 300 ms of coordinator and worker CPU per
frame with one field and 932 ms with four (medians). The larger wall times
include substantial contention on this shared four-core quota. Five frames
are a short diagnostic series. Use the workflow or increase the measured frame
count on the show machine to characterize sustained operation.

The four beam-aware cues at 20,000 pixels produced a median of 38,072
positive field/head pairs with 12,517 heads in multiple fields. Beam contact
outside the sphere added 17,543 pixel/field and 198 mover/field pairs. The
initial optical solver reported 37,978 pairs. The later witness calculation
also catches grazing fixtures. Both versions independently agreed with their
scalar host and all resulting DMX bytes throughout their respective runs.
Origin-only membership counts agreed across versions.

[Download the full baseline and candidate reports with source provenance](../benchmarks/master-performance/weighted-venue-host.zip).

## Fixed emitters and receiver planes

A second comparison fixes the host at the sources from Tolglow `4dfedb8`.
The initial baseline uses the optical implementation from Afterglow `868f4a5`.
A repeat pins the entire Afterglow source tree to `40ff676`, which contains
the same optical code and the predictive runtime.
Afterglow `582d7bf` caches fixed emitter poses, skips the redundant
cone/box pre-test for analytic spheres, merges weights in one pass, and
calculates receiver-plane distances using primitive numbers. The command
above with `host-beam` reproduces this workload.

| 20,000 RGBW pixels, 64 moving heads and four beam-aware cues | Initial baseline | Pinned repeat | Fixed emitters |
| --- | ---: | ---: | ---: |
| Median coordinator and worker CPU, ms | 1,051.42 | 943.79 | 416.37 |
| Median allocated bytes, MB | 1,069.67 | 1,131.17 | 239.81 |
| Median complete frame, ms | 4,389.71 | 2,572.88 | 2,375.25 |
| p95 complete frame, ms | 5,208.92 | 3,698.54 | 2,766.48 |
| Frames over 25 ms | 5 of 5 | 5 of 5 | 5 of 5 |
| Median field/head pairs | 38,072 | 38,072 | 38,072 |

The pinned repeat and candidate generated identical packet digests throughout
the complete comparison and timed replay. Two further full hosts ran an identical
five-frame cue sequence, one using the original beam/field sampler and one
using the revised sampler. The full models, intents, projected lights and all
158 universes of ArtDMX agreed at every tick. The five packet sets have
SHA-256 `8428fc799de597610a755a428fbc65dda4d032c9ec6d04b23bbcdc97133d8ee0`.

For the per-stage comparison, 10,000 pixels and 64 movers used the same
committed light levels and five room receivers. Nine interleaved measurement
pairs verified all returned weights and aim targets. With four fields, median
owner CPU fell from 427.39 to 167.81 ms and allocation from 475.88 to 60.99 MB.
That measurement predates the primitive receiver change. Its separate
20,000-segment assay compared all visibility decisions and reduced median
owner CPU from 10.83 to 3.34 ms and allocation from 16.58 to 0.13 MB across
24 pairs.

Two additional paired assays separate costs outside the optical solver.
Across 80 pairs on the 20,000-pixel rig, the Y/Z pruning and transient accumulation
for four fields agree with the original X index on every position weight.
Median CPU was 25.60 vs 23.65 ms, allocation 14.23 vs 6.50 MB. Across 24
pairs reading each of 20,064 rows four times, the scoped compact reader used
22.03 ms median CPU and 8.30 MB. Eager projection consumed 37.43 ms and
13.36 MB. Every requested attribute in every row agreed.

[Download the optical and whole-host captures](../benchmarks/master-performance/fixed-emitter-field.zip).

## Overlapping optical fields on a mixed-material rig

The next comparison adds a material that reads both world Z and a fixture's
UV coordinates. Half of the 20,064 fixtures have an explicit chart. The rest
use their world X/Y coordinates. Four independently moving fields sample the
same committed emission. The show still packs and publishes 158 ArtDMX
universes for every frame.

| Mixed UV and world input | Separate observations | Shared observations |
| --- | ---: | ---: |
| Median full frame | 545 ms | 376 ms |
| p95 full frame | 1,003 ms | 736 ms |
| Median worker and coordinator CPU | 326 ms | 219 ms |
| Median optical field sampling | 288 ms | 176 ms |
| Median allocations per frame | 129.1 MB | 109.3 MB |
| Frames over 25 ms | 40 of 40 | 40 of 40 |

The two hosts alternated evaluation order. The comparison checked the
complete model, intents, visible fixture values and every packet across 30
warmup pairs and 40 measured pairs. The candidate includes the shared
per-frame observations and improvements to the radial contact arithmetic
and three-coordinate validation. A separate eight-frame run compared the
candidate against the scalar compiler and renderer. All checks passed.

The p95 frames in this run spent 490 ms and 166 ms respectively runnable
on the coordinator, with 481 ms and 182 ms spent inside output publication.
Those time measurements can overlap. Neither frame overlapped a
stop-the-world garbage collection pause. A separate 28-pair repeat of the
preceding sphere-distance change measured p95 at 1,576 ms and 1,464 ms
respectively. Its optimized p95 frame recorded 1,430 ms of coordinator CPU,
14 ms of runnable wait and zero GC pause. The earlier nine-frame comparison
had no per-frame GC or scheduling trace, so its individual delay remains
unattributed. Runner load materially affects these tail measurements.

[Download the per-frame EDN reports, scalar packet digest and Linux replay scripts](../benchmarks/master-performance/optical-frame-captures.zip).
