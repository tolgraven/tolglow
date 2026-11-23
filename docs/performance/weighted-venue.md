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
