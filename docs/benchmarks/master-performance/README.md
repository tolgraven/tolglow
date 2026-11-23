# Mixed-rig profiling, 2026-11-21

The complete Studio host previously spent about 74 ms on 10,000 RGBW pixels
plus eight aiming moving heads. Partitioning target histories in Afterglow keeps
unrelated pixels in the dense path and reduces that median to 18 ms, with about
83% less allocation. This establishes useful throughput progress, not a steady
40 Hz deadline on this shared machine.

The matching full matrices use Tolglow profiler commit
`e0c56302c3c28fbcf63d444fd87bcc4aa62c9fe8`, 40 warmup/120 measured frames per
case, 100/1,000/10,000 pixels, 0/8 movers, direct serial mapping and bounded 1/2
workers. Java 21.0.13, Clojure 1.12.7, 768 MiB fixed heap, AMD EPYC 9V74 host
with a four-core container quota. Reports preserve every measured observation,
stage distribution, allocation/GC data, source SHA and environment.

| Report | Afterglow source | Scope |
| --- | --- | --- |
| [Baseline](2026-11-21-baseline.edn) | `f9513136ff391811e5064c0352957e534de79ed8` | Complete 30-run matrix |
| [Partitioned targets](2026-11-21-partitioned.edn) | `41e3643e300e3a84cbc5882a88f28b1f013513ee` | Identical complete 30-run matrix |
| [10,000-pixel repeat](2026-11-21-partitioned-repeat.edn) | `41e3643e300e3a84cbc5882a88f28b1f013513ee` | Both mover cases; starts directly at 10,000 rather than warming through smaller rigs |

Two-worker **complete host ticks**, in milliseconds; allocation is median MB
(decimal) per frame across owner and warmed effect worker threads:

| Pixels + movers | Version | p50 | p95 | p99 | Maximum | MB/frame |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| 100 + 8 | Before | 1.14 | 1.78 | 2.90 | 11.76 | 1.18 |
| 100 + 8 | After | 0.84 | 1.36 | 17.44 | 21.48 | 0.57 |
| 1,000 + 8 | Before | 4.96 | 5.89 | 9.61 | 9.95 | 8.09 |
| 1,000 + 8 | After | 1.75 | 2.96 | 3.58 | 13.74 | 1.69 |
| 10,000 + 0 | Before | 13.83 | 16.20 | 22.84 | 25.18 | 10.58 |
| 10,000 + 0 | After | 15.17 | 30.52 | 43.21 | 102.10 | 8.67 |
| 10,000 + 8 | Before | 73.56 | 81.69 | 84.33 | 95.45 | 77.16 |
| 10,000 + 8 | After | 18.22 | 25.08 | 38.97 | 111.99 | 13.05 |
| 10,000 + 8 | Repeat | 19.17 | 27.26 | 105.01 | 127.04 | 13.05 |

The mixed 10,000-pixel pipeline render-stage median fell from **60.24 to 11.41
ms**. Complete host mean throughput changed from **13.56 to 50.51 frames/s**;
120/120 baseline host ticks exceeded 25 ms versus 7/120 after. The homogeneous
control and repeat expose scheduling/JIT noise, so the improved median must not
hide their unfavorable tails. Other small idle Studio JVMs remained on this
shared machine. Builds/browser gates were coordinated, but a newly started
frontend build overlapped part of the repeat. That report is retained explicitly
as contaminated evidence; the first full optimized matrix also had unexplained
outliers. Dedicated CI artifacts are the next independent tail check.

Every warmup and measured frame matched the scalar compiler and general renderer
in full model, intents, every ArtDMX byte and selected preview fields. The host
actually advances clock/middleware, admits master commands, renders, publishes
reactive state and encodes into an injected sink. Neither the simulator nor any
physical/network transport is involved. See [profiling instructions](../../performance-profiling.md)
for all stage boundaries and excluded costs.

Afterglow validation: render/IR **41 tests / 478 assertions**, full pure
**256 / 2,966**, and actual source-parity **293 / 3,303**, all passing.
Mixed-rig regression cases cover interleaved/repeated target order, ordinary and
typed geometry, exact Ratio/BigDecimal values, opacity, blackout, RGBW, coarse/fine
cross-universe packing, compact history reorder, invalid values in either
partition, and independent input/output ownership.

These raw EDN files were only pretty-printed at a wider line width after capture;
no observations were removed or altered. The profiler gained dirty-source status
metadata afterward; the measured source paths in these three captures were clean.

## First automatic master run

[GitHub run 44890920256](https://github.com/tolgraven/tolglow/actions/runs/44890920256)
completed successfully on a real master push after PR #17 merged. Its artifact
`master-show-performance-44890920256` (ID `14702006713`) was downloaded and its
SHA-256 digest verified:
`dd42cebb3fe2d4c44482815db541e186a397a959deef9e67c73a87839f33186e`.
It contains the full report, run log, both resolved commit files and timestamp;
GitHub retains it until 2027-02-19.

Measured clean source: Tolglow `8b65ae9c681f4abdcdcb7488c72d6748fee58bd9`,
Afterglow `6e82fcfbaf7863215255cb322ac311e428a815be`. The complete matrix used
Java 21.0.13, Clojure 1.12.7, four available processors on an AMD EPYC 9V45 host,
and the same 768 MiB heap / 40 warmup / 120 samples. All scalar oracles passed.

For 10,000 pixels + eight movers with two workers, complete host
p50/p95/p99/maximum were **12.72/17.75/22.59/24.07 ms**, with **14.25 MB/frame**
median allocation and **0/120** samples above 25 ms. Pipeline p99 was **22.54 ms**,
maximum **26.77 ms**. This independently demonstrates the whole CPU workload on
that runner; it is not a hardware-matched speedup comparison to the local reports
or a guarantee including physical output, scheduling, browser or simulator costs.

## Connected subscriber run

[Run 44894919836](https://github.com/tolgraven/tolglow/actions/runs/44894919836)
passed the complete core and actual WebSocket matrices. The
[raw subscriber report](2026-11-21-subscribers-ci.edn) is retained unchanged.
Artifact `14702819333` contained both reports and logs; its verified SHA-256 is
`85d919d92bd808bc4f7976509f4e2b53dc85e5c464dbf46c6ecd1c0b4d2bd326`.
Clean sources were Tolglow `fee424ffe41786adcfca9b8383da9977b2b56583` and
Afterglow `36abc94562329d8cb6c62c8916629b3e62fe2a21`.

Java 21.0.13/Clojure 1.12.7, 768 MiB heap, four available processors, AMD EPYC 9V74;
40 warmup and 120 measured frames, two effect workers and 25 ms pacing:

| Pixels + movers | Fast clients (+1 withheld ACK when connected) | Host p50 / p95 / p99 ms | Frames >25ms | Whole-JVM MB/tick |
| --- | ---: | ---: | ---: | ---: |
| 1,000 +8 | 0 | 2.70 /3.93 /4.35 | 0 | 1.91 |
| 1,000 +8 | 1 | 2.56 /3.70 /4.36 | 0 | 2.46 |
| 1,000 +8 | 4 | 2.63 /4.22 /4.39 | 0 | 3.30 |
| 10,000 +8 | 0 | 15.84 /19.06 /28.50 | 4 | 14.88 |
| 10,000 +8 | 1 | 17.47 /21.89 /26.12 | 2 | 15.45 |
| 10,000 +8 | 4 | 16.98 /19.04 /21.68 | 0 | 16.25 |

The four fast clients in the 10,000-pixel case each reconstructed 117/120 frames,
about 38.88 deliveries/s, with three coalesced frames and commit-to-apply p99 near
26 ms. Typical payloads were 3.6 KB. The stalled client received exactly one initial
snapshot until ACK, then reached the latest frame in 24.55 ms. All model, packet,
preview and reconstructed-view checks passed.

The production snapshot exposes only 16 targets' live output, while retaining all
rig definitions (initial 10,000-pixel hydration about 1.37 MB). These results do not
measure full-pixel live preview or a browser. Allocation includes JVM clients,
oracle checks and instrumentation. Fixed run order and differing tails do not
establish that adding clients improves performance; retain the complete data.

## Rotated moving heads and current connected workloads

[The dedicated 64/128-head report](2026-11-21-movers-ci/README.md) retains all
three raw matrices and verified artifact provenance from run 44900142541.
It separates actual host ticks from instrumented aiming/packing, checks all
heads through layered opacity sweeps, and records the remaining 10,000-pixel
subscriber deadline misses without claiming a matched speedup.

## General-row scan comparison

[The matched candidate/baseline/repeat report](2026-11-21-general-row-comparison/README.md)
retains the complete verified artifact archive from run 44901945679. Mixed-rig
owner/worker allocation falls by approximately 394 KB/tick in both candidates;
timing and whole-JVM benefits are unproven, and the unfavorable pixel-only
control is explicitly retained for follow-up attribution.

## Per-attribute envelope scale comparison

[The matched Tolglow compiler report](2026-11-21-batch-scale-comparison/README.md)
retains run 44904180335 with Afterglow held fixed. Hoisting the primitive scale
flag reduces measured 10,000-pixel host allocation by about 2.4 MB/tick in both
candidate passes, while inconsistent timing and subscriber tails remain visible.

## Optical fields, packed writes and native shader readback

The November 23 archive contains alternate-order full-show frame pairs on
20,000 pixels and 64 moving heads. It includes a real loopback Studio stream,
a negative optical-bound experiment, an opaque-wash coverage sweep, same-driver
GPU comparisons, and a separate float32-to-scalar precision check. The README
inside gives the two repository revisions, exact workloads and replay commands.
[Download the raw measurements and runners](render-work-elimination.zip)
