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
