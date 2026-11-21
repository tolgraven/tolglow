# Dedicated runner: moving heads, pixels and subscribers

[Performance run 44900142541](https://github.com/tolgraven/tolglow/actions/runs/44900142541)
passed every scalar model/intent/packet/preview oracle and independent moving-head
geometry/channel oracle. Artifact `14705841415` was downloaded and its ZIP
SHA-256 verified as
`5650d6461dfee7f70b1d7483e4e92bd6884152d81ef1891ae3748fdee1182f1d`.
The nine original artifact files in this directory are unchanged.

The clean measured sources were Tolglow `f3a679681530fc721745b6594906fb2622bc3daf`
and Afterglow `d353f18e727b0ffde2e6c540914b06bdfbc406e6`. The runner exposed four
processors on an AMD EPYC 9V74, with Java 21.0.13, Clojure 1.12.7 and a fixed
768 MiB heap. Each lane used 40 warmup frames and 120 measured frames. The
workloads and excluded costs are in [the profiling guide](../../../performance-profiling.md).

## Complete moving-head show ticks

All heads have distinct placements and mount rotations. Twenty-four registered,
launched effects compete in three priorities; their targets evolve with musical
beat. The repeated 20-frame opacity cycle includes exact zero/one, rising and
falling fades, competing layers and all layers off. The independent oracle checks
every head and physical channel, including pan coarse/fine crossing a universe
boundary. Each lane checked all 48 or 112 heads beyond the first sixteen. The
120 measured frames produced 115 distinct checksums; repeated all-off frames
account for the duplicates.

| Heads | Workers | p50 ms | p95 ms | p99 ms | Maximum ms | Ticks above 25 ms | Mean allocated MB/tick |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 64 | 1 | 2.76 | 4.85 | 12.43 | 20.90 | 0 / 120 | 3.42 |
| 64 | 2 | 2.31 | 2.76 | 4.35 | 5.28 | 0 / 120 | 3.48 |
| 128 | 1 | 3.99 | 4.97 | 6.82 | 9.21 | 0 / 120 | 5.52 |
| 128 | 2 | 3.32 | 4.14 | 4.43 | 4.54 | 0 / 120 | 5.58 |

These are uninstrumented headless show ticks with command enqueue/admission,
effect evaluation, rendering, reactive publication and ArtDMX capture. They do
not include a paced output scheduler, network delivery, a browser or physical
fixtures. The earlier 128-head timeout on the loaded shared development machine
remains failed evidence; it was not reproduced on this runner. Zero sampled
budget misses does not establish an indefinitely sustained real-time guarantee.

The separate two-worker diagnostic pass for 128 heads measured these medians:

| Stage | Median ms |
| --- | ---: |
| Effect engine | 0.90 |
| Total renderer | 2.04 |
| Aiming within renderer | 0.43 |
| Remaining fixture packing/preview within renderer | 0.38 |
| Render reduction/dispatch outside fixture packing | 1.21 |
| All-head result projection | 0.14 |
| ArtDMX encoding | 0.008 |

Nested timers add overhead, and percentiles of components do not sum to a total
percentile. Reduction/dispatch is the larger renderer cost here. Fixed mount
trigonometry is a candidate within the smaller aiming stage, not an established
bottleneck. The next profiling target is allocation and reduction in the complete
pixel/subscriber workload before selecting another implementation change.

## Pixel and subscriber costs still need attention

The same runner also completed the original mixed-rig and real WebSocket lanes.
For 10,000 pixels plus eight movers, the two-worker unpaced host had
p50/p95/p99 **16.69 / 22.79 / 27.71 ms**, with mean owner/worker allocation
**16.69 MB/tick**. The separate pipeline stage medians were **5.78 ms** in the
effect engine and **7.08 ms** in rendering.

The paced loopback subscriber cases used a 25 ms requested period:

| Pixels + 8 movers | Fast clients | Host p50 ms | Host p95 ms | Host p99 ms | Ticks above 25 ms | Whole-JVM MB/tick |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 1,000 | 0 | 3.28 | 5.54 | 6.85 | 0 / 120 | 2.26 |
| 1,000 | 1 | 2.99 | 4.51 | 5.31 | 0 / 120 | 4.00 |
| 1,000 | 4 | 2.92 | 4.90 | 6.39 | 0 / 120 | 7.90 |
| 10,000 | 0 | 17.49 | 24.11 | 35.78 | 6 / 120 | 17.97 |
| 10,000 | 1 | 18.92 | 23.03 | 26.60 | 4 / 120 | 19.71 |
| 10,000 | 4 | 19.50 | 31.01 | 37.23 | 15 / 120 | 23.66 |

Connected cases also had one client withholding its acknowledgement. It received
exactly one initial message while stalled and resumed directly to the newest
frame after acknowledgement; the 10,000-pixel/four-fast-client case resumed in
24.61 ms. All receivers reconstructed the final production view exactly.

Whole-JVM allocation includes the in-process clients, projection/delta encoding,
instrumentation and outside-timer oracle checks. It is not directly comparable
to owner/worker allocation in the table above. Only the production view's first
16 live output values are sent, although all rig definitions are hydrated. This
is loopback delivery evidence, not a measurement of every pixel in a browser or
of iPad rendering. Run order and JIT/GC effects prevent interpreting the smaller
1,000-pixel timings with clients as a performance improvement. Comparing these
results with earlier commits requires a matched before/after run; the current
10,000-pixel deadline misses are sufficient reason to continue profiling.
