# General-row scan: matched comparison

[Run 44901945679](https://github.com/tolgraven/tolglow/actions/runs/44901945679)
ran candidate, baseline, then candidate again on the same dedicated runner.
Every scalar state/intent/packet/preview oracle, WebSocket reconstruction and
independent moving-head geometry/channel check passed. This is a small mixed-rig
allocation result, not an established frame-time or whole-JVM improvement.

The [unaltered artifact archive](raw-artifact.zip), artifact ID `14706244349`,
contains nine raw EDN reports, nine logs, three resolved commits and the resolution
timestamp. It is compressed to retain every observation without adding tens of
thousands of generated report lines to the source diff. Its verified SHA-256 is
`cdaacb09e6377ef4ed0638c51fa11ea0444a12040896880c29d60ffb6b596c26`.
Extract with `unzip raw-artifact.zip -d /tmp/general-row-comparison`.
Root files describe the first candidate; `baseline/` and `candidate-repeat/`
contain the other passes. [The host summary](host-summary.edn) retains all
uninstrumented host cases, including unfavorable results.

| Source | Exact revision |
| --- | --- |
| Tolglow, all passes | `499f2eb88c31f7fc905709fe55d2b0be7900442d` |
| Baseline Afterglow | `ba3f9c7949663b810ee837110307dd1eef9b2ea2` |
| Candidate Afterglow | `d5c85e55ab27050ae2dbd9bd9bd587b6ea3cbb4a` |

All source trees were clean. Each matrix's options and environment are identical
across passes: AMD EPYC 7763, four available processors, Linux amd64, Java
21.0.13, Clojure 1.12.7, fixed 768 MiB heap, fresh JVMs, 40 warmup frames and
120 measured frames. The matrices cover 100/1,000/10,000 pixels with zero/eight
movers and one/two workers; real zero/one/four acknowledging subscribers plus
one withheld-ACK client; and 64/128 placed and rotated heads with 24 competing
effects and the full opacity cycle. See [cost boundaries](../../../performance-profiling.md).

## Mixed-rig result and control

The candidate replaces lazy range scanning in `expand-general-rows` with a
primitive index loop. Only selected row indices are retained; assignment maps
still stream in original row/column order. No input or output buffer ownership,
validation, blending, geometry or packing semantics change.

Complete 10,000-pixel + eight-mover show ticks with two workers:

| Pass | p50 ms | p95 ms | p99 ms | Max ms | Ticks >25 ms | Owner/worker MB/tick |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Candidate | 18.60 | 25.13 | 28.75 | 29.54 | 7 | 16.299 |
| Baseline | 18.48 | 21.27 | 29.60 | 31.13 | 4 | 16.693 |
| Candidate repeat | 17.74 | 20.65 | 27.95 | 30.12 | 5 | 16.300 |

The mixed host allocates approximately **394 KB less per tick (2.36%)** in both
candidate passes. The separate two-worker pipeline gives the same roughly
394 KB reduction, from 16.568 MB to 16.174 MB. Timing changes are inconsistent;
the first candidate has worse p95 and more ticks above 25 ms. Neither a throughput
improvement nor a sustained real-time deadline follows from these samples.

The pixel-only control is unfavorable: at 10,000 pixels its two-worker host
allocates **12.693 MB** in the baseline versus **13.892/13.892 MB** in the two
candidate passes. Its p50 is 16.22 versus 15.52/16.74 ms. This control must stay
visible rather than be discarded when assessing the mixed-rig result. Fresh JVM
compilation and escape-analysis decisions can vary, but that is not proof of the
cause. A separate production engine/renderer invocation confirmed zero general
targets and zero calls to the changed helper in that pixel-only corpus. Follow-up
attribution must distinguish that unreachable path from compiler/boxing allocation
before making broader optimization claims. The performance change remains held
while a reversed baseline/candidate/baseline run checks this discrepancy.

## Connected clients and all-geometric heads

Ten thousand pixels plus eight movers, four acknowledging clients and one
withheld-ACK client, with 25 ms pacing:

| Pass | Host p50 ms | p95 ms | p99 ms | Ticks >25 ms | Whole-JVM MB/tick |
| --- | ---: | ---: | ---: | ---: | ---: |
| Candidate | 22.25 | 32.10 | 42.32 | 25 | 22.44 |
| Baseline | 22.84 | 36.72 | 41.67 | 34 | 23.87 |
| Candidate repeat | 22.42 | 29.18 | 35.40 | 17 | 24.65 |

Whole-JVM allocation includes the in-process clients, asynchronous projection
and encoding, measurement instrumentation and oracle checks outside the tick
timer. It rises on the candidate repeat, so it does not establish a repeatable
connected-system allocation improvement. Every client reconstructed the final
view; the slow client remained bounded to one unacknowledged message. Production
snapshots include all rig definitions but only the first 16 live output targets.
Browser/re-frame work and physical transport are not measured here.

The all-geometric workload checks the temporary selected-row vector's other
extreme. Every head, including all heads after the first 16, passed the independent
reference. Two-worker host results:

| Heads | Pass | p50 ms | p95 ms | p99 ms | Mean allocated MB/tick |
| ---: | --- | ---: | ---: | ---: | ---: |
| 64 | Candidate | 3.30 | 4.09 | 5.27 | 3.424 |
| 64 | Baseline | 3.14 | 3.75 | 4.85 | 3.448 |
| 64 | Candidate repeat | 3.11 | 5.29 | 6.08 | 3.479 |
| 128 | Candidate | 4.69 | 6.01 | 6.78 | 5.513 |
| 128 | Baseline | 4.49 | 5.80 | 7.32 | 5.544 |
| 128 | Candidate repeat | 4.30 | 5.45 | 8.16 | 5.570 |

No measured moving-head host tick exceeded 25 ms. Allocation varies by about
one percent rather than showing a convincing benefit. One baseline pass is not
replication; the repeated candidate checks drift, and adjacent frame samples are
correlated. Keep all passes and controls when deciding the next investigation.

## Why this path was investigated

An earlier JFR recording of the real 10,000-pixel/eight-mover/four-client workload
sampled approximately 165 MB under `expand-general-rows` during 160 warmup and
measured ticks, predominantly skipped-row `LongRange.next` allocation. Analysis
excluded the intentionally expensive scalar oracle setup. Sampled weights locate
code worth investigating; they are not exact bytes-per-frame or a promise of
the savings in this separate run. The new subscriber measurement timestamps
make future JFR slicing explicit. Larger remaining sampled costs include IR
evaluation, boxing in batch finalization and packed merging, and subscriber
encoding; fixed mount trigonometry was not the largest measured CPU stage.
