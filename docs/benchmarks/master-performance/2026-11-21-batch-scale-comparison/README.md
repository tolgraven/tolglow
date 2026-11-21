# Per-attribute scale comparison

Tolglow's typed compiler previously used generic `(not= 1.0 scale)` for every
output element. The scale is invariant within an attribute, so the compiler now
computes `(not (== 1.0 scale))` once before its element loop. Multiplication,
finite/range validation and the conditional write still run with the same
semantics. Returned arrays remain privately owned. This avoids repeated generic
comparison/boxing; it does not bypass output validation.

[Matched run 44904180335](https://github.com/tolgraven/tolglow/actions/runs/44904180335)
passed all complete pipeline, actual subscriber and 64/128-head independent
geometry/physical-channel oracles. It ran candidate, baseline, then candidate
again on one dedicated runner. This measured **less allocation**, with mixed
frame-time results and no sustained real-time output claim.

## Reproduce and inspect

The [unaltered artifact archive](raw-artifact.zip), ID `14707791800`, retains all
nine raw EDN reports, nine logs, exact paired commits and their resolution time.
Verified SHA-256:
`84ee49deb6ad94516a8177e64f3e3c0b8c85d67e4d5fdd18bf4b0c22ec8b4102`.
Extract with `unzip raw-artifact.zip -d /tmp/batch-scale-comparison`.
Root reports are the first candidate; `baseline/` and `candidate-repeat/`
contain the remaining passes. [The full host summary](host-summary.edn) retains
every uninstrumented host case, including unfavorable tails.

| Source | Exact revision |
| --- | --- |
| Candidate Tolglow | `013001d2423541905d7825cebaa2821933693742` |
| Baseline Tolglow | `1ddb0608ff6bf048cdb9faadae2c95670ec5eb0c` |
| Afterglow, all passes | `8553cb151b415b4cacbea60cfceacb9cb53b4530` |

All source trees are clean. Each pass uses Java 21.0.13, Clojure 1.12.7, fixed
768 MiB heap, Linux amd64, four available processors on AMD EPYC 7763, fresh
JVMs, 40 warmup frames and 120 measured frames. Options and environment match
except the explicit per-checkout `-Dclojure.compile.path` directory. No source
switching or file patching happens during a measured pass.

The workflow accepts `compare_tolglow_ref` independently of
`compare_afterglow_ref`, so this test holds Afterglow fixed. Its baseline does
**not** include Afterglow PR #30's general-row scan; that change has its own
[matched evidence](../2026-11-21-general-row-comparison/README.md). Do not add
their savings together or interpret these measurements as the combined master.

## Complete host allocation and time

Two-worker host ticks, with 10,000 RGBW pixels and zero/eight moving heads:

| Movers | Pass | p50 ms | p95 ms | p99 ms | Max ms | Ticks >25 ms | Owner/worker MB/tick |
| ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 0 | Candidate | 16.46 | 18.08 | 25.52 | 25.80 | 2 | 11.493 |
| 0 | Baseline | 15.65 | 18.20 | 24.32 | 26.54 | 1 | 13.892 |
| 0 | Candidate repeat | 15.37 | 17.10 | 25.01 | 25.58 | 2 | 11.493 |
| 8 | Candidate | 18.60 | 20.87 | 28.14 | 29.36 | 5 | 15.492 |
| 8 | Baseline | 18.74 | 26.37 | 29.17 | 29.63 | 8 | 17.893 |
| 8 | Candidate repeat | 18.35 | 23.33 | 27.14 | 27.49 | 5 | 15.490 |

Both candidate passes allocate approximately **2.4 MB less per tick** than this
baseline: about 17.3% for pixels alone and 13.4% with movers. The separate mixed
pipeline agrees: 17.767 MB becomes 15.366/15.364 MB. Every smaller host control
also allocates less in both candidates. This does not guarantee a fixed saving
on other JVMs; previous captures demonstrated context-dependent allocation even
with unchanged source. The result is stronger than a JFR sample estimate because
the retained thread counters measure each complete invocation.

Host timing includes command admission, the normal effect pipeline, rendering,
reactive publication and synchronous ArtDMX capture. Pixel-only p99 and sampled
deadline counts are unfavorable; the table retains them. Scheduler wakeup,
physical packet/fixture delivery and browser work remain excluded. See
[the profiling contract](../../../performance-profiling.md) for exact boundaries.

## Subscribers and all-geometric controls

For 10,000 pixels plus eight movers, four acknowledging clients plus one
withheld-ACK client, paced at a requested 25 ms period:

| Pass | Host p50 ms | p95 ms | p99 ms | Ticks >25 ms | Whole-JVM MB/tick |
| --- | ---: | ---: | ---: | ---: | ---: |
| Candidate | 22.92 | 29.84 | 31.46 | 28 | 26.37 |
| Baseline | 23.61 | 30.15 | 34.67 | 38 | 30.29 |
| Candidate repeat | 23.92 | 32.36 | 38.88 | 47 | 28.28 |

The whole-JVM counter includes loopback clients, asynchronous projection and
encoding, instrumentation and oracle comparisons outside tick timers. Its
candidate values are lower here, but the repeat's host tails and deadline count
are worse. All clients reconstructed the final view, and the withheld-ACK client
remained bounded. The live view exposes only the first 16 output targets while
hydrating all fixture definitions; this does not measure an iPad or every pixel's
live browser rendering.

All 64/128 moving-head controls passed the independent reference for every head,
including those after the first 16, across competing priorities and the complete
opacity cycle. No measured moving-head host tick exceeded 25 ms. Two-worker
64-head allocation was 3.419 MB baseline versus 3.403/3.375 MB; 128-head allocation
was 5.514 MB versus 5.452/5.424 MB. Full distributions, checksums and diagnostic
aiming/packing stages remain in the archive. One baseline is not statistical
replication, and neighboring frames are correlated.

## Acceptance

The focused compiler suite passed 6 tests / 58 assertions. Independent review
verified the primitive comparison's behavior for exact inputs after conversion,
NaN, infinities and signed zero, with per-element validation preserved. Final
`dfde976` had identical measured runtime/profiler/workflow code plus documentation
and the earlier renderer evidence. Exact final regression run `44904736085` and
normal performance run `44904736044` both completed successfully before PR #62
merged as `c38397e`. The measured allocation evidence belongs to `013001d` and
the fixed fork above; final CI and the later combined-master measurements are
separate verification records.
