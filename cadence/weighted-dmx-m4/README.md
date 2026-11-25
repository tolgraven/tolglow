# Weighted show and DMX publication on the M4 Air

Tolglow `38d19ea24a5e9766ed8572bb96cef23a42456204` ran with its CI-pinned Afterglow `bcf63ec2fe920543fd72e61e8a961cbf765be433`. The bounded host job used OpenJDK 27.0.1.1, Clojure 1.12.7 and Leiningen 2.13.1. The host CPU wrapper exposed one JVM processor and used the 1024 MiB heap setting. No Studio graphics benchmark or CI guest was running during the timed window.

The generated model contains 20,000 pixels, 64 moving heads, four mixed position/beam volumes, eight washes, a projected chart and moving fixtures. Adaptive keyframes were enabled. The sink captured 158 universes in memory. The run warmed for 160 nominal 25 ms slots and retained the next 240 slots.

| Measurement | Result |
| --- | ---: |
| Captured output slots | 240 / 240 |
| Average sink gap | 25.015 ms |
| p95 sink gap | 29.163 ms |
| Longest sink gap | 33.377 ms |
| Scheduled source delivered | 71 slots |
| Held source delivered | 169 slots |
| Completed render computations | 84 |
| Completed renders costing at most 25 ms | 0 |
| Completed render p50 / p95 | 52.362 / 59.463 ms |
| Lower bound on source age at the sink, p95 | 185.219 ms |
| Incomplete packets or faulted instances | 0 |

The mean sink gap corresponds to 39.98 Hz. All 240 calls arrived before their own deadline plus 25 ms, and none of the measured gaps fell below the harness's 12.49 ms catch-up threshold. The independent publisher kept sending while the engine reused the latest available source on 169 slots. The weighted renderer still exceeds its 25 ms budget. These timings characterize one bounded sample on the Air and its in-memory sink.

```sh
AFTERGLOW_ROOT=/path/to/paired/afterglow tolgraven-cpu run --mode host --java-heap 1024m -- \
lein with-profile +benchmark run -m tolglow.perf.weighted-live report.edn 20000 160 240
```
