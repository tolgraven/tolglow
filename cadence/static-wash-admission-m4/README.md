# Immediate static wash reuse in the weighted runtime

This paired test used Tolglow `ec569ed0`, Afterglow `e1eb9ff5` and the in-memory ArtDMX venue with 20,000 pixels, 64 moving heads, eight literal washes, four volume fields and two workers. It used the physical M4 Air, OpenJDK 27.0.1.1 and the bounded host wrapper with one JVM processor and a 1024 MiB maximum heap. Each arm ran four seconds of warmup and six seconds of measurement. A scratch redefinition admitted only time-invariant effects with unchanged certified inputs that had no cadence setting. Dynamic effects, prediction policy and the physical-output scheduler used the regular code.

| Arm | Render median / p95 (ms) | Current / held | Sent / nominal | Literal wash calls |
| --- | --- | --- | --- | --- |
| Baseline 1 | 62.26 / 74.89 | 49 / 191 | 240 / 240 | 7 |
| Immediate 1 | 54.80 / 63.30 | 51 / 188 | 239 / 240 | 0 |
| Immediate 2 | 57.19 / 66.68 | 60 / 180 | 240 / 240 | 0 |
| Baseline 2 | 54.00 / 64.18 | 61 / 178 | 239 / 240 | 255 |

The change removed all sampled literal wash calls. Existing adaptive admission had already removed almost all of those calls in the first control. Render timings and sample freshness overlapped the controls, with no computation at or below 25 ms. One publisher slot was absent in one arm of each type. Every delivered packet was complete, the scene moved and there were zero faulted instances. The candidate remains a test probe.

To reproduce, copy `probe.clj` into `target/weighted-static-admission-ec569ed0/` in a checkout of the same Tolglow revision. Set `AFTERGLOW_ROOT` to the paired revision and run `lein with-profile +benchmark run -m clojure.main target/weighted-static-admission-ec569ed0/probe.clj` through the bounded host wrapper.
