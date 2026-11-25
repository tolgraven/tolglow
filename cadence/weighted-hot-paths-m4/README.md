# Weighted show hot paths on the M4 Air

The profile used the in-memory ArtDMX weighted fixture on Tolglow `ec569ed0` and Afterglow `e1eb9ff5`. It used the bounded host wrapper, JDK 27.0.1.1, 1024 MiB maximum Java heap and one JVM processor. The fixture had 20,000 pixels, 64 moving heads, four mixed position and beam fields and eight literal washes. JVM Flight Recorder requested Java execution samples every 5 ms. The sampled analysis takes 20.5 seconds starting 10 seconds after the first observed effect-worker sample.

There were 2,490 samples on the publisher's scheduler pool. The stack contained `tolglow.spatial/advance` in 1,601 samples (64.3%), `afterglow.spatial.field/sample-volume` in 1,261 (50.6%) and `afterglow.spatial.beam/select-emission` in 777 (31.2%). The categories overlap. The nearest application frame was emission selection in 399 samples, radial beam geometry in 372, and the packed column merge in 125. An additional 147 samples came from the two effect workers. These figures describe sampled stacks, without equating them to wall-clock latency. The recorded run had 839 delivered slots, 260 scheduled sources, 579 repeat sources and render p50/p95 50.77/59.27 ms. Treat the instrumented run separately from ordinary timing.

The uninstrumented A/B benchmark tested rejecting radial gap candidates when either meridian component alone was beyond the allowed radius, including a floating-point margin. It retained the production hypot calculation and choice whenever a gap could contribute. The old and new functions matched in 20,072 deterministic random and boundary checks. It produced complete packets across the four live test arms. The fixed-step equality of the complete fixture output was not measured for this candidate.

| Arm | Render median / p95 (ms) | Current / held | Sent / nominal |
| --- | --- | --- | --- |
| Baseline 1 | 55.02 / 62.29 | 62 / 178 | 240 / 240 |
| Radial rejection 1 | 61.70 / 70.52 | 68 / 171 | 239 / 240 |
| Radial rejection 2 | 57.51 / 66.87 | 59 / 180 | 239 / 240 |
| Baseline 2 | 58.63 / 71.13 | 58 / 179 | 237 / 240 |

The candidate provides no reliable improvement and remains local. The raw JFR stays on the test machine. The published stack aggregates and in-memory output receipts contain the relevant measurements.
