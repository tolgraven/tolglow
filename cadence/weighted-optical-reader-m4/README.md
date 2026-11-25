# Prepare the optical reader once per volume

The original weighted venue showed the repeated branch through emission selection on sampled scheduler stacks. The candidate selects the read-only radial path once per clear-air beam-only volume when the existing rig certificate covers its fixture origins. It checks the committed emission level, then uses the existing pose, radial geometry and general probes. Fog extinction, uncovered rigs, aimable selection and unsupported shapes keep their existing path.

Both isolated comparisons used Tolglow `ec569ed0`, Afterglow `e1eb9ff5`, the physical M4 Air, one JVM processor and the bounded host JVM. The show had 20,000 pixels, 64 moving heads, four mixed position and beam fields and eight literal washes. Timings are from the in-memory ArtDMX host and exclude the separate fixed-step exactness run.

| Order and arm | Render median / p95 (ms) | Current / held | Sent / nominal |
| --- | --- | --- | --- |
| 1 baseline | 52.39 / 59.77 | 67 / 173 | 240 / 240 |
| 1 candidate | 44.51 / 52.88 | 85 / 153 | 238 / 240 |
| 1 candidate | 45.06 / 58.14 | 83 / 151 | 234 / 240 |
| 1 baseline | 52.01 / 62.16 | 72 / 168 | 240 / 240 |
| 2 candidate | 47.07 / 65.11 | 93 / 222 | 315 / 320 |
| 2 baseline | 53.38 / 67.36 | 85 / 233 | 318 / 320 |
| 2 baseline | 52.56 / 65.46 | 81 / 238 | 319 / 320 |
| 2 candidate | 44.38 / 57.17 | 106 / 212 | 318 / 320 |

The reverse-order candidate retains the original comparison against the intensity threshold's numeric type. Its exact fixed-step replay matched 24 complete model and intent pairs, every Compact field, and each byte of 3,792 ArtDMX packets. A live fixture-placement edit at frame 13 forced the rig to replace its prepared optical observations. The 480 focused optical cases included position, beam, aimable and attenuated queries.

Each candidate completed more renders. Each still exceeded 25 ms for every render, and the candidate lost more publisher slots across the paired runs. Delivered packets were complete. The test supports reducing producer work and leaves publication acceptance open.

To replay, copy the Clojure files into `target/weighted-optical-dispatch-ec569ed0` in the matching Tolglow checkout and set `AFTERGLOW_ROOT` to the matching fork. Use the bounded host wrapper with `lein with-profile +benchmark run -m clojure.main target/weighted-optical-dispatch-ec569ed0/repeat.clj`. For the first order, use `beam-candidate-01.clj` as `beam-candidate.clj` and run `probe.clj`.
