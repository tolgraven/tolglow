# Conductor frame budget · Tolglow #413

The fixture runs 2,048 animated RGBW pixels and 64 moving heads with spatial HDR and a half-beat conductor. Two workers evaluated the real show and an in-memory sink captured all 18 ArtDMX universes. Every logical tick is 25 ms apart. The benchmark calls ticks without pacing to measure how much of that budget the work would consume. The sink receives the complete universe buffers during each tick. ArtDMX encoding and parity checks run after the tick timing.

## Hosted master

The Ubuntu GitHub Actions run [`45577806821`](https://github.com/tolgraven/tolglow/actions/runs/45577806821) measured Tolglow `bc9accb8d0ebe86080b76274320d0f5ded93b9fb` with the pinned Afterglow `8b0e005fbf8035476bd9c6aedba30c4d90b8bc8c`. The exact source revisions were recorded in the downloaded workflow archive. The archived [conductor report](hosted-master-autopilot.edn) records 100 warmup frames and 160 measured frames.

| Tick type | Frames | Median wall | p95 wall | Over 25 ms |
| --- | ---: | ---: | ---: | ---: |
| Between decisions | 144 | 3.905 ms | 10.780 ms | 0 |
| Decision | 16 | 10.054 ms | 21.630 ms | 0 |

The lane captured 4,680 packets including warmup. The overdriven source was `[255 153 153]` and its originally black spill receiver was `[27 10 10]`. All 1,432 selected sources emitted in the final frame. The hosted run had two G1 young collections totaling 19 ms during measurement.

The same [master lane on the shared development host](local-baseline-autopilot.edn) exceeded 25 ms on 142 of 144 ordinary and 16 of 16 decision ticks. Its p95 wall times were 335.67 ms and 598.19 ms. The host exposed five logical CPUs with a four-CPU cgroup quota and concurrent workloads. The first instrumented attempt also reached the 500 ms effect-worker timeout. That attempt produced no stage distribution. [Failure log](local-stages.log)

## Paired scene-meter optimization

Commit `8165e41d8cc9a733606d413bbef12e40e7f68bd1` gives the conductor a scene-only read of delivered brightness and activity. The same scoring loop supplies full group and effect attribution to the callers that use it. The candidate comparison used the benchmark's real model and captured show runtime in a single JVM, with two master-to-candidate pairs in opposite order. Each fresh runtime ran 160 warmup ticks and 160 measured ticks. Both variants used the same 5,000 ms worker timeout so that overloaded wall time stayed in the distribution and packet parity could be verified. The 25 ms reporting threshold remained fixed.

| Tick type | Master A | Candidate B | Candidate B | Master A |
| --- | ---: | ---: | ---: | ---: |
| Between decisions, meter owner CPU median | 1.653 ms | 1.171 ms | 0.985 ms | 1.758 ms |
| Decision, meter owner CPU median | 1.774 ms | 1.101 ms | 1.022 ms | 1.692 ms |
| Between decisions, full owner CPU median | 15.960 ms | 15.000 ms | 12.480 ms | 15.735 ms |
| Decision, full owner CPU median | 30.540 ms | 29.661 ms | 24.650 ms | 27.680 ms |
| Between decisions, over 25 ms | 141 / 144 | 142 / 144 | 116 / 144 | 140 / 144 |
| Decision, over 25 ms | 16 / 16 | 16 / 16 | 16 / 16 | 16 / 16 |

Median owner allocation decreased by about 0.73 MB per tick in each pair. The first measured pass included 13.5 s of aggregate JIT compilation time across JVM compiler threads. Other passes recorded 1.4–1.5 s each. Full stage, wall, GC and CPU results are in [the paired timing summary](timing-summary.txt) and [the EDN lane summaries](paired-summary.edn). The candidate has local stage evidence and still needs a hosted deadline run.

Both candidate passes matched every ArtDMX byte and the complete saved conductor state against the first master pass across 640 ticks and 11,520 packets. The final master repeat matched 5,760 more packets. The focused JVM lane passed 36 tests and 575 assertions.

## Replay

The [still](conductor-capture.png) and [four-second demonstration](conductor-capture.mp4) decode the 2,880 measured packets in the [first candidate capture](candidate.artdmx). The animation advances at the recorded logical show rate of 40 Hz. The timing strip plots measured owner CPU, with conductor decisions marked in amber. The source and receiver values are the actual packet bytes. The display maps RGBW, dimmer, pan and tilt to an illustrative flat view of the fixture patch. No physical DMX interface was connected.

## Reproduce

From the Tolglow checkout, point `AFTERGLOW_ROOT` at the paired Afterglow revision and use the installed Leiningen release. For the official lane, run the following on the source revision being measured.

```sh
lein with-profile +benchmark run -m tolglow.bench-autopilot 2048 64 100 160 autopilot
```

For the four-pass comparison, run from the candidate checkout. Set `CONDUCTOR_ARTIFACT_DIR` to this evidence directory.

```sh
git show bc9accb8:src/tolglow/lighting_activity.clj > /tmp/tolglow-413-activity-master.clj
lein with-profile +benchmark run -m clojure.main "$CONDUCTOR_ARTIFACT_DIR/measure-conductor.clj" 160 160 "$CONDUCTOR_ARTIFACT_DIR"
lein test tolglow.lighting-activity-test tolglow.autopilot-host-test tolglow.autopilot.controller-test tolglow.autopilot.statistics-test
```

The capture renderer takes the raw packets, the associated [timing CSV](capture-timings.csv) and an output directory.
