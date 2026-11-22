# Venue fog and the 40 Hz frame budget

This capture measures what tracking four half-on foggers costs while Tolglow animates 2,048 RGBW pixels. The independent publisher spent an extra 0.027 ms of median CPU time and allocated an extra 9,640 bytes per publication in the fixed-heap run. The complete show exceeded its 25 ms budget with tracking both enabled and disabled.

## Reproduce the capture

From the full Tolglow checkout, with the paired Afterglow checkout beside it, run

```sh
AFTERGLOW_ROOT=../afterglow lein with-profile +benchmark run -m tolglow.perf.venue-fog 2048 125 300
```

The arguments select pixels, warmup frames per case and measured frames per case. The existing master performance workflow publishes the EDN report as `venue-fog.edn` in its performance artifact. Timings are observations. A mismatch in the emitted fog integral or paired ArtDMX packets fails the benchmark.

The host runs the existing spatial RGBW, gain and accent effects, two effect workers and a four-nozzle cue. Each nozzle has a rated output of 60 m³/min and receives the same 50% demand in all cases. The fixture count, effects, packet encoder and capture sink are identical. Each control fixture omits only its fogger model. A completed block checks every byte of all 17 ArtDMX packets against the paired control and each nozzle's accumulated volume against its captured DMX level. No network traffic or fixture hardware is involved.

A full iteration advances the show clock by 25 ms and calls `show/tick!`. The direct case includes the sink call in the tick. The independent case invokes the normal `scheduler/publish!` after the tick, timing that operation separately. This arrangement measures how much work each call takes. Real-time thread wakeup, network transmission, browser preview and air movement in a venue are outside the capture. Both configurations warm together, and measured blocks alternate which configuration goes first.

## Shared Linux host, November 22, 2026

Tolglow `fb9e142`, Afterglow `c15bb7e`, OpenJDK 21.0.13, 768 MiB fixed heap, AMD EPYC 9V74, four CPU equivalents shared with concurrent builds. Each cell covers 300 frames after 125 frames of warmup. CPU sums the coordinator and effect worker threads. Elapsed time includes periods when the operating system schedules other work. MB uses decimal units.

| Output path | Fog tracking | CPU ms/frame | MB/frame | Elapsed p50 / p95 ms | Full iterations over 25 ms |
| --- | --- | ---: | ---: | ---: | ---: |
| Direct | Off | 23.73 | 24.587 | 26.12 / 55.44 | 167 / 300 |
| Direct | On | 24.33 | 24.602 | 30.38 / 91.50 | 197 / 300 |
| Independent | Off | 25.10 | 25.296 | 46.13 / 125.18 | 251 / 300 |
| Independent | On | 24.66 | 25.304 | 47.44 / 113.14 | 243 / 300 |

| Independent publisher only | Median CPU ms | Median bytes | Elapsed p50 / p95 ms | Calls over 25 ms |
| --- | ---: | ---: | ---: | ---: |
| Tracking off | 0.631 | 650,248 | 0.626 / 3.220 | 3 / 300 |
| Tracking on | 0.658 | 659,888 | 0.661 / 2.098 | 4 / 300 |

A separate shared-host run against Afterglow `54f7f69` observed 13–14 KB of extra allocation per full iteration when enabling fog tracking. Direct elapsed p95 moved from 98.84 to 96.20 ms, and independent elapsed p95 moved from 200.82 to 199.92 ms. The changing direction and size of elapsed-time differences between runs make them unsuitable for estimating a small incremental fog cost. The paired allocation and isolated publisher CPU measurements help distinguish that cost from contention.

## Baseline allocation and CPU

A separate Java Flight Recorder capture covered 300 warmed direct frames with fog tracking disabled. It sampled the coordinator and worker threads at 5 ms for CPU and up to 500 allocation samples/s per sampler. Of 13,655 allocation samples ranked by allocation weight, the nearest project frames included scalar IR operation evaluation (22.6%), runtime instance preparation (22.5%), runtime descriptor creation (9.2%) and parameter validation (9.0%). Of 593 CPU samples, packed-assignment classification took 10.1%, followed by a scalar compiler output loop at 8.3% and the purity input snapshot at 8.1%. These are sampled call sites, not exact attribution to a line or an object.

The mixed rig exposes a compiler eligibility problem. The spatial cue selects only pixels, all with floating-point positions. The unrelated fog fixtures use integer positions. The captured revision checks positional arithmetic across every fixture before applying the cue's target selection. This selects scalar evaluation for a spatial cue whose selected pixels qualify for batch rendering. Keep the integer fog coordinates in this corpus so the master profiler can measure fixes to target-specific eligibility.
