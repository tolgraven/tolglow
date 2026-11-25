# Lighting and Studio preview timing on a physical M4 MacBook Air

In the November 23, 2026 replay, every complete calculation for the 20,000-pixel show took more than 25 ms on the M4 Air. Skipping redundant beam obstruction checks brought the CPU median from 65.60 to 62.13 ms and saved about 8.74 MB of allocation per frame. The standalone GPU shader completed its dispatch and readback in a median of 1.91 ms. The complete show with GPU materials took a median of 77.79 ms in the first run and 61.49 ms on a repeat with the same optimization.

## Whole show

Each run used 20,000 RGBW pixels arranged in 3D, 64 moving heads, eight washes and four moving spheres with gradually fading edges. Each sphere selects lights by position or by whether their beams reach the sphere. Floors, walls and the stage can block a beam. The original code tests the beam segment against the modeled surfaces. The optimized code skips those tests when both ends of the segment are strictly inside a region known to be clear. [Afterglow implementation and reference comparison](https://github.com/tolgraven/afterglow/pull/88)

Two independent show hosts ran the same musical timeline, alternating which rendered first. Each host warmed up for 18 frames, then rendered 36 measured frames to an in-memory ArtDMX sink. Both used two effect evaluators. In the GPU-material run, moving-head geometry still ran on the CPU. Elapsed times include fixture output packing. Network transmission, hardware fixtures and Studio’s browser preview were outside this measurement.

The replay calls `create-runtime` without automatic keyframes. It advances a synthetic clock in 25 ms steps and explicitly calls the render and publish functions for each step. The GPU-material lane sets a 60-second budget per frame for native expression work and a 10-second GPU fence timeout per draw so it can check completed native results. The 0/36 counts below answer “how many full calculations fit in one 25 ms period?” Measuring live packet cadence requires the independently scheduled sender started by `show/start!`, with an actual monotonic timestamp at the sink. [Pinned show setup](https://github.com/tolgraven/tolglow/blob/fef01db4a5804559f90f82147dd94c7c19bdfa16/dev/tolglow/perf/weighted_venue.clj) · [Pinned replay step](https://github.com/tolgraven/tolglow/blob/fef01db4a5804559f90f82147dd94c7c19bdfa16/dev/tolglow/bench_show.clj)

In the repeat GPU-material run with clear-region checks, median time in the spatial stage was 39.29 ms. It included 31.59 ms computing how strongly lights fell inside the moving volumes. The subsequent effect engine took 0.67 ms, fixture rendering took 9.97 ms, and publication took 1.07 ms. The stages have individually calculated medians. Reducing the frequency of effect evaluations alone leaves the earlier spatial work in that path.

| Run and material execution | Beam obstruction checks | Median frame (ms) | Slow-end frame, p95 (ms) | Frames at or below 25 ms |
| --- | --- | ---: | ---: | ---: |
| CPU | Original | 65.60 | 106.30 | 0/36 |
| CPU | Skip checks in clear regions | 62.13 | 77.37 | 0/36 |
| First GPU-material run | Original | 83.42 | 97.24 | 0/36 |
| First GPU-material run | Skip checks in clear regions | 77.79 | 96.83 | 0/36 |
| Repeat GPU-material run | Original | 62.38 | 84.88 | 0/36 |
| Repeat GPU-material run | Skip checks in clear regions | 61.49 | 97.12 | 0/36 |

The p95 value is the 35th-shortest frame out of 36. Among CPU frames with no recorded garbage collection pause, p95 was 76.81 ms with the original checks and 77.37 ms with the optimization. The reduction in overall CPU p95 therefore depends on the frames included. On the GPU-material repeat, the optimized p95 frame overlapped a GC pause. The median whole-frame allocation fell from 44,497,152 to 35,749,872 bytes. The two GPU-material runs have distinct distributions. The CPU and GPU-material comparisons ran in separate JVMs. Subtracting their percentiles would mix scheduling, allocation and host changes with GPU overhead.

All configurations, including the GPU-material repeat, produced identical ArtDMX output. The SHA-256 digest across the replay was `41bf4626e8de1f4f84545a4a0788e7755ff59d509f80f5e8c039a64bcc7d2e83`. Both hosts in each GPU-material run reported 333 draws, 495 native dispatches and zero CPU or deadline fallbacks per host. An extra diagnostic frame read back 320,080 bytes per host.

## Hardware shader and readback

The independent shader computed an RGB value and focus for each of 20,000 vertices. Every draw returned 320,000 bytes. Across 18 warmup and 36 measured draws, all 4,320,000 component values exactly matched the scalar Clojure calculation. Elapsed time for submission, driver fence, readback and unpacking was **1.9055 ms median** and **7.607791 ms p95**. The repeat also matched all 4,320,000 components, with a median of **2.078083 ms**, p95 of **5.900375 ms** and maximum of **6.381084 ms**. Scalar calculation and checking took place outside that timer. The driver reported `Apple M4` and `4.1 Metal - 91.7`.

## Live lighting output on the Air

For the eight-head show used in the room preview, a separate run started the production show scheduler and timestamped its in-memory output. With Ember and Slow sweep active it delivered 40.08 publications/s. Median render work was 1.57 ms, p95 was 2.89 ms and the slowest measured render took 5.92 ms. There were zero recorded errors and zero gaps of 50 ms or more. This run measured the eight-head rig under the local JVM wrapper. The 20,000-pixel rig above still needs an equivalent live delivery measurement.

### JVM pauses with the moving mixed stage

The mixed-stage show drove the 40 Hz scheduler and a local two-universe ArtDMX capture sink. A separate hidden Chrome target replayed the committed moving show in two interleaved phases at a 790 × 475 scene raster and 3271 × 1966 output. These runs used JDK 27, two show render workers and one GC worker. The table covers the central 9.5 seconds of each of five 12-second phases. The maximum JVM pause covers the full recording. [Mixed-stage reproduction](https://github.com/tolgraven/tolglow/issues/555)

| Collector | Maximum heap | JVM processors | Captured output cadence | Longest gap | Skipped slots between sends | Longest recorded GC pause |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| G1 | 1 GiB | 1 | 39.79–39.90 Hz | 56.21 ms | 6 | 45.18 ms |
| ZGC | 1 GiB | 1 | 39.998–40.001 Hz | 29.06 ms | 0 | 0.014 ms |
| G1 | 4 GiB | 1 | 39.78–40.00 Hz | 83.62 ms | 6 | 73.16 ms |
| ZGC | 4 GiB | 1 | 39.996–39.999 Hz | 29.24 ms | 0 | 0.023 ms |
| ZGC | 4 GiB | 9 | 39.896–40.008 Hz | 47.13 ms | 1 | 0.011 ms |

Every G1 gap longer than 37.5 ms overlapped a recorded garbage collection pause. The larger heap reduced G1 pauses from 11 to four across the full recording. Its longest pauses grew, and the analyzed windows still missed six output slots. The nine-processor ZGC run missed one slot in the first show-only phase. Its two gaps longer than 37.5 ms had no recorded GC pause, allocation stall or safepoint in that interval. Both concurrent replay phases missed zero slots. The capture measures the interval between sends at the local sink. Physical-network timing and visible Studio frame delivery have separate acceptance checks. [JVM startup and heap configuration](../application-startup.md)

For Airie live-show captures, set the show JVM's processor count to the host's
online logical CPU count minus one, with a minimum of one. This gives HotSpot a
processor count for sizing its work. macOS still schedules those threads.

```sh
online_cpus=$(getconf _NPROCESSORS_ONLN)
show_cpus=$((online_cpus > 1 ? online_cpus - 1 : 1))
JVM_OPTS="-Xmx4g -XX:ActiveProcessorCount=$show_cpus -Xlog:gc+init=info" lein trampoline run
```

The running JVM reported 9 available processors and a 4,294,967,296-byte
maximum heap. Leiningen passes `JVM_OPTS` on the show JVM's command line,
after the agent launcher's `JAVA_TOOL_OPTIONS`. The JVM initialization log
prints the effective values.

## Studio room on the Air

The browser workload used eight visible, emitting moving heads with the Ember and Slow sweep cues. Each sample lasted 20 seconds after an eight-second warmup. The counter records frames accepted by the visible canvas. The window, camera and 2,806 × 1,260 backing buffer stayed the same. The hardware renderer was ANGLE Metal on Apple M4 and the browser supplied approximately 60 animation callbacks per second. Its GPU timer was unavailable, so the recorded wait for GPU completion also includes time waiting to resume the browser worker.

| Quality setting | Lit room with fog, presentations/s | Moving heads with dense fog, presentations/s |
| --- | ---: | ---: |
| Original Auto | 9.55 | 10.35 |
| Fixed Desktop, 2,670 × 1,199 internal scene | 12.39 | 11.45 |
| Fixed Mobile, 1,828 × 821 internal scene | 23.45 | 23.90 |
| Auto with display-clock pacing | 28.75 | 34.60 |

The original Auto policy selected a tier capped at 10 Hz. Aligning its pacing to the observed display cadence raised the frame rate in both scenes. The 40 Hz physical fixture clock is independent of the display clock. The Air panel provided no 120 Hz display measurement. [Display-clock fix and hardware comparison](https://github.com/tolgraven/tolglow/pull/176)

A separate fixed-Desktop run changed only the fog setting within each scene. The fixed lit room rose from 14.40 presentations/s at fog 0.25 to 21.40 with fog off. The moving scene rose from 14.00 at fog 0.7 to 20.70 with fog off. This comparison measures the cost of enabling fog in that renderer. Even with fog off, the room still rendered well below the observed display cadence.

### Mobile fog sampling with the moving mixed stage

Airie replayed 40 committed show snapshots per second into the production Room worker on Chrome's ANGLE Metal renderer. The test fixed the camera, work light, 1580 × 949 opaque scene and 3634 × 2184 canvas. It used an independent synthetic 60 Hz display clock and counted bitmaps accepted by the hidden canvas. Each measured window lasted about six seconds after warmup. [Mixed-stage profiling issue](https://github.com/tolgraven/tolglow/issues/555)

| Fog raster | Ray steps | Accepted bitmaps/s | p95 presentation gap |
| --- | ---: | ---: | ---: |
| 632 × 379 | 32 | 51.27–51.60 | 24.8–25.0 ms |
| 537 × 322 | 32 | 58.61–59.29 | 22.1–22.5 ms |

The smaller Mobile fog raster used 28% fewer rays. Across matched audience, crossing-beam, close-plume and foreground-edge stills, the 99th-percentile maximum RGB channel difference was at most 3 of 255. The foreground-edge view had a maximum change of 47 at isolated pixels. The comparison uses frozen stills and a moving replay. Visible display cadence and other devices require their own checks.

A separate synthetic 120 Hz pass isolation measured about 53 accepted bitmaps/s with the full scene, 92 when bypassing the volumetric fog pass, 55 with bloom disabled and 120 when replaying the same finished image. The system-wide AGX Device Utilization counter read 100% in all sampled full-scene intervals, with a 19% idle median. The pass comparison uses controlled pass toggles and actual canvas acknowledgements.

## Machine and replay

The Air had 10 CPU cores, eight GPU cores and 16 GB of memory. It ran arm64 macOS 27.2.1, JDK 27.0.1.1 and Leiningen 2.12.1-SNAPSHOT. The local wrapper used niceness 10 and gave the JVM an active processor count of one, two compiler threads and one GC thread. These settings describe this capture. The experiment did not measure the machine with unrestricted CPU scheduling or verify a fixed 1.5-core duty limit.

The first standalone shader JVM wrote its verified result and disposed its GL resources, then remained alive with two non-daemon AWT threads. The operator terminated that process, which exited with status 143. The revised launcher explicitly exits after resource disposal and returns a failure status for validation or shutdown exceptions. The operator verified that the standalone shader and complete GPU-material JVMs in the revised kit both exited naturally with status 0 on the same physical Mac. The launcher retains earlier reports if a later lane fails.

Download the versioned kit, check its SHA-256 against `f38f7ec2fafbcd88fbae69fde075c1df29da8507a99f681ce38a57d3e26008e4`, extract it, and run `./run.sh all`. It records separate CPU, shader and complete GPU-material reports. The included README lists pinned source revisions, offline options, individual lane commands and the result schema. [Mac benchmark kit v2](https://raw.githubusercontent.com/tolgraven/afterglow/052dd3d52a0f7e1861296f21ec440475b087718a/pr-88/macbook-air-lighting-benchmark-v2.zip)
