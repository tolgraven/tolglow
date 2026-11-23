# Lighting deadlines on a physical M4 MacBook Air

On November 23, 2026, the M4 Air missed the 25 ms lighting deadline on every measured frame of the 20,000-pixel show. Skipping redundant beam obstruction checks brought the CPU median from 65.60 to 62.13 ms and saved about 8.74 MB of allocation per frame. The standalone GPU shader completed its dispatch and readback in a median of 1.91 ms. The complete show with GPU materials took a median of 77.79 ms in the first run and 61.49 ms on a repeat with the same optimization.

## Whole show

Each run used 20,000 RGBW pixels arranged in 3D, 64 moving heads, eight washes and four moving spheres with gradually fading edges. Each sphere selects lights by position or by whether their beams reach the sphere. Floors, walls and the stage can block a beam. The original code tests the beam segment against the modeled surfaces. The optimized code skips those tests when both ends of the segment are strictly inside a region known to be clear. [Afterglow implementation and reference comparison](https://github.com/tolgraven/afterglow/pull/88)

Two independent show hosts ran the same musical timeline, alternating which rendered first. Each host warmed up for 18 frames, then rendered 36 measured frames to an in-memory ArtDMX sink. Both used two effect evaluators. In the GPU-material run, moving-head geometry still ran on the CPU. Elapsed times include fixture output packing. Network transmission, hardware fixtures and Studio’s browser preview were outside this measurement.

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

## Machine and replay

The Air had 10 CPU cores, eight GPU cores and 16 GB of memory. It ran arm64 macOS 27.2.1, JDK 27.0.1.1 and Leiningen 2.12.1-SNAPSHOT. The local wrapper used niceness 10 and gave the JVM an active processor count of one, two compiler threads and one GC thread. These settings describe this capture. The experiment did not measure the machine with unrestricted CPU scheduling or verify a fixed 1.5-core duty limit.

The first standalone shader JVM wrote its verified result and disposed its GL resources, then remained alive with two non-daemon AWT threads. The operator terminated that process, which exited with status 143. The revised launcher explicitly exits after resource disposal and returns a failure status for validation or shutdown exceptions. The operator verified that the standalone shader and complete GPU-material JVMs in the revised kit both exited naturally with status 0 on the same physical Mac. The launcher retains earlier reports if a later lane fails.

Download the versioned kit, check its SHA-256 against `f38f7ec2fafbcd88fbae69fde075c1df29da8507a99f681ce38a57d3e26008e4`, extract it, and run `./run.sh all`. It records separate CPU, shader and complete GPU-material reports. The included README lists pinned source revisions, offline options, individual lane commands and the result schema. [Mac benchmark kit v2](https://raw.githubusercontent.com/tolgraven/afterglow/052dd3d52a0f7e1861296f21ec440475b087718a/pr-88/macbook-air-lighting-benchmark-v2.zip)
