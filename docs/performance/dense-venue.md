# Dense 3D venue

Run the complete show at both pixel counts with 64 moving heads.

```sh
AFTERGLOW_ROOT=../afterglow bin/profile-master --layout sculpture \
  --heads 10000,20000 --movers 64 --workers 2 --warmup 12 --samples 32 \
  --output /tmp/dense-venue.edn
```

The pixels occupy 24 curved RGBW strands in metre-space. An animated color
field samples all three axes. The heads occupy a ring truss, each with its own
position and mounting rotation. They aim at a moving world-space target. An
envelope controls rig-wide gain and a tween accents 16 pixels. The two rigs
span 80 and 158 ArtDMX universes respectively.

Each candidate frame runs through command admission, the effect worker pool,
fixture rendering, the output sink and compact preview projection. The preview
reads the first 16 pixels and all 64 moving heads. The independent-output run
also exercises frame completion, the publisher's direct presentation policy and
packet materialization. A deterministic physical clock advances by 25 ms each
frame. Real thread wakeup jitter, network delivery and browser rendering require
separate measurements.

The reference runs the scalar compiler and general renderer. It checks every
packet byte, model and intent and compares preview fields within `1e-9`.
Generating the reference for 20,000 pixels can exceed the show worker's 500 ms
deadline. The untimed reference has a 10-second deadline. Measured workers use
the production default. The EDN report includes each sample, GC counts, owner
and worker allocation, revisions and JVM options. CI archives the run as
`dense-venue.edn`.

## Host measurements

Both runs used Afterglow `9b7e456`, Java 21.0.13, Clojure 1.12.7, a 768 MiB
heap, two effect workers and a four-vCPU shared Linux runner. Each row has 12
warmup and 32 measured frames. The first run uses the original scalar compiler
from `5fd518b` with the new harness. Revision `092b2f6` reproduces that
combination in one checkout. The second uses the rig-bound selector at
`ebfbcc2`. The allocation columns give median decimal MB per frame for the
owner and warmed worker threads.

| Pixels | Output | Before p50 / p95 (ms) | After p50 / p95 (ms) | Before / after MB | Frames over 25 ms before / after |
| ---: | --- | ---: | ---: | ---: | ---: |
| 10,000 | Direct | 41.68 / 162.11 | 76.19 / 131.72 | 14.99 / 14.61 | 32 / 32 |
| 10,000 | Independent | 37.46 / 64.61 | 62.46 / 168.92 | 16.85 / 15.99 | 30 / 32 |
| 20,000 | Direct | 104.11 / 238.93 | 42.64 / 149.85 | 29.02 / 27.28 | 32 / 32 |
| 20,000 | Independent | 77.91 / 118.67 | 44.69 / 65.32 | 31.71 / 29.97 | 32 / 32 |

The captured selector reduced median allocation in every host case. Host
latency worsened at 10,000 pixels and improved at 20,000. CPU contention and
short samples limit the comparison. Neither workload sustained 40 Hz.

At 20,000 pixels the baseline two-worker pipeline spent median 17.07 ms
evaluating effects, 26.95 ms rendering and 0.18 ms encoding the packets. The
independent publisher took 3.83 ms. Preview projection took 0.18 ms on that
host run. These medians come from separate series and do not sum to a frame
percentile.

Each frame of the accent tween previously validated and traversed the complete
rig to find 16 targets. The rig-bound scalar fallback now validates and selects
the immutable fixture vector during compilation. It checks a replacement
fixture vector before rendering its targets. The full scalar reference still
resolves the current vector on each frame. Running that one tween on the
20,064-fixture rig for 100 warmup calls and 400 measured calls reduced median
time from 1.349 to 0.079 ms and p95 from 2.539 to 0.169 ms. Median thread
allocation per call fell from 821,352 to 19,080 bytes. The archive includes the
measurement form and both results.

## Paired renderer run

A third run combined the same Tolglow commit with Afterglow `fa22954`. It used
the same JVM options, fixture placements, timing inputs, warmup, sample count
and scalar oracle.

| Pixels | Output | p50 (ms) | p95 (ms) | Frames over 25 ms | Median MB/frame |
| ---: | --- | ---: | ---: | ---: | ---: |
| 10,000 | Direct | 18.06 | 90.28 | 9 / 32 | 5.99 |
| 10,000 | Independent | 21.19 | 61.22 | 11 / 32 | 7.36 |
| 20,000 | Direct | 30.09 | 48.26 | 26 / 32 | 10.04 |
| 20,000 | Independent | 24.81 | 33.80 | 14 / 32 | 12.74 |

At 20,000 pixels the two-worker pipeline now spent median 7.86 ms in effects
and 13.16 ms in rendering. The independent publisher took median 3.79 ms.
The host still exceeded the 25 ms budget at the 95th percentile in every
case. The raw reports include sample-by-sample stage times and GC counts.

[Download the host and tween measurements](../benchmarks/master-performance/dense-venue-host.zip)
