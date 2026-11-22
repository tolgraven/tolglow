# Profiling the shared lighting controls

The mixed-rig workload animates 2,048 RGBW pixels and aims 64 moving heads in 3D. It enables a wattage and source-count budget, beat-based attack and release, and visual activity scoring together. The capture sink encodes the Art-Net packets. The benchmark checks emitted watts, active sources, moving-head aim, and effect health.

Run it from the Tolglow project with the paired Afterglow checkout.

```sh
lein with-profile +benchmark run -m tolglow.bench-performance-controls
```

The default is 180 warmup frames and 240 measured frames with two render workers. A 40 Hz show has 25 ms for a frame. Wall time measures a full, unpaced software tick. Owner and worker CPU are measured separately because workers can overlap. The artifact also records allocations, garbage collection, and the number of ticks over 25 ms. Master and pull-request performance runs publish it as `performance-controls.edn`. [Performance workflow](../../.github/workflows/performance.yml)

## Mixed-rig measurements

These local measurements used the same workload and Afterglow revision `ab811c4` on a shared JDK 21 host. The first implementation at `2a14b02` used the same benchmark harness as the optimized implementation at `16f77f2`. Wall time also includes contention from other processes.

| Full-tick measurement | First implementation | Packed processing |
| --- | ---: | ---: |
| Median wall time | 123.83 ms | 11.64 ms |
| 95th percentile wall time | 272.18 ms | 25.70 ms |
| Frames over 25 ms | 240 / 240 | 14 / 240 |
| Median owner CPU | 81.49 ms | 7.14 ms |
| Mean worker CPU, both workers combined | 3.48 ms | 3.10 ms |
| Mean allocation across owner and workers | 92.39 MB/frame | 5.48 MB/frame |

Both runs ended with 1,056 of 2,112 sources above standby and 341.87 estimated watts under a 633.60 W cap. This corpus uses fallback fixture ratings. Venue output, actual lamp power, and end-to-end controller latency require separate measurements. Large cold scene launches and changes to the writer graph can have different costs from a warmed running look.

## Isolate a stage

- For cached matching on a 2,048-pixel static tint, run `lein run -m tolglow.bench-group-feedback 2048 50 global`. This compares matched output with the same settled look without feedback and checks packet equality. [Group feedback](group-feedback.md)
- For two continuous alpha sweeps across three overlapping effects on 2,048 RGBW pixels, run `lein with-profile +benchmark run -m tolglow.bench-activity-opacity`. Sharing the per-source calculation for identical layer membership reduced median scoring CPU from 19.53 to 2.23 ms and p95 from 27.83 to 4.20 ms (60 warmup, 120 measured). This isolates activity attribution and checks contributions while lower layers show through. The hosted artifact is `activity-opacity.edn`.
- For the allocator alone, run `lein with-profile +benchmark run -m tolglow.perf.lighting-budget`. It alternates packed RGBW frames and checks post-allocation watts and active counts on every frame.

[Operating the lighting controls](../group-lighting-scenes.md) · [Complete profiling guide](../performance-profiling.md)

## Musical conductor and spatial HDR

Run `lein with-profile +benchmark run -m tolglow.bench-autopilot`. The default show contains 2,048 animated RGBW strip pixels and 64 moving heads. It plays three passes with the same authored effects and clock. The passes progressively enable HDR, then a half-beat conductor. Each pass warms up for 100 frames and measures another 160. The final report separates ordinary frames from decision frames and includes wall percentiles, owner and worker allocations, and the number exceeding 25 ms. The master performance workflow archives the report under `autopilot.edn`.

Each pass captures 4,680 ArtDMX packets over 18 universes. Packet comparisons, fixture visibility queries and timing summaries run outside the measured tick. Two isolated probe pixels check that the authored red source whitens and that its explicitly black neighbor receives the halo. The conductor lane records how many selected sources really emitted and the two measured one-bar means. No physical DMX interface is involved.

| Controlled comparison | Before | After | Output check |
| --- | ---: | ---: | --- |
| Mixed integer and floating coordinates, median summed worker CPU/frame | 20.28 ms | 2.26 ms | All 1,440 packets matched over 80 frames |
| RGBW HDR with explicit white, median HDR owner CPU/render | 4.16 ms | 2.21 ms | All 1,105,920 DMX payload bytes matched over 120 frames |
| 10,000 sources, median controller CPU/decision | 15.26 ms | 9.81 ms | The same seeds yielded identical saved state, gains, actions and ordering |

These measurements isolate individual stages on a shared development host. The full-show report includes scheduling and garbage collection, which affect both the tail latency and the number of missed deadlines. [Operator controls](../energy-autopilot.md)
