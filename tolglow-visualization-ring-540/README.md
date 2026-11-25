# Incremental history ring for Tolglow #540

This pilot tests whether packing once on sample admission can deliver the value-buffer savings from the [first precision pilot](../tolglow-visualization-precision-540/README.md) under repeated rendering. It implements the fixed-domain scalar graph store in isolation, with Full and the previously qualified UNORM8 and UNORM16 formats. FP16 is included as a two-byte comparator. [Tolglow #540](https://github.com/tolgraven/tolglow/issues/540)

The Full ring had the lowest median saturated p95 in the replay. UNORM8 and UNORM16 saved 29.1% and 25.0% of retained payload compared with that ring. Neither packed format improved saturated p95 over the Full ring in the two replay runs used to develop this pilot.

## Admission and format changes

The ring keeps 256 slots per trace. Each slot stores an exact binary64 beat and time. The latest admitted value stays exact for caption formatting and same-beat comparison. Invalid clocks, unchanged duplicates and throttled arrivals perform no buffer writes. A changed observation at the same beat replaces the tail, including when the new observation rounds to the same code. Source changes and clock rewinds reset the trace. The chart uses the same window-edge interpolation and SVG coordinates as the fixed-domain Studio and Push history. [Product admission and plotting code](https://github.com/tolgraven/tolglow/blob/c1fcdb9cffcf9329e736f29cd1cbe2efcce0cc8d/src/studio/tolglow/studio/parameter_visualization.cljs#L154-L213)

Each packed trace converts a value once at an append, replacement or reset. Drawing reads the stored values directly and prepares the SVG path without rebuilding or packing the full history. This pilot gives the buffer one synchronous owner. A live integration needs to preserve that ownership at the renderer boundary and route only the previously qualified scalar graph schemas into this store.

Auto uses UNORM8 for the scale-one Push history and UNORM16 for the selected 2× Studio profile. Full uses the binary64 ring. A format change from Full can replay the exact retained values already in that ring. A format change from a packed history, including a larger raster or the Full override, accepts exact replay supplied by the observation owner. The replay must match the retained source, individual clocks, packed codes and exact latest value. Matching those checks verifies correspondence to the retained history. The caller is responsible for supplying original values because two originals can share a packed code.

When exact replay is unavailable, switching from a packed format discards the older lossy history and seeds the new buffer with the exact latest admitted observation and its clock. A newly admitted missing or out-of-domain value follows that reset path into the Full ring. Full preserves the exact overrange number or the gap needed by the graph. Explicit reconfiguration can choose a qualified format after the retained values again meet its domain requirements.

## Workload

The benchmark replays four 32.5-second sections of 40 Hz arrivals to eight traces, using the same eight synthetic source sequences as the first pilot. It includes unchanged duplicates, same-beat changes, invalid clocks, throttled samples, one source change, a time rewind and a beat rewind. Each arrival invokes three synchronous SVG path consumers per trace with windows of 16, 64 and 16 beats. The 64-beat test window stresses the entire 256-slot history at 120 BPM. All formats use the same path generator, arrival clocks and replay order.

The Full record case is a JavaScript mirror of product admission and bounded full-value records. It rebuilds the history on append and keeps exact readings and clocks. The Full ring provides the same storage layout and renderer code as the packed rings and controls for the layout change. The benchmark rotates case order across three rounds. Each case runs in a fresh process with a warm-up replay and a measured replay. The reported p95 is the median of the three run-level p95 values. The stress subset measures arrivals while the ring is full, including repeats and throttled inputs.

The replay verifies exact admission outcomes, clocks, captions, resets and capacity across all eight tracks. Across 5,460 arrivals, including 1,072 full-history cycles, each packed case performed exactly 10,330 conversions. The 32,800 throttled samples, 390 duplicates and 160 invalid clocks performed zero writes. Checked chart displacements peaked at 0.06774 px for UNORM8 at Push size, 0.00138 px for UNORM16 at Studio 2×, and 0.04428 px for FP16 at Studio 2×. [Benchmark receipt](replay-report.json)

## Latency

| Storage | Admission p95 on cycles with a write | Combined p95, all cycles | Combined p95, full histories |
| --- | ---: | ---: | ---: |
| Full records | 10.37 µs | 845.91 µs | 1,053.12 µs |
| Full ring | 5.70 µs | 742.97 µs | 861.07 µs |
| UNORM8 ring | 6.97 µs | 1,046.37 µs | 1,151.59 µs |
| UNORM16 ring | 5.64 µs | 757.53 µs | 944.83 µs |
| FP16 ring | 6.82 µs | 1,351.70 µs | 1,722.79 µs |

The Full ring reduced the measured saturated p95 median by 18% relative to the Full record model. Relative to the Full ring, the packed medians were 34% higher for UNORM8 and 10% higher for UNORM16. The run-level p95 values varied substantially on the shared machine. Treat the ring layout as a candidate for app measurement and compact storage as a capacity trade-off. The Full record baseline models current semantics in JavaScript, so the table cannot establish an improvement in the built app. [Per-run timing](replay-report.json)

## Footprint

| Allocated history for eight traces | Value buffers | Bytes including clocks and latest-value slots |
| --- | ---: | ---: |
| Full binary64 ring | 16,384 | 49,216 |
| UNORM8 ring | 2,048 | 34,880 |
| UNORM16 or FP16 ring | 4,096 | 36,928 |

UNORM8 reduces the retained payload by 29.1% and UNORM16 by 25.0% relative to the Full ring. The buffer sizes are TypedArray byte lengths, with eight bytes per trace counted for each exact latest value. Engine object overhead and the retained heap of the current ClojureScript records need product measurement.

## Reproduce

Run `node replay.mjs` here using Node 24 with `Float16Array`. Keep the first pilot at the adjacent `tolglow-visualization-precision-540` path. The harness writes `replay-report.json` and uses no package installs. Timings cover JavaScript admission and path preparation. Reagent, layout, SVG rasterization, display capture and native device transport remain separate product gates.
