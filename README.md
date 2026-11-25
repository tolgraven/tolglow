# Precision pilot for Studio and Push histories

Auto selected UNORM8 for a 40 px Push history and Studio plots up to 105 px at device scale 1. It selected UNORM16 for the 105 px Studio detail at device scales 2 and 3. Both formats reduce the size of the history value buffer. This isolated pilot defaults to Full and leaves product rollout open because packing and decoding add CPU work. [Tolglow #540](https://github.com/tolgraven/tolglow/issues/540)

## Storage boundary

Studio and Push share an observed-history chart. The existing record holds the sample time, beat, exact values and source. The last record also supplies the numeric caption. The pilot packs only the values consumed by the SVG path and reads the caption from the exact latest observation. The chart still uses full-precision beats and evaluates the left-window interpolation on the decoded endpoints. [History sampling, plotting and labels](https://github.com/tolgraven/tolglow/blob/c1fcdb9cffcf9329e736f29cd1cbe2efcce0cc8d/src/studio/tolglow/studio/parameter_visualization.cljs#L154-L213)

Auto applies only to supplied finite scalar observations with an explicit fixed `[0, 1]` domain and up to 256 samples. Full bypasses storage conversion. Auto falls back to Full for unsupported domains or uses, typed geometry and range values, integer and discrete values, DMX16, missing values, out-of-range samples and unqualified raster sizes. Auto qualifies numeric DMX8 captions by retaining their exact source observations. The waveform sparkline derives its scale from the current batch, so each source value can affect the scale. [Stable and inferred scale for observed history](https://github.com/tolgraven/tolglow/blob/c1fcdb9cffcf9329e736f29cd1cbe2efcce0cc8d/src/studio/tolglow/studio/parameter_visualization.cljs#L177-L190) · [Sparkline scale](https://github.com/tolgraven/tolglow/blob/c1fcdb9cffcf9329e736f29cd1cbe2efcce0cc8d/src/studio/tolglow/studio/parameter_widgets.cljs#L37-L45)

The qualification checks that the exact latest value 0.5001 still formats to 0.5 with the chart's three-significant-digit rule. Feeding its UNORM8 result to that rule would show 0.502. It also checks that the adjacent DMX16 readouts 0.49999 and 0.50001 retain their exact inputs and remain distinguishable. [History caption rules](https://github.com/tolgraven/tolglow/blob/c1fcdb9cffcf9329e736f29cd1cbe2efcce0cc8d/src/studio/tolglow/studio/parameter_visualization.cljs#L104-L113)

## Error and resolution

The pilot sets a maximum centerline displacement of **0.25 raster px**. The path occupies 38 of the 44 SVG vertical units. Nearest UNORM8 rounding has a maximum error of (1 / 510) in normalized space, so it qualifies through 147.63 raster px of SVG height. The test uses CSS height × device pixel ratio. A fixed-domain linear segment and its left-edge interpolation inherit the endpoint error bound. UNORM16 is the next candidate because it has the same two-byte storage as binary16, a smaller maximum error in `[0, 1]` and wider browser support. [Push scale-one viewport](https://github.com/tolgraven/tolglow/blob/c1fcdb9cffcf9329e736f29cd1cbe2efcce0cc8d/resources/devices/push-display/README.md#L43-L50) · [Push and Studio chart heights](https://github.com/tolgraven/tolglow/blob/c1fcdb9cffcf9329e736f29cd1cbe2efcce0cc8d/resources/studio/parameter-visualization.css#L11-L40) · [Studio monitor and detail heights](https://github.com/tolgraven/tolglow/blob/c1fcdb9cffcf9329e736f29cd1cbe2efcce0cc8d/resources/studio/console.css#L140-L194)

| Plot and raster height | Auto | UNORM8 max px | UNORM16 max px | FP16 max px |
| --- | --- | ---: | ---: | ---: |
| Push 40 px | UNORM8 | 0.06774 | 0.00026 | 0.00843 |
| Studio 58 px | UNORM8 | 0.09822 | 0.00038 | 0.01223 |
| Studio 70 px | UNORM8 | 0.11854 | 0.00046 | 0.01476 |
| Studio 90 px | UNORM8 | 0.15241 | 0.00059 | 0.01898 |
| Studio 105 px | UNORM8 | 0.17781 | 0.00069 | 0.02214 |
| Studio 210 px at 2× | UNORM16 | 0.35562 | 0.00138 | 0.04428 |
| Studio 315 px at 3× | UNORM16 | 0.53342 | 0.00208 | 0.06642 |

These are maxima across eight synthetic traces and six beat windows. The 66,815-position scalar sweep includes format midpoints and found normalized maxima of 0.00196078 for UNORM8, 0.00000763 for UNORM16 and 0.00024414 for FP16. All 256 exact DMX8 source levels survived UNORM8 conversion exactly.

Chromium 156 rendered the prototype's actual SVG paths. Full and candidate screenshots were compared at identical raster positions with the same grid and stroke. Exact DMX8 ramps and steps rendered pixel-identically with UNORM8 across all seven sizes. Other Push UNORM8 traces changed at most 1.32% of plot pixels in one trace, with a peak RGB channel change of 28 of 255. Studio detail at 1× reached 3.92% changed pixels and a peak of 55. At Studio detail 2×, UNORM16 changed at most 0.051% of pixels with a peak of 44 on an antialiased edge. The largest mean per-pixel channel difference for that profile was 0.006 of 255. Pixel displacement and pixel color are separate measurements. [Raster report](raster-report.json) · [Push screenshot](push-raster.png) · [Studio 2× screenshot](studio-2x-raster.png)

## Footprint and latency

| Storage | Value bytes across 8 × 256 samples | Bytes with binary64 beats, times and latest readings |
| --- | ---: | ---: |
| Full binary64 | 16,384 | 49,216 |
| UNORM8 | 2,048 | 34,880 |
| UNORM16 | 4,096 | 36,928 |
| FP16 | 4,096 | 36,928 |

UNORM8 cuts value payload by 87.5% and the modeled total by 29.1%. UNORM16 cuts value payload by 75% and the modeled total by 25.0%. These are TypedArray payload measurements and a binary64 clock/readout calculation. App vectors, observation metadata, GC, and the temporarily retained fixture inputs are outside the byte table.

The Node 24 Linux x64 benchmark times three operations on eight traces per batch. It rotates candidate order, warms up for three rounds and records eleven 200-batch rounds. Full is faster than both UNORM selections when selection, packing and SVG path preparation are measured together. The path-only comparison uses prepacked values and the pack-only comparison writes into preallocated buffers. Full selection reads the supplied exact values directly. Repacking the whole history for each redraw would spend CPU repeatedly for the same data. [Benchmark receipt](qualification.json)

## Product gates

- Use a browser-owned graph store after sample admission. Preserve exact duplicate-beat comparison, rewind and source resets, missing-data gaps, the original clock and caption, and bounded memory. The batch prototype rebuilds from exact inputs when the raster grows or Auto switches to Full. For a live incremental store, rebuild the affected window from exact replay data if available. With only quantized older values available, discard that history, seed the wider store with the latest exact admitted observation and its clock, and accumulate new samples. Switching formats cannot recover already discarded precision.
- Measure real retained heap, channel traffic, capture-to-display latency and cadence in the built Studio route and the 960 × 160 Push capture worker. Verify representative slow near-flat modulation, resize, browser/device-scale changes, and connected Push display output. Requalify if chart sizing, interpolation, data domain or display behavior changes. Compare native FP16 and UNORM16 on supported browsers if decode time matters to the complete display path.
- Issue #540 still needs the separate 3D clipping, banding, blend, shadow and fog-motion checks, exact packet parity over complete lighting workloads, and physical Apple GPU traffic and latency measurements at fresh 40 Hz DMX and separate 60/120 Hz presentation. [Tolglow #540](https://github.com/tolgraven/tolglow/issues/540)

## Reproduce

Run from this directory with Node 24 or a Node release with `Float16Array`. The raster check uses Playwright Core and Chromium. `PLAYWRIGHT_MODULE` can point to an existing installation and `CHROMIUM_PATH` defaults to `/usr/bin/chromium`.

```sh
node qualify.mjs
node raster-check.mjs
```
