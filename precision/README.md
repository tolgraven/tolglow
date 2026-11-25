# Lighting precision qualification

This is a numerical probe of storage formats at two points in a normalized lighting pipeline. Run `python3 precision_probe.py` to regenerate `precision-report.json`. It uses no GPU and measures code errors, with no timing claim.

| Storage | Changed codes after DMX8 round-trip | Changed codes after idealized 16-bit round-trip | Changed final DMX8 codes after an intermediate blend store |
| --- | ---: | ---: | ---: |
| UNORM8 | 0 of 256 | 65,280 of 65,536 | 2,510 of 16,384 |
| UNORM16 | 0 of 256 | 0 of 65,536 | 1 of 16,384 |
| FP16 | 0 of 256 | 58,367 of 65,536 | 191 of 16,384 |
| FP8 E4M3 | 207 of 256 | 65,479 of 65,536 | 12,643 of 16,384 |

FP8 E4M3 changed a DMX8 round-trip by up to eight codes. FP16 changed a final DMX8 output by up to one code when stored between two blends.

The second probe calls Afterglow's real `frame/pack-value!` for 65,026 samples per candidate, with the coarse byte in universe 12 and the fine byte in universe 13. Use the Afterglow Leiningen classpath and run `java -cp "$paths" clojure.main afterglow_packet_probe.clj` with Java 21. The maximum error uses Afterglow's fractional-255 scale, `255 × coarse + fine`.

| Intermediate format | Changed pan packets | Changed inverted tilt packets | Largest error on inverted tilt |
| --- | ---: | ---: | ---: |
| Full reference | 0 | 0 | 0 |
| UNORM8 | 64,770 | 64,770 | 49,023 |
| UNORM16 | 0 | 0 | 0 |
| FP16 | 58,098 | 58,099 | 48,896 |

The large inverted error occurs because a small rounding difference can move a value across the configured inversion pivot before packing. The largest error on ordinary pan was 16 for FP16. The UNORM16 motion sweep starts at already quantized values. Other gains, blends, and interpolated sample positions can still incur rounding error, as the first probe shows.

## Candidate precision selection

- Packed physical output stays in integer buffers. Qualify exact output for changed render stages over blends, level thresholds, color, coarse/fine motion, fixture functions and fallback. Keep the reference path when any output or render deadline check fails.
- For graph-only values, qualify UNORM8 for normalized traces if error stays below the display’s vertical resolution and graph labels and controls retain their original values. Try FP16 for wider ranges and use higher precision for signed geometry, overrange, phase and displays that expose fine motion.
- For Room, prioritize bandwidth in intermediate targets. The scene, composer, and fog already use half-float textures. Evaluate narrower storage or half arithmetic pass by pass. Keep higher precision for ray intervals and world distances, blending thresholds, accumulation and HDR where the image comparisons require it. Qualify temporally so error on thin shafts, compressed gradients, or moving plumes can trigger a wider format.
- Select only device- and workload-qualified formats by default. An explicit Full setting should bypass adaptive narrowing. Report storage, arithmetic, reason for fallback, and separate physical-output and display cadence. Check end-to-end cost with ingestion, transfers and readback included.

[Issue #540](https://github.com/tolgraven/tolglow/issues/540) · [Afterglow packing](https://github.com/tolgraven/afterglow/blob/99f61c5b/src/afterglow/render/frame.clj#L597-L609)
