# Legacy whole-frame oracle

This is the small offline RGBW and moving-head scene tracked in [Tolglow #206](https://github.com/tolgraven/tolglow/issues/206). It uses the original 60 W RGBW fixture patched to universe 1, address 37. The scenario combines a base color, an HTP accent, an equal-priority cover, two aim points, a low-priority red-channel override, a dimmer function and a shutter-open function. Eight explicitly timed frames bracket launches, forced ends, an offline show restart and a stale end for a prior activation.

## Source and capture

| Lane | Tolglow | Afterglow |
| --- | --- | --- |
| Historical | [`01146ece`](https://github.com/tolgraven/tolglow/tree/01146ece7731ec06aa1ee6409342373096c8e0dc) | [`4f7a82ea`](https://github.com/tolgraven/afterglow/tree/4f7a82eab41862f80e43d91a8c2530b00aa51925) |
| Released pair | [`40c92266`](https://github.com/tolgraven/tolglow/tree/40c922664c6c330d9c5eab7f4b2974f5807e2b88) | [`d0ee5652`](https://github.com/tolgraven/afterglow/tree/d0ee56528aafb6d73cbe86b53d618a23b64c081e) |

The historical `effects` branch uses the fork's thi.ng color and `Param` types. Its documented default-master baseline (`16b0b4ca`) does not have those types. The historical combined capture uses the compatible `tolmaster` commit. See the [branch audit](../../../docs/branch-compatibility.md).

The historical JVM runs unchanged source for `tolglow.fixtures/rgbw-60-moving-beam`, `tolglow.color/like`, Afterglow's effect constructors and actual historical `show/send-frame`. The capture calls the underlying Afterglow effect constructors directly because the original `tolglow.fx` namespace eagerly imports obsolete profiling and visualizer dependencies. The original show and its Claypoole `:serial` option retain assigner reduction, kind order, byte packing and movement history. The test records assignment values and source order, then intercepts the final `UpdateDmxData` call and copies its `ByteString`. There is no OLA connection or running scheduler. The restart creates a fresh offline show with the existing fixed-origin metronome and patch.

The clock advances at the supplied integer milliseconds and 120 BPM. The capture binds `rand` to `java.util.Random` seeded with `206` while calling the original color authoring function. The resolved RGB triples are saved as authored inputs, before any frame is rendered. This tests seeded palette authoring. Stateful random effects and production scheduling are outside the scenario.

The released pair uses real `control/apply-command`, `engine/advance`, Tolglow's scalar and batch compilers, and the default compact CPU `replay/render-output`. Both compiler lanes must produce the same full universes, logical contribution order and lifecycle data. The comparison checks the active order at every operation, fixed snapshots, source order within legacy assignment chains, and all 4,096 bytes. The released fixture uses its supported `:extract-min` policy. Color components and world-space points come from the same authored inputs. The function mapping uses the fixture's published low shutter-open byte (`8`) for 0% of that range.

## Reproduce

From this Tolglow checkout, point `AFTERGLOW_ROOT` at the released Afterglow pin and use the normal Leiningen application classpath.

```sh
oracle=dev/tolglow/legacy_frame_oracle
AFTERGLOW_ROOT=/path/to/afterglow-released lein run -m tolglow.legacy-frame-oracle.compare \
  "$oracle/legacy.edn" /tmp/paired-report.edn "$oracle/paired-report.edn"
```

To recapture historical data, create detached worktrees at the two historical commits above. Supply the application dependency JARs from a warmed classpath, with Clojure replaced by `org.clojure/clojure:1.11.4` and Java3D replaced by `java3d/vecmath`, `java3d/j3d-core` and `java3d/j3d-core-utils`, all `1.3.1`. `thi.ng/color` 1.5.1, `thi.ng/math` 0.3.2 and Beat Link 8.0.0 were present in the warmed dependencies. The remaining dependencies came from the warmed application classpath, so the run does not reconstruct the historical dependency lock. Use only JAR entries in `LEGACY_JARS`.

```sh
java -Xmx384m -Djava.awt.headless=true \
  -cp "dev:$LEGACY_TOLGLOW:$LEGACY_AFTERGLOW/src:$LEGACY_AFTERGLOW/resources:$LEGACY_JARS" \
  clojure.main -m tolglow.legacy-frame-oracle.capture /tmp/legacy-oracle.edn
cmp /tmp/legacy-oracle.edn "$oracle/legacy.edn"
```

[`legacy.edn`](legacy.edn) contains every original byte. [`paired-report.edn`](paired-report.edn) records each mismatch as `[universe address legacy paired]`, with 1-based DMX addresses. The comparison command exits nonzero if the generated report differs from the checked report. Intentional output changes require review of the exact updated bytes.

## Observed differences

The two independent historical JVM runs produced identical EDN captures. The paired scalar and batch executions matched each other. The historical and paired outputs differ at 47 of 4,096 byte positions across all eight frames. All 16 lifecycle operations matched after normalizing instance IDs. The old guarded end ignored the stale instance because its ID differed from the relaunched ID. The released reducer rejected the old epoch. Each legacy assignment chain had the same source order as the corresponding paired contributions. The low-priority raw override wrote red byte `49` in both engines, and the explicit dimmer and shutter commands wrote `191` and `8` in both engines.

| Frame time | Changed bytes | Observation |
| --- | --- | --- |
| 0 ms | 1 | Idle dimmer `0` → `255` |
| 100 ms | 7 | Front aim and RGBW extraction differ. The raw red override agrees. |
| 200 ms | 8 | The HTP accent and front aim differ. |
| 300 ms | 7 | White cover and front aim differ. The latest equal-priority cover wins in both lanes. |
| 400 ms | 7 | The HTP accent and back aim differ. |
| 500 ms | 7 | The base color and back aim differ. |
| 600 ms | 1 | Post-restart idle dimmer `0` → `255` |
| 750 ms | 9 | The stale end leaves the relaunched aim and base active. Movement, RGBW and implicit dimmer differ. |

- At 200 ms, the legacy reduction returns the accent RGB `[0.1674…, 0.125, 0.875]` after evaluating `[:base :accent]`. The historical HTP assigner computes a merge and discards its result before returning the resolved accent. The paired `:max` contribution blends the component maxima. [Historical HTP assigner](https://github.com/tolgraven/afterglow/blob/4f7a82eab41862f80e43d91a8c2530b00aa51925/src/afterglow/effects/color.clj#L35-L52)
- At 300 ms, both lanes resolve the white cover, with legacy RGBW `[255 255 255 255]` and paired `[0 0 0 255]`. The legacy conversion writes RGB and computes white separately from HSL. The paired fixture subtracts its minimum RGB component to obtain the white channel. [Historical color resolver](https://github.com/tolgraven/afterglow/blob/4f7a82eab41862f80e43d91a8c2530b00aa51925/src/afterglow/effects/color.clj#L91-L109) · [Paired RGBW packing](https://github.com/tolgraven/afterglow/blob/d0ee56528aafb6d73cbe86b53d618a23b64c081e/src/afterglow/render/frame.clj#L609-L648)
- With the same calibration and front point, legacy pan/tilt bytes are `[132 7 0 0]` and paired bytes are `[131 138 24 133]`. With the back point and movement history they are `[216 93 14 252]` and `[216 53 0 0]`. The old solver selects among pan/tilt solutions using prior movement and the historical channel writer packs coarse and fine bytes separately. The paired solver converts local yaw/elevation and packs the normalized result as a continuous 16-bit value. [Historical transform](https://github.com/tolgraven/afterglow/blob/4f7a82eab41862f80e43d91a8c2530b00aa51925/src/afterglow/transform.clj#L333-L374) · [Historical channel writer](https://github.com/tolgraven/afterglow/blob/4f7a82eab41862f80e43d91a8c2530b00aa51925/src/afterglow/effects/channel.clj#L18-L44) · [Paired aim](https://github.com/tolgraven/afterglow/blob/d0ee56528aafb6d73cbe86b53d618a23b64c081e/src/afterglow/render/frame.clj#L410-L456)
- The paired packer defaults a fixture with emitters to full dimmer when no contribution sets it. The legacy frame clears unassigned channels. The idle RGBW and shutter bytes are zero in both lanes. [Historical buffer clearing](https://github.com/tolgraven/afterglow/blob/4f7a82eab41862f80e43d91a8c2530b00aa51925/src/afterglow/show.clj#L170-L174) · [Paired dimmer packing](https://github.com/tolgraven/afterglow/blob/d0ee56528aafb6d73cbe86b53d618a23b64c081e/src/afterglow/render/frame.clj#L615-L644)

This witness covers one fixture in one universe. The checked-in 36-input functional replay remains the separate witness for worker reordering and seeded running-instance state. Cross-universe legacy output, live schedulers, graceful fades, larger rigs and physical fixtures require their own captures.
