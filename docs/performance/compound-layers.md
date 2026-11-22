# Compound cues on 64 moving heads

The benchmark drives 64 RGBW moving heads across universes with two workers.
Twenty-four live effects animate their aim in 3D and write color, white,
dimmer, focus and zoom. Eight lower-priority cues continue to run in all three
cases. Compare these complete show frames using the paired Afterglow checkout.

```sh
AFTERGLOW_ROOT=../afterglow lein with-profile +benchmark run -m tolglow.perf.compound-layers
```

The first case launches all cues individually. The second puts the upper
sixteen cues into a single opaque macro. The third lowers the macro’s opacity
to 0.65 and sets the last eight members to 0.7 opacity. Those members translate
their target by `[0.35 0.15 -0.75]` meters and rotate hue by a sixth of a turn.
The benchmark interleaves the three cases, checks that the opaque macro matches
the individual cues byte-for-byte on every frame, and checks that the transforms
change the resulting frame. The sink encodes ArtDMX packets in memory.

The optional positional arguments select warmup and measured frames per case.
Defaults are 80 and 200. Elapsed time includes each complete `show/tick!`.
Allocation counts include the frame owner and the effect workers. The master
performance workflow retains the EDN report, including both repository revisions,
CPU, JVM and heap size, as `compound-layers.edn`.

## Opaque layer optimization

The following measurements used the same corpus, two workers and a fixed 768 MiB
heap with OpenJDK 21.0.13 on a shared AMD EPYC 9V74 host. The comparison used Tolglow `1d4606d` and
Afterglow `c15bb7e` and `6feda96`. [Full measurements](../benchmarks/2026-11-22-compound-layer-batches.edn)

| Case | Previous median MB/frame | Optimized median MB/frame | Previous median ms | Optimized median ms |
| --- | ---: | ---: | ---: | ---: |
| Individual cues | 3.190 | 3.206 | 7.01 | 6.61 |
| Opaque compound | 8.201 | 3.253 | 14.65 | 6.02 |
| Fractional compound with transforms | 8.719 | 8.732 | 16.31 | 14.79 |

An opaque compound of direct physical replacements retains the original typed
buffers through slot ordering and fixture packing. Native colors, musical
fields, transforms and alpha blends use the typed field compositor. The
optimized opaque case still exceeded 25 ms on 16 of 200 frames under the
shared runner load. Timings reflect these two captures.
