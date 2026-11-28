# Colour modifiers on 2,048 pixels

A colour modifier is a native layer member that rewrites the colour beneath
it. Examples are the keyhole bloom, the colour guard, the shading bloom and
the darkening sparkle. On a large pixel rig it used to cost more than a whole
frame. Afterglow now composes these modifiers from typed colour columns into
one owned typed batch (#908). The design is in
[Afterglow's native layer notes](https://github.com/tolgraven/afterglow/blob/master/doc/architecture/native-layers.md#typed-colour-layers).

```sh
AFTERGLOW_ROOT=../afterglow lein with-profile +benchmark run -m tolglow.perf.pixel-modifiers [warmup samples [pixels]]
```

Every case runs the same moving positional wash over 2,048 RGB pixels, three
channels each, at priority 10. Each modifier case adds modifiers over all
pixels:

| Case | Modifier |
| --- | --- |
| `:multiply` | Keyhole bloom: the colour beneath multiplied by a bloom-band mask |
| `:rotate-hue` | A sixth of a hue turn with no colour output of its own |
| `:color-grade` | Shading bloom: `:color-grade` with coefficients from the bloom band |
| `:color-guard` | Colour guard with a restricted palette |
| `:guard-sparkle` | The colour guard at 50 and the darkening sparkle (`:color-grade` from a seeded hash) at 60 |

The guard, shading bloom and sparkle use the same expressions as the
post-effects in #920. The benchmark authors them directly, so it does not
depend on that branch. Each case is a separate show runtime with two effect
workers and an in-memory ArtDMX sink, and the cases are interleaved frame by
frame. Every modifier must change the wash's bytes on every frame. The
`-general` lanes repeat each modifier case with the runtime option
`:typed-layers? false`. That option composes every native layer on
Afterglow's general path, and those lanes must send the same packets as the
typed lanes on every frame. The time is a complete `show/tick!`. Allocation
counts the frame owner and the effect workers.

## Results

Both runs used an Apple M4 Pro with OpenJDK 25.0.2 and the benchmark
profile's fixed 768 MiB heap. Each case ran 100 warmup and 300 measured
frames. Other agents shared the machine. The one-minute load average was
5.3 to 7.2 before and 4.2 to 2.9 after.
[Raw reports](../benchmarks/2026-11-28-pixel-modifiers.edn)

| Case | Before median ms | Before p95 ms | Before MB/frame | After median ms | After p95 ms | After MB/frame |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Wash only | 1.53 | 2.98 | 1.25 | 1.19 | 1.39 | 0.99 |
| `:multiply` | 29.58 | 35.13 | 99.7 | 1.24 | 1.62 | 1.24 |
| `:rotate-hue` | 23.41 | 29.64 | 75.4 | 1.32 | 1.56 | 1.25 |
| `:color-grade` | 29.59 | 35.52 | 99.6 | 1.15 | 1.35 | 1.30 |
| `:color-guard` | 29.73 | 34.60 | 98.6 | 1.09 | 1.31 | 1.22 |
| `:guard-sparkle` | 58.34 | 69.34 | 193.8 | 1.82 | 2.28 | 1.52 |

"Before" is Tolglow `ed87c4c7` with Afterglow master `336ab5e2`. "After" is
Tolglow `fd336e06` with Afterglow `310e8013`.

Before, one modifier added 28.0 ms (`:multiply`), 21.9 ms (`:rotate-hue`),
28.1 ms (`:color-grade`) or 28.2 ms (`:color-guard`) to the wash's median.
The guard and sparkle together added 56.8 ms, and every frame of those cases
exceeded 25 ms. After, a single modifier adds between -0.10 and 0.13 ms to
the median, which is within noise. The guard and sparkle add 0.63 ms, and no
frame exceeds 25 ms. The single-modifier cost is so small partly because the
typed output is an opaque replacement batch. The coverage planner can then
skip blending the wash's colour column underneath it, though it still
validates that column. In the same after run, the general-path lanes took
31.3 ms (`:multiply-general`), 31.4 ms (`:color-guard-general`) and 61.8 ms
(`:guard-sparkle-general`), with identical DMX on every frame.

The typed path applies to typed batches, the default `:batch? true` runtime.
A `:batch? false` runtime emits assignment rows, so its modifiers stay on the
general path. The exception is a transform-only member such as the hue turn.
The times include the effect workers' evaluation of the modifiers' own
outputs. They exclude Studio and preview projection, HDR (off here; the
Afterglow oracle test covers it) and network or device output.
