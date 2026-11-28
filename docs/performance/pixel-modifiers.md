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
| `:multiply-translucent` | The keyhole launched at instance opacity 0.6, so it mixes over the wash |
| `:color-guard-hdr` | The colour guard over the same wash with HDR on, against `:wash-hdr` |

The guard, shading bloom and sparkle use the same expressions as the
post-effects in #920. The benchmark authors them directly, so it does not
depend on that branch. Each case is a separate show runtime with two effect
workers and an in-memory ArtDMX sink, and the cases are interleaved frame by
frame. Every modifier must change its baseline's bytes on every frame.

The `-general` lanes repeat each modifier case with the runtime option
`:typed-layers? false`. That option composes every native layer on
Afterglow's general path, and those lanes must send the same packets as the
typed lanes on every frame. Afterglow reports in each frame's metadata
whether typed composition rendered it, or why it fell back. When the paired
Afterglow has the typed path (`:typed-path?` in the report), the benchmark
fails if a typed lane renders any frame without it. Each lane's
`:composition` counts its frames by outcome. The time is a complete
`show/tick!`, including the effect workers' evaluation of each modifier's
own output. Allocation counts the frame owner and the effect workers.

## Results

[Raw reports](../benchmarks/2026-11-28-pixel-modifiers.edn)

All runs used an Apple M4 Pro with OpenJDK 25.0.2 and the benchmark
profile's fixed 768 MiB heap. Each case ran 100 warmup and 300 measured
frames. Other agents shared the machine.

| Run | Tolglow | Afterglow | One-minute load average |
| --- | --- | --- | --- |
| Before | `ed87c4c7` | master `336ab5e2` | 5.3 to 7.2 |
| After | `f58f95fe` | `1afe5dd2` | 2.3 to 1.9 |

| Case | Before median ms | Before p95 ms | Before MB/frame | After median ms | After p95 ms | After MB/frame |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Wash only | 1.53 | 2.98 | 1.25 | 1.10 | 1.29 | 1.49 |
| `:multiply` | 29.58 | 35.13 | 99.7 | 1.06 | 1.14 | 1.75 |
| `:rotate-hue` | 23.41 | 29.64 | 75.4 | 1.17 | 1.26 | 1.76 |
| `:color-grade` | 29.59 | 35.52 | 99.6 | 1.14 | 1.24 | 1.80 |
| `:color-guard` | 29.73 | 34.60 | 98.6 | 1.16 | 1.24 | 1.72 |
| `:guard-sparkle` | 58.34 | 69.34 | 193.8 | 1.87 | 2.10 | 2.02 |
| `:multiply-translucent` | | | | 1.22 | 1.32 | 1.75 |
| Wash only, HDR | | | | 1.08 | 1.17 | 1.60 |
| `:color-guard-hdr` | | | | 1.53 | 1.75 | 1.83 |

The before run predates the translucent and HDR cases.

**Before:** one modifier added 28.0 ms (`:multiply`), 21.9 ms
(`:rotate-hue`), 28.1 ms (`:color-grade`) or 28.2 ms (`:color-guard`) to the
wash's median. The guard and sparkle together added 56.8 ms. Every frame of
the modifier cases exceeded 25 ms.

**After:** an opaque single modifier adds between -0.05 and 0.07 ms to the
median, which is within noise. The translucent keyhole adds 0.11 ms, the HDR
guard 0.45 ms over the HDR wash, and the guard and sparkle 0.77 ms. No frame
exceeded 25 ms. Every typed-lane frame reported typed composition.

In the same after run, the general-path lanes added 31.9 ms (`:multiply`),
31.8 ms (translucent `:multiply`), 30.5 ms (HDR guard) and 63.5 ms (guard
and sparkle). They sent the same DMX as the typed lanes on every frame.

The speed comes from composing in primitive arrays rather than per-head maps
and colour records. The translucent case is as fast as the opaque one. A
review measured the same thing at the `resolve-contributions` level: an
opaque `:multiply` added 0.20 ms, layer opacity 0.8 added 0.23 ms, member
opacity 0.6 added 0.21 ms and target weights of 0.9 added 0.30 ms, against
25.6 to 29.9 ms on the general path. Afterglow's planner for overwritten
columns can also skip blending the wash's colour column under an opaque
modifier. That only applies when the modifier is unweighted, has group
opacity 1 and writes its targets in patch order.

An earlier after run, Tolglow `fd336e06` with Afterglow `310e8013` at load
4.2 to 2.9, gave the same picture: -0.10 to 0.13 ms per single modifier and
0.63 ms for the guard and sparkle. Afterglow `569a5824` changed only
documentation after that run. `1afe5dd2` then added the exact nested literal
product and the composition report. The raw file keeps that run as
`:first-after`.

## Limits

The typed path applies to typed batches, the default `:batch? true` runtime.
A `:batch? false` runtime emits assignment rows, so its modifiers stay on the
general path. The exception is a transform-only member such as the hue turn.
Multi-member groups, `:targets :previous` and transforms of other attributes
also stay on the general path. A frame's metadata names which of these
applied.

The measurements exclude Studio and preview projection, and network or
device output.
