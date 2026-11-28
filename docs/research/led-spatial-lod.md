# Spatial level of detail for LED strips and matrices

Research for [#822](https://github.com/tolgraven/tolglow/issues/822). The idea:
evaluate effects on coarser virtual "chunk heads", each covering a run of LEDs,
and interpolate the per-LED values from them. Lower layers could run coarser
than the layers on top, and a moving shape should light a chunk when it touches
the chunk's nearest real pixel, not its centre.

This is a research report. It changes no production code. The measurements
come from `dev/tolglow/perf/led_lod.clj`, a throwaway harness, and from
`bin/profile-mixed-rig`.

## Answer

1. **Per-pixel effect evaluation is not small.** On the mixed rig it is 36% of
   the serial host tick at 10,000 pixels, 53% at 20,000 and 60% at 40,000.
   Fixture packing is 19–26% and merging 8–9%.
2. **Most of that time is repeated work, not arithmetic.** A hand-written loop
   computes the rig's sine wash 8 to 13 times faster than the expression
   evaluator, and a soft sphere about 200 times faster. The evaluator walks
   321 expression nodes per pixel for the sphere, of which 27 are distinct
   per-pixel work. On the mixed rig, 44% of the typed pixel values written in
   a frame are overwritten by a later opaque layer.
3. **One exact form of the idea is very effective.** Evaluating once per
   distinct coordinate that an effect reads makes the mixed rig's evaluation
   about 20 times cheaper at 20,000 pixels (11.8 to 0.57 ms) and halves the
   tick (22.1 to 10.0 ms p50), and every ArtDMX byte stays identical.
4. **Interpolated chunk heads are only safe for smooth effects.** Linear
   interpolation between samples four pixels apart reproduces a slow wash to
   one 8-bit code. Any effect with an edge, a sawtooth, a gradient stop or a
   shape narrower than a chunk is off by 50 to 255 codes at some pixel, and the
   error flickers as the shape moves.
5. **Recommendation: do not build interpolated LOD now.** Build the exact work
   first:
   [#872](https://github.com/tolgraven/tolglow/issues/872),
   [#873](https://github.com/tolgraven/tolglow/issues/873),
   [#874](https://github.com/tolgraven/tolglow/issues/874) and
   [#875](https://github.com/tolgraven/tolglow/issues/875).
   Then measure again. An opt-in interpolated tier
   ([#876](https://github.com/tolgraven/tolglow/issues/876)) is specified below for
   the case where a real show still misses its deadline on the CPU path.

## Scope and method

| | |
| --- | --- |
| Tolglow | `bce22156` plus the harness in this change. Master has since moved to `1bc5ac78`, which pins this Afterglow revision; there the harness was only smoke-tested at 2,000 pixels |
| Afterglow | `ef539be0` |
| JVM | JDK 25.0.2, Leiningen `:benchmark` profile, 768 MiB heap, G1 |
| Machine | Apple M4 Pro, 14 cores, shared with other agent lanes |
| Rig | `tolglow.perf.mixed-rig`: RGBW pixels on a 100-column matrix at unit pitch, 48 dimmers, 48 RGB pars, 16 moving heads, 26 cues at five priorities, envelopes, a tween and 8 LFO bindings |

Every run used an ArtDMX capture sink. Nothing was sent to a network or a
device. The measured interval is one `show/tick!`. The publisher thread, packet
delivery, Studio subscribers, the browser and physical output are excluded.

The machine ran other lanes throughout. Load averages were 10–16 for the
stage and host runs and 20–42 for the prototype runs, so absolute times in the
prototype tables are high. Prototype speedups are the median, over frames, of
full-resolution time divided by candidate time measured back to back on the
same frame. Error figures do not depend on load.

The harness has six scopes:

- `stages` wraps the real tick with inclusive timers on one effect worker,
  for the mixed rig or for a show named with `--show`.
- `families` times ten effect families through the batch compiler's own step.
- `lod` evaluates those families at full resolution and through prototype
  chunk plans, and reports error, shimmer and time.
- `e2e` runs the real tick again with a prototype evaluator swapped in for
  large emission batches, and compares every ArtDMX byte with the plain run.
- `hidden` counts the typed writes of one frame that a later opaque
  replacement overwrites.
- `nodes` counts the expression nodes the evaluator visits per pixel.

The prototype calls the production `ir/evaluate-batch` on the sample positions
and expands the fields with a primitive loop. Reproduction commands are at the
end.

## Where the frame goes

Host tick with two effect workers, 400 frames after 200 warm-up frames, each
frame equal to the serial scalar oracle in model, intents, every packet byte
and preview (load average 15.9):

| Pixels | Universes | Tick p50 / p95 / p99, ms | CPU p50, ms | Frames over 25 ms | Allocation p50, MB |
| ---: | ---: | --- | ---: | ---: | ---: |
| 1,000 | 9 | 2.64 / 5.28 / 7.11 | 3.17 | 0 | 1.90 |
| 10,000 | 79 | 7.49 / 10.72 / 14.48 | 9.95 | 0 | 4.75 |
| 20,000 | 158 | 12.45 / 16.43 / 24.15 | 17.23 | 4 of 400 | 7.93 |

The same tick on one effect worker with inclusive timers (load average 12.2,
400 frames, no oracle comparison). Milliseconds per frame, and the share of
the tick:

| Stage | 1,000 px | 10,000 px | 20,000 px | 40,000 px |
| --- | ---: | ---: | ---: | ---: |
| Whole tick, mean | 3.22 | 7.70 | 17.91 | 34.13 |
| Per-pixel effect evaluation (`ir/evaluate-batch`) | 0.49 (15%) | 2.76 (36%) | 9.48 (53%) | 20.64 (60%) |
| Envelope scaling and range check (`finish-fields`) | 0.01 | 0.04 | 0.08 | 0.15 |
| Engine, everything else | 1.14 (35%) | 0.86 (11%) | 1.21 (7%) | 1.38 (4%) |
| Merge layers into the frame | 0.14 (4%) | 0.73 (9%) | 1.48 (8%) | 3.19 (9%) |
| Pack fixtures to DMX (`pack-dense-state`) | 0.28 (9%) | 2.02 (26%) | 4.07 (23%) | 6.63 (19%) |
| Batch validation and other render work | 0.18 | 0.28 | 0.44 | 0.84 |
| ArtDMX encoding | 0.03 | 0.08 | 0.16 | 0.32 |
| Host, everything else | 0.96 (30%) | 0.93 (12%) | 1.00 (6%) | 0.99 (3%) |

A second run of this table with the final harness, at load averages of 19 to
50, took 1.6 times as long and gave evaluation 39%, 49% and 57% of the tick
at 10,000, 20,000 and 40,000 pixels.

A JFR recording of the two-worker run agrees. At 20,000 pixels
`ir/evaluate-batch` holds 53% of all CPU samples and `resolve-contributions`
34%, of which `pack-dense-state` is 23% and the merge 8%.

Three things stand out.

- **Evaluation grew faster than the pixel count between 10,000 and 20,000
  pixels, and in step with it above.** It takes 2.76 ms at 10,000 pixels,
  9.48 ms at 20,000 (3.4 times) and 20.64 ms at 40,000 (2.2 times). The second
  run gave 5.30, 14.33 and 32.02 ms (2.7 and 2.2 times). Both runs were
  loaded. `ir/load-position!` and `ir/finite?` together
  hold 13–15% of all CPU samples. The compiler hands the evaluator a vector of
  `[x y z]` vectors of boxed doubles, and the evaluator supports a flat
  `double[]` that Tolglow does not pass. This is consistent with cache misses
  on the boxed positions. It was not isolated further.
- **A one-component field costs as much per pixel as a three-component one.**
  At 20,000 pixels the colour and white field on all pixels takes 2.74 ms and
  two one-component white layers on 10,000 pixels each take 2.64 ms together.
  The per-pixel overhead dominates the arithmetic.
- **Only pixel batches matter.** Every dimmer, par and mover plan together is
  under 0.1 ms.

Which effects dominate on the mixed rig: the pixel field on all pixels (29% of
evaluation at 20,000), four white layers on pixel subsets (49%) and the colour
overlay on every second pixel (20%). All of them read only `x`.

### Effect families

Ten families on a 10,000-pixel matrix through the batch compiler's step, median
of 300 frames, in two runs. The first report was overwritten by the second
and is not among the retained reports.

| Family | What it is | Step, ms, load 10.1 | Step, ms, load 19.9 |
| --- | --- | ---: | ---: |
| `uniform-wash` | constant colour from a parameter | 0.35 | 0.45 |
| `sine-wash` | the mixed rig's pixel field, wavelength 125 px | 1.05 | 1.59 |
| `saw-chase` | the mixed rig's overlay, sawtooth of period 50 px | 1.01 | 1.48 |
| `cell-chase` | stepped chase on 5-pixel cells | 1.23 | 1.73 |
| `fine-ripple` | sine of wavelength 4.2 px | 1.15 | 2.04 |
| `plasma` | sum of sines in `x`, `z` and beat | 3.07 | 3.57 |
| `soft-sphere` | sphere of radius 12 px with a 12 px smoothstep edge, moving | 11.13 | 12.84 |
| `hard-sphere` | the same with a 0.5 px edge | 11.01 | 12.08 |
| `small-sphere` | radius 1.5 px with a 0.5 px edge | 11.23 | 11.81 |
| `palette-gradient` | 8-stop gradient parameter along `x`, repeating | 12.32 | 17.46 |

One palette gradient or one sphere written as an expression costs half of a
25 ms frame at 10,000 pixels. Controls for comparison from the same two runs.
The sphere loop was added for the second:

| Step, ms | Load 10.1 | Load 19.9 |
| --- | ---: | ---: |
| `sine-wash`, compiled | 1.05 | 1.59 |
| `sine-wash`, hand-written primitive loop over flat `x` | 0.078 | 0.207 |
| `soft-sphere`, compiled | 11.13 | 12.84 |
| `soft-sphere`, hand-written primitive loop over flat `x` and `z` | not in that run | 0.062 |
| One moving sphere through `afterglow.spatial.field/positions` (a volume cue's weights) | 0.14 | 0.17 |

The hand-written loops agree with the compiled families to `1e-12`. They show
what the arithmetic costs once the expression tree is out of the way.

The expression tree explains the gap. The authoring DSL builds trees by value,
so a shared term appears once per use. Nodes the evaluator visits per pixel,
counted by the harness's `nodes` scope the way `ir/scalar-component` walks
each component. A hoisted subtree counts as one slot read:

| Family | Today | With frame-constant subtrees hoisted | And each shared subexpression once |
| --- | ---: | ---: | ---: |
| `sine-wash` | 12 | 12 | 12 |
| `plasma` | 58 | 52 | 40 |
| `soft-sphere` | 321 | 153 | 27 |
| `palette-gradient` | 417 | 375 | 96 |

Each coordinate of the sphere's moving centre is recomputed twelve times per
pixel, with a sine or cosine each time. `sine-wash` gains nothing from
sharing, but two of its three colour components are constants that are
written per pixel today. Measured cost fits about 60 ns per pixel plus 3 ns
per node, so the sphere would drop from about 1,100 to about 140 ns per pixel
with the same arithmetic in the same order. That estimate is a model from the
node counts. It was not prototyped.

### A named show

`--scope stages --show tolglow.showcase/model --timeline tolglow.showcase/play!`
splits the tick of the repository's showcase show instead of the mixed rig
(129 fixtures, seconds 5 to 15 of its timeline, load average 27.2): the tick
is 4.55 ms mean and 8.74 ms p95, and evaluation is 1.03 ms, 23% of it. One
colour field on 112 fixtures takes 0.88 ms of that, about 5 µs per fixture per
call. A shorter run over seconds 2 to 5 had evaluation at 6%. A small show can
have a large evaluation share and no deadline problem, so a share alone is not
a reason to build anything.

### Hidden writes

On one frame at 20,000 pixels the pixel batches write 141,667 typed values.
A later opaque replacement of the same attribute on the same pixel overwrites
61,667 of them (44%). Two white layers are hidden completely. Afterglow's
`coverage/plan` cannot skip them, because it only recognises a covering write
that addresses a prefix of the patch, and these layers address every second or
third pixel. The renderer also validates a hidden value where it was written,
so skipping evaluation would change which faults an effect can raise.

The 44% counts values, so a colour pixel counts three times, and cost per
pixel does not follow the component count. Weighted by the per-plan times of
the stage table instead:

- the two completely hidden white layers (10,000 and 5,000 pixels) take 2.0 of
  the 9.47 ms, **21% of evaluation**;
- skipping the hidden half of the colour field and of the 6,667-pixel white
  layer as well reaches 4.05 ms, **43%**. That needs coverage per pixel known
  before evaluation, and output that stays dense over the group's targets.

Both figures are models on this synthetic rig. Nothing was skipped in a run.
[#306](https://github.com/tolgraven/tolglow/issues/306) found no eligible
writes on the weighted venue.

## Prototype results

Ten thousand pixels as 100 rows of 100, 96 frames at 40 Hz and 120 BPM.
Chunks run along rows. The colour field is compared with the full-resolution
field.

- **Max codes** is the largest difference on any colour component after
  rounding both to 8 bits.
- **ΔE** is the distance in OKLab between the two linear RGB colours, taking
  sRGB primaries. About 0.02 is a just-noticeable difference for adjacent
  patches. LED primaries differ, so read it as a relative measure.
- **Shimmer** is the summed frame-to-frame change of the error divided by the
  summed frame-to-frame change of the reference. Zero means the error is
  static. One means the artefact changes as much as the picture does.
- **Speedup** is for field evaluation only. Merge and packing are unchanged.

### Methods

| Method | Samples | Reconstruction |
| --- | --- | --- |
| Nearest, centred | one head at each chunk's centre | every pixel takes its chunk's head |
| Linear, centred | the same heads | interpolate between neighbouring heads, hold the ends |
| Linear, nodes | real pixels at chunk boundaries and both ends of the run | interpolate between the two nodes |
| Cubic, nodes | the same nodes | Catmull-Rom over four nodes, clamped to [0,1] |
| Bilinear, nodes | a node grid in both matrix directions | four nodes |
| Adaptive | nodes plus each chunk's middle pixel | linear where the middle pixel lies within one code of the line between its nodes; otherwise evaluate every pixel of the chunk |
| Distinct coordinates | one sample per distinct tuple of the axes the fields read | copy (exact) |
| Certified constant | interval arithmetic over a chunk's bounding box | constant where every enclosure is narrower than one code; otherwise evaluate every pixel. Counted and checked against the reference, not timed and not run through the show |

### A smooth wash

`sine-wash`, amplitude 0.5, wavelength 125 pixels:

| Method | Spacing, px | Samples | Max codes | Pixels over 1 code | ΔE max | Shimmer | Speedup |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Nearest, centred | 4 | 2,500 | 10 | 78% | 0.008 | 0.052 | 2.6× |
| Nearest, centred | 8 | 1,300 | 23 | 86% | 0.018 | 0.103 | 4.0× |
| Linear, centred | 4 | 2,500 | 10 | 3% | 0.008 | 0.005 | 2.4× |
| Linear, centred | 8 | 1,300 | 14 | 36% | 0.010 | 0.019 | 3.3× |
| Linear, nodes | 2 | 5,100 | 1 | 0% | under 0.001 | 0.001 | 1.5× |
| Linear, nodes | 4 | 2,600 | 1 | 0% | 0.001 | 0.003 | 2.2× |
| Linear, nodes | 8 | 1,400 | 3 | 33% | 0.003 | 0.012 | 3.3× |
| Linear, nodes | 16 | 800 | 11 | 75% | 0.011 | 0.049 | 4.2× |
| Linear, nodes | 32 | 500 | 39 | 91% | 0.040 | 0.191 | 4.7× |
| Cubic, nodes | 8 | 1,400 | 3 | 6% | 0.002 | 0.003 | 2.6× |
| Bilinear, nodes | 8 | 196 | 3 | 33% | 0.003 | 0.012 | 4.0× |
| Adaptive | 8 | 2,700, then 55% of pixels | 1 | 0% | under 0.001 | 0.002 | 0.5× |

The speedup stops near 4–5 because expanding a field costs about 10 ns per
pixel however few samples there are, against 73 ns per pixel for the whole
wash in this run. The adaptive prototype is slower than full resolution here:
it keeps boxed per-chunk tables and re-evaluates more than half the pixels to
hold one code.

Linear interpolation between nodes on real pixels is the only lossy method
worth having. The error follows `A (k h)² / 8` for amplitude `A`, spatial
frequency `k` and node spacing `h`: predicted 0.6, 2.5 and 10.2 codes at 4, 8
and 16 pixels, measured 1, 3 and 11. Nearest is first order, `A k h / 2`, and
leaves a fixed staircase on the strip that the moving wash slides through.
Centred heads are worse than nodes at the same cost because the ends of each
run are held. Cubic costs more taps and can overshoot.

### Edges, saws, stops and shapes

| Family | Linear, nodes, 4 px | Linear, nodes, 8 px | Adaptive, 8 px | Certified constant, 8 px |
| --- | --- | --- | --- | --- |
| `plasma` | 2 codes, ΔE 0.012, shimmer 0.006 | 7 codes, ΔE 0.037 | 1 code, 68% of pixels re-evaluated | 0% of pixels certified |
| `soft-sphere` | 13 codes, ΔE 0.12, shimmer 0.09 | 36 codes, ΔE 0.24 | 6 codes, 8% re-evaluated | 89% certified, within 1 code |
| `palette-gradient` | 82 codes, ΔE 0.16, shimmer 0.22 | 95 codes, ΔE 0.18 | 1 code, 34% re-evaluated | not supported by the prototype |
| `saw-chase` | 153 codes, ΔE 0.26, shimmer 0.68 | 179 codes | 1 code, 12% re-evaluated | 0% |
| `cell-chase` | 191 codes, shimmer 0.42 | 249 codes | exact, 41% re-evaluated; **204 codes at 16 px** | 53%, 0 codes |
| `hard-sphere` | 153 codes, ΔE 0.52, shimmer 0.76 | 204 codes | **77 codes**, 3% re-evaluated | 97%, 0 codes |
| `small-sphere` (radius 1.5 px) | 204 codes, shimmer 0.97 | 204 codes | **204 codes**, 0.2% re-evaluated | 99.7%, 0 codes |
| `fine-ripple` | 254 codes | 251 codes | **157 codes** | 0% |

Bilinear interpolation on the matrix has the same errors as linear for effects
that read one axis, and larger ones for two-axis effects: `plasma` at 4 pixels
is 4 codes and ΔE 0.044, at 8 pixels 14 codes and ΔE 0.13.

For the expensive families the speedup follows the sample count: linear nodes
are 3.5 times faster at 4 pixels and 6.5 times at 8 for the sphere and the
gradient, and bilinear nodes 10.7 and 22 times. Those are the cases whose
errors rule them out.

What this shows:

- **A soft shape is not smooth enough.** A sphere with a 12-pixel smoothstep
  edge is 13 codes off with nodes 4 pixels apart. Its edge profile sharpens on
  the rows it grazes.
- **A gradient is not smooth.** The palette has a kink at every stop and a
  jump where it repeats. The mean error is under one code and the worst pixel
  is 82 codes off.
- **Fixed interpolation fails on every edge**, and the failure moves. A
  shimmer of 0.7 means the artefact changes almost as much as the effect does.
- **The midpoint test has no bound.** It repairs a sawtooth and a wide edge.
  It misses any feature narrower than half a chunk: the rows a hard sphere
  grazes, a 1.5-pixel sphere, 5-pixel cells under 16-pixel chunks, and a
  ripple that happens to agree at three samples.
- **Interval bounds catch what the midpoint test misses, as far as it was
  checked.** A chunk is constant only if the expression's enclosure over the
  chunk's bounding box says so. This is the precise form of "the nearest real
  pixel decides entry": a chunk is refined when the shape can touch any point
  of it. For compact shapes it certifies 89–99.7% of pixels at 8 pixels (84%
  of the soft sphere's at 32). For a wash it certifies nothing.
- **What "certified" was measured to mean.** The prototype never ran this
  through the show. The `lod` scope compares the reference with each chunk's
  enclosure on 12 of the 96 frames: 14.4 million sampled values over eight
  families and five chunk sizes, none outside its enclosure. Filling a
  certified chunk with its first pixel's value was at most 1 code off, and 0
  codes where the enclosure is a single value. At 8 pixels that is all of the
  hard and small spheres' certified pixels and 88.8 of the soft sphere's
  89.3%. So the bound is one code, not zero, and it holds
  only if the enclosure rules are sound. They were checked on these families
  only. The prototype has no rule for `:mix`, `:pow`, gradients or vector
  constants, and its `:clamp` rule needs single-valued bounds.

### The exact form: distinct coordinates

| Family | Axes read | Samples for 10,000 px | Max codes | Evaluation speedup |
| --- | --- | ---: | ---: | ---: |
| `sine-wash` | `x` | 100 | 0 | 8.4× |
| `saw-chase` | `x` | 100 | 0 | 9.2× |
| `cell-chase` | `x` | 100 | 0 | 10.5× |
| `fine-ripple` | `x` | 100 | 0 | 10.3× |
| `palette-gradient` | `x` | 100 | 0 | 39× |
| `plasma` | `x`, `z` | 10,000 | 0 | 0.9× |
| `soft-sphere` | `x`, `z` | 10,000 | 0 | 0.9× |

When several pixels share every coordinate an effect reads, they are one
chunk head with no error. This covers a matrix under any one-axis effect and
parallel strips under an effect that runs across them. It does nothing for an
effect along a single strip or for a radial shape, and there the bookkeeping
makes it 10% slower, so a plan must only engage when it removes samples.

### Through the real show

The mixed rig's tick with the prototype evaluator swapped in for batches of at
least 256 pixels that write only colour and white. One effect worker,
300 frames after 200 warm-up frames. "Differing bytes" compares all ArtDMX
payload and header bytes with the plain run:

| Pixels | Evaluator | Tick p50 / p95, ms | Evaluation, ms per frame | ArtDMX against the plain run |
| ---: | --- | --- | ---: | --- |
| 10,000 | plain | 11.13 / 18.37 | 4.20 | reference |
| 10,000 | distinct coordinates | 6.19 / 8.09 | 0.31 | identical |
| 10,000 | linear, nodes 8 px, every large batch | 7.66 / 15.80 | 1.23 | 12.7% of bytes differ, up to 148 codes |
| 10,000 | adaptive, 8 px, one code | 10.64 / 16.53 | 5.08 | 0.19% of bytes differ, by 1 code |
| 20,000 | plain | 22.07 / 33.79 | 11.80 | reference |
| 20,000 | distinct coordinates | 10.00 / 15.27 | 0.57 | identical |
| 20,000 | linear, nodes 8 px, every large batch | 10.64 / 14.41 | 2.34 | 12.7% of bytes differ, up to 148 codes |
| 20,000 | adaptive, 8 px, one code | 19.69 / 26.01 | 11.05 | 0.19% of bytes differ, by 1 code |

The load average was 37.8, so the plain tick is slower here than in the stage
table. "Evaluation" is all of `ir/evaluate-batch` for the plain run and
sampling plus expansion of the large batches for the prototypes. The small
plans add under 0.1 ms to each. A second run with the final harness, at load
average 66, gave the same byte comparisons and no effect fault. Distinct
coordinates took the tick from 29.8 to 10.1 ms p50 at 20,000 pixels and from
15.6 to 7.7 ms at 10,000.

The mixed rig is a favourable case for distinct coordinates: every pixel effect
reads `x` and the matrix has 100 columns. Linear interpolation without an
eligibility test breaks the sawtooth layers. The adaptive prototype allocates
boxed tables per frame, so its time shows the method's bookkeeping cost at this
level of care, not a floor.

## Design space

### Chunking

- **Fixed size along a detected run.** A run is a maximal sequence of targets
  in patch order with equal spacing on one line. The prototype's `runs`
  function finds matrix rows and every-nth subsets of them. It is a pure
  function of the group's ordered positions, so the plan is compiled once per
  group and rig.
- **By curvature.** For a strip on a tube or sphere, interpolating in the strip
  parameter adds a term `∇f · p''` to the error. Ending a chunk wherever the
  direction changes by more than a set angle bounds it. The mixed rig has no
  curved strips, so this was not measured.
- **By effect frequency.** The node spacing for a tolerance `ε` on a wave of
  amplitude `A` and spatial frequency `k` is `h ≤ sqrt(8 ε / A) / k`. That needs
  `k`, which is either declared by the author or derived from the expression.
  The expression set is closed, so a derivative bound can be computed with
  interval arithmetic. `afterglow.render.chain` already computes value ranges
  of expressions over a group's coordinate ranges for GPU fusion.
- **By coordinate class.** The exact case above. Chunks are sets of pixels, not
  runs.

### Interpolation

Linear between nodes on real pixels, for the reasons in the wash table. Nearest
is the wrong default. Bilinear is only worth it on a matrix under a two-axis
effect, where it cuts samples by the square of the spacing and roughly doubles
the error. Splines overshoot the [0,1] range that `finish-fields` and the
renderer enforce.

### Where it engages

| Option | Finding |
| --- | --- |
| Per effect, declared by the author | Simple. The prototype's `:smooth?` flag was wrong for the palette gradient and the soft sphere, both written in good faith. A declaration needs a check. |
| Per effect, derived | Exclude any field containing `floor`, `fract`, `abs`, `min`, `max`, `clamp` or a gradient, unless an interval bound proves the chunk smooth. On the mixed rig that leaves the pixel field: 29% of evaluation. |
| Per layer, by position in the chain | Lower layers do matter less, by a known factor: a later replace at opacity `α` scales an error below it by `1 − α`, and an opaque one removes it. Additive and maximum layers never hide anything. But the useful case is the opaque one, and there skipping beats coarsening. |
| By occlusion from later opaque layers | 44% of the mixed rig's typed pixel writes are hidden: a modelled 21% of evaluation as whole layers, 43% with per-pixel coverage. Skipping them is exact in the packed frame. It needs coverage of arbitrary rows, known before evaluation, and a decision on hidden faults. See [#306](https://github.com/tolgraven/tolglow/issues/306), where a merge-stage prototype found no eligible writes in the weighted venue. |

The engine does know the chain before it evaluates: targets, blend and system
opacity are data on the instance and the compiled group. Spatial cue weights
are the exception, since they come from sampling. The deferred backend already
separates deciding from evaluating: a step returns a `:field-request` and
`fields/frame-evaluator` evaluates the ordered requests at the frame owner.
A CPU evaluator at that boundary is where a chain-aware plan would live. It
would move evaluation off the effect workers, so it needs its own measurement.

### Geometry hit tests

Three cases behave differently.

- **Volume cues** (`tolglow.spatial.cues`). Afterglow already rejects pixels
  outside the shape's expanded bounding box through a sorted position index.
  One moving sphere over 10,000 pixels takes 0.14 ms. A chunk test adds
  nothing there. The host cost of those cues is in weights keyed by fixture ID
  and in mask composition, which chunking does not touch.
- **Shapes written as expressions.** No culling exists. Every pixel pays the
  full expression. An interval test per chunk is conservative when its
  enclosure rules are sound, and it is the correct reading of the operator's
  rule about the nearest real pixel. It was checked against sampled values
  here, within one code, and not run through the show. A centre test misses a shape that passes between centres.
- **Entry and exit.** With bounds there is no separate rule. A chunk is refined
  while the shape's support intersects its bounding box, which starts when the
  nearest pixel is reached and ends when the farthest is left.

### Blending

Linear interpolation commutes with replace at uniform opacity and with add, so
those layers could be composed on chunk heads and expanded once. It does not
commute with maximum, with per-row mask opacities, or with the range checks.
An interpolated value with non-negative weights stays inside the range of its
samples, so it cannot create an out-of-range fault. It can hide one at a pixel
that was not sampled.

### Integration

- **Batch compiler.** The narrow integration point is `output-groups` in
  `tolglow.compiler.batch`: the group's `evaluate` closure evaluates the sample
  positions and expands to a dense array for the group's own `:target-ids`.
  Nothing downstream changes.
- **Target plans (afterglow#167).** The renderer memoizes target indices by the
  identity of the target-ID vector. A design that emits a different subset of
  pixels each frame would allocate a new vector each frame and defeat the memo.
  Results must stay dense over the compiled vector, or use per-row opacities.
- **Shared read-only outputs (afterglow#167).** Expanded arrays are fresh per
  evaluation and never written again, so the contract holds. One limit was
  found on the way: `reuse/evaluator` charges array elements against its
  65,536-element bound even when outputs are shared. A colour and white group
  stops being retained at 16,384 pixels, so a stopped transport re-evaluates it
  every frame ([#875](https://github.com/tolgraven/tolglow/issues/875)).
- **Uniform fields.** Afterglow has `UniformField` for a field that is one
  tuple. A chunked field the renderer expands while merging would be the
  analogue. It would save one array and one pass per field, and it changes the
  physical column reader, which has its own test obligations.
- **GPU paths.** The native backend evaluates every position in one
  transform-feedback draw. [#305](https://github.com/tolgraven/tolglow/issues/305)
  records 0.09 ms of device time for 20,000 positions. Evaluation is then not
  the cost, and fused chains need identical coordinates across stages. Spatial
  LOD should be off whenever the GPU compute backend is active.
- **Mixed precision ([#540](https://github.com/tolgraven/tolglow/issues/540)).**
  Independent. Narrower buffers reduce memory traffic in merge and packing,
  which LOD does not touch. A one-code tolerance is natural for 8-bit emitters
  and almost never satisfiable for 16-bit dimmers.
- **Temporal keyframes.** Afterglow already interpolates predictable effects
  in time under a governor (`[:settings :adaptive-keyframes]`). Space and time
  interpolation are both linear, so their errors add. A governor should widen
  one of them at a time.

## Error, perception and fallback

- **Colour.** Compare in OKLab, not in codes. LED drive is linear, so one code
  near black is a large step and one code near full is barely visible. A bound of one
  8-bit code is still the practical floor, because the output is quantised to
  it anyway.
- **Shimmer.** A static error is a slightly different picture. A moving error
  is flicker. Nearest sampling leaves a comb fixed to the strip. Interpolating
  across an edge turns it into a ramp that jumps from chunk to chunk. The
  shimmer ratio separates the two: 0.012 or less for the washes that stayed
  within three codes, 0.09 or more for every shape and edge at 4 pixels.
- **Edges.** LEDs are discrete, so an edge one pixel out of place is one wrong
  LED. An edge needs exact evaluation near it.
- **Strobe and phase.** Spatial LOD does not resample time. A uniform strobe is
  one value per frame and is never chunked. A strobe or chase whose phase
  varies along the strip is a spatial step function. Nearest sampling makes
  whole chunks flash together, up to half a chunk early or late. The mixed
  rig's overlay moves 1.25 pixels per frame, so with 8-pixel chunks that is
  ±3 frames, ±80 ms, about ±0.16 beat at 120 BPM. Linear
  sampling makes grey ramps where the effect has on and off. Discrete
  attributes (strobe, shutter, function ranges) and geometry (aim, direction,
  pan, tilt) must never be interpolated in space.
- **Bounding the error.** Four mechanisms, in order of strength:
  1. exactness by construction (distinct coordinates, hoisting, shared
     subexpressions);
  2. an interval enclosure of the value over a chunk: within the tolerance if
     the enclosure rules are sound, measured at one code or less here, counted
     and checked but not reconstructed in a show;
  3. an interval bound on the second derivative over a chunk, which gives
     `|f''| h² / 8` (not prototyped);
  4. the midpoint test, which is a heuristic and misses narrow features.
- **Fallback.** Per group and per frame: evaluate every pixel when the bound
  is not met, when a parameter or rig change invalidates the plan, or when the
  operator turns the tier off.

## Determinism and parity

- A chunk plan is a pure function of the ordered positions, the axes read and
  the chunk setting. Evaluation and expansion are pure and single-threaded per
  group. The adaptive variant depends on the data, and the same inputs still
  give the same refinement.
- **Exact tier.** Distinct coordinates, hoisting and shared subexpressions
  perform the same floating-point operations on the same operands. The scalar
  oracle in `bin/profile-mixed-rig` already checks every byte and needs no
  tolerance. The `e2e` scope confirmed byte-identical output for distinct
  coordinates at 10,000 and 20,000 pixels.
- **Lossy tier.** It needs a tolerance oracle: the full-resolution scalar run
  as reference, a declared bound in normalized units per emission attribute,
  comparison of the decoded channel value (so a 16-bit pair is compared as one
  number, not two bytes), and exact equality for every discrete and geometric
  channel. The `e2e` scope's byte comparison is the starting point.
- **Operator control.** A show setting beside `:adaptive-keyframes` with
  `:off` (default) and a ceiling on spacing and tolerance, a per-effect opt-out,
  the same command from the REPL and Studio, and a readout of which groups ran
  coarse and their measured error.

## Expected gains and alternatives

For the mixed rig at 20,000 pixels on one worker. Each row names its source.
"Through the show" is the table above at load 37.8, where the plain run has
11.8 ms of evaluation in a 22.1 ms tick. "Stage table" is the calmer run at
load 12.2, with 9.48 ms in a 17.9 ms tick. Model rows are shares of the stage
table and were not run.

| Change | Kind | Source | Evaluation | Tick |
| --- | --- | --- | --- | --- |
| Distinct coordinates | exact | measured through the show | 11.8 to 0.57 ms | 22.1 to 10.0 ms p50; second run at load 66: 29.8 to 10.1 ms |
| Linear nodes at 8 px on every large batch | lossy, **not acceptable**: errors up to 148 codes | measured through the show | 11.8 to 2.3 ms | 22.1 to 10.6 ms p50 |
| Linear nodes at 4 px on the one smooth layer | lossy, one code | model: wash table speedup on that layer's stage-table share | 29% of evaluation, 2.2 times faster: about 16% less | about 8% less |
| Skip whole layers hidden by later opaque layers | exact in the frame, changes fault semantics | model: hidden count and stage-table plan times | about 21% less | about 11% less |
| Skip hidden pixels inside partly hidden batches as well | the same, plus per-pixel coverage before evaluation | model: hidden count and stage-table plan times | about 43% less | about 23% less |
| Shared subexpressions and hoisting | exact | model: node counts | little here; about 8 times less for a sphere | little here |
| Flat positions | exact | not measured; JFR has 13–15% of all CPU samples in position loading | not measured | not measured |
| Native GPU compute | float32, existing fallback rules | #305: device time 0.09 ms per 20,000 positions | not measured here | not measured here |
| Mixed precision (#540) | exact where qualified | stage table | none | acts on merge and packing, 31% of the tick |

When interpolated LOD would be worth building:

- the show runs on the CPU path at more than about 20,000 pixels;
- the exact changes above are in and evaluation is still the largest stage;
- the expensive effects are smooth radial or two-axis fields on long single
  strips or large matrices, where distinct coordinates do not help.

At that point the gain is bounded by the spacing a one-code tolerance allows.
For a wash of wavelength 125 pixels that is 4 pixels, about 2.2 times on
evaluation. After the exact work, evaluation should be a small share of a tick
dominated by packing, so the remaining gain on the tick is small.

## Recommendation

1. **Do not build chunk heads with interpolation now.** The measured cases
   where it is accurate are the cases the exact changes make cheap.
2. **Build the exact changes**, in this order:
   - [#872](https://github.com/tolgraven/tolglow/issues/872): evaluate each shared subexpression once per pixel, and
     frame-constant subtrees and components once per frame (Afterglow);
   - [#873](https://github.com/tolgraven/tolglow/issues/873): flat fixture coordinates from the batch compiler (Tolglow);
   - [#874](https://github.com/tolgraven/tolglow/issues/874): one evaluation per distinct coordinate read (Tolglow);
   - [#875](https://github.com/tolgraven/tolglow/issues/875): retain shared outputs above 65,536 elements (Afterglow).
3. **Keep the interpolated tier specified, gated and off**: [#876](https://github.com/tolgraven/tolglow/issues/876).
   Linear interpolation between nodes on real pixels along detected runs, only
   for emission fields that pass a derived smoothness test, with a tolerance
   oracle, default off.
4. **Treat "layer-aware" as skipping, under #306.** It needs an operator
   decision on hidden faults first, and its gain is unproven outside this
   synthetic rig.
5. **Do not pursue**: nearest chunk heads, centred heads, cubic
   reconstruction, the midpoint test as a guarantee, centre-based hit tests,
   chunking for volume cues, or any spatial LOD on the GPU path.

## Decisions for the operator

None of these blocks
[#872](https://github.com/tolgraven/tolglow/issues/872),
[#873](https://github.com/tolgraven/tolglow/issues/873),
[#874](https://github.com/tolgraven/tolglow/issues/874) or
[#875](https://github.com/tolgraven/tolglow/issues/875).

1. **Hidden faults. Blocks: skipping hidden writes, under #306.** May an
   effect whose output is completely covered by a later opaque layer skip
   evaluation, so that an out-of-range value it would have produced is not
   reported that frame? Today the renderer validates it. A yes is a
   prerequisite for skipping, not a gain by itself. On the synthetic mixed rig
   skipping is a modelled 21% of evaluation for whole hidden layers and 43%
   with per-pixel coverage. #306 found no eligible writes on the weighted
   venue.
2. **Lossy tier at all. Blocks: [#876](https://github.com/tolgraven/tolglow/issues/876) only.** Is a bounded error of one 8-bit
   code on emission channels acceptable when the operator turns the tier on,
   or must output always be byte-exact on the CPU path?
3. **Engagement. Blocks: [#876](https://github.com/tolgraven/tolglow/issues/876) only.** The proposal is by hand only. The
   operator turns the tier on, the chunk plans compile off the frame thread
   and the show adopts them when they are ready. Nothing reacts to a slow
   frame. That follows the standing rule that costly work runs off-thread and
   is adopted when ready, with no in-frame budgets. The alternative is a
   governor that turns the tier on after a run of slow frames. That is an
   in-frame budget by another name, and it is not recommended. Confirm "by
   hand only", or say that a governor may change the setting.

## Reproduce

```sh
export JAVA_HOME=/Library/Java/JavaVirtualMachines/microsoft-25.jdk/Contents/Home
export AFTERGLOW_ROOT=../afterglow   # ef539be0

bin/profile-mixed-rig --pixels 1000,10000,20000 --workers 2 \
  --jfr target/led-lod/jfr/mixed --output target/led-lod/mixed-rig-host.edn

run() { lein with-profile +benchmark run -m tolglow.perf.led-lod "$@"; }
run --scope stages --pixels 1000,10000,20000,40000 --output target/led-lod/stages.edn
run --scope families --pixels 10000 --warmup 300 --samples 300 --output target/led-lod/families.edn
run --scope lod --pixels 10000 --warmup 60 --samples 96 --output target/led-lod/lod.edn
run --scope e2e --pixels 10000,20000 --warmup 200 --samples 300 --output target/led-lod/e2e.edn
run --scope hidden --pixels 20000 --output target/led-lod/hidden.edn
run --scope nodes --pixels 10000 --output target/led-lod/nodes.edn
run --scope stages --show tolglow.showcase/model --timeline tolglow.showcase/play! \
  --output target/led-lod/stages-showcase.edn
```

`--families a,b` limits the `lod` scope, `--chunks 2,4,8` sets the spacings,
`--tolerance-codes 1` sets the adaptive and interval tolerance,
`--e2e-chunk 8` the spacing in the `e2e` scope and `--timing false` skips the
paired timing in the `lod` scope. `--show` takes a qualified symbol for a
function that returns a show model, and `--timeline` one for a function of the
runtime that starts its programme. The full `lod` scope takes several minutes.
The reports record both source revisions, the working-tree state, JVM
arguments and the load average. The reports behind the tables are in
[`reports.zip`](../benchmarks/led-spatial-lod/reports.zip), with the worktree
path replaced by `.`.

### Provenance

The timing tables come from the first group of reports. They were produced
while the harness was still an uncommitted file on Tolglow `bce22156`, and the
harness changed between them (paired timing, the fault check, the end-to-end
and hidden scopes). Each records `?? dev/tolglow/perf/led_lod.clj` as its
working tree; the two `-confirm` reports ran the harness as first committed
and record it staged. `bce22156` predates #814 and pins Afterglow `8d969a15`.
These runs set `AFTERGLOW_ROOT` to `ef539be0`, the revision master pins from
`1bc5ac78` on, so the pair was an override at the time.

The second group ran on the committed harness with a clean working tree, on
this branch after merging master `c180a49c`. The `lod` scope was re-run there
with `--timing false`. All 1,998 error, shimmer and refinement figures of the
earlier `lod` reports came out identical, and the enclosure check and the
exactly-constant fractions come from these runs. The stage, family and
end-to-end timings were not re-run at head.

| Report, first group | Load average | Used for |
| --- | ---: | --- |
| `mixed-rig-host.edn` | 15.9 | two-worker host table |
| `stages.edn`, `stages-confirm.edn` | 12.2, 19.5 | stage table and its second run |
| `families.edn` | 19.9 | second column of the family and control tables |
| `families-earlier.edn` | 23.1 | an extra run, not tabulated |
| `lod-light-families.edn` | 21.2 | wash, saw, cell, ripple and plasma rows, paired timing |
| `lod-palette-soft-sphere.edn` | 22.0 | gradient and soft sphere rows, paired timing |
| `lod-hard-small-sphere-unpaired-timing.edn` | 42.3 | hard and small sphere error rows only; its speedups predate paired timing |
| `e2e.edn`, `e2e-confirm.edn` | 37.8, 65.8 | through-the-show table and its second run |
| `hidden.edn` | not timed | hidden writes |

| Report, second group | Revision | Load average | Used for |
| --- | --- | ---: | --- |
| `lod-head-light.edn`, `lod-head-heavy-a.edn`, `lod-head-heavy-b.edn` | `f544f023` | 52.0, 32.6, 17.4 | error figures reproduced; certified and exactly-constant fractions; enclosure check |
| `stages-showcase.edn` | `f544f023` | 27.2 | the named-show split |
| `nodes.edn`, `hidden-head.edn` | `31f874eb` | not timed | node counts; the hidden count again, identical |

The JFR recordings are not retained. The `stages`, `e2e` and `hidden` scopes
stop with an error if any effect faults during the run.

Not measured: curved strips, real fixtures or a real venue, the two-worker tick
with any prototype, Studio or browser cost, GPU compute on this rig, and any
viewer's judgement of the artefacts. The perceptual thresholds are from the
literature, not from a panel.
