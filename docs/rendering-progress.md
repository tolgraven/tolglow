# Functional rendering integration and measured progress

The new studio uses the batch compiler and mixed contribution renderer by
default (`:batch? true`). The same path is callable from the REPL. Effects still
receive explicit frame/instance/state context; threads, caches, publication and
transport remain owned boundaries. Ordinary contributions and exact numeric expressions remain supported.
Geometry fields can travel alongside colors and focus in typed batches.
This is the new model/engine pipeline; it does not run old effect closures.

A show can optionally evaluate supported expression groups on one shared native
GPU session. [Configuration, deadlines and fallback](native-shader-show.md)

## Implemented path

- `tolglow.compiler/compile-plan` supplies shared validation and prepared output
  expressions to both compilers. `tolglow.compiler.batch/compiler-for-rig`
  captures a validated rig and returns `definition -> step`. The studio caches
  those steps by definition and rebuilds its factory/cache when the rig changes.
- Compiled numeric outputs run through `afterglow.render.ir/evaluate-batch` into
  owned named float64 fields. Output selectors intersect definition selectors by
  stable fixture ID. Consecutive compatible outputs share a batch; repeated
  attributes form separate ordered layers, preserving replace/add/max semantics.
- Response middleware still transforms only the frame's instance parameter copy.
  Envelopes, captured musical meter, release, duration and hot reload are exercised
  through the real control model and engine. No canonical parameters are changed
  by response evaluation.
- The runtime merges whole effect streams by priority, launch order and original
  position, independent of worker completion. The engine returns a canonical
  `:contributions` vector. When it contains any batch, aggregate `:assignments`
  is **nil** so a consumer cannot silently render only its ordinary part.
- `render/resolve-contributions` reduces mixed blocks into one target state and
  packs once. Scalar/color transport frames use dense private scratch. Geometry,
  exact numeric ordinary assignments, heterogeneous patches and requested rich
  values use the general path in the same contribution order.
- ArtDMX encoding copies an owned byte-array payload with `System/arraycopy`.
  Numeric collection inputs retain per-element signed/unsigned validation. The
  returned packet never aliases its input. Packet headers, 15-bit addressing,
  sequence values and real localhost sequencing have independent tests.

A step context always has a vector `:assignments`. New middleware adds blocks
with `runtime/append-contributions`; that helper first flushes any ordinary
assignments into an ordered block. Ordinary middleware may then append new
assignments, which become the tail. The two block forms are:

```clojure
{:kind :assignments :assignments [{:target-id :head-1
                                  :attribute :dimmer :value 0.5 :blend :replace}]}
{:kind :batch :batch {:target-ids [:head-1]
                     :count 1 :blend :replace
                     :outputs {:dimmer {:type :number :components 1
                                        :values (double-array [0.5])}}}}
```

Arrays belong exclusively to the producing invocation until it returns, then
become read-only inputs to reduction/publication. Workers never share writable
output arrays. The compiler, kernel and renderer contain no global clock,
executor, RNG or mutable cache.

## Numeric and feature fallbacks

The original scalar compiler is the CPU behavior reference. Automatic batching
is conservative because its primitive operations use float64, while Clojure can
preserve ratios, decimals and integer intermediates.

- Tween definitions select ordinary compilation. Their
  function metadata reports `:backend :ordinary` and `:fallback-reasons`.
- Exact literals, or nonfloating literals/positions participating in arithmetic,
  select ordinary compilation. Ratios, BigDecimals and integers outside the
  exactly representable float64 range also select ordinary compilation.
- Exact parameters, nonfloating parameters used by arithmetic, or integer clock
  inputs select the scalar step for that frame. Only one effect evaluation runs.
  Typical floating point lighting expressions continue through typed batches.
- Unsupported expression/query nodes fail shared authoring validation; they do
  not become arbitrary code or an unimplemented silent query backend.
- `{:enabled? false}` on the compiler factory and `:batch? false` on the studio
  explicitly select scalar compilation. Rich renderer results remain the public
  default when `:include-values? false` is not requested.

Tolglow compiles declarative aim points and directions into the same typed
contributions as color and focus. Coordinates can be negative or greater than
one. Afterglow resolves the final geometry after blending and packs the physical
pan and tilt. The default evaluator uses float64 on the CPU.

## Preview, movement history and saving

`{:include-values? false :compact-values? true}` returns `:values nil` and an owned
`:compact` value containing `:count`, full `:target-order`, and named fields with
`:type`, `:components`, `:values` double arrays and `:valid` boolean masks. Color,
intensity, pan/tilt, focus/zoom/strobe, aim and direction have distinct fields.

`render/project-compact` turns selected stable indices into ordinary maps later.
It does not evaluate effects or repack output. The studio publishes a bounded
16-head projection while retaining the entire output and rig internally. A
caller can project another visible selection or publish less frequently.

The next render can pass `:previous-compact` plus
`:previous-values (or rich-values {})`. Geometry reads only the required pan/tilt
history from typed fields. If the rig is reordered, history remaps by stable ID;
there is no full preview-map reconstruction each frame.

`render/portable-frame` is a save-boundary operation: it projects **all** compact
targets, merges any richer values, converts DMX byte arrays to unsigned vectors,
and returns `:values`, `:target-order`, and `:universes`. It retains movement
history for targets outside the visible preview. Saving does not rerun effects;
loading does not need to deserialize mutable Java arrays as show data.

## Physical patch ownership

An optional fixture `:footprint` is a positive integer covering every mapped
coarse and fine offset. It reserves the entire physical span, including sparse
unmapped channels and crossings into later universes. Without an explicit
footprint, the previous mapped-channel ownership rules remain unchanged.
Validation uses intervals, so a sparse long reservation does not allocate an
entry for every physical byte. Invalid spans, overlapping reservations and
mapped offsets beyond the declared footprint fail before publication.

## Reproduce the pipeline benchmark

From the Tolglow checkout with the paired Afterglow checkout available:

```sh
clojure -J-Xmx768m -Sdeps '{:paths ["src" "resources" "dev"]}' \
  -M -m tolglow.bench-pipeline \
  --heads 1000,10000 --warmup 60 --samples 200
```

Setup uses real commands to publish three definitions, place cues and launch
instances. Two effects use typed numeric output; a tween remains ordinary and
targets 16 heads. Each measured replay includes cached compilation lookup,
response middleware, musical transport/envelope sampling, the real engine,
ordered mixed reduction, RGBW/DMX conversion, compact preview, a 16-head ordinary
projection, actual ArtDMX encoding and an injected capture sink. Every model,
intent and packet byte is checked against the scalar compiler pipeline outside
the timed interval. Published preview values use absolute tolerance `1e-9`.
Allocations include the owner and warmed runtime worker threads.

The following 10,000-head results use Java 21, Clojure 1.11.4, a 768 MiB heap,
60 warmups and 200 measured samples in this workspace. The output contains 79
universes and 41,870 encoded packet bytes per frame.

| Measured path | p50 ms | p95 ms | p99 ms | Maximum ms | Median allocation MB/frame |
| --- | ---: | ---: | ---: | ---: | ---: |
| Scalar compiler, packet copy | 88.07 | 142.15 | 159.58 | 168.93 | 119.71 |
| Batch, serial, packet copy | 13.57 | 18.46 | 25.54 | 30.59 | 11.89 |
| Batch, two workers, packet copy | 11.95 | 15.90 | 18.66 | 26.80 | 11.89 |
| Batch, two workers, prior packet loop | 16.06 | 19.55 | 21.72 | 22.09 | 20.20 |

Raw reports are retained as [pipeline before packet copy](benchmarks/2026-11-21-runtime-pipeline.edn)
and [pipeline with packet copy](benchmarks/2026-11-21-runtime-pipeline-byte-copy.edn).
The prior report also contains its scalar and serial results. Timing differences
between runs include host/JIT/GC variation; the lower maximum in the prior run
has not been hidden. The packet change materially reduces measured allocations.

The two-worker measured path fits the 25 ms budget at p99 in these runs, while a
maximum still exceeded it. This is not a guaranteed 40 Hz complete-show result.
The benchmark replays the same captured frame. It excludes command admission,
clock advancement, auto/sequence planning, hot reload compilation, the full
studio tick lock/queue, HTTP serialization, browser/simulator contention, network
and physical devices. The real studio is wired to these APIs, but its complete
steady-state and stressed workloads require separate measurement.

Earlier isolated rendering and GPU reports remain in the
[benchmark index](benchmarks/README.md). They measure different workloads and
must not be used as direct speedup comparisons with this three-effect pipeline.
The WebGL experiment ran on software SwiftShader; it provides no hardware GPU
acceleration evidence.

## Verification

Focused timing, runtime, engine, IR and frame suites passed 66 tests / 534 assertions.
The compiler batch integration suite passed 5 tests / 48 assertions. ArtDMX output
passed 4 tests / 30 assertions, including an owned localhost sender/receiver
closed after the test. These cover mixed ordering, reversed worker completion,
responses, release, hot reload, exact-number fallback, byte parity, compact
history across rig reordering, portable save data, fractional quarter-note meters
(including a 7/8 bar as 3.5 beats), footprint collisions, packet
layout, buffer ownership and transport sequence behavior.

Physical Art-Net destinations, fixtures, moving-head calibration, hardware GPU,
iPad Safari performance and longer-running full-show deadlines remain separate
evidence gates. Tests and benchmarks here did not drive physical lighting.
