# Renderer work elimination

The baseline is the paired full show, including candidate selection, effect calculation, native blending, fixture packing and output delivery. Keep a scalar full-evaluation path as the oracle. The [compound benchmark](compound-layers.md) already has an opaque-group fast path. It removes group compositing overhead for a particular case. The next experiment asks whether the lower effect kernel ever has to execute.

| Experiment | What must hold | What to measure |
| --- | --- | --- |
| Backward coverage over ordered layers | Traverse priority and launch order in reverse. Track coverage by fixture/head **and native field**. A replace at effective opacity one can satisfy that field. Other reads or blends keep the corresponding input live. Include group and master transforms at the correct stage. | Skipped kernel calls, surviving fields and heads, planning cost, allocation, universe byte equality. Sweep upper opacity through 0, 0.5 and 1. Remove or reorder the upper layer while running. |
| Spatial broad phase | Cache stable rig transforms and conservative bounds by rig revision. Intersect candidate bitsets for patch groups, spatial cells, field types and reachable cones before exact tests. Unknown/moving bounds must remain conservative. | Dense and sparse occupancy, static and moving fields, rebuild time and false positives. A single false-negative head invalidates the fast path. |
| Fused GPU path | Lower eligible pure expressions into a versioned typed program with shared subexpressions, batch instances and cache programs by structure. Keep changing params in buffers/uniforms. Protect last working program across compile errors and show reloads. | Program compile/link cold and warm, queue depth, uploads, readback/fences, DMA to DMX packing, fallback frames, peak GPU memory and 99th-percentile show latency. |
| Data layout and scheduling | Compare tiled structure-of-arrays, SIMD/vectorized CPU, clustered selection, incremental dirty regions and amortized transfer. Include candidate push-down before heavy geometry and an all-active worst case. | Branch/cache cost, frame budget under true 40 Hz output, dropped deadlines and total energy on relevant hardware. |

A fully covered pure contribution can be skipped. A state machine, event emitter, random stream, fault reporter, selected trace, or native operation that reads the prior value can still be observable. Preserve lifecycle and inspectable state or split state advancement from sample generation. Sample newly exposed instances at the correct musical time when coverage disappears. A dimmer write cannot independently prove that pan, shutter or every other channel is irrelevant.

Start with 64 moving heads and mixed strips at 4k and 20k pixels. Use identical seeds, scene changes and music clock. Include total blanking, dense overlap, partial coverage, release envelopes, additive, multiplicative and native-space blends. Compare full output buffers and events frame-by-frame, keep exceptions and quality errors visible, then compare timings. Report negative results with the same workload. [Paired host baselines](dense-venue.md) · [Native shader execution](../native-shader-show.md)

## What the dense rig showed

On November 23, 2026 we replayed a rig with 20,000 RGBW pixels on non-collinear strips, 64 mounted moving heads, four moving optical fields and a material reading both surface UV and world Z. Half the fixtures supplied explicit UVs. The hosts advanced the same cues and musical clock in alternating order, using two show workers and the independent output scheduler. Every frame compared the full model, intents and all packet bytes across 158 ArtDMX universes. The host's selected preview rows used an absolute tolerance of 1e-9. All timings below come from a shared Linux machine. A fresh JVM, software OpenGL and simulated output are identified in the replay instructions. [Raw frame pairs, checksums and replay scripts](../benchmarks/master-performance/render-work-elimination.zip)

| Experiment | Observed cost on that workload | Decision |
| --- | --- | --- |
| Share the committed optical emitter observations across all four fields | Repeat of 30 warmup plus 40 measured pairs. Median coordinator and show-worker CPU fell from 387 to 271 ms and allocation from 129.6 to 108.9 MB. Host p95 fell from 2,157 to 1,747 ms. The coordinator spent 1,676 and 1,034 ms respectively runnable in those two frames, with zero overlapping GC pause in either. | Keep the per-frame observation cache. The earlier isolated p95 spike lacked a scheduler trace and its cause is still unknown. [Afterglow #70](https://github.com/tolgraven/afterglow/pull/70) |
| Reject an optical contact when its largest possible contribution cannot exceed the known positional weight | 15,129 fewer radial contact tests per observed frame. Allocation fell from 112.7 to 104.3 MB. Across 24 measured pairs, median CPU rose from 310 to 314 ms and median elapsed rose from 774 to 793 ms. | Retain the experiment on its branch. The observed allocation saving did not translate into lower CPU on this rig. [Experimental implementation](https://github.com/tolgraven/afterglow/commit/812481f1e80bcb1d119254d77ddc2185b77923b5) |
| Elide physical column writes shadowed by later opaque replacements | With eight concurrent washes, 581,856 of 1,003,456 components were covered in one instrumented frame. Across 24 measured pairs, median CPU fell from 305 to 268 ms, allocation from 139.2 to 132.1 MB and host p95 from 1,495 to 915 ms. Removing the washes yielded zero covered components out of 361,408, essentially flat allocation and host p95 of 1,504 → 2,150 ms. | Keep the contiguous-prefix pass after composition. Covered values are still validated. Masks, fractional alpha, transforms, additive/maximum blends, permutations and ordinary assignments retain their established paths. Effect evaluation and fault attribution still run. [Afterglow #73](https://github.com/tolgraven/afterglow/pull/73) |
| Broadcast a native group whose expressions read only clocks, parameters or constants | Across 24 measured pairs, median elapsed fell from 499 to 456 ms and p95 from 1,199 to 755 ms. Median coordinator and show-worker CPU rose from 202 to 216 ms. One observed frame required 40,078 vertices and 400,468 readback bytes, down from 300,960 vertices and 4,013,824 bytes. Both submitted 16 draws. | Keep one native vertex per uniform group. A position or UV input retains all vertices. A 20,000-head isolated RGB/focus group read 16 bytes and took 5.45 ms median. Its full-vertex reference read 320,000 bytes and took 7.99 ms. The varying-UV control read 400,000 bytes in both paths. All returned arrays matched exactly. [Afterglow #74](https://github.com/tolgraven/afterglow/pull/74) |

The native host replay allowed a two-second compute deadline to measure both implementations actually executing. Mesa llvmpipe completed 688 native requests per lane, including one untimed instrumentation frame. With the normal 12 ms budget for a 40 Hz host, all 128 requests per lane fell back to CPU. No frame met the 25 ms output deadline. The host's sampled thread counters exclude driver CPU. Native buffer byte counts describe requested OpenGL transfers. This workspace has no physical-GPU timing or PCIe measurement.

A separate six-frame scalar-to-native comparison found eight DMX slot differences in total, each one byte step. The maximum difference in the selected emitted-color components was 4.94e-9. At the 12 ms deadline that replay used CPU fallback on all 96 requests and matched the scalar packets and preview. Preserve both the exact same-backend oracle and a cross-backend precision check when promoting changes.

### Independent physical output

Each physical completion passes from a renderer-owned byte array to an immutable packet and then to transport-owned buffers. Sampling the independent clock on a 20,000-pixel sculpture found boxed doubles in the validation of signed and unsigned DMX values. Integer comparisons and a direct primitive store reduced allocation during publication. Both runs in each pair compared their entire output with a third show running the scalar renderer. Each measurement used 64 moving heads, 18 warmup frames, 32 timed frames, two workers and a manually stepped physical clock.

| Pixels | Universes | Allocated per frame, p50 | Publication p95 | Whole-show p95 | Frames over 25 ms |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 10,000 | 80 | 14.56 → 7.21 MB | 5.72 → 1.73 ms | 38.61 → 29.22 ms | 6 → 2 |
| 20,000 | 158 | 27.10 → 12.59 MB | 34.50 → 10.20 ms | 96.45 → 98.13 ms | 32 → 23 |

Every ArtDMX byte matched in all 50 frames at each size. The independent packet validation also runs on edited packets at admission or conversion to transport buffers. The slowest whole-show frames include work on the shared host outside publication. [Afterglow #81 and captured traces](https://github.com/tolgraven/afterglow/pull/81)

#### Clojure 1.12 at the JVM array boundary

The effect model and physical packet retain Clojure values. The renderer uses locally owned primitive arrays to blend fields and encode DMX. Keep coercion, bounds checks and writes together in a small packing function. Callers should be able to see whether they receive an immutable packet or a transport-owned buffer. The Clojure style guide makes room for primitive arrays on performance-sensitive paths. [Clojure style guide](https://github.com/bbatsov/clojure-style-guide/blob/37e55f657d75d7e4da26523cfe5672e5ee97c02d/README.adoc#avoid-java-arrays)

| Boundary on Afterglow `9b8762f` | Applicable Clojure idiom | How to assess a change |
| --- | --- | --- |
| Art-Net and OLA payloads, GPU positions and compact pan/tilt history | `(bytes? payload)` identifies a primitive byte array. For other component types, Clojure 1.12 accepts `(instance? double/1 values)` and `(instance? boolean/1 valid)`. The same symbols can mark function arguments. Existing `^doubles` and `^bytes` hints are valid. | Exercise arrays, vectors, empty and invalid inputs. Several namespace-level class constants allocate a zero-length array once on load. Moving those checks to a literal is primarily a readability change. [GPU position inputs](https://github.com/tolgraven/afterglow/blob/9b8762f1c5f5902b390a8375738f558b1c59a473/src/afterglow/render/gpu/expressions.clj#L9) · [Clojure array class syntax](https://clojure.org/reference/java_interop#_class_access) |
| Dense emission, physical field packing and transport conversion | A known `^double/1` receiver and `(aset values index (double level))` select the double overload. For DMX use `(aset buffer index (unchecked-byte level))` on a known byte array after validating the protocol range. | Compiling these two forms with Clojure 1.12.7 selected `RT.aset(double[], int, double)` and `RT.aset(byte[], int, byte)`. The corresponding `aset-double` and `aset-byte` forms invoked an `IFn` with boxed arguments. Preserve the numeric coercion and the return value if the caller uses it. [Core definitions](https://github.com/clojure/clojure/blob/435bdf34790c852a0bc6daf1d05e8959e081b607/src/clj/clojure/core.clj#L3970) · [Dense emission](https://github.com/tolgraven/afterglow/blob/9b8762f1c5f5902b390a8375738f558b1c59a473/src/afterglow/render/emission/dense.clj#L77) |
| Bulk fill or copy, OpenGL readback | Keep `Arrays/fill`, `System/arraycopy` and typed JOGL calls in their owning kernels or driver. These calls express the buffer operation directly. Clojure 1.12's qualified method values can simplify callbacks that only invoke a Java method. Overloaded method values need parameter tags to select a signature. | Verify identical row order and ownership across callers. Measure complete physical output before changing range fills, buffer copies or shader readback. [Native field expansion](https://github.com/tolgraven/afterglow/blob/9b8762f1c5f5902b390a8375738f558b1c59a473/src/afterglow/render/gpu/expressions.clj#L151) · [Qualified methods](https://clojure.org/reference/java_interop#methodvalues) |

In the 158-universe materialization control, validation improvements alone used 1.77 MB and 4.39 ms median CPU. Adding typed transport stores and a transducer for the universe entries reduced that to 0.28 MB and 2.04 ms over 56 samples. The full rig results above include the separate rendering and publication costs. They supply the acceptance check for this change. The same comparison is still needed before rewriting the other kernels in the table. [Measurements and standalone replay](https://github.com/tolgraven/afterglow/pull/81)

### Uniform fields at the physical fixture packer

On the mixed-UV sculpture, shader fusion eliminated seven of fourteen uniform result expansions. The remaining seven repeated 341,600 scalar components, or 2.73 MB per frame. The local native renderer now passes one detached tuple per uniform field into physical packing. A unit-opacity normalized replacement can fill head slots from this tuple after checking the value once. Cue gain with different eligibility across heads expands that field before blending.

A 20,000-pixel show with 64 movers, four moving optical volumes and eight stacked washes compared 40 measured pairs after 16 warmups. The reference used the released packing functions from `844198c`. The complete code change reduced median frame allocation from 114.53 to 107.24 MB and physical render p95 from 241.40 to 92.21 ms. Whole-frame p95 fell from 411.98 to 310.02 ms, while whole-frame p50 rose from 202.73 to 205.97 ms and median worker plus coordinator CPU rose from 192.12 to 206.49 ms. All 158 universes, model and intents matched. Selected continuous fixture previews agreed within 1e-9. Both hosts issued the same 345 draws with no CPU fallbacks on Mesa llvmpipe.

The 10,000-pixel CPU-only control matched the scalar oracle across 52 frames. Median allocation fell from 14.54 to 13.75 MB, and p95 rose from 60.18 to 68.15 ms. The native captures used a 60-second diagnostic deadline and the same CPU moving-head solver in both lanes. All native samples exceeded 25 ms. The eight-frame follow-up on the reviewed gain change also matched every packet. [Afterglow #82 and runnable captures](https://github.com/tolgraven/afterglow/pull/82)

### Fixture entry and exit events

Four moving volumes generated 3,162 fixture entry and exit events on the 10,000-pixel rig. Filtering changed IDs before computing their sort keys reduced median allocation inside the transition calculation from 6.68 to 0.40 MB per frame. Its CPU time fell from 2.94 to 1.40 ms at the median and from 16.10 to 3.99 ms at p95. The two hosts emitted identical ordered event maps and ArtDMX bytes across 14 warmups and 28 timed frames.

Whole-frame p95 on the 10,000-pixel rig fell from 545 to 424 ms. On the 20,000-pixel rig, median allocation fell from 114.69 to 101.10 MB while whole-frame p95 rose from 495 to 502 ms over 32 measured pairs. Both rigs had 64 moving heads and ran under software OpenGL with an extended compute deadline. [Afterglow #83 and raw event comparisons](https://github.com/tolgraven/afterglow/pull/83)

### Combined fixture packing, publication and spatial events

The same mixed-UV show exercised the three changes together against Afterglow `844198c`. The paired capture uses commits `4769e27` for packet publication, `c83375f` for uniform fixture fields and `252e546` for spatial events. The default Afterglow revision for a Tolglow CI run is recorded in the [performance workflow](../../.github/workflows/performance.yml).

Each rig ran for 16 warmups and 36 measured frame pairs. All ordered entry and exit events, effect intents, committed models and ArtDMX packets matched. Both hosts issued 477 native requests and 321 draws per rig without a CPU fallback at the extended deadline.

| Pixels plus 64 movers | Allocated per frame, p50 | CPU per frame, p50 | Whole-frame p50 | Whole-frame p95 |
| ---: | ---: | ---: | ---: | ---: |
| 10,000 | 59.95 → 42.63 MB | 161.77 → 161.26 ms | 219 → 215 ms | 822 → 861 ms |
| 20,000 | 114.59 → 79.16 MB | 236.64 → 205.26 ms | 353 → 291 ms | 2,661 → 1,567 ms |

The 20,000-pixel coordinator spent 1,904 and 1,036 ms runnable in the two frames at p95. Neither frame overlapped a GC pause. At 10,000 pixels those waits were 25 and 462 ms on the respective p95 frames. Host contention accounts for a substantial part of the wall-time variation. All measured frames missed 25 ms on this software renderer. The archive includes the ordered-frame checks, CPU and scheduler observations, GC recordings, Clojure array bytecode and replay commands. [Download the paired rig measurements](../benchmarks/master-performance/render-packet-and-membership.zip)

#### Publishing weighted fixture membership

Each spatial cue exposes the fixture IDs in its weight map as a Clojure set. Across four moving volumes on the 20,000-pixel rig, the median frame contained 36,913 membership entries. Building each set with `(into #{} (keys weights))` reduced median allocation for this projection from 8.74 to 1.84 MB. Its CPU p95 fell from 20.86 to 10.83 ms. Both hosts emitted matching model, event and packet data over 18 warmups and 40 measured pairs.

Whole-show median allocation fell from 80.10 to 73.23 MB. Median elapsed time rose from 553 to 600 ms, while p95 fell from 1,797 to 1,681 ms. A separate trial using a transient set and `reduce-kv` saved another 0.29 MB inside the projection. Reusing a previous set helped isolated nearly stationary volumes, but cost more CPU when a volume changed all its members. The host uses the single `into` expression for every volume. [Projection comparisons and playback commands](../benchmarks/master-performance/render-packet-and-membership.zip)

#### Checking masked layer weights

A spatial mask assigns a weight to each selected fixture. Afterglow checks that every weight is finite and nonnegative before using the mask. It also checks whether all weights are within the unit interval before choosing the direct packed blend. The two checks can stop at the first failing entry. Using `reduce-kv` avoids constructing an intermediate sequence of map values on each scan. Ratios, decimal values just above one and invalid weights on unpatched targets retain their original handling.

Two show hosts ran four moving optical fields, eight overlapping washes, a mixed-UV material and 64 moving heads. All packets, ordered fixture events, preview samples, model and intents agreed over 18 warmups and 40 measured frame pairs at each size. Both hosts dispatched 357 GPU draws without a fallback at each size on Mesa software GL.

| Pixels | Median allocation inside the scans | Scan CPU p95 | Median whole-frame allocation | Whole-frame elapsed p95 |
| ---: | ---: | ---: | ---: | ---: |
| 10,000 | 8.10 → 1.04 MB | 11.27 → 7.61 ms | 38.64 → 31.51 MB | 370 → 402 ms |
| 20,000 | 16.44 → 2.07 MB | 23.31 → 13.96 ms | 72.42 → 58.05 MB | 650 → 572 ms |

The 20,000-pixel case checked a median of 73,826 weights per frame. A separate run without scan timers saved 14.30 MB per frame. Its elapsed p95 went from 373 to 649 ms with 106 and 222 ms of runnable wait on the coordinator in those two frames. Neither frame overlapped a recorded GC pause. The consistent result across the two runs is lower allocation. The shared machine did not establish a whole-show latency improvement. [Afterglow #85, raw captures and replay](https://github.com/tolgraven/afterglow/pull/85)

#### Checking beam visibility in a moving sphere

None of the 144 measured frames reached the 25 ms lighting deadline on the shared Linux machine. Four overlapping spheres moved through a rig with 64 moving heads and either 10,000 or 20,000 pixels. Each sphere painted fixtures whose origin or emitted beam reached it, with the weight falling toward the edge. The floor, three walls and raised stage could block a beam. If both ends of a beam segment are strictly inside the same cell bounded by those planes, the entire segment stays inside and the renderer can skip the exact receiver test. In three sampled lit frames at 20,000 pixels, it skipped 38,399, 38,316 and 38,235 segment tests. A segment test could inspect up to five surfaces.

| Per-frame measurement, original → changed | 10,000 pixels | 20,000 pixels |
| --- | ---: | ---: |
| Frames meeting 25 ms, original and changed | 0/36 and 0/36 | 0/36 and 0/36 |
| Allocation sampling the four spheres, median | 18.37 → 12.82 MB | 34.75 → 23.64 MB |
| CPU sampling the four spheres, median | 75 → 71 ms | 133 → 151 ms |
| CPU sampling the four spheres, p95 | 270 → 245 ms | 531 → 281 ms |
| Whole-frame allocation, median | 32.28 → 26.57 MB | 56.68 → 45.48 MB |
| Whole-frame elapsed, p95 | 2,831 → 2,867 ms | 2,826 → 2,321 ms |

P95 is the 35th-shortest of 36 measured frames for each renderer. The original 20,000-pixel frame at elapsed p95 spent 1,939 ms runnable and waiting for CPU. The changed frame at elapsed p95 spent 1,786 ms waiting. Worker CPU and coordinator wait can overlap. Subtracting separately ranked percentiles would mix different frames. The full show state, effect operations, ordered fixture events and captured ArtDMX bytes agreed throughout. The production code also matched the original on 48,000 deterministic sphere/receiver cases, including tilted surfaces and openings beside finite panels. [Afterglow #88, measurements and reproduction](https://github.com/tolgraven/afterglow/pull/88)

In the first version, `(< low value high)` inside the beam loop increased whole-frame median allocation from 56.35 to 73.39 MB. Clojure 1.12 compiled that three-argument call through an `IFn` and boxed each double. The local `inside-slab?` helper uses two binary comparisons on primitive values. Its bytecode calls `Numbers.lt(double, double)` twice. The README and bytecode dumps in the benchmark archive keep this exception to the usual readable chained comparison specific to the measured hot loop. [Evidence archive](https://raw.githubusercontent.com/tolgraven/afterglow/d50dca0cceae0076e217140658caf6ed56eb571d/pr-88/room-cell-evidence.zip)

On the physical M4 MacBook Air the optimized CPU show took 62.13 ms median, with zero of 36 frames meeting 25 ms. The standalone hardware shader including readback took 1.91 ms median. Full-show results include the rig, scheduling settings, paired GPU-material timings and a link to the corrected checksum-pinned replay kit. [MacBook Air measurements](https://github.com/tolgraven/tolglow/pull/174)

#### Preparing the sphere fade curve

Two complete show hosts rendered four moving spheres, eight overlapping washes, 64 moving heads and either 10,000 or 20,000 RGBW pixels. Each pair alternated execution order. A prepared Clojure function computes the response for floating-point fade widths. Across 18 warmup and 36 timed pairs all fixture transitions, state, effect operations, sampled preview and ArtDMX bytes agreed. Both CPU configurations missed the 25 ms deadline on all 36 frames at each size.

| Median per frame | 10,000 pixels, original → prepared | 20,000 pixels, original → prepared |
| --- | ---: | ---: |
| Sphere sampling allocation | 13.39 → 10.98 MB | 21.18 → 19.22 MB |
| Sphere sampling CPU | 68.29 → 57.24 ms | 103.09 → 94.80 ms |
| Whole-show allocation | 33.05 → 30.62 MB | 63.22 → 61.24 MB |
| Whole-show elapsed | 479.90 → 399.58 ms | 197.64 → 179.63 ms |

On shared Linux, whole-show elapsed p95 fell from 1,255.09 to 901.55 ms at 10,000 pixels and from 1,189.75 to 1,100.24 ms at 20,000 pixels. The separate native-material run used Mesa software OpenGL. At 20,000 pixels its median allocation fell from 46.73 to 41.95 MB while its median elapsed time increased from 200.57 to 218.79 ms. All 72 GPU-material frames missed 25 ms. Samples and bytes matched the CPU run. [Complete traces, comparison and reproduction](https://github.com/tolgraven/afterglow/pull/90)


#### Omitting a covered shader before readback

A beat-driven material computes white for each pixel. Four moving spheres paint color, then an opaque wash replaces white on the same fixtures. Afterglow can prove the covered material cannot contribute, including across the independent color masks. Reducing the wash's opacity or releasing it reveals the material on the current beat. A changing parameter that becomes invalid still faults at the source.

Across two 36-pair covered replays and one 24-pair uncovered control, all 192 measured whole frames missed 25 ms. Model, ordered fixture contacts, effect intents and ArtDMX bytes matched for every pair. The two independent show hosts traded planners and evaluation order. Each rig included 64 moving heads.

| Measurement, original → changed | 10,000 pixels, covered | 20,000 pixels, covered | 10,000 pixels, wash removed |
| --- | ---: | ---: | ---: |
| Shader draws per diagnostic frame | 7 → 6 | 7 → 6 | 7 → 7 |
| Shader readback bytes | 200,336 → 160,080 | 400,336 → 320,080 | 200,336 → 200,336 |
| Median whole-frame allocation | 23.83 → 23.40 MB | 41.24 → 40.41 MB | 22.26 → 22.24 MB |
| Median whole-frame elapsed | 151.74 → 158.25 ms | 149.82 → 152.60 ms | 219.76 → 219.62 ms |
| Whole-frame p95 | 527.05 → 296.66 ms | 746.21 → 864.72 ms | 697.65 → 416.76 ms |

The 20,000-pixel shader evaluation median fell from 8.77 to 8.21 ms. Planner time grew from 1.44 to 1.65 ms. The two p95 frames had zero overlapping GC and 3.44 and 148.86 ms of runnable wait on the coordinator. The lower transfer volume and fewer draws reproduced on Mesa software OpenGL. Timing on a physical GPU remains open. [Afterglow #91 and runnable data](https://github.com/tolgraven/afterglow/pull/91)

#### Classifying fixture origins for four spheres in one shader

An experimental shader shares the rig's fixed positions across four moving spheres. For each fixture it returns a single float encoding whether each sphere definitely includes or excludes that origin. The CPU applies the original double-precision fade to the remaining origins. It still computes all beam intersections and surface occlusion. On the 20,000-pixel rig a median of 11,148 of 80,256 origin–sphere pairs needed the CPU fade. The shader read back 80,256 additional bytes per frame.

Two complete show instances ran 36 paired frames after 18 warmups at each size, alternating execution order and implementation. Fixture contacts, show state, effect operations, sampled preview and all ArtDMX bytes matched. The 19,968 dedicated boundary cases also matched. Neither implementation reached 25 ms on any of the 144 timed whole frames.

| Elapsed time or diagnostic count, current → experimental | 10,000 pixels + 64 moving heads | 20,000 pixels + 64 moving heads |
| --- | ---: | ---: |
| Whole-frame median | 110.47 → 91.16 ms | 217.94 → 198.49 ms |
| Whole-frame p95 | 565.65 → 310.33 ms | 479.06 → 466.96 ms |
| Draws per diagnostic frame | 7 → 8 | 7 → 8 |
| Bytes read back per diagnostic frame | 200,336 → 240,592 | 400,336 → 480,592 |

At 20,000 pixels, a separate 48-frame comparison timed origin selection alone at 10.15 ms median with the current CPU code, 8.41 ms with a squared-distance CPU shortcut and 6.85 ms with the shader and CPU fade. These measurements used Mesa software OpenGL on shared Linux. The experiment covers four spheres and positions within ±32 metres. Production adoption would require a measured hardware gain, general shape and coordinate handling, and lifecycle and resource ownership in the show host. Run `./replay.sh ./results` from the extracted kit to reproduce both comparisons. [Measurements and portable replay kit](https://raw.githubusercontent.com/tolgraven/afterglow/7243b1a2df037f35adece24ead95eae86757229e/experiments/four-sphere-classification.zip)

#### Tracing late frames on the 1,024-pixel show

Two workers rendered a beat-driven wash and aim movement on 128 heads, with a pulse, an independent 40 Hz publisher and an in-memory sink. The comparison used the exact source revisions of Afterglow #89. Each run covered 240 physical slots after 120 warmup slots. The owner thread selects and blends effect results, converts head positions into mechanical commands and packs DMX while the independent publisher prepares its own output. Timing the effect functions alone misses both costs.

| Replayed slot | Time until ready | Observed on that slot |
| --- | ---: | --- |
| 142, shared host | 602 ms | Lighting packing took 555 ms elapsed and 74 ms owner CPU. Over the complete tick the owner spent 539 ms runnable waiting for a CPU. No GC pause overlapped. |
| 281, same source with two bounded CPU competitors | 956 ms | Lighting packing took 898 ms elapsed and 191 ms owner CPU. Total owner runnable wait was 721 ms. The longest wait from effect submission to invocation was 2.39 ms. |
| 344, same competitors with the #81 transport change | 148 ms | A 134 ms GC pause overlapped the render. Lighting packing took 2.28 ms of owner CPU. |

In all three cases the wash interpolated with a 200 ms horizon and zero evaluator calls. The moving-head aim and pulse each ran once directly. The three observations belong to different runs. Thread durations can overlap, and the shared cgroup's throttling counter covers other jobs. The original screenshot did not capture these measurements.

Afterglow #92 removes a sort and intermediate vectors from calibrated pan and tilt selection. Two hosts replayed 119 actual clocks and their observed effect intervals. Every ArtDMX byte, all 1,152 preview records, model and effect intents matched. Across 94 warmed pairs, median packing allocation fell from 1,184,312 to 944,696 bytes and packing CPU p95 from 3.68 to 3.29 ms. Whole-tick median was 10.07 and 10.65 ms respectively, with p95 of 51.72 and 41.02 ms. An independent physical comparison of #81 reduced median allocation to prepare an 11-universe packet from 572,192 to 17,568 bytes. The 25 ms guarantee remains open. [Afterglow #92, calibration example and local replay source](https://github.com/tolgraven/afterglow/pull/92)

### Studio delivery and the next bottleneck

A real Ring/Jetty server and two loopback Transit WebSocket clients exercised the rig before the packed-write and broadcast changes. One client acknowledged every frame and received all four measured deltas of 65,775–70,529 bytes after a 4,229,339-byte initial snapshot. Median committed frame to application took 48 ms. The other client withheld acknowledgement, received one initial message and caught up to the latest state in 9.3 ms after resuming. Both reconstructed views matched the server. Every host tick missed 25 ms. Of 198 sampled CPU stacks, 34% involved packed field merging and 12% involved optical selection. A larger sample on dedicated hardware is needed for a stable ranking. [WebSocket workload](../../dev/tolglow/bench_subscribers.clj)

The native host also compiled 559 programs across 688 submissions and evicted 551 at a resident capacity of eight. Changing the resident working set, separating reusable shader structure from rig buffers, or fusing eligible groups could change the balance between compile time, residency and transfer. Measure compile/link misses, resident bytes and complete publication on a real GPU. Backward elimination of *effect evaluations* remains open; the accepted packed pass eliminates only covered writes. A candidate for earlier elimination has to prove that clock, state, failures and subscribed observations remain available when a layer is uncovered.

### Size the native program cache from the running show

At 8, 16 and 32 program slots, three fresh hosts advanced 30 matching frames each on the same llvmpipe rig. With room for all 13 active programs, the 18 warmed frames had zero link misses and evictions. At eight slots they had 234 of each. Each lane submitted 288 jobs. The larger caches issued 252 actual draws because some unchanged field groups reused results. Measured p95 was 3,833, 1,879 and 1,608 ms respectively, with median coordinator and show-worker CPU of 288, 265 and 266 ms. The two larger caches had the same resident set and operation counts. The difference in their wall times illustrates host noise.

The 16-slot cache retained 641,412 bytes of vertex data, 400,432 bytes of feedback and 400,432 bytes of host readback at the sampled frame end. Evictions left only uniform programs resident at eight slots, using 96, 128 and 128 bytes respectively. These are buffer sizes at one point in time and exclude driver/compiler memory and cached results. The cold first frame took 3,097, 4,471 and 1,736 ms, respectively. On this rig a ten-second diagnostic compute budget kept every lane native. A separate two-second cold run mixed CPU and native precision, with identical packet bytes but approximately 2e-9 preview differences. Keep the ordinary program limit configurable per show. [Set the cache and deadline](../native-shader-show.md)

Two later production-host smoke runs on newer Tolglow integration states hit the ordinary 500 ms effect-worker deadline. One stopped inside measurement, the other in warmup. Those runs cannot supply a completed latency distribution; their exact host revisions and failure stage are in the archive. Benchmarks using a larger deadline explicitly identify it. [Timing records and replay configuration](../benchmarks/master-performance/render-work-elimination.zip)

### Reusing a uniform field result

A 20,000-head RGB/focus field expands to 80,000 doubles. The general result cache accepts 65,536 values, so a uniform group of that size used to redraw even when its inputs were unchanged. The resident native session now retains the single validated tuple and expands it into fresh output arrays for each caller. Each changing clock or parameter still participates in the cache key.

On Mesa llvmpipe, holding parameters for eight submissions reduced 48 actual draws to six and median session time from 24.17 to 5.41 ms. Changing a uniform on every submission or including positions kept all 48 draws. The changing-uniform control's p95 rose from 32.72 to 36.18 ms. All widened doubles matched in all three isolated comparisons.

With the complete 20k/64-mover rig and a 16-program cache, each lane submitted 464 jobs including the untimed traffic probe. Actual draws fell from 408 to 184. Both lanes performed CPU movement calibration to eliminate the previously observed CPU/native solver precision difference. The 28 matched frame pairs had no field fallbacks. Median full frame time was 1,025 → 844 ms, p95 3,274 → 2,495 ms and median sampled coordinator and worker CPU 258 → 288 ms. A 60-second native diagnostic budget and five-second worker limit isolated field execution; the physical 25 ms deadline remains open. [Implementation, per-submission data and full-show replay](https://github.com/tolgraven/afterglow/pull/79)
