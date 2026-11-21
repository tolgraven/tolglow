# Rendering, acceleration, and simulation

## Current implementation

Afterglow owns the calibrated moving-head shader, fixture and target buffer
layouts, GPU dispatch and readback. A headless JVM host can run the same GLSL
as WebGL2. Both implementations accept world-space aim points and direction
vectors, preserve the pan from the last accepted frame and report targets
outside a fixture’s limits. Tolglow supplies authored effects and rig maps.
A show can enable the fork’s calibrated post-blend solver through
`:movement-backend :native-gpu`. It can use either CPU or native effect
expressions and shares their frame deadline. Tolglow's integration workload
compares Afterglow’s answers against the packed DMX universes. Studio owns the shaders that draw the room, surfaces and fog.
[Afterglow solver and JVM example](https://github.com/tolgraven/afterglow/blob/master/doc/architecture/moving-head-shaders.md)
· [Whole-show measurement](../moving-head-shader.md)

Afterglow can also compile a map of expressions for color, aim, focus or other
named fields into a single transform-feedback program. Each fixture gets an
independent record with typed field offsets. A 2,048-position WebGL run compared
14,336 arithmetic components with the CPU and matched them exactly. This workload
used exactly representable inputs and outputs. Trigonometric precision varies by
driver. The shipping CPU renderer remains the default for physical output.
[Show configuration and CPU fallback](../native-shader-show.md) · [Expression interface](https://github.com/tolgraven/afterglow/blob/master/doc/architecture/numeric-shader-fields.md)

The remaining targets include physical GPU timing on live-show hardware,
complete frame deadlines including readback and transfer, and wider fixture
and device qualification. The functional runtime defines assignment ordering,
identity and reload rules. [Functional runtime](functional-runtime.md)

## Original show path and profiling hypotheses

At afterglow fork `16b0b4cac70433e4118038dfbfd304e22d1d9018`,
[`show/send-frame`](https://github.com/tolgraven/afterglow/blob/16b0b4cac70433e4118038dfbfd304e22d1d9018/src/afterglow/show.clj)
clears a 512-byte array per universe, updates effect lifecycle, gathers generated
assigners with Claypoole, groups them in persistent maps by kind and target,
reduces each target's chain, resolves kinds in order, copies arrays into protobuf
`ByteString` values, and submits them to OLA. The render loop sleeps relative to
the scheduled snapshot and reports a rolling mean load. OLA is the existing output
boundary; Art-Net transmission timing also depends on the configured OLA/network
path, so optimizing Clojure alone may not fix every observed pixel delay.

[`transform.clj`](https://github.com/tolgraven/afterglow/blob/16b0b4cac70433e4118038dfbfd304e22d1d9018/src/afterglow/transform.clj)
solves calibrated movement in 3D and chooses legal solutions using former pan/tilt
values. [`movement.clj`](https://github.com/tolgraven/afterglow/blob/16b0b4cac70433e4118038dfbfd304e22d1d9018/src/afterglow/effects/movement.clj)
and color resolvers update movement/preview history as well as output. Existing
byte packing has specific fine-channel and inversion behavior that optimization
must preserve or deliberately version.

Potential costs to measure include per-head maps and records, lazy sequence
realization, dynamic parameter dispatch, geometry allocation, repeated fixture
search, shared atom contention, per-target task overhead, color conversions,
channel packing, protobuf copies, and output scheduling. These are hypotheses
from code inspection, not profiler findings. Increasing worker count may worsen
small workloads or GC pressure.
Tolglow's `param/auto-resolve` is already marked as a bottleneck in its source,
and the bloom implementation comments on resolving shared parameters per head.
Those comments prioritize measurements; their old ad hoc timings are not a
reproducible current benchmark. `utils/iters` offers transient-vector helpers,
but an allocation-friendly collection helper alone does not establish that the
whole rendering path is fast.

Tolglow has experimental Quil/Processing graphics and PixelFlow work in
[`viz.cljc`](../../src/tolglow/viz.cljc) and
[`graphics/`](../../src/tolglow/graphics). Afterglow already serves a WebGL preview
using a templated
[`fragment.glsl`](https://github.com/tolgraven/afterglow/blob/16b0b4cac70433e4118038dfbfd304e22d1d9018/resources/templates/fragment.glsl),
with a configured light cap and polling every 50 ms in the
[`visualizer.html`](https://github.com/tolgraven/afterglow/blob/16b0b4cac70433e4118038dfbfd304e22d1d9018/resources/templates/visualizer.html)
template. Its animated noise and beam ray marching are useful reference material,
but do not simulate fog-machine emission, room air flow, or evolving density.

## Establish a benchmark before choosing a backend

Use checked-in synthetic rigs plus captured representative real configurations.
Everything must run against a recording or discard sink without starting live
output. Fix random seeds, fixture order, effect versions, time snapshots, and
workload duration. Include warmup and separate steady state from startup/JIT/shader
compilation. Publish machine, OS, CPU, GPU/driver, JVM/flags, heap, commit IDs,
thread count, transport mode, and benchmark settings with every report.

| Dimension | Initial benchmark cases |
| --- | --- |
| Scale | 100, 1,000, and 10,000 pixels, plus 16/64 moving heads; mixed rigs as well as homogeneous strips. |
| Pixel format | RGB and RGBW; multiple universes, a strip crossing a universe boundary, and noncontiguous channel layouts. |
| Effect work | Constant output; per-head sine/color wave; spatial distance field; several blended layers; nested fade/chase; moving aim with continuity. |
| Control churn | Idle, dense knob changes, cue replacement, page switching, registry reload, and fixture patch change. |
| Scheduling | Serial, bounded parallel, multiple pool sizes; preview disconnected and connected. |
| Transport | No-op sink; local OLA recording/test transport; measured Art-Net output on the intended rig network. |

Measure total frame latency p50/p95/p99/max and deadline misses, each pipeline
stage, allocations/frame, GC pauses, CPU utilization, queue depth, throughput, and
end-to-end input-to-output latency. For a nominal 40 Hz case the period is 25 ms;
that is an example target, not an assertion about every venue's required rate.
Reserve time for transport and control; meeting only mean compute time is
insufficient. Maintain a per-hardware budget in benchmark results rather than
inventing one universal performance threshold.

Use Java Flight Recorder/async-profiler where available for CPU and allocations;
existing Tufte instrumentation is a useful starting point. Do not leave expensive
per-head instrumentation enabled in production timing comparisons. CI tests
numerical correctness and coarse smoke limits; stable dedicated hardware measures
regressions. Tight timing assertions in shared CI are unreliable.

Record raw output buffers and normalized head state for golden comparisons.
Byte-for-byte parity is required for integer packing; floating operations need
documented tolerances before quantization. Include zero, extremes, invalid/NaN
values, absent channels, color/gamut limits, pan wrap, unreachable aim, and dimmer
precedence. Diagnose a difference before updating a golden.

## CPU first: compile stable rig data

Build an immutable rig plan when the patch changes, not once per frame:

* Stable fixture/head indices; positions, rotations, calibration coefficients,
  capabilities, and preselected groups.
* Attribute-to-channel scatter tables including universe, coarse/fine index,
  inversion, function ranges, and fixture conversion policy.
* Contiguous batches for common fixture types and operations; track heterogeneous
  or opaque effects separately.
* Explicit channel ownership/conflict map. Overlapping physical writes cannot
  rely on worker timing.
* Versioned dimensions and spatial indexes; rebuild relevant caches after a rig
  change and retain the previous plan until replacement is ready.

First remove unnecessary recomputation, reflection, boxing, tiny tasks, and
intermediate lazy sequences in measured hotspots. Reuse pure specialized
functions for invariant portions of parameter graphs. Evaluate frame-global
parameters once, fixture-global parameters once per fixture, and per-head
parameters only where needed. Cache keys include definition version, rig revision,
parameter dependencies, and the relevant clock domain; a cache must not freeze a
supposedly dynamic parameter.

Partition work into substantial contiguous chunks and use a bounded executor.
Benchmark scheduling thresholds. Never parallelize the ordered blend chain for a
single target as though it were associative. Pure immutable public semantics can
coexist with primitive arrays or transients privately owned by one kernel/task;
those objects must not escape and be mutated after publication.

## Buffer contracts

Keep three representations with explicit conversion boundaries:

| Representation | Contents and ownership |
| --- | --- |
| Authoring/model data | Persistent maps/vectors with stable IDs, units, declared color space, target selection, parameter and lifecycle state. |
| Compiled frame buffers | Indexed, typed arrays for linear light RGBW or emitter values, intensity, direction/quaternion, pan/tilt, beam angle, focus, zoom, shutter/strobe and validity masks. One task owns each writable range. |
| Transport frame | Universe byte buffers after fixture calibration, clipping, function mapping, coarse/fine quantization, and precedence resolution. Immutable to consumers once published. |

Separate fields for RGBW are necessary: white emission is not merely a UI alpha
channel. Color space, white extraction/calibration, physical units, handedness,
and rotation conventions must be declared. Focus and beam angle are separate
attributes; some fixtures have discrete gobo/zoom/shutter behavior rather than
continuous normalized values. Unknown capabilities remain unknown instead of
being fabricated for preview.

Use structure-of-arrays where it helps bulk computation; benchmark against
interleaved forms for transfer/preview. Buffer metadata includes schema version,
show ID, epoch, frame ID, model/rig/registry revisions, logical timestamp, element
count, and valid attribute mask. An index is meaningful only with its rig revision.

The output shell can own double/triple buffers or a bounded pool. Ownership passes
to a sink with a completion receipt; a buffer is never recycled while a socket,
GPU transfer, or preview encoder still reads it. A published Clojure map containing
a mutable array is not automatically immutable. Copy, transfer ownership, or give
consumers a lifetime-limited read view explicitly.

The final normalized fixture state should reflect channel/function overrides and
fixture response. Sending an early color intent to the preview while a later
function closes the shutter creates a misleading image. If decoding from final
DMX is needed for a fixture, its decoder belongs in the fixture capability model.
Movement continuity state is an explicit prior-frame buffer, never a shared atom
updated while another head tries to read the frame.

## Declarative expression language

Use a typed, versioned expression graph represented as nested keyword maps.
Implement validation and a scalar CPU interpreter first. Higher-level Clojure
builders provide Quil-like composability and emit the same graph for supported
operations. Arbitrary Clojure functions remain a documented CPU escape hatch;
they are not transparently translatable to GLSL.

```clojure
;; Proposed schema example, not an implemented evaluator.
{:schema-version 1
 :output {:color
          {:op :color/hsl
           :h {:op :math/add
               :args [{:op :param :key :hue}
                      {:op :math/multiply
                       :args [{:op :position/component :axis :x}
                              {:op :param :key :spatial-rate}]}]}
           :s {:op :param :key :saturation}
           :l {:op :math/multiply
               :args [{:op :param :key :level}
                      {:op :wave/sine
                       :phase {:op :clock/phase :unit :beat}}]}}}}
```

Each operation specifies types, units, legal ranges, constant/frame/fixture/head
dependence, numerical behavior, and available backends. The compiler validates
arity and type compatibility, rejects undeclared references/cycles, folds
constants, shares common subexpressions, and emits a cached execution plan.
Bounds on graph size and compilation cost prevent live edits from monopolizing
the show thread. Diagnostics retain source paths so a no-code editor can identify
the offending node.

An effect has an outer pure lifecycle/state function and an optional inner batch
graph. Make clock, seeded noise, previous-frame samples, and feedback delay
explicit nodes; shaders cannot read arbitrary JVM state. Logical selections and
control commands remain on the CPU. Tween and envelope builders compile down to
ordinary time sampling and curve operations where supported.

Generate backend-specific source from this validated IR: scalar Clojure/JVM,
specialized primitive JVM kernels, optional Vector API/native SIMD, and optional
GLSL/WGSL kernels. Record capability requirements. Uniform-only parameter changes
must not force recompilation; graph or rig layout changes may. Compile away from
the frame thread, validate result, and atomically replace the plan at a boundary.

## SIMD and GPU evaluation

SIMD is a plausible fit for homogeneous pixel arithmetic, color transforms, and
distance fields. JVM Vector API availability and deployment flags depend on the
chosen JDK; AVX is only one CPU architecture family. Keep scalar fallback for
ARM and unsupported runtimes, and measure whether boxing or data rearrangement
erases the gain. Native SIMD adds packaging/FFI costs that must justify themselves.

GPU suitability depends on arithmetic intensity and transfer cost, not merely
the fact that fixtures inhabit 3D space. Thousands of trivial RGB values may be
faster on a CPU; expensive per-pixel fields may benefit greatly. Stateful movement
solutions, heterogeneous fixture logic, and small batches may remain on the CPU.
Split the graph only where crossing the boundary is cheap and correctness is
clear.

Compare the complete path:

```text
CPU: snapshot + compute + fixture conversion + pack + send
GPU: snapshot + upload + queue + dispatch + synchronization + readback
     + fixture conversion/pack + send
```

Read back numeric fixture/head buffers, not the displayed preview image. An
offscreen framebuffer with float textures and fragment shaders is a possible
WebGL2 calculation path, subject to extension/format limits. WebGL does not offer
general compute shaders; native OpenGL compute, Vulkan, or WebGPU compute require
different platform support and bindings. Browser WebGPU uses WGSL rather than
GLSL. Keep a shared IR, not a promise that one generated shader runs everywhere.

The authoritative lighting loop runs on the show host. A browser tab may be
throttled, suspended, disconnected, or lose its graphics context; an iPad preview
cannot be a required link in live Art-Net output. The JVM shader solver uses
a private native OpenGL context on the host. The browser can reuse IR
for local previews and editor feedback, while the host remains authoritative.

Asynchronous readback can hide stalls but often adds a frame of latency. Every
submitted job has epoch/frame/revision metadata and a bounded completion queue.
Discard stale results and define the deadline response: use the last complete
frame, recompute on CPU if budget permits, or apply an explicit failure policy.
Never mix new color with old aim merely because one buffer arrived first. Beat
prediction can compensate known latency for stateless graphs, but stateful
feedback cannot be extrapolated silently. Test device/context loss and fallback
under load before enabling GPU output in a performance.

Adopt acceleration only when total p99 latency, output parity, allocation/CPU
budget, supported deployment targets, and fallback behavior improve for the
actual workload. Keep the CPU reference implementation for all supported graph
operators as an executable specification.

## Output timing and backpressure

Give the scheduler, renderer, transport, and UI separate bounded queues. Preserve
ordered commands and completion events; high-frequency UI/preview snapshots may
drop superseded frames. A slow tablet must never hold the lighting loop open.
Control acknowledgments identify the model/frame where a change took effect.

Measure OLA submission and actual packet arrival separately. Art-Net bandwidth,
universe count, receivers' refresh limits, packet burst timing, sequence handling,
and network topology can dominate. Direct Art-Net, sACN, or OPC adapters are later
transport options with the same sink contract, not assumptions about what the
current fork already implements. Cross-universe fixtures need deliberate mapping
and packet consistency tests.

The scheduler uses monotonic deadlines; musical synchronization is input to the
logical clock. Define overrun policy rather than building an unbounded queue of
obsolete frames. Preview and metrics receive completed-frame events. Remote
observers get timestamps and revision IDs to detect missed data and request a
fresh snapshot.

## Shared rig scene and simulator

The rig editor and preview share a versioned scene model: room geometry in metres,
fixture/truss instances, fixture mode and patch, transforms, materials, obstacles,
fog emitters, ventilation hints, and calibration metadata. Presentation cameras
and selection handles are editor state. A rig edit is validated as a command,
then rebuilds the host rig plan and browser scene against the same rig revision.
Support undo/redo and grouping without treating meshes as authoritative fixture
definitions.

Separate three levels of quality:

| Tier | Intended behavior |
| --- | --- |
| Functional | Correct placement, aim, color, dimmer, shutter, beam width and focus; inexpensive beam approximation; works smoothly beside controls on phones. |
| Enhanced | Shadowed lights, gobos where supported, depth-aware volumetric scattering, physically meaningful attenuation, and temporal reconstruction on capable tablets/desktops. |
| High fidelity | Room-aware evolving fog density, calibrated emission/photometry, richer materials and multi-scattering approximations, bounded by measured GPU budget. |

"AAA" is a visual aspiration, not an acceptance criterion. A documented reference
scene, perceptual comparisons, and target-device frame budgets make progress
reviewable. Fixture photometry, optics, calibration, and room measurements limit
accuracy even with an expensive renderer. Preserve physical output independently
of preview quality.

## Fog and light transport

Fog should be a density field that exists independently of beams. A fog machine
injects density, temperature/buoyancy, and velocity in a localized nozzle volume.
Advection moves it; diffusion/turbulence mix it; ventilation, settling, and decay
remove it. Initial plumes can be dense and shaped before the room gradually fills.
Allow explicit initial density for a pre-hazed venue and a reset/replay control
for authoring comparisons.

An incremental implementation can start with an emission/density approximation
and advance to a low-resolution 3D grid with fixed simulation time steps. Render
volumetrics into a lower-resolution froxel/volume target and reconstruct
temporally with depth rejection. Account for transmittance and directional
scattering along shadowed light paths; tune density, albedo, extinction, phase
anisotropy, and fog flow separately from fixture brightness.

With zero particulate density, beams should not appear as glowing solid cones in
empty space; illuminated surfaces remain visible. As density rises, shafts
scatter into the camera and attenuate illumination behind them. Dense local fog
can obscure distant lights rather than just add brightness. RGBW spectral behavior
can initially be approximated with calibrated linear color; label that limitation.

Decouple fog simulation ticks, camera rendering, and lighting frame frequency.
Interpolate committed fixture state for display only. Do not feed preview fog or
lighting approximations back into physical output unless an explicit effect
requests that simulation as a versioned input source.

Acceptance scenes should include no fog; a single stationary beam; a short fog
burst retaining near-nozzle shape; gradual dispersion; ventilation; multiple
overlapping beams; moving head shadows; blackout with persistent fog; and RGBW
color/intensity changes. Fixed seeds and cameras enable image comparisons, but
mobile/desktop rendering differences need tolerances and visual review.

## Mobile and browser behavior

Controls have priority over preview pixels. Adapt internal resolution, volumetric
steps, shadow updates, fixture detail, and simulated density resolution to measured
frame time and thermal behavior. Use capability detection and named quality
settings; keep a low-power fallback and pause preview work when hidden. Recover
from context loss by requesting the latest scene/frame without restarting the
show. Test iPad Safari touch input and split view, phone portrait/landscape,
orientation changes, reconnects, and sustained thermal load, not just a resized
desktop window.

A bounded binary frame stream or compact deltas should replace polling only after
the versioned snapshot contract exists. Send rig data on revision changes, dynamic
attributes at an appropriate rate, and complete snapshots after missed revisions.
Backpressure may discard intermediate preview frames; UI gestures and parameter
acknowledgments require reliable ordering. Avoid per-frame DOM work proportional
to all pixels.

## Delivery sequence and proof

1. Capture existing semantics and representative output fixtures; add a dry-run
   sink and reproducible stage benchmarks.
2. Establish immutable snapshot/instance state contracts and serial CPU parity.
3. Compile rig/parameter plans, reduce measured allocation, and verify deterministic
   bounded parallel execution.
4. Publish normalized completed head state to an improved functional preview and
   shared rig editor; measure actual tablet/phone behavior.
5. Add typed declarative graph evaluation and specialize CPU kernels.
6. Prototype SIMD and host GPU on the same benchmark corpus; record transfer,
   latency, numerical differences, unsupported operators, and fallback behavior.
7. Add richer simulation tiers and evolving fog, each with reference scenes and
   performance budgets.

No phase is complete because a demo looks smooth or because one microbenchmark
is fast. Keep exact commands, captured artifacts, numerical checks, measured
hardware, and unresolved limitations alongside implementation status.
