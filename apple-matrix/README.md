# Tolglow #539 Apple matrix probe

Compare a synthetic fixture-row affine mix on the same Mac using Accelerate on CPU, Metal Performance Shaders on GPU, inline Metal 4 TensorOps on GPU, and compiled Core ML. The probe uses public APIs and creates its small Core ML source models locally. It can run offline with the Apple tools already installed.

The input is one row per fixture, with 63 synthetic normalized controls and a final column of ones. A 64-by-64 matrix produces 64 output parameters per fixture. The final weight row supplies bias. The dynamic cases alternate two independently generated input and weight matrices. The fixed-weight Core ML case holds the first matrix constant to probe the placement of a 1-by-1 convolution.

## Build and run on a Mac

Use macOS 14.4 or later and an Xcode installation with a matching SDK for Accelerate, MPS, and the Core ML compute plan. For the TensorOps route, use macOS 26 or later and Xcode 26 or later with its Metal compiler toolchain installed.

```sh
mkdir -p build/models
sw_vers
sysctl -n hw.model machdep.cpu.brand_string
xcodebuild -version

xcrun --sdk macosx clang -O3 -std=gnu11 -fobjc-arc -fblocks \
  -mmacosx-version-min=14.4 -Wall -Wextra issue539.m \
  -framework Foundation -framework Accelerate -framework CoreML \
  -framework Metal -framework MetalPerformanceShaders -o build/issue539

time xcrun --sdk macosx metal -std=metal4.0 -mmacosx-version-min=26.0 \
  -c issue539.metal -o build/issue539.air
time xcrun --sdk macosx metallib build/issue539.air -o build/issue539.metallib

build/issue539 all 64 500 build/models build/issue539.metallib
build/issue539 all 0 500 build/models build/issue539.metallib > build/all.log 2>&1
```

On a Mac without the Metal 4 toolchain, omit the two shader build commands and pass `-` as the last argument. The route `all` records the TensorOps skip and runs the other eligible routes. Pass `0` for the default 1, 64, 256, 1,024, and 20,000 fixture rows. Use `1` or a multiple of 32 through 20,000 to isolate another size.

To isolate one backend for tracing, use `cpu`, `mps`, or `tensorops`. Core ML selectors are `coreml-ane`, `coreml-cpu`, `coreml-gpu`, `coreml-auto`, and `coreml-all` for all four settings. Use the `fixed-` prefix for the fixed-weight probe.

```sh
build/issue539 coreml-ane 256 1000 build/models -
build/issue539 fixed-ane 256 1000 build/models -
build/issue539 tensorops 256 1000 build/models build/issue539.metallib
```

## What the timings mean

- `setup` records Core ML model compilation and load time separately. The compilation is shared across compute-unit settings for each model file. The first prediction can include framework initialization and specialization. TensorOps records library/pipeline setup and the shader-build commands record offline compiler and link time. OS and driver caches may survive between runs.
- Each result reports p50/p95/p99 in milliseconds for input packing, execution or submission-to-completion, output readback, and total measured wall time. MPS adds command-buffer GPU time, with zero indicating unavailable timestamps. GPU queue and wait overhead are part of the comparable wall time. Core ML prediction can include internal staging and output allocation. For the least warmed first-call measurement, launch one route and shape per process.
- Accelerate receives packed FP32 arrays and writes directly to the host output. MPS consumes shared FP32 buffers and includes the copies from and to the common host arrays. TensorOps includes conversion to and from FP16, wraps shared buffers with `MTLTensor`, and dispatches tiles of 64 by 64. It rounds up the row count to a full tile.
- The compiled Core ML dynamic model consumes row-major input and weight arrays. The fixed model converts fixture rows to channel-first spatial positions. Compare the fixed route as a placement experiment for reusable weights.
- The scalar reference accumulates in FP64 and compares every element for each input variant late in warmup and again at the end. A non-finite result or an absolute error above 0.001 sets a failing exit code. The probe also reports changes to synthetic output bytes after clamp and rounding. Check those counts alongside the exit status and run the real packet oracle.

A Linux simulation using the same seeds and scalar reference rounded both operands and the output to FP16. At 20,000 rows its largest absolute error was 0.000516, and 85,831 of 2,560,000 synthetic bytes differed across the two input variants. The simulation isolates representation and output rounding. The Mac run measures the frameworks' actual arithmetic and transfers.

## Establish placement on the measured machine

- **MPS** uses the selected Metal GPU. Record the device name and retain its command timestamps or an Instruments Metal System Trace.
- **Metal 4 TensorOps** runs with Metal 4 on M1 through M5. Apple documents shader implementations on M1–M4 and dedicated in-core GPU neural accelerators on the Apple10/M5 family. Capture the labeled compute dispatch in Xcode or Instruments. On M5 use the GPU profiler's neural accelerator counters when exposed to verify this particular kernel. `apple10=1` establishes the capability. [Apple inline TensorOps sample](https://developer.apple.com/documentation/metal/running-inline-ml-operations-in-a-shader-with-metal-4), [Apple M5 GPU talk](https://developer.apple.com/videos/play/tech-talks/111432/), [Metal feature tables](https://developer.apple.com/metal/Metal-Feature-Set-Tables.pdf)
- **System Neural Engine** is exposed through Core ML on Apple silicon. `coreml_available` reports devices Core ML exposes on the current Mac. `CPUAndNeuralEngine` allows both ANE and CPU execution. The `plan` line reports the preferred and supported devices for the layer, which can differ between the dynamic and fixed forms and between shapes. Availability and plan preference are separate from observed runtime execution. [Core ML compute units](https://developer.apple.com/documentation/coreml/mlcomputeunits), [Compute plan device usage](https://developer.apple.com/documentation/coreml/mlcomputeplandeviceusage)
- Keep the generated `.mlmodel` from `build/models`. Open that file's Performance tab in Xcode, run a report on the same Mac with the relevant compute configuration, and export its operation-level placement. For confirmed runtime placement, profile the matching one-route executable invocation in Instruments and retain the Core ML/Core AI or Neural Engine activity correlated to predictions. A missing trace, unsupported layer, or CPU preference leaves the ANE result unconfirmed. [Xcode performance report](https://developer.apple.com/documentation/coreml/analyzing-a-core-ml-model-s-performance-in-xcode), [Core AI runtime profiling](https://developer.apple.com/documentation/coreai/analyzing-model-runtime-performance-with-instruments)

## Tolglow acceptance still requires the show path

Use one real fixture and active-parameter row snapshot from the owner frame for each route. Exercise live coefficient changes, ordering, invalidation, native alpha, and irregular graph fallback. Keep the CPU path for frames or graphs that fail eligibility, correctness, or the deadline. Tie the handoff to the fused chain and use the same resolved sample across equivalent consumers. [#305](https://github.com/tolgraven/tolglow/issues/305), [#533](https://github.com/tolgraven/tolglow/issues/533)

Repeat the mixed-rig profiling setup with small fixture counts and 20,000 fixtures. Record complete owner-frame p50/p95/p99, misses against the actual show deadline, allocations, packed packet equivalence, and relevant output backpressure. Run idle, under sustained thermal load, and with the user's local model workload active. Check for contention on the GPU, system ANE, memory bandwidth, and CPU. Require the complete show to meet its deadline and preserve packet equivalence before automatically selecting a backend. [#387](https://github.com/tolgraven/tolglow/issues/387)

This source was prepared in Linux. The Apple SDK build, model compilation, hardware placement, and timing measurements require the Mac run above.
