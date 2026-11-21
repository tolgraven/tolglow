# Retained benchmark evidence

These reports were collected on 2026-11-21 from this task's working trees.
They preserve the measured machine/workload metadata; they are not release
benchmarks or guarantees for another rig. No physical lighting output was sent.

| Artifact | Provenance and scope |
| --- | --- |
| [Original assignment/packing](2026-11-21-assignment-packing.edn) | Initial Afterglow default-master baseline; described in [benchmark-baseline](../benchmark-baseline.md) |
| [CPU backend before](2026-11-21-backend-before.edn) | `/tmp/afterglow-backend-legacy.edn`, 04:03:35 UTC; new backend before profile-guided optimization plus legacy packing comparison |
| [Allocation before](2026-11-21-backend-allocation-before.edn) | `/tmp/afterglow-backend-allocation-before.edn`; focused allocation checkpoint |
| [CPU profile](2026-11-21-backend-profile.edn) | `/tmp/afterglow-backend-profile.edn`; profile used to select optimization targets |
| [CPU backend optimized](2026-11-21-backend-optimized.edn) | `/tmp/afterglow-backend-optimized.edn`, 04:11:38 UTC; full backend and physical-only paths with allocation measurements |
| [WebGL2 verification](2026-11-21-webgl2-swiftshader.json) | Frozen `/tmp/tolglow-gpu-browser/verification-2026-11-21.json`, 04:29:45 UTC; actual Chromium 156/ANGLE Vulkan SwiftShader **software** context |

All copies retain the original data. The CPU-before source began with the REPL
print `#'afterglow.bench-backend/-main`; only that non-EDN prefix was removed.
The raw CPU reports identify `:source-revision "working-tree"`, so they do not
prove results for a committed release. The artifact names describe collection
checkpoints rather than an independently reconstructed historical commit.

The measured 10,000-head RGBW CPU full-frame stage improved from roughly
**77.06 ms to 34.10 ms median**, with reported allocation falling from about
**120.45 MB to 23.77 MB per frame**. The optimized physical-only typed path was
**19.85 ms median, 27.83 ms p95**, about **6.44 MB/frame**. That is useful stage
progress, not proof of a stable 25 ms complete-show deadline. Effect generation,
transport and end-to-end scheduling are not included. Only matching packing
stages in the report directly compare the new and legacy packing routines.

The final software WebGL run passed **7 oracle cases and 8 lifecycle groups**.
Strict arithmetic maximum absolute error was about `7.45e-9`; trigonometric
operations use a separately declared exploratory `2e-4` tolerance, not fine-DMX
parity. With 3 warmup and 12 timed samples, total median/p95 were **5.46/13.22 ms
for 1,000 elements** and **5.13/5.41 ms for 10,000 elements**. These small samples
are exploratory. The detailed report includes stage samples and compared values.

WebGL numbers exclude JVM/browser transfer, assignment reduction, fixture
conversion/packing, Art-Net/output, UI and simulator rendering. They cannot be
compared to the CPU full-frame values as a speedup claim. Physical GPU hardware,
long-run jitter, transfer costs and production fallback remain unverified.
See [GPU experiment](../gpu-experiment.md) for harness and lifecycle details.

## Later typed-batch and musical-IR checkpoint

Subsequent work added a shared typed batch path, independent compact preview
projection and musical beat/bar/phrase IR. The owner reported **31 tests / 318
assertions passing**. Retained reports supersede earlier numbers only for their
matching workload; they do not erase historical measurements.

- [Final stage comparison](2026-11-21-backend-stage3-final.edn): actual old-source
  parity, 100 warmup/100 measured samples. At 10k targets, typed batch to DMX plus
  compact preview p50/p95/p99 **5.43/7.14/8.32 ms**; spatial IR through the same
  output path **8.86/10.49/10.89 ms**, approximately **9.075 MB/frame**.
- [Longer isolated run](2026-11-21-typed-final-cpu.edn): 100 warmup/300 samples;
  spatial IR p99 **23.97 ms**, maximum **41.88 ms**. The [previous noisy run](2026-11-21-typed-noisy-run.edn)
  recorded **46.33 ms p99**. These demonstrate environmental variation rather
  than a guaranteed 40 Hz deadline.
- [Earlier spatial interpreter/batch comparison](2026-11-21-stage3-batch.edn):
  the reported old interpreter allocation was **94.602 MB/frame** for the same
  corpus; use this artifact when assessing the typed-path allocation reduction.
- [Musical WebGL verification](2026-11-21-webgl2-musical-swiftshader.json):
  **8 oracle cases / 992 compared components and 9 lifecycle groups** passed in
  the actual SwiftShader software context. No physical GPU claim follows.

The final stage report's rich projection still measured **26.66 ms median**.
Raw new packing measured **4.38 ms median** versus **3.72 ms** for the actual old
packer, so the improvement must not be described as a raw packing speedup.
Whole engine generation/scheduling, transport, real IO and UI/simulator costs
remain excluded. Original sources were `/tmp/afterglow-backend-stage3-final.edn`,
`/tmp/afterglow-typed-final-cpu.edn`, `/tmp/afterglow-typed-final.edn`,
`/tmp/afterglow-render-stage3-batch.edn` and
`/tmp/tolglow-gpu-browser/musical-verification-2026-11-21.json`, respectively.
Source was the task working tree.

## Actual engine and ArtDMX capture checkpoint

[Rendering progress](../rendering-progress.md) documents the currently integrated
batch compiler and mixed renderer. These later reports include three effects,
response/envelope middleware, the real engine, a 16-head projection and ArtDMX
packet capture:

- [Runtime pipeline](2026-11-21-runtime-pipeline.edn) records the baseline for
  this larger workload, including its selected execution modes.
- [Runtime pipeline with byte-array copy](2026-11-21-runtime-pipeline-byte-copy.edn)
  records the tested packet-copy improvement. At 10,000 targets with two workers,
  p50/p95/p99 were **11.95/15.90/18.66 ms**, maximum **26.80 ms**, with reported
  allocation **11.89 MB/frame** versus **20.20 MB/frame** before the copy change.

These are broader pipeline measurements than the earlier renderer-only reports,
so compare the paired workloads rather than dividing unrelated stage timings.
They still exclude whole-host scheduling, network delivery and contention from
the live UI/simulator. The maximum exceeds a 25 ms budget; this is not a guaranteed
40 Hz production-show claim. Captured packets were not sent to physical fixtures.

## Sensor library and output-presentation evidence

[Sensor package smoke](2026-11-21-sensor-library-smoke.json) records three actual
Capacitor Motion8.0.1 assertions with an injected EventTarget; it proves neither
physical sensor access nor browser permission behavior.

[Presentation report](2026-11-21-presentation.edn) and its [exact harness](presentation-bench.clj)
cover100 universes/12,800 RGBW fixtures. [Output policy](../output-presentation.md)
explains immutable packet ownership, per-stage allocation/timings and excluded
engine/scheduling/network costs. No physical fixture output was sent.

[Captured waveform report](2026-11-21-audio-waveform.edn) and
[harness](audio-waveform-bench.clj) measure bounded PCM summary plus immutable
view construction at 48 kHz in 480-sample blocks, with 256 retained columns.
Median/p99 were 0.080/0.284 ms; the complete capture/analyzer/display pipelines
were excluded. This establishes only the cost of the new summary stage.
