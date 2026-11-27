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

## Hot-reload takeover frames

[`dev/tolglow/bench_reload_takeover.clj`](../../dev/tolglow/bench_reload_takeover.clj)
measures owner ticks when 1, 8 or 32 effects get a new revision in one owner
cycle (#774).

- **Fallback on every takeover.** Every probe cue has a waveform bound to its
  level, and each revision changes the level's bounds, so the owner supplies
  the engine's reload fallback (#663) on every takeover.
- **Outcomes.**
  - Accepted.
  - Rejected at evaluation: the fallback pass re-evaluates every instance.
  - Rejected by the renderer after the engine accepted it: a second
    evaluation and a second render, one candidate per frame.
- **Per-fixture case.** On the mixed rig, one more case revises one effect
  launched once per moving head with its own `:targets`, as spatial actions
  launch. That is 64 programs.
- **Two modes.** `:inline` compiles in the frame, as before #774.
  `:background` compiles on the show's worker and adopts once ready.
- **Rigs.**
  - The showcase with five cues and their waveforms plus 32 probes: 129
    fixtures, 37 instances.
  - The mixed 2,048-pixel/64-head corpus plus 32 probes and the 64-instance
    effect: 2,112 fixtures, 100 instances.
- **Run.** Two effect workers, capture sink and a 25 ms cadence held with a
  spin. 200 warmup ticks, 240 ordinary ticks, and 20 rounds per case.
- **Environment.** Fresh `lein run` JVM, JDK 25.0.2, Apple M4 Pro, at
  `b5f56219`. Load average was 2.3 to 8.3, from other agent lanes.
- **Report.** [2026-11-27-reload-takeover.edn](2026-11-27-reload-takeover.edn).

Mixed rig, tick p50 / p99 ms, ticks over 25 ms out of 20, adoption latency
in frames, and programs compiled on the owner during the takeover:

| Case | Inline: takeover frame | Background: publication frame | Background: adoption frame | Latency | Owner compiles (inline → background) |
| --- | --- | --- | --- | --- | --- |
| Ordinary frame | 4.3 / 13.8 | 3.7 / 11.9 | | | |
| 8 accepted | 11.1 / 18.4, 0 | 6.7 / 18.3, 0 | 6.1 / 13.2, 0 | 1 | 8 → 0 |
| 32 accepted | 25.4 / 41.8, 14 | 10.3 / 21.4, 0 | 7.2 / 14.1, 0 | 1 | 32 → 0 |
| 32 rejected at evaluation | 28.9 / 41.2, 20 | 11.2 / 14.0, 0 | 7.9 / 14.9, 0 | 1 | 32 → 0 |
| 32 rejected by the renderer | 30.2 / 42.3, 20 | 10.6 / 22.0, 0 | 9.0 / 15.9, 0 | 1 | 32 → 0 |
| One effect on 64 heads | 37.3 / 48.7, 20 | 7.8 / 16.2, 0 | 7.4 / 26.2, 1 | 2 | 64 → 0 |

On the showcase, 32 revisions inline took p50 12.6 to 15.0 ms, with 2 of 20
rounds over 25 ms (rejected at evaluation). In the background mode the
adoption frame was p50 4.2 to 5.2 ms, with none over 25 ms and a latency of
1 frame.

Findings:

- **The extra engine pass is not what missed the deadline.** Rejection at
  evaluation costs about one ordinary evaluation more than acceptance. A
  renderer rejection adds a second render. The renderer turns down one
  candidate per frame, so 32 renderer-refused revisions take 32 frames of
  about twice the ordinary cost.
- **Compiling new programs inside the frame did.** That is about 0.45 ms per
  program on the mixed rig, one per revision and launch target set. In the
  background mode no program is compiled on the owner during a takeover. The
  revision is adopted one frame after publication (two for 64 programs), by
  all its instances on the same frame.
- **Publishing still costs time on the owner.** The publication frame still
  validates each revision, at about 0.1 to 0.4 ms each depending on load (#784).
- **A full compile cache.** It recompiled every running effect in one frame.
  It now keeps a retired generation.

Excluded: physical output and the output scheduler, Studio/browser load, GPU
compute (a show with GPU expression groups compiles on the owner), auditions,
and the worker's CPU time, which runs on its own thread.
