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
measures the owner tick (`show/tick!`) when 1, 8 or 32 effects get a new
revision in one owner cycle (#774). Every probe cue has a waveform bound to its
level and each revision changes the level's bounds, so the owner supplies the
engine's reload fallback (#663) on every takeover frame. Outcomes: accepted;
rejected at evaluation (the fallback pass re-evaluates every instance); and
rejected by the renderer after the engine accepted it (a second evaluation and
a second render). Rigs: the showcase with five cues and their waveforms plus
32 probes (129 fixtures, 37 instances), and the mixed 2,048-pixel/64-head
corpus plus 32 probes (2,112 fixtures, 36 instances). Two effect workers,
logical 25 ms clock, capture sink, 200 warmup ticks, 240 ordinary ticks and 30
rounds per case. Fresh `lein run` JVMs on JDK 25.0.2, Apple M4 Pro, while about
a dozen other agent lanes loaded the machine (load average 12 to 18 in the
second pair). Stage times come from wrapping `engine/advance` (which includes
compiling), `engine/reject-output` (which includes the fallback re-evaluation
after a renderer rejection), the runtime's compiler, command handling and
modulation sampling. The second render after a renderer rejection is inside
the tick but not separately timed.

Mixed rig, tick p50 / p95 / max ms, and ticks over 25 ms out of 30:

| Case | [Before 1](2026-11-27-reload-takeover-before-1.edn) | [After 1](2026-11-27-reload-takeover-after-1.edn) | [Before 2](2026-11-27-reload-takeover-before-2.edn) | [After 2](2026-11-27-reload-takeover-after-2.edn) |
| --- | --- | --- | --- | --- |
| Ordinary frame | 2.2 / 2.8 / 3.2 | 2.7 / 3.5 / 3.8 | 4.5 / 8.8 / 20.3 | 4.9 / 8.1 / 12.7 |
| 8 accepted | 8.9 / 12.3 / 13.9, 0 | 10.3 / 11.9 / 12.1, 0 | 10.8 / 32.4 / 35.3, 3 | 16.1 / 42.6 / 59.6, 6 |
| 32 accepted | 25.2 / 35.5 / 42.0, 16 | 15.0 / 19.7 / 21.2, 0 | 30.1 / 67.4 / 69.3, 28 | 25.5 / 62.6 / 108.9, 17 |
| 32 rejected at evaluation | 28.2 / 39.8 / 44.6, 28 | 17.5 / 35.8 / 58.6, 5 | 31.9 / 66.6 / 75.2, 30 | 27.7 / 83.6 / 99.9, 18 |
| 32 rejected by the renderer | 28.3 / 37.1 / 45.8, 30 | 17.6 / 23.8 / 23.9, 0 | 32.6 / 68.6 / 69.7, 30 | 29.4 / 51.1 / 52.2, 18 |

Where the time goes on the mixed rig, p50 ms of the takeover frame:

| Stage | 1 accepted | 32 accepted, before | 32 accepted, after |
| --- | --- | --- | --- |
| Compiling new revisions (pair 1) | 0.5 | 13.8 | 4.3 |
| Engine including compiling (pair 1) | 1.4 | 15.1 | 5.5 |
| Command handling, publishing the revisions (pair 2) | 0.7 | 6.9 | 12.2 |
| Modulation and waveform sampling, both outcomes (pair 2) | 0.7 | 1.9 | 1.1 |

Findings:

- **The extra engine pass is not what misses the deadline.** Rejection at
  evaluation adds about 0.2 to 1.6 ms of engine time over accepting the same revisions, which
  is about one ordinary evaluation of these shows. A renderer rejection adds a
  second render as well; a frame that follows one costs about twice an
  ordinary frame (mixed rig p50 4.0 to 4.8 ms against 2.2 ms in pair 1).
- **Compiling every new revision inside the takeover frame does.** About
  0.45 ms per revision on the mixed rig (0.1 ms on the showcase), so 32
  revisions took 14 to 16 ms of compiling. A frame now attempts at most 8 new
  revisions; 32 take over across 4 frames, each compiling 8.
- **A full compile cache recompiled every running effect in one frame.** The
  256-entry cache was emptied when full, which added a spike of about 10 ms once
  the benchmark had published enough revisions. The cache now keeps a retired
  generation.
- **The renderer turns down one candidate per frame.** With a reload fallback,
  each renderer rejection re-evaluates the frame on the fallback inputs and
  leaves the other candidates pending, so 32 renderer-refused revisions take 32
  frames of about twice the ordinary cost. That is within the deadline here.
- **Publishing still costs about 0.1 to 0.4 ms per revision on the owner**
  depending on load: each publication validates the definition 4 times and
  calls the scalar compiler 8 times. Under the heavy load of pair 2 the 32
  publications took 12 ms of the frame, and the frame still exceeded 25 ms in
  17 of 30 rounds with the fix. That cost does not depend on running instances
  and is tracked separately.

Excluded: physical output and the output scheduler, Studio/browser load,
GPU compute, auditions, and frames whose revisions are already compiled.
Showcase results are in the same reports; at 32 revisions p50 went from 14.0
to 11.9 ms (accepted) and from 17.0 to 13.9 ms (rejected at evaluation) in pair
1, with no 8-revision case over 25 ms in pair 1 before the fix.
