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
    evaluation and render, one candidate per frame.
- **Mixed-rig cases.**
  - One effect launched once per moving head with its own `:targets` (64
    programs) is revised.
  - A grid burst publishes a revision of another effect and, in the same
    owner cycle, schedules 64 per-head launches of it for the next beat, as
    firing a Live scene of per-fixture clips does. *Launch latency* counts
    frames from the frame that crosses the scheduled beat to the frame where
    all 64 first render.
- **Two modes.** `:inline` compiles in the frame, as before #774.
  `:background` compiles ahead on the show's worker.
- **Selector-cue corpus.** The mixed corpus plus 100 or 500 cues, each
  selecting its own 64-fixture slice. Each round:
  - publishes one revision, a full warm rebuild;
  - schedules a grid launch of one selector cue, a pad press;
  - measures the beat frame it fires on.
- **Run.**
  - Two effect workers, a capture sink, and a 25 ms cadence held with a spin.
  - 200 warmup ticks, 240 ordinary ticks, 20 rounds per case.
  - A fresh `lein run` JVM on JDK 25.0.2, Apple M4 Pro, at `6aa8e028`.
  - Load average was 2.4 to 7.6 from other agent lanes.
- **Report.** [2026-11-27-reload-takeover.edn](2026-11-27-reload-takeover.edn).

Mixed 2,048-pixel/64-head rig. Tick p50 / p99 ms, ticks over 25 ms out of 20,
adoption or launch latency in frames, and programs compiled on the owner:

| Case | Inline (as before) | Background: publication frame | Background: adoption or beat frame | Latency | Owner compiles |
| --- | --- | --- | --- | --- | --- |
| Ordinary frame | 3.1 / 4.9 | 3.9 / 4.4 | | | |
| 8 accepted | 10.2 / 14.6, 0 | 5.0 / 9.5, 0 | 4.3 / 11.7, 0 | 1 | 8 → 0 |
| 32 accepted | 25.5 / 33.4, 14 | 8.8 / 20.6, 0 | 5.9 / 15.8, 0 | 1 | 32 → 0 |
| 32 rejected at evaluation | 28.3 / 31.8, 20 | 10.1 / 13.9, 0 | 6.5 / 7.9, 0 | 1 | 32 → 0 |
| 32 rejected by the renderer | 29.7 / 42.3, 20 | 9.4 / 15.4, 0 | 7.3 / 9.0, 0 | 1 | 32 → 0 |
| One effect on 64 heads | 36.1 / 47.4, 20 | 4.7 / 7.8, 0 | 5.3 / 17.6, 0 | 2 | 64 → 0 |
| Grid burst of 64 launches | beat frame 50.1 / 61.7, 20 | 5.3 / 8.5, 0 | beat frame 22.1 / 27.7, 2 | 0 (launch) | 64 → 0 |

Selector-cue corpus, background mode, tick p50 / p99 ms (no frame over 25 ms
and launch latency 0 in every case):

| Selector cues | Publication frame | Pad-press frame | Beat frame |
| --- | --- | --- | --- |
| 100 | 1.7 / 3.3 | 1.4 / 2.2 | 2.6 / 3.8 |
| 500 | 1.7 / 3.1 | 1.7 / 2.7 | 3.1 / 4.7 |

Before the warm list moved to the worker, the independent review measured
the owner spending 61 to 82 ms per rebuild with 100 such cues and about
350 ms with 500, on every publish and twice per pad press.

On the showcase at 32 revisions, inline took p50 13.5 to 16.9 ms. In the
background mode the adoption frame was p50 4.8 to 5.4 ms, with none over
25 ms and a latency of 1 frame.

Findings:

- **The extra engine pass is not what missed the deadline.** Rejection at
  evaluation costs about one ordinary evaluation more than acceptance, and a
  renderer rejection adds a second render. The renderer turns down one
  candidate per frame, so 32 renderer-refused revisions take 32 frames of
  about twice the ordinary cost.
- **Compiling new programs inside the frame did.** That is about 0.45 ms per
  program on the mixed rig, one per revision and launch target set. In the
  background mode:
  - no program is compiled on the owner during a takeover;
  - a revision is adopted one frame after publication (two for 64 programs),
    by all its instances on the same frame;
  - launches land on their beat, and the 64 scheduled programs are warmed
    before the beat.
- **A 64-launch burst on one beat was close to the deadline without any
  compile.** p50 22 ms, 2 of 20 over 25 ms; see
  [launch bursts](#launch-bursts) for the fix.
- **The warm list is built on the worker.** Each cue's selection is resolved
  once per cue value, rig and settings. The owner only compares identities, so
  a publish or pad press costs about the same with 500 selector cues as with
  none.
- **Publishing still costs time on the owner.** The publication frame still
  validates each revision on the owner, at about 0.1 to 0.4 ms each (#784).
- **A full compile cache recompiled every running effect in one frame.** It
  now keeps a retired generation. The warm tier, about 55 KB per program on
  this rig, is kept apart from it, and programs leaving the tier never evict
  a running instance's program.

Excluded: physical output and the output scheduler, Studio/browser load, GPU
compute (a show with GPU expression groups compiles on the owner), auditions,
and the worker's CPU time, which runs on its own thread.

## Launch bursts

#798 measured the beat frame of the grid-burst case above: 64 per-head
launches of one effect scheduled for one beat, right after a publish.

**Where the time went.** Sampling the owner's stack during those frames
showed two passes over the whole 2,112-fixture rig for every launch:
- the synchronized-aim-path check indexed every fixture by wire ID before it
  knew the effect was not an aim path;
- the capability report filtered the rig to find the one head each launch
  targets.

The engine took about 3 ms of the frame; these two passes, 64 times over,
took most of the rest.

**The fix.**
- The aim-path check now indexes the rig only for a synchronized path.
- The capability report looks authored targets up in a rig index. The show
  keeps that index with its compiled patch, so each runtime has its own, a
  burst on one rig builds it once, and a rig edit replaces it. A caller
  outside an owner cycle indexes the rig once per launch.

Nothing moved off the owner. Admitting a launch assigns instance IDs and
orders and reads the beat's clock, so it is not pure ahead of the beat, and
without the two passes the remaining cost fits the deadline.

**Measurement.** Fresh `lein run` JVMs, JDK 25.0.2, Apple M4 Pro, background
compile mode, 20 rounds; before at harness `2b5eb1b3` with master's two
sources, after at `02575c15`. Beat-frame tick in ms:

| | p50 | p95 | p99 | max | over 25 ms | engine p50 | load average |
| --- | --- | --- | --- | --- | --- | --- | --- |
| [Before](2026-11-27-launch-burst-before.edn) (master `1d7f3a8a` sources) | 25.4 | 29.4 | 30.1 | 30.1 | 16 of 20 | 2.9 | 8.5 → 4.7 |
| [After](2026-11-27-launch-burst-after.edn) | 11.4 | 13.0 | 17.5 | 17.5 | 0 of 20 | 2.7 | 3.7 → 2.9 |

Both runs launched on the beat (latency 0) and compiled nothing in the frame.
Ordinary frames were unchanged (p50 3.7 to 4.3 ms).

**Still left in the beat frame, about 9 ms beyond the engine:**
- picking a free live-run lane, which scans lanes per launch;
- per-frame history bookkeeping in sync observation;
- Afterglow's command admission and history;
- packing the 64 new contributions.
