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
- **Publishing still cost time on the owner.** The publication frame
  validated each revision on the owner, at about 0.1 to 0.4 ms each. See
  [publication checks](#publication-checks) for the fix (#784).
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

## Publication checks

#784 measured the owner cost of publishing effect revisions in the
reload-takeover benchmark above: 32 revisions published in one owner cycle on
the mixed 2,048-pixel/64-head rig, in background compile mode.

**Where the time went.** Counting calls during publication showed 4
validations and 2 compiles of each revision on the owner. (Counting calls of
`compile-effect` gives 8, the figure in the issue, because each compile
passes through its 4 arities.) `show/apply-message` validated the revision, then
compiled it, and that compile validated it again. Then `spatial/publication-errors`
ran `compiler/definition-errors`, which validated and compiled it once more,
and that compile validated it a fourth time. Timing the owner per publication
in a pooled JVM (JDK 25.0.2, load average about 7, 870 archived revisions):

| Stage | ms per publication |
| --- | --- |
| Whole publication on the owner | 0.11 |
| Its validations and compiles | about 0.04 |
| Afterglow's `latest-version` scan of the archive | 0.03, growing with every kept revision |
| Command admission, history and the rest | about 0.04 |

**The fix.**
- *One check, off the owner* (`tolglow.show.publication`, see
  [effect fault isolation](../effect-fault-isolation.md)). The definition is
  validated once and compiled once, on the compile worker, before the owner
  sees the message. The owner derives the revision against its registry and
  adopts the worker's verdict when it checked that same revision. It holds a
  publication whose check is not ready, with every later message, for a later
  cycle.
- *An archive index in Afterglow* (tolgraven/afterglow#168, master
  `8d969a15`). A publication finds the
  latest archived version from an index kept beside the archive map it
  describes, instead of scanning every revision the show has kept. The
  benchmark keeps about 4,000 revisions by its last rounds, so this scan was
  most of its publication cost.

**Measurement.** Pooled JVMs (`bin/repl run tolglow.bench-reload-takeover 30
mixed background`), JDK 25.0.2, Apple M4 Pro, 30 rounds, run one after the
other. Publications now arrive half a frame before the owner cycle, as an
operator's do between frames. The before run used the earlier harness, which
submitted them just before the cycle; the owner checked them inline either
way. The Tolglow-only run used branch code at `5dc882f2`. The final run used
`b65d87a5`, which holds only messages that depend on a waiting publication,
for at most one cycle, and lets only an explicit allow-list go ahead. 32 accepted revisions, publication frame, ms:

| | Commands p50 / p99 | Tick p50 / p99 | Over 25 ms | Ack latency | Load average |
| --- | --- | --- | --- | --- | --- |
| [Before](2026-11-27-publication-before.edn) (master `ed0dad0f`, Afterglow `6f4871fd`) | 5.12 / 9.95 | 11.2 / 25.0 | 1 of 30 | 0 | 3.2 → 4.1 |
| [Tolglow change only](2026-11-27-publication-tolglow-only.edn) (Afterglow `6f4871fd`) | 4.63 / 7.84 | 9.9 / 22.5 | 0 of 30 | 0 | 4.2 → 5.1 |
| [Both changes](2026-11-27-publication-after.edn) (`b65d87a5`, Afterglow `8d969a15`) | 0.69 / 1.38 | 5.7 / 10.1 | 0 of 30 | 0 | 2.7 → 2.5 |

An earlier before run on the same machine gave commands 5.37 / 10.89 ms and
ticks 9.9 / 23.2 ms (load 2.8 → 3.9). The same both-changes code before the
selective hold (`5dc882f2` with Afterglow `0dec26eb`) gave 0.74 / 1.72 and
5.8 / 15.8 ms at load 4.3. With both changes, the other 32-revision cases
have commands p50 0.65 to 0.90 ms, against 6.3 to 6.7 ms before.

**Blackout behind publication.** Each round also sends 32 revisions just
before an owner cycle, too late for the worker to check them, followed by a
blackout. In all 30 rounds the blackout applied in that first cycle (latency
0 frames), and the publications landed one cycle later (latency 1). That
first frame ran tick p50 / p99 4.1 / 11.3 ms, commands 0.08 / 1.52 ms. (The
run at `85cf52af`, before the allow-list, gave the same latencies, commands
0.61 / 1.39 ms for 32 accepted revisions, at load 2.2.)
Before the selective hold, the review measured such a blackout waiting 2
cycles with an idle worker, and 9 cycles (332 ms) behind a warm-tier rebuild
with 500 selector cues. In that probe the blackout now applies in the
first cycle after it is submitted (35 to 39 ms of wall time, including the
probe's 25 ms sleep). A
single publication's command time is about 0.55 ms in every run; most of it
is not publication work. Every publication was acknowledged in its first
frame, adoption latency stayed 1 frame, and ordinary frames ran p50 2.9 to
4.3 ms across the runs.

Excluded: the worker's own CPU time (one validation and one compile per
publication, on its thread), inline mode, physical output and Studio/browser
load.

## Rig edits

#811 added a `:rig-edit` case to the reload-takeover benchmark above. It moves
one fixture that no probe aims by a centimetre, half a frame before an owner
cycle, with every probe, base cue and fan instance running. That is 37
programs on the showcase and 100 on the mixed rig.

**Where the time went.** The frame that applied a rig edit compiled the new
patch and every running effect's program for the new rig. On the mixed rig
`render/compile-patch` alone took about 150 ms (pooled JVM, JDK 25.0.2), more
than the 100 program compiles.

**The fix.** With a worker, the owner holds the edit while the worker
compiles the patch and the running programs ahead of other compiles. The
cycle that finds them ready applies the edit, and its frame renders the new
rig with both. See [effect fault isolation](../effect-fault-isolation.md).

`bin/repl run tolglow.bench-reload-takeover 10 showcase mixed background inline`,
Tolglow `f5837e1f`, Afterglow `ef539be0`, JDK 25.0.2, Apple M4 Pro, load
average 16 to 23 from other work. 10 edits per case; tick p50 / max ms:

| Case | Edit frame | Adoption frame | Over 25 ms | Adoption latency (frames) | Owner compiles in frames |
| --- | --- | --- | --- | --- | --- |
| Showcase, inline | 25.4 / 40.9 | same frame | 6 of 10 | 0 | 37 |
| Showcase, background | 3.3 / 7.8 | 4.5 / 5.8 | 0 | 1 | 0 |
| Mixed, inline | 139.8 / 219.2 | same frame | 10 of 10 | 0 | 100 |
| Mixed, background | 6.8 / 9.5 | 7.4 / 10.9 | 0 | 4 to 6 | 0 |

Ordinary frames ran p50 2.8 to 5.2 ms. On the mixed rig the worker's patch
compile and 100 programs take 4 to 6 frames of wall time, during which the
held edit's launches wait. Every other case's frame compiles stayed 0 in
background mode. The report is `2026-11-28-rig-edit.edn`. The heavy load
makes the tick maxima noisy; the compile counts and latencies do not depend
on it.

Excluded: the submitting thread's own `rig/prepare-edit`, which also compiles
the patch to validate the edit (about 120 to 250 ms, off the frame), and the
worker's CPU time.
