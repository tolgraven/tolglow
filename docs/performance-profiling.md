# Profiling the running show

`bin/profile-master` measures a deterministic mixed rig through the real engine,
renderer and ArtDMX encoder, then through `show/tick!`. It never opens a network
socket or physical output. Use an isolated paired checkout of current master:

```sh
AFTERGLOW_ROOT=../afterglow-performance bin/profile-master \
  --heads 100,1000,10000 --movers 0,8 --workers 1,2 \
  --warmup 40 --samples 120 --output /tmp/master-performance.edn
```

The launcher fixes the heap at 768 MiB and overrides the dependency with the
explicit Afterglow checkout. The report records both commits, CPU quota/model,
JVM/Clojure versions, JVM arguments and workload options. Run without other
builds or browser gates; keep every report, including unfavorable repeats.

The corpus includes spatial RGBW pixel color, beat-based attack/release, an
ordinary tween overlay, and optionally eight moving heads with aim, focus, zoom,
RGBW and physical dimmers. Fixtures cross universe boundaries. The pipeline
replays a captured frame and separately times engine evaluation, one mixed render,
selected preview projection and packet encoding. It compares serial mapping and
explicitly bounded pools. The host workload advances 25 ms ticks and submits a
changing master command every eight ticks; it includes command admission, clock
and middleware work, effects, packing, reactive publication and synchronous
ArtDMX capture. This host lane uses the same bounded worker executor as Studio.

Every warmup and measured frame is checked against the scalar compiler and
general renderer: full model and intents, every packet byte, and visible preview
fields (numeric tolerance `1e-9`). Checks run outside the timed interval.
Percentiles, maximum, frames over 25 ms, mean throughput, owner/worker allocated
bytes and GC counters are retained alongside individual observations. GC counters
span the measured loop, including oracle comparisons outside the timed samples.

This does not measure scheduler wakeup jitter, independent output publication,
actual packet delivery, OLA/USB/fixture latency, browser or simulator work. It is
not a GPU benchmark. Render stage costs are present only in the pipeline lane;
the host lane deliberately times the real production tick without instrumentation.
One worker is serial effect execution with dispatch overhead; pipeline `:serial`
is direct `mapv` without dispatch.

## Connected Studio subscribers

`bin/profile-subscribers` runs the same changing show with zero, one and four
acknowledging clients. Each connected case also holds one slow client at its
initial, unacknowledged snapshot. It starts an owned loopback Jetty server and
real JVM WebSocket clients using production Transit encoding, projection and
delta preparation. Physical output remains an ArtDMX capture.

```sh
AFTERGLOW_ROOT=../afterglow-performance bin/profile-subscribers \
  --heads 1000,10000 --clients 0,1,4 --movers 8 --workers 2 \
  --warmup 40 --samples 120 --period-ms 25 --output /tmp/subscribers.edn
```

Every host tick still matches the scalar model, intents, all packet bytes and
selected preview values. Clients validate patch base tokens, reconstruct their
views and compare the final result to the actual production snapshot. The slow
client must receive exactly one message until it acknowledges, then reconstruct
the latest view. These are correctness gates, not timing thresholds.

The report separates host tick duration, initial hydration, commit-to-client
decode/apply latency, decoding time, delivery frequency, bytes and coalesced
frames. Initial hydration and slow-client catch-up are outside the measured
window; the final fast-client delivery drain is recorded separately and included
in allocation/GC totals. A small frame-commit timestamp watch performs no
projection or socket work. Wall-clock pacing avoids catch-up bursts; `period-ms`
changes that pacing only, while both oracle and measured shows advance a fixed
25 ms of logical time per tick.

Allocation covers all live JVM threads, including server and clients, oracle
comparisons and measurement instrumentation. It is not comparable to the isolated
profiler's owner/worker allocation. Retired threads whose allocation counters are
unavailable are counted explicitly. Snapshot preparation and delivery are
asynchronous, so their cost must not be inferred from host duration alone.

Each run records `:rig-targets` and `:visible-output-targets`: the current
production view includes all rig definitions but bounds live output to its first
16 targets. A 10,000-pixel workload therefore does **not** demonstrate streaming
every pixel's live state. Browser JavaScript/re-frame, rendering, remote network
latency and physical output are excluded. The retained delivery observations
describe JVM loopback subscribers, not measured iPad performance.

## Rotated moving heads and layered fades

`bin/profile-movers` constructs 64 or 128 independently placed and rotated RGBW
moving heads. Twenty-four real effects divide them into eight groups and three
competing priorities. Every head follows a distinct world-space target derived
from its placement and the captured musical beat. Effects launch in reverse
priority order, so launch order and worker completion cannot silently replace
the required priority order.

The launcher uses the Leiningen `:benchmark` profile and its explicit paired
Afterglow developer classpath. The independent geometry/DMX oracle lives in
`afterglow.bench-movement-reference`; the Tolglow fixture/effect corpus does not
implement another hardware renderer.

```sh
AFTERGLOW_ROOT=../afterglow-performance bin/profile-movers \
  --heads 64,128 --workers 1,2 --warmup 40 --samples 120 \
  --output /tmp/movers.edn
```

The layers affect aim, color, white, dimmer, focus and zoom. Repeated opacity
sequences include exact zero and one, rising and falling intermediate values,
all layers off and simultaneous mixtures. Each frame checks all heads, including
every target beyond the first 16. An independent matrix implementation converts
the blended world-space target into local pan/tilt, chooses calibrated pan wraps
using prior state, and verifies every coarse/fine and emitter channel byte,
including universe crossings. Its arithmetic and known-axis checks do not call
the renderer's private geometry functions. A scalar compiler/general-renderer
oracle additionally checks full model, intents, packets and preview. Frame
checksums and alpha combinations remain in the report.

Separate lanes measure direct serial execution, bounded workers and actual
headless show ticks. Host timing includes command enqueue/admission, the normal
frame pipeline, reactive publication and captured ArtDMX encoding. All-head
reference projection happens outside its timer. The advancing pipeline lane
includes all-head projection and preserves prior compact movement history.

A separate diagnostic pass instruments the renderer's geometry and per-fixture
packing boundaries, exposing aiming, remaining fixture packing/preview work and
render reduction/dispatch. Its nested per-fixture timers add overhead; use the
uninstrumented lanes for CPU frame-budget counts. Allocation includes result
extraction; GC includes checks outside frame timers. These measurements do not
include scheduler jitter, network delivery, a browser/GPU or physical fixtures,
and they do not prove real-time output deadlines.

## Keeping master under observation

The `Master show performance evidence` workflow runs on master pushes affecting
runtime/compiler/dependencies and accepts reusable/manual calls. GitHub schedules
only execute from the repository's default branch, currently `effects`. A minimal
`performance-schedule.yml` on that branch supplies the weekly/manual trigger and
calls the implementation on master; it contains no benchmark or application copy.
There is only one scheduled trigger. If the default branch moves to master, move
that trigger there too and remove the old copy.

Profiler changes also run on pull requests, measuring that exact PR head in an
independent runner before merge. These jobs retain the same oracle gates and
artifact-only timings and do not share a concurrency group with master runs.
Push runs check out the exact pushed master SHA. Scheduled/reusable calls check
out master, even though the caller's event SHA may belong to effects. Manual
calls default to Tolglow master and the immutable Afterglow revision pinned alongside the CI tests. An explicit Afterglow candidate can be selected for upstream comparisons. Each
run resolves both checkouts
once and records their immutable SHAs before running the matrix on Java 21. Raw
EDN and logs for both the complete show and connected subscriber matrices are
retained for 90 days, including failed runs. Oracle failures fail
the job; timings have no pass/fail threshold. Scheduled runs also run a separate upstream job against Afterglow master, with a distinct artifact name. To run that comparison manually, dispatch the workflow with `afterglow_ref=master`. Compare artifacts
only after checking their source and machine metadata.

On every merged change to the engine, compiler, renderer, host middleware or
output, fetch both explicit master refs and run the relevant corpus in a clean
paired checkout. For changes outside those paths, a small smoke run still checks
that the profiler can construct a real show:

```sh
AFTERGLOW_ROOT=../afterglow bin/profile-master \
  --heads 100 --movers 8 --warmup 2 --samples 3 --output /tmp/profile-smoke.edn
```

Use the full matrix before accepting an optimization. Compare identical options,
source revisions and machine configuration; retain raw before/after reports under
`docs/benchmarks/master-performance/`. Investigate p95/p99, allocations and complete
host ticks, not just favorable medians. Timing changes are evidence to investigate,
not portable pass/fail CI thresholds. Hardware output always remains injected.

The profiling task stays open while integration continues; each agent reports its
last measured commits and the next changed paths to recheck. A stopped agent does
not imply an ongoing background monitor. The command and retained reports are the
durable handoff for subsequent sessions.

## Matched Afterglow comparisons

Manual workflow dispatch accepts `tolglow_ref`, `afterglow_ref` and optional
`compare_afterglow_ref` / `compare_tolglow_ref`. Use branch/tag names or full
40-character commit SHAs. Resolve candidate and baseline once, then run the same
complete matrices in candidate → baseline → candidate-repeat order on one runner.
Each pass has fresh JVMs, identical 40-frame warmup and 120-frame samples, heap,
workloads and correctness checks. Baseline and repeat data live in separate
artifact directories. Immutable commits are recorded before execution. Normal
master, pull-request and weekly calls keep their previous workload and checkout
selection; the optional comparison has its own concurrency group.

Either repository can supply a baseline independently. If only one comparison
ref is provided, the other repository stays at its candidate revision in every
pass. When both are provided, the baseline uses both baseline checkouts. Source
directories and output paths are explicit; the script never edits source or
switches a branch between measured passes. Compare workloads and runtime
dependencies too: older hosts may require a matching older Afterglow revision.

This supports investigating implementation changes without comparing unrelated
machines or silently changing the workload. Repeated tails and allocations still
need inspection: a single favorable median does not establish an improvement.
The workflow fails on oracle/test errors, never on a noisy timing threshold.

Subscriber reports also record a wall-clock `:measurement-window` around the
measured tick/fast-client-drain interval. Use that interval when inspecting a JFR
allocation recording. The process first runs a deliberately expensive scalar
oracle; including its allocations would incorrectly attribute oracle cost to
the production typed pipeline. JFR-weighted allocation samples locate candidate
code paths; they do not replace the recorded thread-allocation counters or an
uninstrumented matched timing run.

The [mixed lighting controls profile](performance/lighting-controls.md) measures calibrated source processing, output envelopes, budgeting and activity scoring on moving heads and a changing LED strip.
