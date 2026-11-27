# Live effect errors and recovery

An invalid effect no longer prevents healthy effects from advancing. The shared
coordinator quarantines the failed revision, retains its last valid state and
omits attempted output/intents. A valid new revision or Retry can recover it.

## Existing behavior and the change

The fork already caught Throwable around the entire `send-frame` in
`afterglow.show/rendering-loop`, and separately around notification callbacks.
That kept the scheduler alive, but parallel `generate` calls did not isolate the
offending effect. The declarative engine originally rejected the whole frame on
any compiler/step failure; Studio repeated its previous output.

Recovery now belongs to the shared explicit-state engine. No old-effect adapter
or alternate lifecycle was introduced. Compilation, reload, evaluation, result
validation and rendering failures identify their owning instance. A healthy
frame uses the normal single render path. A failed render resolves again with
one already-evaluated result removed; healthy effect code is never run twice.

## Recovery behavior

| Situation | Result |
| --- | --- |
| Current version throws or returns invalid output | Preserve committed state; discard attempt; store `:fault`; suppress version |
| New candidate fails preparation, compile or evaluation | Store `:reload-fault`; evaluate captured last-good code in the same frame |
| Renderer rejects a candidate | Drop its attempt this frame; captured code resumes next frame |
| Candidate that modulation or a spatial cue follows is rejected at any stage | Every instance renders its captured revision with the last-good modulation and spatial fields in the same frame; other reloads of that frame wait one frame |
| A published revision's programs are still compiling on the show's worker | Every instance of the effect keeps its captured revision everywhere; all adopt it together on the first frame after the programs are ready |
| The worker fails to compile a revision | The engine turns it down at `:compile` on the adoption frame, as for an inline compile failure |
| New valid version with preserve/restart policy | Adopt using the existing state/time policy and emit recovery |
| Explicit Retry | Try the same version with current parameters and retained state |
| Faulted one-shot/release reaches deadline | Finish normally; quarantine does not make instances immortal |
| Global model, worker deadline/cancellation or patch error | Keep frame-level failure behavior; never blame an arbitrary effect |

Pinned cues remain pinned. A parameter edit does not silently retry failed code:
send Retry after correcting it, or publish a new version under the cue's reload
policy. Failed attempts never hold stale emission indefinitely.

Known quarantined modulation owners are removed before the next modulation
reconciliation. Modulation overlays are computed before effect evaluation, so
the frame that first discovers a failure of an owner's current revision has
already used that frame's overlay. Dependent effects are not rerun to undo
that prior computation.

A hot reload is different, because modulation follows the candidate before the
engine has accepted it. While a reload is pending, the show owner samples
random controllers and bound waveforms for both outcomes from the same prior
state and passes the last-good samples to the engine as its reload fallback.
A candidate rejected at preparation, compilation, migration, evaluation or
renderer output then leaves the frame, the controller's seed and schedule and
a held layer priority exactly as they are in a show that never published it.
The owner supplies the fallback only when the outcomes differ; other rejected
reloads keep the per-instance recovery in the table. A running beat-clock
ramp keeps running through a rejected reload: its frame value is not committed
by the rejection, and a candidate reads only the ramped parameters its schemas
accept. A candidate that refuses a ramped parameter's committed value, its
frame value or its target is rejected at preparation. See
[modulation](modulation.md#a-frame-with-a-pending-hot-reload) and
[scheduler](scheduler.md#ramp-semantics).

Spatial cues have the same shape of problem with more at stake: the spatial
pass runs before the engine and shapes the model (scene member leases, ball
bodies, contacts) as well as frame inputs (aim points, sourced parameters)
and the native-layer masks applied after the engine. It therefore reads the
revision each cue runs and commits that frame. While a reload is pending it
also prepares the candidate's fields, from the candidate's definition and the
candidate's waveform samples, without committing them. When they differ, the
owner hands the engine the committed aim points, parameter fields and group
levels as part of its reload fallback and applies the masks of whichever input
set the engine reports in `:reload-inputs`. A rejected revision then leaves
DMX, the spatial view and the member leases as a twin show that never
published it; an accepted one paints its new field on its takeover frame,
where the owner also publishes the candidate's scene view, and member leases
and bodies follow it from the next frame. Standalone and managed HDR values
apply after the engine, so the owner prepares them from both sample sets and
picks by `:reload-inputs` as it does the masks. Autopilot decisions are the
exception: they edit the model before the engine runs, so they read the
running revisions' samples while a reload is pending and reach an accepted
candidate one frame later. See [spatial cues](spatial-show.md).

A scheduled show (`show/start!`, or `:background-compile true` on
`show/create-runtime`) compiles effect programs ahead of need on its own
worker thread, outside the lighting frame (#774). The engine compiles one
program per revision and launch target set. Compiling them inside the frame
took about 0.45 ms each on the mixed 2,048-pixel/64-head rig, so 32
revisions, or one revision launched on 64 heads, made that frame miss the
25 ms deadline.

- **Compiled ahead of launches.** The worker warms these programs:
  - every registered effect's untargeted program;
  - each cue's fixture or group selection;
  - each recorded live-run take's targets;
  - each quantized launch as soon as it is scheduled. One beat at 120 BPM
    gives 500 ms of lead.

  The worker rebuilds that list from the immutable model snapshot whenever
  the registry, rig, scene groups or recorded takes change; the owner only
  compares identities. Each cue's selection is resolved once per cue value,
  rig and scene groups, which is all a selection reads, so unrelated settings
  writes resolve nothing again. Scheduled launches are added one by one as
  they are queued; after 256 additions the next frame rebuilds the tier. This warm tier is sized by the registry, about 55 KB per program on
  the mixed rig, and is kept apart from the 256-entry cache: programs leaving
  it are dropped, so they never evict a running instance's program. Volume
  selections follow moving volumes and are not warmed.
- **A launch never waits.** If a launch's program is not ready, the frame
  compiles it, so the launch renders on its start. A short one-shot is never
  lost, and a burst of grid launches lands on the beat.
- **Pending revisions wait.** Until every program a revision needs is ready,
  the effect's instances keep their captured definition in modulation,
  waveform sampling, spatial cues and the engine alike. Then they all adopt it
  on the same frame. A launch of that effect in the meantime starts on the
  revision its running instances use, compiling at most its own program, and
  adopts the new revision with them, so no frame runs two revisions of one
  effect. It does so only when nothing is lost: every value the older revision
  has must resolve there unchanged, and every value only the newer revision
  has, from the launch or the cue's saved values, must be its default, and
  its targets must reach the same fixtures on both revisions. A launch that
  would lose a value or a fixture, and a pinned launch, start on the revision
  they captured.
- **Order and supersession.** The worker runs first in first out. It skips a
  compile that no frame still needs, such as a superseded revision.
- **Compile failures.** Every compile failure is kept and met once, and
  nothing compiles that program again.
  - A recoverable failure refuses the revision at `:compile`.
  - A StackOverflowError is kept as a plain failure naming it, because the
    stack has unwound and the overflow belongs to that definition's nesting.
  - Any other VM error, such as an out-of-memory error, is rethrown on the
    owner once and then forgotten, so that frame fails as an inline compile
    would and the next request compiles again.
  - A CancellationException is retried once, then treated like a VM error.
  - A program that fails transiently 5 times in a row is still retried, and
    the frame status names it under `:compile-failures` until it compiles.
- **Events.** Each `:effect/reloaded` event carries `:pending-since-frame` and
  `:adopted-frame`.
- **Where compiles stay on the owner.**
  - An unscheduled runtime (the default for manual `tick!`, tests and
    auditions) compiles on the owner before the engine runs, which keeps
    manual ticks deterministic.
  - A show whose GPU expression groups prepare against its compute session
    compiles on the owner too.
  - A function passed as `:background-compile` receives each compile task, so
    tests and replays can step the worker.
- **The extra engine pass.** Once adopted, the takeover runs with no
  compiles. The extra engine pass of a frame that turns a candidate down costs
  about one ordinary evaluation, plus a second render after a renderer
  rejection.
- **Compile cache.** The runtime's compile cache keeps its previous 256
  programs as a retired generation, so filling it does not recompile every
  running effect in one frame.

Publishing a definition (`submit! :definition`, which Studio's editor, the
REPL and agents' `publish_effect` use) checks the revision once (#784). It
used to validate each effect revision 4 times and compile it twice on the
owner. Its pure checks are now in `tolglow.show.publication`:

- **One check.** Validation, then the effect's compile, then its modulation
  period, in that order. The compile does not validate the same definition
  again. One that captured shared prefab stages is validated once more,
  because the capture differs from what was validated. Afterglow's structural
  publish (version order, dependent cue presets) still runs at commit.
- **Off the owner, ahead of compiles.** With a compile worker, `submit!`
  queues the check there, ahead of the program compiles already queued. The
  check runs against the last committed model.
- **The owner decides and commits.** The owner derives the revision again
  against its current registry: the expected version, an output patch and the
  next version. If the worker checked that same revision, the owner uses its
  verdict and validates and compiles nothing. If an earlier message in the
  same cycle changed what the revision derives to, such as two unversioned
  publications of one effect, the owner checks it again on the worker. The
  owner checks a message itself only if that second check is stale too.
- **At most one cycle.** The owner holds a publication whose check is not
  ready for one owner cycle. If the worker has not checked it by then
  (stalled, or busy with a long compile or warm-tier build), the owner checks
  it inline, at about 0.1 to 0.4 ms. A publication submitted between frames
  is normally checked before the next one, since a small effect checks in
  under 0.1 ms.
- **Only what depends on it waits.** These wait with a held publication, in
  submission order:
  - later publications;
  - registry and page edits (`:cue/put`, `:cue/delete`, saved cue
    parameters, `:page/put`, `:page/delete`), `:instance/retry` and
    `:show/restart`;
  - messages that publish or restore (agent cues, aim paths, spatial cues,
    compositions, imports);
  - launches, immediate or quantized, of the held effect or cue, or of a cue
    the show does not know yet.

  Everything else is applied in the same cycle, in order: blackout, master,
  transport, parameters, gestures and their ends, other controller input, and
  launches of other cues. Revision numbers, the order of dependent messages
  and their acks are unchanged.
- **Queue capacity.** Held messages count against the queue's 256 slots, so
  a stalled check pushes back on submitters instead of growing a list.
- **Time.** A held message is applied at the time of the cycle that applies
  it, at most one cycle (25 ms at 40 Hz) after it would have been. Quantized
  launches keep their scheduled beat.
- **Unchanged.** The acks, refusal messages and codes, and the error order
  are the same with and without a worker. An invalid revision leaves the last
  valid definition running. `stop!` answers a held publication with "Show
  stopped".
- **Where it runs on the owner.** An unscheduled runtime (no
  `:background-compile`) checks on the owner, once, so manual ticks and
  replay stay deterministic. A function passed as `:background-compile`
  receives the check as a task marked `programs/ahead?`, so tests can step
  it.

Measurements: [reload takeover benchmark](benchmarks/README.md#hot-reload-takeover-frames).

Full backups preserve faults and event history. Restore remaps active diagnostic
instance IDs/epochs; historical events remain historical evidence. Restore does
not execute a failed version to recreate the saved image.

## Events, subscriptions and Retry

The bounded model journal uses `[:variables :runtime :events]` (latest 64) and
`[:variables :runtime :event-sequence]` (monotonic). Event types are
`:effect/failed`, `:effect/reload-failed` and `:effect/recovered`. Failures identify
show/epoch, logical time/frame, instance/cue/effect/version, stage (`:reload`,
`:compile`, `:evaluate`, `:result`, `:output`), message and bounded cause/source
details. A suppressed version emits once, not every frame.

Fresh runtimes register portable subscriptions that survive normal full-show
registry restoration:

```clojure
(afterglow.reactive/subscribe (tolglow.show/reactions runtime)
                             [:runtime/effect-events])
(afterglow.reactive/subscribe (tolglow.show/reactions runtime)
                             [:runtime/effect-faults])
```

Existing reactive watchers may return ordinary commands for the next owner cycle.
Use sequence numbers when processing only newly appended history. Retry uses the
same queue as other controls:

```clojure
(tolglow.show/submit! runtime
  {:op :instance/retry :instance-id "0:1" :expected-epoch 0})
```

The existing Ring `POST /api/command` accepts the same operation in the configured
wire format. Normal runtime/epoch checks and idempotent command IDs apply.
Snapshots expose instance faults, `:effect-events` and `[:status :effect-logs]`
queue/drop/failure counts. A polished fault inspector/editor remains separate UI
work; the backend supplies real state and the shared retry operation.

## Logging and limits

After commit, the host offers events to a dedicated daemon executor with a
128-item queue. Java logging is the default. `create-runtime` optionally accepts
`:effect-log!`, invoked with each structured event on that worker. A blocked sink
cannot block the lighting owner. Full queues drop log delivery and increment
telemetry; model diagnostics remain. Stop closes this owned resource. No logging
occurs inside CAS retries, pure effects or physical output callbacks.

Ordinary exceptions, assertions and linkage failures are isolated. Cancellation,
interruption, ThreadDeath and VM failures are not user faults. This does not
sandbox arbitrary Clojure: immutable inputs/private buffers and interruption
remain code obligations. Side effects cannot be rolled back and the JVM cannot
safely terminate arbitrary uncooperative code in-process.

## Verification

Engine checks cover retained state, compile/evaluate/reload failures, malformed
results, faulted expiry, retry suppression, renderer rejection, bounded journals,
deterministic replay and serial/worker equivalence. Host checks use actual DSL
division by zero, bounded workers, captured DMX, Ring editing/retry, durable
reactive actions, save/restore/restart, zero-distance aim, typed unsupported
attributes and additive overflow. Physical lighting is never activated. See the [effect isolation issue](https://github.com/tolgraven/tolglow/issues/396) for the verification record.
