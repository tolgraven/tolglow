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
and member leases and bodies follow it from the next frame. Autopilot
decisions and standalone HDR values also read the running revisions' samples
while a reload is pending, so an accepted candidate reaches them one frame
later. See [spatial cues](spatial-show.md).

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
