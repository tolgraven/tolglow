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
| New valid version with preserve/restart policy | Adopt using the existing state/time policy and emit recovery |
| Explicit Retry | Try the same version with current parameters and retained state |
| Faulted one-shot/release reaches deadline | Finish normally; quarantine does not make instances immortal |
| Global model, worker deadline/cancellation or patch error | Keep frame-level failure behavior; never blame an arbitrary effect |

Pinned cues remain pinned. A parameter edit does not silently retry failed code:
send Retry after correcting it, or publish a new version under the cue's reload
policy. Failed attempts never hold stale emission indefinitely.

Known quarantined modulation owners are removed before the next modulation
reconciliation; rejected candidates retain their captured modulation config.
Modulation overlays are computed before effect evaluation, so the frame that
first discovers a new owner failure has already used that frame's overlay.
Dependent effects are not rerun to undo that prior computation.

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
attributes and additive overflow. Physical lighting is never activated. Exact
aggregate evidence is recorded in `work-items.edn`.
