# Owned JVM REPL sessions

The studio REPL evaluates submitted Clojure against the actual application JVM.
It is intended for the trusted operator/developer, including Codex driving a
show through the same runtime APIs. It is not an execution sandbox. Opening the
panel, completing a symbol, reading documentation, restoring history or loading
scratch text does not evaluate that text.

The service lives in `tolglow.studio.repl`. `tolglow.studio.repl-manager` owns up
to eight named sessions and is part of the connection bundle. The Ring layer
and re-frame popup are adapters around these services; their UI behavior needs
separate browser verification. Full-codebase file editing, branch/worktree
isolation, git actions and a Docker terminal are separate development services,
not capabilities implied by a working REPL.

## Lifecycle and runtime access

```clojure
(require '[tolglow.studio.repl-manager :as repls])

;; Construction is inert: no namespaces, workers, callbacks or evaluation.
(def owner
  (repls/create-manager
    {:runtime-supplier (fn [] @runtime-holder)
     :session-options {:queue-size 16 :max-output-chars 65536}}))

;; The default main session is saved metadata until an explicit operation opens it.
(repls/snapshot owner "main")
(def receipt
  (repls/submit! owner "main" "(+ 1 2)"
                {:on-event (fn [event] (consume-stream-event event))}))
@(:completion receipt)
;; => {:status :completed, :result "3", :request-id ..., :interrupt-requested? false}
```

Each live session has a unique owned namespace and debug namespace. Its runtime
supplier runs once at opening and the returned handle is available as `runtime`
in that namespace and as dynamic `tolglow.studio.repl/*runtime*` during evaluation.
Optional `:bindings` map unqualified symbols to live values. The supplier is
provided by the host after its runtime has been constructed, so these services
do not import the main studio namespace or introduce a circular dependency.

One owned daemon worker evaluates each session serially. Different sessions can
run concurrently. Default bounds are 16 queued requests, 131072 source
characters, 256 forms per request, 65536 streamed text characters, 8192 printed
characters per individual result, and 100 completed history entries. Scratch
buffers are limited to 32 names. Printing also bounds collection length and
nesting. These bounds control the service's queue and output, not the memory,
CPU, threads or external side effects of arbitrary user code.

Queue-full requests return `{:status :rejected :reason :queue-full}` and are not
evaluated. Closed sessions/managers refuse new work. Request IDs include a fresh
session-generation token, so reopening a named session cannot reuse an old
request identity. A client may instead supply a nonempty `:request-id` of at most
256 characters; every event and history entry preserves it. IDs already active,
pending or retained in history are rejected with `:duplicate-request-id`, without
rerunning the source. This duplicate window is bounded by retained history.
Namespace changes made explicitly with normal REPL code remain
possible; closing removes only the namespaces actually created by this service.
It does not remove or undo application namespaces that the operator modified.

`connections/repls` returns the current manager. Connection save/export writes
its portable state to `:integrations :repl`; prepare-restore validates it before
resource publication. The replacement manager remains inert. The old manager is
retired outside the show-owner lock, and new requests through that retired
manager fail. The HTTP layer must also guard runtime/show epoch identity, as it
does for other command surfaces. An already running arbitrary JVM form is not
made harmless merely by replacing the show model.

## Streaming and completion

`submit!` accepts an optional per-request `:on-event`; a session can also have a
global observer. Events carry `:session-id`, `:request-id` and a monotonically
increasing session `:sequence`:

| Type | Payload |
| --- | --- |
| `:queued`, `:started` | Request lifecycle |
| `:out`, `:err` | `:text`, at most 4096 characters per chunk |
| `:result` | `:value` printed text, `:form-index`, `:truncated?` |
| `:output-truncated` | `:limit`, emitted once for that request |
| `:error` | `:class`, `:message`, printed `:data`, final `:status` |
| `:done` | `:status`, result/error summary and `:interrupt-requested?` |

Callbacks run outside state locks and CAS retries. Their failures are recorded
as bounded observer diagnostics and do not cause duplicate evaluation. Output
from inherited writers after a request finishes is not attached to a later
request. The HTTP adapter can stream a submitted request directly over SSE;
the service does not retain an unbounded event replay log.

All forms in a request run in order until completion or the first error. `*1`,
`*2`, `*3` and `*e` are retained as live, session-local values. Reader evaluation
(`#=`) and custom tagged-reader callbacks are disabled during source reading;
normal submitted Clojure evaluation remains fully privileged. Code execution
does not happen during save-state validation.

`complete` inspects loaded namespaces, aliases, Vars and classes. It does not
evaluate forms or require arbitrary namespaces. `lookup` returns loaded Var
metadata: candidate, namespace, name, kind, documentation, printed arglists,
source filename and line. Completion returns a bounded vector of those maps.
This is metadata completion, not a static-analysis guarantee for every local
binding at a Neovim cursor position.

The Ring/frontend contract uses `GET /api/repl?session-id=main` for status,
direct SSE `POST /api/repl/eval` for explicit evaluation, and separate session,
interrupt, scratch, completion and documentation routes. Runtime/epoch guards
belong to the host routes; aborted clients must not redirect old stream events
into a newer session/window.

## Interruption and shutdown

`interrupt!` targets the current request, optionally requiring its exact request
ID. It returns `:interrupt-requested` with `:cooperative? true`. It uses Java
thread interruption, never `Thread.stop`. Sleeping, waiting or otherwise
cooperative code can terminate with `:interrupted`. Code that catches or ignores
interruption can continue running and occupy that session's single worker.
An exact request ID that is still queued is removed and completed as
`:cancelled`; a job being admitted is marked `:cancel-requested` and cancelled
before evaluation. `cancel-request!` is an alias for this operation, allowing
HTTP stream disconnects to prevent pending source from unexpectedly running.
If it eventually finishes normally, its final status is `:completed` with an
interrupt-requested flag; the service does not claim it was forcibly killed.

`close!` immediately refuses new submissions, cancels queued requests and asks
the active evaluation to stop. Its wait is bounded. `:closing` means code has
not returned; its namespace remains owned until the worker actually exits.
Managers report `:stopped? false` when a session or opening operation remains.
A REPL can request its own close without waiting on its own thread. Background
threads started by submitted code are not automatically owned or terminated by
this service. A separate process would be required for a true kill boundary.

## Lexical locals without replacing let

The development macro observes ordinary compiler lexical bindings:

```clojure
(let [level 0.7 fixture-id :front]
  ((fn [speed]
     (tolglow.studio.repl/capture-locals!)
     (* level speed))
   2))
```

`capture-locals!` uses `&env` to build a map of bound symbols and their values.
The runtime capture occurs only when that expression executes. Macro expansion
does not evaluate values, and production `let`/`fn` are not rewritten. The macro
returns the owned debug namespace name and captured local names; the actual
values are interned in that debug namespace for subsequent inspection.

Inside an evaluation the current session is bound automatically. A callback
invoked later can explicitly pass its still-open session:
`(tolglow.studio.repl/capture-locals! session)`. A closed session rejects capture.
Use this only in developer code outside pure effects/render workers: capturing
live values is an intentional debug-boundary side effect. Remove the macro call
or use normal code once inspection is finished.

Captured objects, functions, handles, `*1` values and runtime references are never
serialized. Saving the show preserves scratch source and completed history, not
an opaque heap snapshot. Recreate desired bindings by explicitly evaluating
source after load. Durable reactive subscriptions/effect handlers have their
own data/source registration contract described in [reactive.md](reactive.md);
history text is not an automatic startup script.

## Saved schema and verification

```clojure
{:version 1
 :selected-session-id "main"
 :sessions
 {"main" {:schema-version 1
          :history [{:request-id "main:unique-generation:1"
                     :source "(+ 1 2)" :namespace "tolglow.repl.session_..."
                     :status :completed :result "3" :interrupt-requested? false}]
          :scratch {"scratch.clj" "(+ 1 2)"}}}}
```

Validation rejects opaque objects, invalid entry metadata and unsupported states.
Restoring saved text does not recreate old namespace identities, evaluate history
or restore old request queues. A live session must be retired before replacement;
the owner stages a fresh inert manager during full-show load.

The completed JVM checkpoint runs 18 new REPL/manager tests with 106 assertions.
Together with the connection lifecycle suite, 34 tests / 264 assertions pass,
including exact client-ID and queued-cancellation races.
The tests exercise actual evaluation, output bounds, queue rejection/order,
cooperative and ignored interruption, self-close, completion/docs, lexical-local
capture, portable history, concurrent lazy opening, retirement during opening,
and full connection save/restore without replaying source. Browser popup,
Neovim selection evaluation, streaming UI and application-editing workflows
remain separately verified surfaces; do not infer their completion from these
JVM tests alone.
