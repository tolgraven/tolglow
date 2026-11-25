# Deterministic command and frame replay

`afterglow.runtime.replay` records and replays the **new** control model,
functional engine, and mixed contribution renderer. It is executable regression
infrastructure for this architecture, including typed output, explicit instance
state and owned worker execution. It does not run the old scheduler/effect
closures, prove complete old-show parity, or send output to devices.

## Recorded contract

A schema-v1 recording contains an initial model, optional previous output,
descriptive metadata, and an ordered vector of command/frame inputs. The initial
model includes the current and historical registry definitions, captured running
definitions, parameter values, instance state, seed data, priority/launch order,
show epoch, transport and command retry receipts. Starting partway through a show
is supported; include the previous rendered output to retain movement history.

Each input is one of these forms:

```clojure
{:kind :command
 :command {:command-id :launch-1 :show-id :demo :epoch 0 :source :replay-example
           :op :cue/launch :cue-id :wash :time-ms 0.0}}

{:kind :frame
 :frame {:time-ms 250.0}
 :clock {:time-ms 250.0 :beat 0.5}}
```

The command envelope is passed to the real `control/apply-command`. Accepted,
duplicate and rejected commands are all observable results; a rejected command
does not become a thrown replay error. Frames use the real `engine/advance`,
followed by one `render/resolve-contributions` call. Frame fixtures default to
`[:rig :fixtures]` in the model; explicit frame fixtures are supported. Missing
frame show ID and epoch default to the current model; explicitly stale values
still fail through the engine.

Time is never read from a system clock and beats are never extrapolated from BPM.
An optional `:clock` captures the host's authoritative time/beat **before** the
operation, updating only those fields in model transport. Its time and beat must
be nondecreasing and agree with the operation's time/explicit frame beat. This is
needed when later launches should capture the host's current beat. Tempo, meter
and play/pause changes remain ordinary `:transport/set` commands. A frame may
instead supply a flat `:beat` for sampling without changing model transport;
those two uses are deliberately distinct.

Every completed input records:

- A SHA-256 digest of the entire next model, including registry history, state,
  epoch, frame number, current parameters and retry receipts.
- A digest of the entire last completed portable output: all target IDs,
  projected values and every DMX universe byte, not just visible preview heads.
- Reviewable instance identity/version/state/parameter/lifecycle summaries.
- The exact command acknowledgement and events, or frame intents, in order.

Container tags and sorted map/set content make the digest independent of
insertion order and printer limits. Digests are behavioral checks, not
authenticity signatures. This is an exact comparison for the same numeric and
projection contract; it is not an approximate CPU/GPU comparison. Compiler and
renderer revisions belong in metadata, and intentional semantic changes need a
reviewed baseline update.

## Callable APIs

| Function | Purpose |
| --- | --- |
| `new-recording model {:output previous :metadata {...}}` | Capture a portable starting point; optional second argument. |
| `record model steps compile-effect options` | Execute each supplied operation once, returning `:recording`, final `:model`, and portable `:output`. |
| `append-observation recording step result output` | Append an already computed commit without running the reducer, effect or renderer again. |
| `replay recording compile-effect options` | Reexecute and verify every observation, returning final portable `:model`, `:output`, and `:verified-steps`. |
| `digest value` | Compute the canonical portable-data digest for related diagnostic checkpoints. |

`record` and `replay` accept a three-argument arity without options. Options are
`:map-work` (defaults to `mapv`) and `:render-output`; recording additionally accepts
`:initial-output` and `:metadata`. The compiler is the normal
`definition -> pure-step` function. Metadata is never loaded or evaluated as code.

The injected renderer signature is
`[next-model normalized-frame contributions previous-output] -> rendered-output`.
This lets a caller reuse compiled patches or choose the CPU reference backend.
The default uses compact typed output, honors master/blackout, and passes both
rich and compact prior movement history to the renderer. No executor, compiler
cache, external callback, RNG or physical output destination is created here.

For example, from the paired Tolglow checkout:

```clojure
(require '[afterglow.control.model :as model]
         '[afterglow.runtime.replay :as replay]
         '[afterglow.runtime.executor :as executor]
         '[tolglow.compiler.batch :as batch])

(def rig [{:id :wash :position [0.0 2.0 0.0]
           :universe 1 :address 1
           :channels {:red 1 :green 2 :blue 3 :white 4}}])
(def initial (model/new-show {:show-id :demo :rig {:fixtures rig}}))
(def compile-effect (batch/compiler-for-rig rig))
(defn command [id op payload]
  {:kind :command
   :command (merge {:command-id id :show-id :demo :epoch 0
                    :source :replay-example :op op} payload)})

(def inputs
  [(command :definition :registry/publish
            {:kind :effects
             :definition {:id :wash :version 1
                          :params {:level {:type :number :default 0.6
                                           :bounds [0.0 1.0]}}
                          :outputs {:color {:op :const :value [0.8 0.1 0.2]}
                                    :dimmer {:op :param :key :level}}}})
   (command :cue :cue/put {:cue {:id :wash :effect-id :wash}})
   (command :launch :cue/launch {:cue-id :wash :time-ms 0.0})
   {:kind :frame :frame {:time-ms 250.0}
    :clock {:time-ms 250.0 :beat 0.5}}])

(def captured
  (replay/record initial inputs compile-effect
                 {:metadata {:compiler :tolglow-batch-v1
                             :renderer :cpu-compact-v1}}))

(with-open [pool (executor/create {:workers 2 :timeout-ms 1000})]
  (replay/replay (:recording captured) compile-effect
                 {:map-work (executor/mapper pool)}))
```

The pool is a caller-owned boundary and closes after the replay. For useful
concurrency coverage, replay a show with multiple independent active effects.
Saving/loading the returned plain EDN belongs to the caller; the utility contains
no file/network/device IO. Use the application's approved data reader and size
limits when loading recordings from outside the workspace.

## Live recording and ownership

An owner can call `append-observation` after committing a successful command or
rendered frame, with its exact input, actual reducer/engine result, and last
completed output. Pass the unchanged previous output when recording a command;
only a frame changes physical output. Until the first render, output may be nil.
This API trusts the owner's association of inputs/results and commit order;
subsequent replay verifies that association. It does not itself attach to the
studio, subscribe to publications, or make an atomic commit with another owner.

Projection uses `render/portable-frame`: every compact target becomes ordinary
values and all byte arrays become unsigned vectors. This preserves aim history
outside the bounded UI preview. The recorder never reruns effects to obtain those
values, and its saved data does not alias mutable render arrays. Transferring a
completed frame to an asynchronous recorder still requires the normal read-only
buffer ownership rule until the recorder has copied it.

Plain maps/vectors/lists/sets, EDN scalars and finite floating values are accepted.
Metadata is dropped and Java Float values become their exact Double value for
EDN round trips. Functions, atoms, records, Java arrays in model/state, lazy
sequences and nonfinite floats are rejected. Typed arrays are allowed only as
renderer output, where the projection boundary copies them. Random effects must
keep their seed/PRNG state in definition/instance data; this utility neither
intercepts `rand` nor repairs a nondeterministic effect.

Full model hashing and full output projection are diagnostic work and can be
expensive at 10,000 heads. There is no zero-overhead 40 Hz recording claim. Bound
capture windows or perform a planned capture run; dropping arbitrary operations
breaks the continuity needed for deterministic replay. Renderer behavior depends
on previous movement history, so independent segments need complete checkpoints.

## Failures and regression evidence

Recoverable effect faults are successful isolated transitions recorded with
their portable journal. Healthy effects continue. Fault recordings replay across
serial/worker execution; runtime caller frames are excluded from source traces.
See [effect recovery](effect-fault-isolation.md).

`::replay/mismatch` identifies the first changed step index, operation kind,
changed expectation fields, and expected/actual observations. A renderer-only
change reports `:output-digest`; an instance-state change also reports the model
digest and state summary. `::replay/step-failed` retains the failing step index and
underlying exception/cause data. Neither API returns a partial recording/result
after a failed operation. Callers own any diagnostic retention of earlier work.

The dedicated Afterglow test namespace is `afterglow.runtime.replay-test`. It
exercises actual control commands, engine coordination, the typed IR, mixed
ordinary/typed rendering, and a real bounded two-worker executor with controlled
reversed completion. Its small injected compiler is an explicit seeded test
program, not a replacement for testing Tolglow's authoring compiler.

The checked-in
[`replay_fixture.edn`](../../afterglow/test/afterglow/runtime/replay_fixture.edn)
contains 36 ordered inputs with a fixed initial model, seeded state, all three
reload policies, priority/launch order, parameter edits, rejected/duplicate
commands, release, show restart, a stale release, and one-shot expiry. Recorded
digests and independent expected final channel bytes provide a baseline that
future implementations must match. Additional tests cover portable EDN round
trips, serial/threaded equality, first-divergence diagnostics, previous compact
aim history and detached output buffers.

This lane does not yet capture the entire studio's queue admission, automatic
planner, reactive graph writes, audio features, external sensors, device sessions,
HTTP/UI projections or network timing. Such inputs must become explicit replay
operations or checkpointed data before claiming complete studio replay. The old
Afterglow compatibility gates, long-running deadline tests and physical fixture
verification remain separate work.

## Historical whole-frame comparison

The [offline legacy oracle](../dev/tolglow/legacy_frame_oracle/README.md) replays
one bounded RGBW moving-head scenario through the original Afterglow frame path
and the paired functional runtime. It records the old assignment chain values,
kind resolution and complete universe bytes, then compares the same authored
inputs with the released scalar and batch compilers. The report records exact
byte differences for this scenario. The historical JVM runs separately from the
functional runtime and sends its completed frames to an in-memory capture.
