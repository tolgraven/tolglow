# JVM reactivity and durable REPL registrations

`afterglow.reactive` supplies an explicit JVM control-plane owner with source
ratoms, tracked reactions, cached subscriptions and effect watchers. It does not
change the pure effect context contract or install reactions in pixel workers.
The browser continues to use actual ClojureScript re-frame and Reagent.

The JVM module is an implementation of the relevant reactive semantics, not a
copy of the full Reagent browser library. It provides normal `IDeref`, `IAtom2`
and `IRef` source behavior, an explicit batch boundary, and portable registration
data. It does not provide a React component renderer, automatic UI mount/unmount
reference counting, or a hidden global scheduler.

## Sources, derived values and one owner cycle

```clojure
(require '[afterglow.reactive :as rx]
         '[afterglow.reactive.clock :as clock])

(def reactive (rx/create-system initial-show-model))
(clock/register! reactive)
(def intensity (rx/ratom reactive ::intensity 0.7))

@intensity                         ; 0.7
(reset! intensity 0.5)             ; immediate source value, just like an atom
(swap! intensity + 0.1)            ; callback runs once, outside CAS retries

(rx/reg-sub! reactive ::level
  (fn [{:keys [db]} _]
    (get-in @db [:variables :level] 0.7)))
(def level (rx/subscribe reactive [::level]))
@level
```

The registration above is an opaque runtime function; use the durable forms
below before saving a show. Source values may be ordinary immutable Clojure
data. Custom source values and captured bindings must be portable EDN when the
show is exported. Mutable Java buffers or objects are not immutable snapshots.

`publish!` freezes every source under one owner lock. It compares source values
with the previous published snapshot, marks direct consumers dirty, and
propagates changes through the dependency DAG only when derived values differ.
A raw `db` subscriber runs when that complete map changes; consumers of its
selected value do not run if that selected value stays equal. Conditional
dereferences replace the previous dependency set, so unused branches stop
invalidating the reaction.

The host cycle is:

1. Drain `rx/drain-actions!` and apply those semantic payloads through the common
   command reducer, assigning current command IDs, epoch, source and time.
2. Advance the show from the authoritative frame/transport snapshot.
3. Call `(rx/publish! reactive resulting-show-model)` on the owner thread.
4. Publish the resulting model/output projection. Actions produced by the
   reactive batch remain queued until the next owner cycle.

Neither `publish!` nor a reactive handler launches a render worker or executes a
lighting command. The shell owns that integration. A source write during a
reaction changes the source immediately, but all derivations in the current
batch continue to see the captured value; the next batch observes the change.
Concurrent writers wait for the owner lock, so two reads in one reaction cannot
see opposite sides of a concurrent reset.

The complete derived graph, watcher comparisons and emitted action data commit
together. If a derivation or effect handler throws, that graph/action batch does
not commit. Immediate source writes, external I/O, or arbitrary side effects
performed by user callbacks cannot be rolled back. Return semantic actions
instead. Ordinary `add-watch` callbacks on derived signals run after graph
commit; exceptions from those callbacks do not undo that committed graph.

## Cached and form-2 subscriptions

```clojure
(rx/reg-sub! reactive ::scaled
  (fn [{:keys [subscribe]} [_ factor]]
    ;; Outer function runs once for each cached query.
    (let [level (subscribe [::level])]
      (fn [] (* factor @level)))))

(identical? (rx/subscribe reactive [::scaled 2])
            (rx/subscribe reactive [::scaled 2])) ; true
```

A handler may return a value directly; no inner function is required. If it
returns a function, that zero-argument function becomes the tracked computation
for that cached query. The outer function is rerun only after an explicit
registration replacement or graph reconstruction. Query parameters are portable
data and are part of cache identity.

`reaction` creates a named transient derivation with a zero-argument function.
It has the same tracked dereference semantics but cannot be saved as an opaque
function. Export reports an error until transient reactions are disposed or
their definitions are represented by durable registrations. This avoids silently
dropping user work from a supposedly complete saved show.

`dispose!` removes a shared signal from the graph/cache. Existing holders become
invalid, and a later subscription creates a new handle. There is no implicit
component reference count: the owner decides when a shared subscription is no
longer in use. Cycles, cross-system dependencies and reads of disposed handles
raise structured errors. `close!` explicitly releases the whole graph.

## Musical subscriptions and effect watchers

`afterglow.reactive.clock/register!` installs durable var-backed subscriptions:

| Query | Value |
| --- | --- |
| `[:clock/beat]` | Floor of the authoritative absolute beat |
| `[:clock/beat-in-bar]` | Zero-based beat index within the current meter |
| `[:clock/bar]` | Zero-based absolute bar index |
| `[:clock/phrase]` | Zero-based absolute phrase index |
| `[:clock/phase]` | Fractional progress through the current beat |

These read `:transport :beat` and `:transport :meter` from the supplied model.
They do not read a wall clock or assume a fixed four-beat bar. A paused transport
therefore keeps the same beat selection.

```clojure
(rx/reg-fx reactive ::launch-on-beat {cue-id :red-flash}
  (fn [_ beat previous-beat]
    {:op :cue/launch :cue-id cue-id :launch {:mode :one-shot}}))

(rx/watch! reactive ::beat-flash [:clock/beat] ::launch-on-beat)
```

The handler receives `[context new-value old-value]`. It may return one semantic
action map, a vector of action maps, or `nil`. It may also use the context's
`:queue-action!` function. A new watcher fires on the next `publish!`; subsequent
frames with the same selected value do not fire it again. Multiple watchers run
in their stable creation order. `unwatch!` removes one watcher.

Action payloads require `:op`. They cannot contain `:epoch`, `:command-id`,
`:source` or `:time-ms`; those belong to the current host cycle, not saved data.
Typed fields such as `:instance-id` remain available for the persistence layer
to rebind when restoring an entire show.

## Durable REPL definitions

`reg-sub` and `reg-fx` are macros for trusted user-side REPL work. Each retains a
literal function form, its namespace, and explicitly captured portable values.
Capture expressions are evaluated once. They are not inferred from an opaque
Java closure.

```clojure
(let [factor 0.6]
  (rx/reg-sub reactive ::scaled-level {factor factor}
    (fn [{:keys [db]} _]
      (* factor (get-in @db [:variables :level] 1.0)))))
```

A named implementation can instead provide a descriptor directly to `reg-sub!`
or `reg-fx!`:

```clojure
{:id :clock/beat
 :kind :sub
 :implementation {:type :var
                  :symbol "afterglow.reactive.clock/beat"}}
```

The portable schema returned by `export-registry` is:

```clojure
{:registrations
 [{:id :user/scaled-level
   :kind :sub
   :implementation
   {:type :source
    :namespace "user"
    :form "(fn [{:keys [db]} _] (* factor (:level @db)))"
    :bindings {"factor" 0.6}}}]
 :state
 {:cells {:user/intensity 0.5}
  :pending-actions []
  :watchers []}}
```

Watcher entries contain `:id`, `:query`, `:effect-id`, `:initialized?` and the
last observed `:value`. The canonical `:db` cell is not duplicated: the owning
show document already persists that model. Other ratom values, pending actions
and watcher comparison data are retained. Derived function closures and caches
are rebuilt from definitions. Restoring a watcher whose last handled beat is 8
while the transport is still in beat 8 does not launch it again.

`afterglow.reactive.registry/validate-registry` (also exposed as
`rx/validate-registry`) validates the full portable shape without evaluating
code. It checks registration references, rejects nonportable captures and stale
command envelopes, and reads exactly one literal `fn`/`fn*` form with reader
evaluation and custom tagged readers disabled.

`restore!` resolves var references and rebuilds a complete graph before replacing
the active one. Source compilation additionally requires
`{:trusted-source? true}`. Loading arbitrary fixture/show EDN does not grant that
trust; the host must make the trusted user-code restore decision explicitly.
Trusted source is ordinary Clojure and can call code when its handler runs. A
failed compile or initial graph evaluation leaves the previous live graph intact.

## Evidence and current integration boundary

The initial `afterglow.reactive-test` suite runs actual handlers, real worker
threads with latches, and EDN round trips. It verifies source atom semantics,
dependency pruning, form-2 caching, coherent snapshots, cycle errors, queued
actions, batch failure behavior, durable captures, same-beat restore behavior,
disposal and explicit meter subscriptions. No hardware or network is involved.

The module is usable independently. The studio/show owner must call its cycle
API and include the exported registry in full-show persistence; merely requiring
the namespace does not wire it into an application. Browser re-frame remains a
separate presentation boundary, and immutable effect computations remain
separate from both.
