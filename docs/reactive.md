# JVM reactivity and durable REPL registrations

`afterglow.reactive` runs ratoms, subscriptions and effect watchers on the JVM
show owner. It supplies a consistent immutable snapshot for each publication.
The Studio uses Reagent and re-frame on the browser side. Mounted components
can acquire JVM signals through the [Studio subscription stream](studio-subscriptions.md).

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

A throwing calculation keeps its last validated value. Use a validator and a
fallback when a first value is required. A failed effect watcher discards the
actions it queued in that call. Other watchers still run. The owner records
attributed faults and publishes healthy changes. Read recent faults with
`(rx/faults reactive)`. Exceptions during initial graph construction or staged
restore reject that construction. Immediate user writes and external I/O are
outside the graph transaction.

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

For a direct REPL subscription, call `dispose!` when finished. For a UI, socket,
or temporary listener, use `acquire!`. It atomically returns the initial value,
its availability flag and an idempotent close function. The listener receives
distinct committed changes. Equal queries share their upstream work.

```clojure
(let [{:keys [available? value close!]} (rx/acquire! reactive [::scaled 2] draw!)]
  (try
    (when available? (draw! value))
    (run-until-view-closes!)
    (finally (close!))))
```

The final release removes unused upstream calculations and source watches.
A direct REPL subscription or a show-owned effect watcher also counts as
demand. `reg-ref!` can track a projection of an ordinary atom. `reg-cofx!`
accepts on-demand, frame or background samplers. For example, a background
source with `:interval {:beats 1/2}` samples from the published musical clock
while observed. Closing its final lease cancels outstanding work. An old read
that finishes late cannot publish into a newly acquired lease.

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

## Sample a slow source on an oscillator's cycles

A background coeffect can follow a named oscillator instead of a beat, bar or
phrase. Register the oscillator as data on the show's graph, then name it with
an `:interval` in cycles. Fractional intervals are allowed: `1/4` samples on
each quarter cycle.

```clojure
(require '[tolglow.show :as show]
         '[afterglow.reactive :as rx])

(def reactive (show/reactions runtime))

(rx/reg-oscillator! reactive :groove {:period {:beats 2} :phase 1/4})
(rx/reg-cofx! reactive ::weather
  {:mode :background :interval {:cycles 1/4} :oscillator :groove
   :initial nil :sample (fn [db] (read-weather db))})
(rx/reg-sub! reactive ::weather (fn [{:keys [cofx]} _] (cofx ::weather)))

(rx/oscillator-position reactive :groove) ; 1/4 + beat/2 at the last frame
(def weather (rx/acquire! reactive [::weather] println))
((:close! weather))
```

The oscillator's position starts as `phase + beat / period`, with the period
measured in beats, bars or phrases of the committed transport and meter. A
one-bar period in 7/8 is 7/2 beats. When the meter changes, the oscillator
continues from the position it has reached rather than recomputing from beat
zero, so changing from 4/4 to 7/8 at beat 64 keeps a one-bar oscillator at
bar 16 instead of jumping to about 18.3. Afterglow has no meter map: the change
takes effect at the first frame that reports it, and seeking back past it
extrapolates the current meter. A new show epoch starts again from
`phase + beat / period`. The source samples once when that position enters a
new interval. If a frame crosses several boundaries, it samples once, just as
frame cadences skip missed frames. Tempo changes do not add samples. A seek
inside the current interval adds none; a seek or loop into another interval
adds one. A stopped transport holds the current interval.

Every reader of `::weather` shares one sampler. When the last lease closes,
any read in progress is cancelled. Replaying the same committed frames gives
the same samples. Registering a different definition under the same name, or
removing and registering it again, starts the oscillator again and takes a
fresh sample on the next frame. `reg-cofx!` rejects an unregistered oscillator name. If you
later remove or rename the oscillator, the graph reports a fault naming the
missing and registered oscillators, and readers keep the last value until
the name is registered again. Oscillator and coeffect registrations are setup
code for the show session; they are not part of the saved reactive registry.
[Afterglow reactive coeffects](https://github.com/tolgraven/afterglow/blob/master/src/afterglow/reactive/AGENTS.md#subscription-ownership-and-coeffects)

**Studio parity.** Both clients use the same query. Add `::weather`, or a
subscription derived from it, to `[:settings :studio :public-queries]`. A
Studio component then acquires it through the
[subscription stream](studio-subscriptions.md) and receives each changed
sample. Closing the last browser or REPL lease releases the sampler. Studio
cannot yet create or rename an oscillator, and its browser-side `:period`
offers beats, bars, phrases, frames, fps and ms but not oscillator cycles.
Define the oscillator from the REPL until those controls exist.

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

## Restoring trusted show code after a restart

When a show opens, Tolglow installs its local clock, fault and spatial handlers.
Saved definitions retain precedence for each `[kind id]`. A partial registry
gains any missing built-ins. A stored Var descriptor is resolved against its
locally registered kind, ID and qualified Var name. On live import the running
graph also supplies local registrations. A cold start can admit additional
named implementations with a host-supplied set of qualified symbols.

```clojure
(require '[tolglow.show :as show])

(show/create-runtime
  {:model saved-model
   :reactive-options {:allowed-vars #{'my-show.signals/level}}})
```

To restore literal function forms written in your own web REPL, enable source
compilation in the host when opening that trusted show. The saved document
holds the function form, captured data and namespace context. After the
authoring REPL closes, a new runtime can reconstruct its aliases, referred
Vars and imported classes. Trust is set in host options. The incoming show
document cannot enable it.

```clojure
(show/create-runtime
  {:model saved-model
   :reactive-options {:trusted-source? true}
   :event-callback-options {:trusted-source? true}})
```

The second option controls saved event middleware and defaults to the
reactive trust setting. Set it explicitly to `false` to enable only reactive
source on a trusted host. Compilation runs ordinary Clojure. Keep unrestricted
source disabled when accepting public show files. A rejected staged restore
preserves the active graph and show epoch.

The host tests save a subscription and event middleware through the web REPL,
close the authoring runtime, import the Transit document into a fresh runtime,
change the master level, and verify the restored subscription and event result.
They also cover ordinary built-ins, older partial registries and rejection of
untrusted source. [Show restore tests](../test/tolglow/studio/connections_host_test.clj)
