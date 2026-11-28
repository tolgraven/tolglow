# Studio subscriptions

A mounted Reagent component can subscribe to a named Afterglow signal for the lifetime of its view. Query arguments and results keep their Transit types, including keyword IDs and nested map keys.

```clojure
(defn fixture-members [volume-id]
  (let [members @(rf/subscribe [:studio/remote [:spatial/members volume-id]])]
    [:span (str (count members) " members")]))
```

Use `:studio/remote-result` to distinguish a successful `{:value nil}`, loading (`nil`) and a rejected or unavailable query (`{:error ...}`). Studio exposes the musical clock and the `:spatial/scene`, `:spatial/body` and `:spatial/members` queries by default. The visible rig preview shares one scene lease across all volumes. Add IDs for show-specific read models to `[:settings :studio :public-queries]` in the show model. Public names may be registered during the show. An existing client acquires their first valid value at the next publication.

The state WebSocket carries the union of a tab’s queries using `{:op :queries :queries [[id ...] ...]}`. The server shares an Afterglow lease across tabs asking the same query, coalesces updates to at most 40 Hz, and waits for acknowledgment of each delivery. Changing a query set, reconnecting, resyncing, or replacing a show establishes a fresh baseline. Otherwise a packet contains typed changed paths relative to the acknowledged token. A full checkpoint follows 240 deltas by default. A consumer that needs only its selected signals can open `/api/state/stream?view=queries&session=<id>`; unchanged values produce no traffic on this stream. An empty query set, unmounted component, hidden tab, or closed socket releases demand on the relevant producers.

## Browser sources and validation

Use `tolglow.studio.reactions/reg-sub!` to combine a Reagent cursor, a plain watchable atom, existing re-frame coeffects and the application database. Source factories run once on acquisition. Calculation reads track reactive dependencies. Source watches and scheduling end with the last consumer.

```clojure
(reactions/reg-sub! ::gain
  {:sources {:sensor (fn [_] sensor-level)}
   :cofx [[::gain-for :front-truss]]
   :validate #(and (number? %) (<= 0 % 1))
   :fallback 0}
  (fn [{:keys [sources gain-for db]} _]
    (if (:muted? @db) 0 (* gain-for (:sensor sources)))))
```

The optional `:period` takes one positive value in `:beats`, `:bars`, `:phrases`, `:frames`, `:fps` or `:ms`; for example, `{:beats 0.25}` refreshes sampled coeffects at sixteenth-note boundaries. Musical periods follow the show's reported transport beat on the `:clock` frame lane and the meter in app-db. Frame periods follow committed show frames on the same lane. An optional `:sample (fn [query abort-signal] ...)` supplies a Promise or value as `:sampled`. Slow reads have at most one request in flight. The initial `:sampled` is nil. A sampler must pass the signal to its I/O API to cancel work immediately on disposal.

An invalid value or exception leaves the last valid value visible. Supply `:invalid :fallback` to return the configured fallback on each failure. `:validate` accepts a predicate or registered ClojureScript spec. Applications may install an event-commit schema with `validate-app-db!`. When a handler fails, other event dispatches can proceed. Browser diagnostics go to the console, the bounded `[:ui :reactive-faults]` feed and the connected server's effect logger. Inspect the latest received reports with `(tolglow.studio/client-faults runtime)` at the REPL.

See [JVM reactivity and durable registrations](reactive.md) for show-owned sources and handlers.

## Panel slices of app-db

A panel written against the whole db subscribes to a slice of it instead:
`(db-slice/view paths panel & args)` renders `(panel slice & args)`, where the
slice holds only the listed paths, so the panel re-renders only when one of
them changes. List every path the panel and its helper functions read; an
unlisted path reads as absent. An entry `{:path p :select ks}` keeps only the
keys `ks` of each item of the collection at `p`, as the cue grid does for
instances so that a parameter edit leaves the pads alone. Keep the path
vector a constant: it is the subscription query. Derived props, such as a
count or a control map, belong in their own `reg-sub` so the panel compares
the result rather than its inputs. `db_slice_test` renders each sliced panel
from its slice and from the whole db and requires the same markup.

A view rendered in its own window (#669) is that window's current view:
`(db-slice/view-as {[:ui :view] view} paths panel)` puts the override in the
slice and leaves the overridden path out of the subscription, so a main-page
view change does not re-render the window's panels. Perform passes the same
override to its cue grid and Room. Development builds wrap a slice so that a
read of a key no listed path covers warns once (`db-slice/misses`).

The root component subscribes to nothing but the toast. The cue actions
dialog, the Develop view and the added tiles of a saved Perform layout still
take the whole db, only while they are shown.
