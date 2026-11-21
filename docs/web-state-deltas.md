# Browser state deltas

Studio's show-state endpoint now supports versioned deltas. The re-frame client
requests `/api/state?projection=delta-v1`; initial requests receive a full view.
Following requests send the last accepted `base` token. Unchanged authoring data,
fixtures and instance fields are then omitted instead of being resent after
every parameter edit. The existing foreground/background poll intervals remain
100ms/1500ms. This change does not introduce a WebSocket server; the envelope can
also be carried over a future WebSocket or SSE connection.

Plain `/api/state` remains a complete inspection API. Conditional inspection
queries using runtime, epoch and revision still omit unchanged authoring data.
Neither interface is a serialized runtime shell or the complete show-file format.

## Envelope and recovery

The protocol is `tolglow-state/1`. A full envelope has `kind: "full"`, `token`,
`base-token`, show identity and `view`. A patch has `kind: "patch"`, the exact
`base-token`, a new `token`, the same show identity and `changes`:

```json
{"protocol":"tolglow-state/1","kind":"patch",
 "base-token":"previous-opaque-token","token":"next-opaque-token",
 "identity":{"runtime-id":"process","show-id":"main","epoch":0},
 "changes":[["set",["instances",0,"params","level"],0.8],
            ["remove",["status","error"]]]}
```

Paths contain JSON object keys as strings and array indices as integers.
`set` with `null` differs from `remove`. Object keys such as `rig/aim` retain
their namespace through actual JSON encoding/decoding. When an array changes
length or its identifiable records change order, the array is replaced as one
value. Indexed edits only apply to the exact acknowledged base. Operations are
bounded at 4096; when a patch exceeds that bound or its encoded size exceeds a
full envelope, the server sends the full view.

A missing or evicted token, different runtime/show/epoch, or invalid patch causes
a full refresh. The client keeps the last complete view visible while recovering.
Duplicate patches do not apply twice. Replies carry the request's base, and the
client also captures its current token when issuing each request: late replies,
including full replies from an earlier server process, cannot replace a newer
accepted baseline. Connection failures request a fresh full view on reconnect.

## State and concurrency

The frontend stores the authoritative `{token, view}` in `:server-projection`.
It applies pending human parameter drafts only to `:show`, after patching the
server baseline. A server delta cannot accidentally include an unacknowledged
knob value as its base. Drafts retire when their acknowledged revision arrives.
Re-frame still receives a complete persistent map; it can reuse unchanged
subtrees while subscriptions react to changed values.

Each runtime owns one inert cache, cleared on stop. It retains at most eight
snapshots and 16MiB of serialized view bytes. This is serialized-size accounting,
not a claim of a 16MiB JVM heap limit. Oversized views remain full responses and
are not retained. There is no per-browser history or outbound frame queue.
Concurrent requests retain their explicit bases; slow browsers simply request
the newest projection and recover if their base has expired.

Projection, comparison and JSON encoding happen on HTTP request threads.
Rendering workers and the physical output publisher continue using complete
immutable context and never compute network diffs. The cache lock is separate
from the lighting owner lock. This reduces wire bytes; it does not eliminate
snapshot traversal or JSON encoding costs on the server.

## Verification

The backend suite exercises real Ring responses and commands, actual JSON
serialization, removals/null, record reorder, epoch/runtime replacement,
concurrent requests, eviction, bounds and cache cleanup. The frontend suite
exercises protocol application and registered re-frame transitions, including
optimistic edits, acknowledgements and stale full responses. Browser and payload
measurement results are recorded below after execution.

The test command now compiles the Node test bundle without shadow-cljs autorun
and runs Node explicitly. An intentionally failing temporary assertion was used
to verify that `npm run studio:test` exits 1; the probe is not retained. This
avoids shadow-cljs compilation returning success after autorun prints failures.

Reproduce wire measurements with
`clojure -M docs/benchmarks/state-delta-bench.clj`. Its actual Studio runtime uses
1000 patched RGBW pixels, 1012 cues and one active effect, alternates parameter
edits and ordinary ticks, and compares full, conditional and delta Ring bodies.
The measurements exclude physical output, network transport and browser work.
