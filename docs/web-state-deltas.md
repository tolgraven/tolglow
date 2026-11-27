# HTTP state deltas and benchmark history

The Studio browser consumes the [Transit WebSocket state stream](state-stream.md).
This document records the HTTP inspection endpoint and its original measurements.
An inspector can request `/api/state?projection=delta-v1` to receive a full view,
then supply the last accepted `base` token to request changed fields.

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

The frontend stores the authoritative `{token, view}` in `:server-projection`,
apart from high-rate frame lanes held outside app-db at the same token (see
[browser frame lanes](state-stream.md#browser-frame-lanes)).
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
Unchanged persistent subtrees reuse their normalized wire values, and exact
incremental JSON size accounting avoids serializing the entire library merely
to decide whether a delta is smaller. The cache retains both source and wire
forms; unchanged subtrees are shared across its bounded history.
Rendering workers and the physical output publisher continue using complete
immutable context and never compute network diffs. The cache lock is separate
from the lighting owner lock. This reduces wire bytes; it does not eliminate
all comparison or JSON encoding costs on the server.

## Verification

The backend suite exercises real Ring responses and commands, actual JSON
serialization, removals/null, record reorder, epoch/runtime replacement,
concurrent requests, eviction, bounds and cache cleanup. The frontend suite
exercises protocol application and registered re-frame transitions, including
optimistic edits, acknowledgements and stale full responses. Browser and payload
measurement results are recorded below.

The test command now compiles the Node test bundle without shadow-cljs autorun
and runs Node explicitly. An intentionally failing temporary assertion was used
to verify that `npm run studio:test` exits 1; the probe is not retained. This
avoids shadow-cljs compilation returning success after autorun prints failures.

Reproduce wire measurements with
`lein run -m clojure.main docs/benchmarks/state-delta-bench.clj`. Its actual Studio runtime uses
1000 patched RGBW pixels, 1012 cues and one active effect, alternates parameter
edits and ordinary ticks, and compares full, conditional and delta Ring bodies.
The measurements exclude physical output, network transport and browser work.


### Executed gates and wire measurements

- Backend full parity after the master integration: **485 tests / 3779 assertions**, green.
- Final incremental normalization/accounting and actual Ring gate: **7 tests / 62 assertions**, green.
- Frontend: **143 tests / 764 assertions**, green through the explicit Node runner.
- Optimized build: **148 files, zero warnings**.
- Actual optimized-browser workflow: initial full/patch polling, cue launch and
  parameter editing, missing-base recovery, connection-failure recovery and show
  restart all passed with no page errors. Preview was explicitly paused for this
  transport test; it makes no renderer or physical iPad Safari performance claim.

Retained reports use the same 1000-pixel/1012-cue workload on Java 21.0.13:

| JSON body | Observed bytes |
| --- | ---: |
| Full view, median | 450,198 |
| Previous conditional view during ordinary ticks, median | 3,342 |
| Previous conditional view after a parameter edit, median | 450,198 |
| New delta, minimum / median / maximum | 409 / 4,280 / 5,255 |

All 40 measured updates used patches. Parameter-edit updates were approximately
99% smaller than full/previous conditional responses. Ordinary settled frames
were around 0.4KB; initial effect output changes can produce larger patches.
These are UTF-8 JSON body bytes before HTTP compression, not network throughput.

The [initial report](benchmarks/2026-11-21-state-delta-before.edn) at checkpoint
`16b1e08` measured HTTP projection/diff/encoding at median 31.031ms / p95 110.134ms.
The [incremental report](benchmarks/2026-11-21-state-delta-incremental.edn), after
reusing unchanged wire subtrees and exact incremental size accounting, measured
median 2.295ms / p95 12.615ms. Both timing summaries exclude the first five samples;
35 observations remain. The second run's maximum was 16.526ms. These are small
local JVM measurements, separate from frame-render time, real network latency,
physical outputs and browser costs; they are not a production deadline guarantee.
