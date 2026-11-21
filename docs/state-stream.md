# Studio state stream

Studio pushes committed show state through `/api/state/stream?session=<session-id>`.
Open the WebSocket with the same session ID used by the browser's cue commands.
The Ring route accepts same-origin browser upgrades. Jetty connections and
runtime instances each own their lifecycle.

The first Transit JSON text message contains a full view. Each later patch uses
the exact token acknowledged by this receiver. Map keys retain their original
types. Colours and other tagged records change as a whole. Identifiable vectors
with changed ID order are replaced in one operation.

```clojure
{:protocol "tolglow-state/2" :kind :full :base-token nil :token "A"
 :identity {:runtime-id "studio-1" :show-id :main :epoch 2}
 :view {...}}

{:op :ack :token "A"}

{:protocol "tolglow-state/2" :kind :patch :base-token "A" :token "B"
 :identity {:runtime-id "studio-1" :show-id :main :epoch 2}
 :changes [[:set [:output :values :head-1 :intensity] 0.75]
           [:remove [:settings :example]]]}

{:op :ack :token "B"}
```

Send the ACK after applying the message to the client's authoritative projection.
`{:op :resync}` requests a fresh full view. Reconnecting also starts with a full
view, as does switching show identity or exceeding the 4,096-operation patch
limit. A valid ACK renews that session's held-cue lease at most once a second.
`{:op :heartbeat}` can renew an idle session explicitly.

A frame commit replaces one read-only state reference and signals a daemon
worker when clients are connected. The worker projects output, builds changes
and starts asynchronous socket writes. Connections share the current projection
and reuse encoded messages when they acknowledge the same baseline. Each client
holds one acknowledged view and at most one unacknowledged message. When a
client falls behind, its next delta uses the most recent committed state. Up to
32 clients can subscribe per runtime.

The worker caps delivery at the configured show frame rate or 40 Hz, whichever
is lower. Render and network delays can lower the observed rate. Show time and
frame number describe the captured fixture state. A client can animate its
preview independently between deliveries. A client ACK alone generates no
new publication. Delivery stops when the host closes its stream.

Tests exercise a real JVM WebSocket client against Jetty and the Studio Ring
route. They cover the initial snapshot, cue launch, typed deltas, single-message
backpressure, coalescing across missed frames, reconnect, epoch changes and the
cadence bound. A blocked projection test checks that a frame publisher can
continue while the delivery worker is busy.

## Connections status

The visible Connections page sends `{:op :connections :enabled? true}` on the
show socket. The full view then includes `:connection-status` containing
`:sync` and `:audio`, including the bounded waveform. The next patches use that
client’s acknowledged token. A hidden document or navigation away sends
`{:op :connections :enabled? false}` and starts a fresh, ordinary baseline.
On reconnect the browser requests its current interest again.

The first interested socket attaches watches to the show’s sync and capture
managers. Changes signal the delivery worker even while show frames stand
still. Committed show time advances the displayed clock age, and the committed
audio event count describes events admitted by the show. Restore replaces the
watches with the new managers. Closing the last interested connection drops
the watches and their shared projection cache. Other sockets continue receiving
their own show projection.

MIDI port and audio input enumeration run once on entering the page and on
explicit refresh. Capture, synchronization and routing changes use the existing
commands. Browser verification with a touch viewport is available at
`test/studio/connections_stream_browser.cjs`.
