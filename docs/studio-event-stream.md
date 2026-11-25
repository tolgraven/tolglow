# Live event inspection

The headless owner gathers events alongside the immutable candidate show. Control operations (including nested composition launches), consumed audio and sync input, sensor middleware, command receipts, engine intents and crossed musical boundaries contribute to the committed batch. Discarding an unsuccessful candidate also discards its staged events. Show handlers and the Studio inspector share the same event types, source identity, logical clock and frame identity. A clock event after a jump carries the number of boundaries crossed.

Each inspected row carries a sequence ID and a Transit Date for the wall time at commit. The separate `:time` map identifies show, epoch, frame, show time in milliseconds and musical beat. Source timestamps and confidence are included when provided by the actual producer. The inspector projects a small set of identifying and numeric fields. Full submitted commands, user source, arbitrary parameter strings, handler arguments and raw input histories stay outside the feed.

For a custom effect intent, `:emitter` identifies the effect definition that produced it. The event's `:data` fills in omitted identity fields from the originating instance, including its definition version and cue when available. A one-shot can emit on its final frame and still appear under its effect and instance IDs.

## Subscribe

Open a same-origin WebSocket at `/api/debug/events`. The server sends an initial Transit message even when the history is empty. An acknowledgement signals that the client has consumed all rows through that cursor.

```clojure
{:protocol "tolglow-debug/1" :kind :events :runtime-id "..." :cursor 42 :skipped 0
 :events [{:id 42 :timestamp #inst "2026-11-21T08:00:00Z"
           :category :reactive :type :reactive/dispatch
           :emitter {:kind :subscription :id :clock/beat-in-bar}
           :delivery :observed :subscribers [:show/accent]
           :time {:show-id :main :epoch 3 :frame-number 180 :time-ms 4500 :beat 9}
           :data {:watcher-id :show/accent :registration-id :show/flash}}]}
{:op :ack :cursor 42}
```

An observed reactive read or handler dispatch reports the IDs actually involved. A semantic event can report the IDs matched by the action admission step (`:delivery :matched`). A source without receiver metadata has `:subscribers nil` and `:delivery :unknown`. An empty vector represents a known empty match. Reactive diagnostics are produced after publication and never recursively enter the semantic event batch.

After the first inspector opens, the runtime retains the latest 512 rows. Up to 12 attached sockets each receive at most 64 events per batch and hold one unacknowledged batch. Reconnect with `?runtime=<runtime-id>&after=<cursor>` to replay newer retained rows. Sequence gaps report an exact `:skipped` count. A different runtime initializes a fresh cursor. The frame owner hands the batch to a worker. The handoff retains up to 16 unprocessed batches and counts the IDs of an evicted batch in the next cursor gap. The worker projects event metadata, retains history, encodes Transit and limits each socket to 40 messages per second across its active streams. A headless show has no inspector worker until a client connects.

## Inspect DMX

On the open debug socket send `{:op :dmx/subscribe :universe 1}`. A `:kind :dmx` reply includes the runtime ID, epoch, committed frame number, universe, token and `:bytes`. Patched output uses Transit's native binary tag for the 512 channel bytes. The browser reads the unsigned value for an address at `address - 1`. An unavailable universe returns nil. Each packet is copied before encoding on the worker. This is the committed render output, before an independent output scheduler presents it to hardware.

Reply with `{:op :dmx/ack :token "..."}` to receive the newest available frame. Send another subscribe to change universes or `{:op :dmx/unsubscribe}` to stop DMX deliveries. Event row acknowledgements and packet acknowledgements advance independently.
