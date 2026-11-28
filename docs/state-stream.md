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

`tolglow.studio.output-isolation-test` checks the lighting side against a real
show and output scheduler. Clients that never or slowly acknowledge, 32
connected clients and a query sampler that blocks for the whole run leave every
published packet identical to a run with no clients. A sampler blocks the one
delivery worker, so the server's other Studio clients wait with it
([#906](https://github.com/tolgraven/tolglow/issues/906)). See
[output presentation](output-presentation.md#preview-and-studio-consumers).

## Browser frame lanes

Studio splits high-rate subtrees from each accepted view into frame lanes
outside re-frame app-db (`src/studio/tolglow/studio/frame_stream.cljs`):

- `:room` owns `[:output :values]`, the emitted fixture values only the Room
  renders, and `[:draft-output :values]`, a staged scene draft's audition
  values.
- `:clock` owns every field the show host changes on each frame: frame number,
  show time, transport beat and time, schedule beat, frame compute time and
  output-scheduler status.
- `:remote` owns `[:subscriptions]`, the results of remote queries such as the
  timeline playhead, pad previews and Room scene signals.
- `:connections` owns the parts of `:connection-status` that change on every
  frame while the Connections page is open and audio or a sync source runs:
  capture features, waveform, metrics, sample and event counters, and the
  sync clock projected at each commit.
- `:signals` owns signals sampled on every frame while audio capture runs, on
  any view: the patchbay's observed sources (`[:patch :sources]`), audio
  energy (`[:audio/energy]`) and lighting activity
  (`[:output :lighting-activity]`).

The stream event joins the lane values back into the projection to apply a
patch. When every change lies inside a lane subtree, which is true of an idle
show and of a frame whose cues only move fixtures, the event writes nothing to
app-db: no subscription over app-db recomputes and no app-db view re-renders.
Otherwise it installs the remaining view in `:server-projection`, whose token
then names that message. Its `:fx` install one whole frame per lane, send the
ACK and notify lane readers, in that order: reader work never delays the next
server frame. A lane frame is an immutable map with the token, the token of the
last app-db view write, show identity, revision, frame number, show time,
master, blackout, cut intents, atmosphere and the typed parts present. If a
lane does not hold the current token and app-db view, the next patch requests a
full view.

Views read lane fields through subscriptions. `:studio/clock`,
`[:studio/clock-part part]` and `:studio/transport` (the committed transport
with its per-frame beat and time) are in `clock.cljs`; `:studio/remote-result`
reads one query's result. Event handlers inject `:studio/clock` or
`:studio/remote`. Connections readers use `:connections/live` (with
`connections/with-live`); signal readers use `:studio/signals` (presented),
`:studio/signal-values` (server types) or the `:studio/signals` coeffect.
Each published lane frame resets one Reagent atom, so readers
re-render on animation frames, only when their own value changes, and not
while the page is hidden. Subscription periods of `{:beats n}`, `{:bars n}`,
`{:phrases n}` and `{:frames n}` follow the `:clock` lane.

A Room attaches to the lane with its owner (`:perform` or `:rig`) when its
engine starts and releases it on unmount. Delivery is coalesced to one frame
per animation frame of the window showing the Room, newest first. It runs only
while the engine reports the Room active: visible, in the viewport and focused,
or shown in another window. Resuming delivers the newest frame, never a
backlog. Only the Perform Room shows an audition, from the same lane frame, so
each frame reaches the engine once. The Room component's Reagent arguments hold
only structure (rig, editor, identity, revision, instances). A show frame
therefore updates the engine without rendering the Room component. The engine
receives a lane frame only when its identity and revision match that
structure. A frame that arrives ahead of a rig or cue change waits for the
render that delivers it. The engine-facing `update(frame)` data is unchanged.
A consumer that throws is logged once per failure and then counted.

Known gap: when Studio is hidden, its animation frames stop. A popped-out Room
window flushes Studio renders from its own frames. A picture-in-picture Room
has no such window: its lane frames stop with Studio's frames, as before lanes.

Measured in headless Chrome 155 with the Metal GPU on the M4 Pro
(`dev/studio/room-frame-cost.mjs`, Perform view, about 29 frames a second;
before is master 50260e75, after is #790):

| Per received frame | Idle show, before | Idle, after | 96 heads and 3 cues, before | Dense, after |
|---|---|---|---|---|
| Components rendered | 85.2 | 6.6 | 91.2 | 6.6 |
| Reaction runs (development build) | 72.9 | 15.4 | 91.6 | 15.6 |
| DOM mutations | 56 | 13 | 57 | 14 |
| Main-thread task time | 5.53 ms | 4.46 ms | 6.81 ms | 5.62 ms |

What still renders on an idle frame is the pad whose preview animates, the pad
grid, the pad clock and the frame-time readout.

With audio capture running (`tolglow.qa.timeline`, synthetic PCM), the
`:connections` and `:signals` lanes stop every frame from writing app-db. In
the same setup, development build, with counters on (master bce22156 → the
follow-up in #790), each received frame on Perform rendered 117.9 → 24.5
components and ran 113.9 → 29.3 reactions, with 87.1 → 44.7 DOM mutations.
What remains is mostly the timeline's section and meter readouts, which
follow the `:remote` lane. The idle demo show stays at about 6 components
per frame.

## Panel subscriptions

Panels subscribe to their own slice of app-db (`db_slice.cljs`, see
[Studio subscriptions](studio-subscriptions.md)), so a real app-db change
re-renders only the panels that read the changed paths.
`dev/studio/panel-render-count.mjs` counts components rendered per app-db
change, by component. Measured on a development build in headless Chrome 155
(Metal), master bce22156 → #790 follow-up, per app-db change:

| Scenario | Before | After |
|---|---|---|
| Perform, idle demo show, `param/set` through the API | 102.2 | 19.8 |
| Perform, idle demo show, fader keyboard edit (draft, command, ACK) | 93.1 | 7.7 |
| Connections, idle demo show, `param/set` through the API | 55.0 | 16.6 |

About six of the components counted per change (pad grid, animating pad,
pad clock, frame time) render on every frame anyway and share the commit. A
parameter edit now re-renders the Perform board and the card that shows the
parameter, the Room scene (its structural input carries the revision), the
footer's revision and the modulation panel. Remaining whole-show readers
are listed in #790. Headless results are not foreground or iPad evidence. Headless results are not
foreground or iPad evidence. `dev/studio/room-frame-lane-qa.mjs` checks Room
delivery, the offscreen pause, the resume frame and the rig editor's Room
against a capture-only host.
