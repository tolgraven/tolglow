# Clock and Live integration boundary

The immutable control clock is in `afterglow.control.clock`. A per-show
`afterglow.sync` manager owns the event queue, monotonic clock and resources
opened by the MIDI, Beat Link and Ableton Link adapters. Protocol acquisition
lives in `afterglow.sync.midi`, `afterglow.sync.beat-link` and
`afterglow.sync.ableton-link`. Creating a manager and restoring its
configuration leave these connections closed.

`tolglow.integrations.live-sync/create-manager` adds a Live clip adapter to
this manager. Its pairing and mapping definitions live in `live-sync-config`,
the HTTP and outbound MIDI executor in `live-transport`, and the pure command
and recording mappings in `tolglow.integrations.ableton`. These functions can
be used from the REPL or a headless show. Studio binds host callbacks and
supplies show time so capture and rendering share a time domain.

These protocols are different:

| Source | What this implementation receives | Qualification |
| --- | --- | --- |
| MIDI Clock | 24 pulses per quarter note, Start, Continue, Stop and Song Position Pointer | Pure decoding and injected capture tests; physical ports unverified |
| Pro DJ Link through Beat Link 8 | Player announcements, beat BPM/phase and optional player status | Actual library packet getters tested; network/hardware connection unverified |
| Ableton Live companion bridge | Song tempo/playhead, selected clip position/recording state, MIDI notes/CC, acknowledged note edits and existing Drum Rack chain feedback | Loopback HTTP and injected LiveAPI tests; actual Max/Live host acceptance unverified |
| Ableton Link through local Carabiner | Session beat, tempo, peers and optional shared start/stop | [Configuration, pause lighting and qualification](ableton-link.md) |

`afterglow.sync.beat-link` registers its listeners when the show starts the
adapter and releases them when that session closes.

## Explicit clocks and source selection

`clock/new-state`, `clock/apply-event`, `clock/select-source` and `clock/snapshot`
are pure. Every input has a source identity and timestamp. MIDI SPP is in six
clock pulses, or one sixteenth note; the first tick after Start/Continue/SPP
establishes the exact position. A bounded median interval history estimates BPM
and jitter confidence. While stopped, ticks may estimate tempo without moving
the playhead. Duplicate and out-of-order ticks do not advance it.

Beat Link status beat numbers are integer track positions, not fractional
playheads. The adapter uses actual beat timestamps for phase, labels its initial
bar-meaningfulness heuristic, and optionally aligns a known 4/4 bar. It does not
claim track position or meter inference. `DeviceUpdate.getTimestamp()` uses
`System.nanoTime()`; the adapter converts packet age into the host time domain.
Announcement timestamps use wall-clock milliseconds, so announcement data uses
the host's observation time instead.

The selected source never changes automatically. An unseen source waits; a stale
source either holds after the configured timeout or explicitly freewheels at
zero confidence. Disconnect freezes it. Selecting `:manual` returns no external
clock so the studio's own transport remains authoritative. Queue overflow emits
`:sync/input-overflow`, marks the affected source lost and requires resync;
missing MIDI ticks are not silently treated as a correct position. Callbacks
must return promptly because the input queue and IO queues are deliberately
bounded.

## Manager API and persistence

```clojure
(require '[afterglow.sync :as sync]
         '[afterglow.sync.session :as session]
         '[tolglow.integrations.live-sync :as live-sync])

(def manager
  (live-sync/create-manager {:now-ms #(logical-show-time)
                        ;; Optional injection seams, no factories invoked here:
                        ;; :midi-factory (fn [config clock-ms] ...)
                        ;; :beat-link-factory (fn [config clock-ms] ...)
                        ;; :live-factory (fn [config] ...)
                        :queue-capacity 256}))
(sync/configure! manager
  {:selected-source :midi-clock
   :midi {:enabled? true :input-index 0 :expected-input-name "Saved port name"
          :control? true}
   :live {:mappings {36 {:cue-id :red :mode :held}
                     37 {:cue-id :flash :mode :one-shot :duration {:beats 1/4}}}}})
(sync/start! manager {:events! enqueue-event!
                      :commands! enqueue-commands!
                      :envelope! current-control-envelope
                      :resolve-instance! current-instance-for-cue})
(session/clock-at manager next-frame-time-ms)
(sync/stop! manager)
(sync/close! manager)
```

`events!` receives **one event map**; `commands!` receives a vector of semantic
commands. `envelope!` is zero arity and provides the current show/epoch/time and
fresh command ID. `resolve-instance!` takes a logical cue ID and returns its
current activation ID for CC control. Factories return `open! [receive!]`,
`close! [handle]` and, when applicable, `send! [handle bytes]` or
`request! [handle id op payload]`. Native factories are defaults. Resource IO
and host callbacks run outside pure state transitions and manager locks.

Useful additional functions are `midi-ports`, `status`, `select-source!`,
`handoff!`, `ingest!`, `ingest-live!`, `acknowledge!`, `record!` and `flush!`.
`flush!` observes prior input-owner work; it does **not** acknowledge outbound
IO. The `:live/execution` event reports its result separately. `status` contains
`:state`, `:running?`, `:active?`, `:closed?`, configuration, discovered peers,
recording data, metrics and last error/execution. A start returns `:run-id` and
a result promise. Stop is bounded and retains the owner if an injected IO
transport ignores cancellation, preventing unlimited replacement workers.

Portable state is `{:version 1 :config ... :wanted? boolean}`. Use
`validate-config`, `validate-state`, `export-state`, `restore!`. The connections
bundle persists the desired `:wanted?` state and decides whether to reconnect;
`sync/restore!` itself is inert. Configuration retains disabled pairings. MIDI
indices have optional expected-name guards. Pro DJ Link pairing uses device
number plus IP address or MAC; announcements resolve MAC matching. Discovery
alone never chooses a player. Live pairing stores track/clip-slot/rack indices
and optional expected track/clip names; Live object IDs are session-specific and
are not saved as durable pairings. Default canonical settings can be obtained
from `(live-sync/validate-config {})` rather than copied into another implementation.

## Live control, recording and connection ownership

The companion source assets and installation steps live in
[`ableton-live/README.md`](../resources/devices/ableton-live/README.md). It is a
Max for Live device source patch, not a fabricated `.amxd` binary. It must be
opened and saved in an actual licensed Max/Live installation. The current
dictionary-call codec for Live's note editing APIs still needs actual-host
acceptance testing; an uncertain write is never retried using another codec.

The studio posts `{id, protocol-version: 1, op, payload}` to an explicit loopback
endpoint, default `http://127.0.0.1:8750/live`. Acknowledgements distinguish
`executed`, `rejected` and `indeterminate`. An inspect handshake carries the
configured `expected-bridge-id` (default `tolglow-m4l`) and ephemeral
`sync-session-id`. All subsequent requests carry those identities. Incoming
callbacks use `{protocol-version, bridge-id, sync-session-id, event-id, kind,
status|event|mode}`. `ingest-live!` admits only the currently open bridge and run,
with a bounded duplicate-event history. Old queued callbacks retain their old
session identity and cannot become commands in a newly loaded show. This is
logical ownership validation, not a substitute for authentication at an exposed
HTTP service.

The pure Ableton bridge maps an 8x8 page to notes, preserves normalized RGB and
supports lighting/Live ownership modes. MIDI cue commands include source note,
source ID, sync-run ID and the logical time when the adapter received the MIDI
message. Set `:live :launch-grid` to `:off` (the default), `{:beats 1/2}`,
`{:bars 1}` or `{:phrases 1}`. Each note mapping can override it with `:grid`.
The show owner schedules launches against the selected transport. Note-off
uses the original launch ID to cancel a pending request or end its exact
accepted instance. Graceful handoff or stop releases held cues and asks the
owner to cancel pending
launches. The host returns receipts through `acknowledge!`. A receipt from a
prior run or an earlier press leaves the current note binding alone.
Polled Live clock samples flag forward jumps beyond what can occur within the
elapsed time at Live's documented maximum tempo. The calculation includes
request round-trip windows and a quarter beat of measurement tolerance.
Smaller forward seeks can look like ordinary progress between polls.
[Live Object Model Song transport](https://docs.cycling74.com/apiref/lom/song/#tempo)

After publishing a control receipt, the host calls
`record! manager stamped-command acknowledgement`. Recording requires fresh,
armed, writable Live recording state. The native CC route also checks automation
arming. Parameter velocity lanes use ordinary MIDI note recording. Echo origins and duplicate command receipts are suppressed. Musical cue
launch durations remain beats/bars/phrases; clip time is supplied by Live. Clip
loop position and absolute song beat are distinct so held-note lengths survive
loop wraparound. The adapter writes a cue at the Live clip position corresponding
to actual show admission. The clip-write request carries `:cue-timing` with the
raw gesture, grid, due beat and actual admission. The show receipt retains this
timing for later comparison. Live's MIDI note stores its clip position and duration.

Exactly one recording route is selected:

- `:bridge`: actual companion acknowledgements for Clip note editing. Launch
  uses a short provisional note, then end adjusts its exact note ID using the
  activation identity and absolute song time. Timed launches include their
  beat length. Scalar parameter samples occupy separate editable velocity lanes.
  Connections can pair either a Session slot or an Arrangement clip and place
  finished clips at a chosen song beat.
- `:midi`: sends actual note/CC bytes to the configured MIDI output. This reports
  delivery only. Live determines where it records the incoming bytes.

DrumPad exposes no color setter and its name is read-only. The companion updates
existing chains under pads and reports nearest-palette colors/empty-pad limits.
It does not invent a `DrumPad.color` API. Abrupt process or hardware loss still
requires the host's disconnect/lease handling; graceful stop alone cannot prove
cleanup in a crashed external process.

## Evidence and remaining validation

Focused tests execute the actual clock transitions, musical mappings, manager
workers, real Beat Link `Beat`/`DeviceAnnouncement` getters on constructed
datagrams, actual Java HTTP against an owned local server, and injected MIDI/
Live resource seams. They cover explicit acquisition/cleanup, failed acquisition,
cancellation, selected-peer filtering, queue overflow, held-note acknowledgement
races, recording gates, route exclusivity and callback session isolation. They
do not open physical MIDI ports, send Pro DJ Link broadcasts, or use Live.

Verified Beat Link 8 source APIs:

- [DeviceUpdate timestamp and getters](https://github.com/Deep-Symmetry/beat-link/blob/v8.0.0/src/main/java/org/deepsymmetry/beatlink/DeviceUpdate.java)
- [Beat data](https://github.com/Deep-Symmetry/beat-link/blob/v8.0.0/src/main/java/org/deepsymmetry/beatlink/Beat.java)
- [Device announcements](https://github.com/Deep-Symmetry/beat-link/blob/v8.0.0/src/main/java/org/deepsymmetry/beatlink/DeviceAnnouncement.java)
- [BeatFinder](https://github.com/Deep-Symmetry/beat-link/blob/v8.0.0/src/main/java/org/deepsymmetry/beatlink/BeatFinder.java), [DeviceFinder](https://github.com/Deep-Symmetry/beat-link/blob/v8.0.0/src/main/java/org/deepsymmetry/beatlink/DeviceFinder.java), [VirtualCdj](https://github.com/Deep-Symmetry/beat-link/blob/v8.0.0/src/main/java/org/deepsymmetry/beatlink/VirtualCdj.java)

The adapter holds an exclusive lease over those library singletons and refuses
components already running under another owner. Optional VirtualCdj status mode
actively announces on the network and may wait for peers; acquisition is explicit
and stop interrupts it. Actual deck interoperability, physical MIDI timing,
Max/Live installation and note editing, Link/AbleSet multiple-computer authority,
and session-wide clip/audio observation remain separate acceptance work.
