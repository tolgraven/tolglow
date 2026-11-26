# Callable integration slice

These modules require Clojure and `org.clojure/data.json`; namespace loading
does not start a show or connect to devices/services. They use the shared
command envelope described in `docs/implementation-contract-v1.md`.

## Fixtures

`fixtures/import-ofl [json-or-map options]` and
`fixtures/import-qxf [xml-string options]` return `:valid?`, `:definition`,
`:candidate`, `:diagnostics`, and `:provenance`. Select `:mode` explicitly when
a source has several modes. Options also accept `:id`, `:version`, `:source`,
`:license`, and for OFL `:manufacturer` when known from the library path.

Definitions use the registry's attribute-to-offset channel map, one-based
offsets and coarse/fine pairs. They retain channel capabilities and the complete
source data. SHA-256 identifies source text or deterministic canonical JSON,
as marked by `:checksum-kind`. Geometry/calibration remain `:unverified`.

| Format | Implemented | Explicit limits |
| --- | --- | --- |
| OFL JSON | Ordinary selected mode, recognized intensity/color/position roles, one fine byte, capabilities, source preservation | Matrix/insert blocks, ambiguous repeated attributes and >16-bit channels are errors; unknown control/functions remain unmapped data |
| QLC+ QXF XML | Namespaced XML, selected mode numbering, Group/Colour roles, coarse/fine pairing, capability ranges, provenance | No automatic geometry or physical calibration; unmapped groups preserved; DTD/entities rejected |
| GDTF/MVR/proprietary formats | None in this slice | Require separate adapters/corpus/license review |

No web fixture search/downloader is included. Importing source data is distinct
from adding it to a registry or applying a physical patch. `:definition` is nil
on errors; only `:candidate` remains for repair.

## Live bridge protocol

`ableton/new-bridge` configures note mappings, parameter CC mappings, channel,
and ownership mode. `pad-page` produces 64 pads with logical RGB and packed
Live RGB metadata. These colors are bridge data, not an asserted MIDI color
protocol. `midi->commands [bridge event envelope]` emits shared cue/parameter
commands. Events use `:note-on`, `:note-off`, or `:cc`, 1-based channels and
7-bit values. Note-on velocity zero is release.

`pad-note [layout [x y]]` gives a page cell's Drum Rack note: `:push-64-pads`
follows Push's 64 Pads drum grid, `:rows` the earlier rows of eight.
`map-page` uses the bridge's `:pad-layout`. `tolglow.integrations.live-launcher`
projects a page's 64 pads, sets the launcher track up over a companion
connection and turns run-record takes into a clip of pad hits (#228,
[Live routing](../../../docs/live-routing.md#play-a-cue-page-from-one-drum-rack-track)).

Apply emitted launch commands through the control reducer, then call
`acknowledge-launch [bridge note ack envelope]` to retain the exact activated
instance. If note-off or mode handoff preceded that ack, it emits an end for
that instance. `handoff [bridge :live-or-lighting envelope]` releases held
instances and invalidates old surface feedback without stopping unrelated cues.

`record-command [bridge command ack recording-state]` observes accepted actions.
Recording state supplies booleans `:track-armed?`, `:recording?`, `:writable?`,
`:automation-armed?`, plus `:destination-id` and musical `:beat`. It returns MIDI
events and `:live/record-event` intents. Live playback origins and duplicated
show/epoch/command IDs do not recursively record. A Max for Live or supported
Control Surface bridge must still implement recording-state discovery, clip
writes, rack metadata/colors and execution acknowledgements. No Live version
or physical Push mode negotiation is claimed tested by these functions.

## Audio parameter transport

`audio/value->frequency` and `frequency->value` map schema bounds onto a
logarithmic carrier range. `sine-block [options state value sample-count]` returns
newly owned samples and next oscillator state, ramping frequency and amplitude
while preserving phase. Options include sample rate, frequency range, amplitude
and parameter bounds. It does not play or route audio.

`analyze-block [mono-samples options]` computes RMS, peak, DC, YIN pitch and
confidence, inverse-mapped value, Hann FFT centroid/flatness, optional normalized
spectral flux, and residual energy after fitting a sine/cosine plus DC reference.
Supply `:reference-frequency` from the emitted carrier and `:previous-spectrum`
for flux. Gain and phase changes alone do not imply texture changes. Supported
analysis blocks contain 64–16,384 finite samples; low pitches need long enough
windows. `:window-latency-ms` is only analysis-window latency, not end-to-end audio
latency. Signals are assumed isolated and monophonic; effects that create several
pitches may reduce confidence or confuse a monophonic estimator.

`new-tail [captured-target time-ms options]` captures show/epoch/instance/param.
`advance-tail [tail features time-ms envelope]` returns tail state and bounded
parameter commands only for confident values. It allows delayed pulses until
sustained silence or maximum lifetime, and expires on epoch mismatch. The caller
must retain a target effect snapshot/route; this adapter does not resurrect an
ended runtime instance. Actual audio device routing, plugin behavior and Live
rack-chain creation remain separate integration work.

## Providers

`providers/request [config messages credential]` returns an HTTP request map
with method, full URL, headers and JSON string body. Providers are `:openai`,
`:anthropic`, `:openrouter`, and OpenAI-compatible `:self-hosted`; models are
explicit. Self-hosted endpoints may omit authentication. Neutral messages use
system/user/assistant text; assistant `:tool-calls` contain ID, name and argument
data. A `:tool` message contains `:tool-call-id` and result data. The codec emits
OpenAI `tool_calls`/`tool` or Anthropic `tool_use`/`tool_result` blocks and groups
adjacent Anthropic user blocks. Generic `:tools` declarations contain name,
description and a parameter JSON Schema.

Credentials are deliberately separate from configuration. Use `public-config`
for UI/export and `redact-request` for request diagnostics. `invoke!` takes an
injected function that returns `{:status integer :body string}`. It is a bounded
response convenience; a production transport owns HTTP lifetime, timeouts and
cancellation. The included optional `http-transport! [request on-chunk! options]`
uses Java HttpClient with explicit connect and whole-call timeouts, response size
limits, incremental UTF-8 decoding, no automatic redirects/retries, and a caller
owned `:cancelled?` atom that closes an active body. It makes a connection only
when called. `start-http!` returns a future and cancellation function. Local
HTTP-server tests exercise streaming, cancellation and timeout without contacting
a provider. For incremental parsing, call `new-stream`, then `stream-feed`
for each decoded text chunk, and `finish-stream` at EOF. Events normalize text,
usage, tool fragments and terminal/error markers. Truncated/malformed streams
are errors, and `completed-tools` does not expose partial arguments as finished
calls. Transport code must decode UTF-8 incrementally before passing chunks.

Protocol tests exercise real parsing and math with injected transport, not
external provider availability, billing, authentication, or model/tool compliance.

## Streaming agent loop

`agent/run! [config credential messages tools dispatch! emit! options]` is the
explicit orchestration boundary. It streams text, validates tool names/argument
schemas, dispatches tool operations, appends tool-result conversation messages,
and asks the provider for the next response. It defaults to 8 rounds and 32 tool
calls. Options can inject transport, supply a cancellation atom, set limits and
set the stable run ID. Duplicate tool IDs with identical arguments reuse their
results; reusing an ID with changed arguments is rejected. Malformed/truncated
provider output cannot execute partial tools.

`default-tools` declares `inspect_show`, `publish_effect`, `put_cue`, `launch_cue`,
`stop_instance`, `set_parameter` and `configure_auto`. The supplied
`dispatch! [name arguments metadata]` uses the same semantic registry/control
operations as humans and returns portable result data. Metadata includes run,
round, tool-call and idempotent command IDs. The dispatcher still validates
definitions, parameter bounds, show epochs and auto policy. `configure_auto`
takes an optional `at` (a zero-based `{bar, beat, phrase}` position or
`next-bar`, `next-phrase`, `next-section`): the change is posted on the beat
scheduler as this run's event and applies on that line
([auto show](../../../docs/auto-show.md#on-a-musical-line)). No eval/shell tool is
provided. Event consumers receive `:text/delta`, `:tool/start`, `:tool/result` and
`:agent/done`/`:agent/error`; a disconnected consumer cancels generation.

JSON Schema support is deliberately bounded: types, object properties/required,
boolean additionalProperties, arrays/items, enum, numeric bounds and string/array
lengths. Unsupported constructs reject execution. Domain-specific validation is
not replaced by the schema check. Tests prove an inspect/publish/cue/launch loop
against the real control reducer with scripted provider output. Real model
quality, provider tool compatibility and a complete creative show remain runtime
integration evidence rather than conclusions of those tests.

## Studio configuration, saved conversations and worker lifetime

`tolglow.agents/create-manager` owns bounded chat workers, conversation
history and runtime key objects. `configure!` validates provider/model/endpoint
and limits without network access; `public-config` exposes settings/readiness,
never raw credentials. Pass `:credential` or `:api-key` when changing a key, omit
it to retain the key for the same provider/endpoint, or use
`:clear-credential? true` to remove it. Changing destinations drops the retained
key unless explicitly supplied again. Hosted settings can be saved without a key;
readiness remains false. Reconfiguring does not redirect already queued runs.

The host may supply `:credential-store (credentials/file-store explicit-path)`.
That adapter saves keys separately from the show/project, returning opaque
`credential-UUID` references. Adapter construction performs no IO. Explicit key
configuration writes a private temporary file, forces its content, and atomically
replaces the credential file with POSIX mode 0600. Newly created directories use
0700; existing parent permissions are not changed. Reads reject nonprivate files,
symlink files, invalid schemas and executable/tagged/trailing EDN. The file is a
local plaintext secret store protected by filesystem permissions, not an encrypted
vault. Keep it in the user's application configuration directory outside Git and
show exports. POSIX permission support and atomic replacement are required; use
one adapter per file (concurrent independent processes are not a multiwriter
store). Actual OS-keychain integration is a separate adapter opportunity.

`export-state` produces schema version 1: provider `:config`, `:credential-ref`,
`:selected-conversation-id`, and bounded `:conversations`. `validate-state` is
pure and returns normalized defaults/roles; `restore!` validates then resolves the
local reference. Restore makes no provider requests, starts no workers and replays
no tool actions. Missing/unavailable references preserve settings/history but
report not-ready until the key is configured. No key is inferred from a show file.
Without a credential-store option, keys remain runtime-only and exports have no
credential reference. Explicit clear deletes the referenced stored key; normal
close clears the in-memory key while preserving saved settings/reference/history.

`start-chat! [manager payload callbacks]` accepts `:message`, optional
`:conversation-id`, `:messages` and `:run-id`. Providing a conversation ID appends
to its server-owned history; omission creates a new conversation. Explicit
`:messages` still overrides prior history. The handle and emitted events identify
the conversation. `history` returns conversation data and selection;
`select-conversation!` changes the selected identity without calling a provider.
Only one active run per conversation is allowed, while different conversations
can use the bounded worker pool concurrently.

Callbacks remain web-agnostic: `snapshot! []`, `dispatch! [name arguments metadata]`,
`emit! [event]`, and `done! [result]`. The handle exposes `:run-id`,
`:conversation-id`, `:cancel!` and dereferenceable `:result`. A fresh bounded rig
snapshot and beat/bar/phrase authoring examples become the system prompt; old
system messages are replaced and never persisted as conversation history.
Provider configuration and credentials are frozen when a run is accepted.

Both successful and failed conversations retain text and matched tool-call/result
messages. Active export snapshots include streamed text and dispatched tools,
mark the conversation `:interrupted`, and supply an explicit unknown-outcome result
for an action whose result has not arrived. Such a snapshot must inspect current
show state before retrying an action; it never claims that cancellation undid an
accepted operation. Partial stream text is retained on interruption. History
bounds preserve complete tool-call/result groups and mark truncation. Defaults
are 20 conversations, 40 messages and 262,144 history characters per conversation;
total retained conversation data is bounded to 4 MiB, with older inactive
conversations evicted first. Configurable bounds have fixed validation ceilings.

Full worker queues reject new work. Completion releases capacity and invokes
`done!` once even on ordinary transport/callback failure. `cancel-all!` cancels
runs without closing the manager and waits at most its total timeout, outside its
lock; call it outside show-owner locks before restoring a replacement state.
`close!` cancels runs and shuts down the pool, reporting termination. Ring/SSE,
file-save policy and browser state remain in the host. Fake streamed providers
and temporary fake-key files test persistence, continuation, partial progress,
cancellation, bounds, atomic file replacement and private permissions; no real
provider account or credential was used.

## Causal kick, energy and breakdown analysis

`tolglow.integrations.analysis` is an explicit-state, bounded JVM fallback for
live input. It does not open an audio device. `new-analyzer` returns immutable
state; `analyze` receives one owned mono block and returns the next state, latest
features and event data. Primitive arrays are private to each call; no sample
buffer or atom escapes into the state. Arbitrary input block sizes accumulate
into a fixed analysis hop, so signal results are independent of how a device
chunks its input. A partial hop produces no features/events yet.

```clojure
(require '[tolglow.integrations.analysis :as analysis])

;; The host owns this state, capture timestamps and authoritative transport.
(let [state (analysis/new-analyzer {:source-id :main-input
                                    :noise-floor 0.003 :hop-ms 10.0})
      result (analysis/analyze
              state {:samples mono-samples :sample-rate 48000.0
                     :started-at-ms 1000.0 :received-at-ms 1012.0
                     :clock {:time-ms 1000.0 :beat 2.0 :bpm 120.0
                             :source :midi-clock :epoch 1 :confidence 0.9}})]
  ;; Save (:state result) through a single owner, then publish outside any
  ;; retried swap!/effect transition. No callback runs inside analyze.
  (analysis/publish! subscriptions (:events result))
  (:state result))
```

The example block is 480 samples (10 ms). `:started-at-ms` is the timestamp of
its **first captured sample**; `:received-at-ms` must be at/after its final sample.
Both timestamps and clock `:time-ms` use the same monotonic time domain. Inputs
are 8–192 kHz, 1–65,536 finite samples per call; normalized PCM is expected, with
headroom up to magnitude 16 permitted. Invalid/overlapping blocks are rejected.
A timestamp gap or sample-rate change clears filters, partial hops and learned
baselines and reports `:discontinuity? true`; it does not invent transitions
across missing audio. The default 500 ms calibration suppresses kicks/breakdowns
while initial levels are learned. Start a new analyzer to deliberately recalibrate.

The kick path uses a causal 35–160 Hz Butterworth high/low-pass pair, per-hop RMS,
an adaptive mean/deviation threshold, positive rise, amplitude rearming and a
140 ms refractory period. Input gain, noise floor, frequency band and thresholds
are configurable. This is an onset heuristic: bass notes and other low-frequency
transients can resemble kicks, and dense mixes need real recording evaluation.
Steady tones do not continuously retrigger. The carrier analyzer's YIN and FFT
are intentionally outside this cheaper path.

Energy uses RMS, attack/release smoothing and a slowly changing reference.
Breakdowns require both a relative drop and a minimum prior signal level, then
700 ms confirmation; recovery uses a separate higher threshold and 150 ms hold.
The reference is frozen during a breakdown, preventing continued silence from
becoming its own normal level. Short dropouts and initial silence do not count as
breakdowns. `:energy-class` is `:low`, `:typical` or `:high` relative to this input's
history; `:interpretation` explicitly identifies signal energy, **not emotion**.
There is no objective mood classifier or trained music-structure model here.

Events include `:type`, monotonic `:sequence`, `:source`, `:inference`, bounded
heuristic `:confidence`, `:time-ms`, `:observed-at-ms` and `:latency-ms`:

| Event | Meaning |
| --- | --- |
| `:audio/kick` | Adaptive low-band onset; includes threshold, measured band RMS and optional clock alignment. |
| `:audio/beat` | A kick within the configured tolerance of a supplied running clock beat, with clock source/epoch, beat number, BPM and signed phase error. |
| `:audio/energy` | Throttled update with the normalized, smoothed RMS under `:audio/energy` (100 ms by default). The same key names the show variable and patchbay source. |
| `:audio/breakdown-start` | Confirmed relative-energy drop; includes candidate time, confirmation time and held duration. |
| `:audio/breakdown-end` | Confirmed recovery above the higher threshold. |

No clock means no `:audio/beat` event. Paused, uncertain or out-of-phase clocks
also suppress that event while retaining kicks. Alignment reports residual phase
against the supplied transport and never estimates, replaces or rewrites tempo.
The confidence fields are heuristic strengths, not statistically calibrated
probabilities. General tempo/downbeat inference remains a separate optional
engine. The [upstream-library research](../../../docs/audio-analysis.md) evaluates aubio and alternatives; no
native/WASM engine is silently activated by this namespace.

Kick/energy event times are hop midpoints. Breakdown event time is the start of
the candidate interval, so it precedes confirmation. `:latency-ms` measures
caller-reported arrival time minus that estimate; `:input-age-ms` reports the
age of the analyzed window at the boundary. These measures do **not** include
execution time after the call begins, unknown hardware delay or downstream
scheduling. Filter group delay is not compensated. Long supplied blocks increase
observable arrival latency even though processing hops remain short.

Subscriptions are independently owned, composable values:

```clojure
(let [subscriptions (-> (analysis/new-subscriptions)
                        (analysis/subscribe :kick-lighting #{:audio/kick} on-kick!)
                        (analysis/subscribe :meter #{:audio/energy} update-meter!)
                        (analysis/subscribe :capture :all capture-event!))]
  (analysis/publish! subscriptions events)
  (analysis/unsubscribe subscriptions :capture))
```

`publish!` runs matching handlers in event/ID order and reports successful
delivery counts and handler exceptions. One failure does not prevent other
subscribers from receiving the event. It does not retry events or own a hidden
global event bus. Handlers may enqueue validated shared control commands; they
must preserve the current show/epoch/command identity and run outside rendering
and CAS retry functions. The host owns device input, backpressure, subscription
replacement, shutdown and clock authority.

Focused tests generate decaying kicks, steady tones, high-frequency transients,
silence, gain changes and short dropouts. They check beat phase/confidence gates,
refractory/rearming, breakdown hysteresis, immutable inputs, arbitrary chunk
boundaries, discontinuity reset, timestamp validation and subscriber failures.
No microphone, Live instance or real music corpus has been qualified.

A bounded local timing sample on Linux amd64 / Java 21.0.13 / 4 available JVM
processors used 48 kHz mono, 480-sample blocks, 10 ms hops, 500 warmup calls and
2,000 measured `analyze` calls. It measured p50 **0.052739 ms**, p95 **0.344487 ms**,
p99 **0.500250 ms**, maximum **15.204995 ms**. The signal was a prebuilt 1 kHz sine;
measurement includes input copying, filtering, feature transitions and event
construction, excluding device capture, callbacks, clock acquisition, carrier
FFT/YIN and rendering. Allocations were not measured. The maximum exceeds one
10 ms block interval: this sample is not a hard realtime or end-to-end latency
guarantee, nor a comparison with another engine.

## Observe a known local recording in mpv

Use the absolute path to a trusted mpv session's existing UNIX IPC socket. In
mpv, select the recording using its canonical absolute path so the reported
file and opened stream agree with the catalog entry. Use the recording's known
tempo and rebuild the profile if the file changes.

```clojure
(require '[clojure.java.io :as io]
         '[tolglow.integrations.mpv-player :as mpv]
         '[tolglow.integrations.track-profile :as track-profile]
         '[tolglow.show :as show])

(def track (.getCanonicalPath (io/file "/home/operator/music/known-track.wav")))
(def profile (track-profile/analyze-file track {:beat-grid {:bpm 120}}))
(def provenance {:source-id :mpv :session-id (random-uuid) :clock-id :show-monotonic})
(def runtime
  (show/start! {:workspace-options nil
                :section-patterns {:enabled? true :on :bar :show-seed 514
                                   :patterns [:warm :pulse :slow]}
                :section-reference {:profile profile :provenance provenance}}))
(defn open-player! [callbacks]
  (mpv/open! {:socket-path "/home/operator/.local/run/mpv.sock"
              :provenance (:provenance callbacks)
              :adapter-options {:alignment-options (:alignment-options callbacks)}
              :catalog {track (select-keys profile [:reference :duration-ms])}}
             callbacks))
(def callbacks (show/section-reference-callbacks runtime))
(def player (open-player! callbacks))

;; Inspect alignment while mpv plays the known recording in the preview runtime.
((:state! callbacks))

;; Close before replacing the connection. Run the next two forms when mpv is ready again.
(deref ((:close! player)) 2000 :still-closing)
(def callbacks (show/attach-section-reference! runtime (random-uuid)))
(def player (open-player! callbacks))

;; Stop both owners when finished.
(deref ((:close! player)) 2000 :still-closing)
(show/stop! runtime)
```

The worker uses the show's monotonic clock and sends property subscriptions and
reads on one connection. `:done` resolves on close, EOF or error and reports
`:reason` and whether the show accepted the final revoked alignment. Reattach
with a fresh session ID for each reconnect, including after a show restart or
restore. The new connection starts unavailable and must confirm the recording
and its position again. Earlier callbacks expire as soon as the session changes.
The socket lifecycle is available from the REPL. Studio can attach to the same
show runtime for event inspection.

## Explicit studio audio capture

`tolglow.audio.capture` owns the optional JavaSound input boundary. Requiring it,
creating a manager and calling `list-inputs` do not open a device. `start!` requires
an explicit `:mixer-index` from the current enumeration or `:default? true`, then
opens signed 16-bit PCM on one daemon capture worker. An optional
`:mixer-name` rejects a stale index after device enumeration changes.

Without `:input-channel`, mono passes through and stereo averages both channels.
To analyze a single channel, specify the stream's physical `:channels` width
and a one-based `:input-channel` within that width. Widths from 3 to 64 always
require a channel selection. The mixer must support the requested PCM format.
For an active show in the REPL, the same operation as Studio's **Start capture**
selects channel 3 of a four-channel stream:

```clojure
(require '[tolglow.show :as show])
(show/audio-action! runtime :start {:default? true :channels 4 :input-channel 3})
(show/audio-action! runtime :status {})
(show/audio-action! runtime :stop {})
```

The same capture config can be used under the application's `:audio` module.
Show files retain the settings and whether capture was requested. Stopped
capture stays idle on load and its settings are available for the next start.

```clojure
(require '[tolglow.audio.capture :as capture])
(let [manager (capture/create-manager)
      handle (capture/start! manager
                             {:mixer-index chosen-index :sample-rate 48000.0
                              :channels 2 :block-frames 480 :buffer-blocks 8
                              :analyzer-options {:source-id :main-input}}
                             {:clock! current-show-transport
                              :events! enqueue-events! :features! update-meter!})]
  ;; Later, explicitly release the input; close! also prevents future starts.
  (capture/status manager)
  (capture/stop! manager)
  (capture/close! manager))
```

The host resumes requested capture after rebasing the restored show clock.
`:clock!` supplies `:time-ms`, `:beat`, `:bpm`, `:source`, optional `:source-epoch`
and `:confidence`, and canonical `:playing?`. `:epoch` can supply the source epoch
when `:source-epoch` is absent. Pausing suspends aligned beat events.
The event callback receives a vector and should enqueue quickly. Returning false
or `{:accepted? false}` reports queue rejection; the capture status counts dropped
events. Optional feature/completion callbacks and a fixed `:subscriptions` map
are separate boundary operations. Consumer errors are isolated and counted;
slow consumers still create backpressure, so effect evaluation must not run in
them. There is no unbounded sample/event queue or automatic device reconnect.

`status` exposes lifecycle, config, latest features, sample counter, buffered
frames and bounded counters for processed blocks/frames, known input losses,
discarded partial blocks, possible overruns and callback/subscriber failures.
`start!` returns a run ID, a `:stop!` function and a final-result promise. Device
open/read errors appear in status and resolve that promise. The source closes
once on EOF, failure or cancellation; stop also interrupts the worker to unblock
reads. Stop's join is bounded, but an injected source's own `close!` must return
promptly. A source that fails to stop retains the manager's single capture slot.

PCM conversion handles either endianness, signed endpoints and stereo averaging.
Partial byte reads are assembled before decoding; incomplete final PCM frames
are errors. First-sample timestamps derive from the monotonic capture-start
estimate plus an explicit frame counter. JavaSound does not expose portable
hardware capture timestamps or precise overflow counts. A full buffer is marked
as **possible** overflow; timing becomes uncertain and clock-aligned beats stay
disabled until restart. No exact lost frames are invented. An injected source
may report exact `:dropped-frames`; those advance the counter and recalibrate the
analyzer. A gap within a partial block discards its old prefix. Source clock
regression/skew is reported instead of silently manufacturing arrival timestamps.

Tests inject `:open-source!` and `:now-ms`, using actual signed PCM byte streams.
They cover endianness/stereo averaging, partial reads/EOF, PCM-to-kick/beat flow,
known/possible loss, bounded admission, cancellation during open/read, restart,
consumer failures and source cleanup. They do not open or qualify physical audio
hardware. The final result includes portable analyzer state; native line handles,
callbacks and PCM buffers remain runtime-only resources.
