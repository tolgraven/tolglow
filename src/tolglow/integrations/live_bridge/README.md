# Max for Live bridge source

This folder contains an editable Max patch, the low-priority Live API adapter,
and a Node for Max loopback transport. It is source for a **Max for Live MIDI
Effect**, not a fabricated compiled `.amxd` file. The source and protocol tests
run in this workspace; Ableton Live and Max are not installed here, so opening,
saving, freezing and playing the device in a real Live Set remain acceptance
steps.

## Install into Live

1. Use Ableton Live 11 or later with Max for Live and a Max version supporting
   `node.script`. The note APIs used here were added in Live 11.
2. Create a dedicated MIDI track with a blank **Max MIDI Effect** followed by an
   existing Drum Rack. Open the blank effect in Max.
3. Open `tolglow-live-bridge.maxpat` in Max. Copy its patcher contents into the
   Max MIDI Effect's editing window, replacing that blank patch's objects.
4. Keep `live_api.js`, `max_adapter.js` and `node_bridge.js` together beside the
   saved device, and include this folder in the device/project search path.
   `max_adapter.js` includes `live_api.js`; Node uses a relative `require` of it.
5. Save the actual Max for Live device from that Live-connected editing window
   as an `.amxd`. Use Max's dependency collection/device freezing workflow when
   packaging it, then verify all three scripts resolve from the packaged copy.
   Merely renaming the JSON `.maxpat` to `.amxd` does not compile a device.
6. Create or choose an existing MIDI clip on the track. Enter the **zero-based**
   track, clip-slot and Drum Rack device indices and click **Pair destination**.
   With this Max effect at device index 0, a following Rack is commonly index 1;
   choose the actual index in the Set instead of assuming it.
7. Start the studio and configure its Live transport to
   `http://127.0.0.1:8750/live`. Enable **Bridge on / off** in the device. The
   editable start message supplies the callback URL
   `http://127.0.0.1:8500/api/live` and bridge ID `tolglow-m4l`. Give multiple
   devices distinct bridge IDs and loopback ports.
8. The device defaults to **Live** MIDI pass-through. Request **Lighting** when
   the studio is connected. The studio releases held lighting activations
   before acknowledging the handoff; that acknowledgement controls the MIDI
   gate. **Live** returns ownership to ordinary instrument playing.

Stopping the bridge first requests a bounded best-effort Live-mode handoff while
its listener is still available, then closes it. Abrupt device/process loss
cannot deliver that callback; the studio must retire held activations when the
bridge disconnects or its session lease expires.

`live.thisdevice` initializes the adapter. Every message reaching the JavaScript
Live API object passes through `deferlow`. Network I/O runs only in
`node.script`; it never constructs a Live API object. Requiring the Node module
does not bind a port. The device's explicit Start operation binds only
`127.0.0.1`.

To record accepted lighting actions, arm the paired track and record into its
paired MIDI clip. The bridge rechecks the track's arm state, clip recording
state, clip type, numeric object IDs and saved names immediately before writing.
Replacing or renaming the paired track/clip requires pairing again. Numeric Live
object IDs are session-scoped: durable connection settings should retain indices,
expected names and bridge ID, then establish a fresh pairing after Set reload.

## What the adapter executes

- MIDI note/CC input in Lighting mode is forwarded to the studio's common
  control model. Live mode passes raw MIDI onward and suppresses lighting input.
- Clip cue recording uses `Clip.add_new_notes` and the returned note ID. A
  note-on immediately inserts an explicitly provisional **1/16-beat** note.
  A paired note-off retrieves that exact note and uses
  `apply_note_modifications` to set its duration. The protocol carries
  `instance-id` so overlapping activations do not steal each other's notes.
  Without it, the adapter reports `latest-note` pairing as a limited fallback.
- Absolute `song-beat` determines held duration across clip loop wraps.
  `beat` is the clip-relative insertion position. These are deliberately
  distinct. A backwards transport jump rejects the duration change and keeps
  the provisional note rather than inventing a duration.
- The existing `ableton/pad-page` 8×8 mapping supplies each cue's MIDI pitch,
  label and packed RGB. The bridge changes **existing Drum Rack chains'** names
  and colors. Empty pads are reported; the bridge does not invent instruments
  or chains. Optional clip color is supported. Live quantizes these colors to
  its nearest palette entry, and receipts include the actual read-back color.
- No `DrumPad.color` setter is used. The documented `DrumPad.name` property is
  read-only, so labels are set on its chains. Whether a particular Push display
  presents those chain labels/colors exactly as desired still requires hardware
  acceptance.

Choose **one recording route** in the studio. This bridge performs explicit
Clip API insertion. Sending the same feedback as native MIDI into a recording
track at the same time would record it twice. The alternative native MIDI route
can deliver notes and CC, but a successful MIDI send does not prove Live recorded
an editable clip event.

The verified Clip note API does not provide arbitrary MIDI CC envelope writing.
`record-event` with a CC event therefore returns `unsupported-cc-recording`.
Parameter recording through native MIDI routing remains a separate integration
choice; this device does not claim to insert CC envelopes through a nonexistent
Clip method. Clip note dictionaries also do not carry a per-note MIDI channel;
configure the track's MIDI routing to match the shared channel mapping.

## HTTP and IPC protocol

The Node side accepts `POST /live`, `Content-Type: application/json`, on its
explicit loopback listener. Browser `Origin` requests, malformed/oversized bodies
and unknown endpoints are rejected. Request size defaults to 64 KiB, with at
most 32 pending Max executions.

```json
{"id":"unique-request-id","op":"configure-destination","payload":{
  "expected-bridge-id":"tolglow-m4l","sync-session-id":"studio-run-uuid",
  "track-index":0,"clip-slot-index":0,"rack-device-index":1,
  "expected-track-name":"Lights","expected-clip-name":"Light sequence"
}}
```

| Operation | Payload |
| --- | --- |
| `inspect` | `{"expected-bridge-id":"tolglow-m4l","sync-session-id":"studio-run-uuid"}` establishes callback ownership after Max acknowledges |
| `configure-destination` | Zero-based track/clip-slot indices; optional Rack index and expected names |
| `record-event` | Existing protocol-v1 `live/record-event` intent, optionally augmented with `song-beat` and `instance-id` |
| `pad-page` | `{"pads":[...],"clip-color":16711680}`; clip color optional |
| `handoff` | `{"mode":"lighting"}` or `{"mode":"live"}` |

Receipts have `id`, `protocol-version: 1`, and `status`:

- `executed`: Max returned after the requested API calls; inspect `result` for
  `recorded?`, note ID, provisional status, skipped pads and actual colors.
- `rejected`: validation prevented the operation before any attempted write.
- `indeterminate`: a write may already have happened, IPC delivery failed, or
  Max did not acknowledge before the deadline. Do not retry under a fresh ID.

The host first sends `inspect` with its unique run ID and the configured bridge
ID, then waits for the successful receipt before configuring a destination or
writing. Every non-inspect operation must carry those same two payload fields;
missing, mismatched or superseded ownership is rejected before dispatch to Max.
Receipts include `bridge-id` and the originating `sync-session-id`. Read-only
`inspect` without a session remains available for diagnostics but cannot enable
callbacks or authorize writes. An older inspect acknowledgement cannot displace
a newer acknowledged handshake. Starting or stopping the Node listener clears
callback ownership; a fresh handshake is required.

The Node process caches completed request IDs and detects conflicting reuse.
A timeout is retained as indeterminate; a late matching execution receipt can
replace it. The API adapter separately deduplicates accepted command IDs and
retains ambiguous writes so they are not inserted a second time automatically.
These histories are bounded and in memory. They are not a durable transaction
log across process/Live restarts, and multiple Live API setters are not atomic.

The Max script sends callback data through Node to the studio's `/api/live`:

```json
{"protocol-version":1,"bridge-id":"tolglow-m4l","sync-session-id":"studio-run-uuid",
 "event-id":"process:sequence",
 "kind":"midi","event":{"type":"note-on","note":36,"velocity":127,
                           "channel":1,"origin":"live-playback"}}
```

`kind: "status"` carries an inspect result under `status`; `kind: "handoff"`
requests a mode change. Callbacks are delivered in order, without automatic
retry. Queued status snapshots are coalesced while MIDI ordering is preserved.
Ownership is captured when each event enters the queue, so an old callback is
never relabelled for a newer studio run. Before the handshake, status polling is
not forwarded and MIDI/handoff forwarding reports that ownership is missing.
Status includes arm/recording/automation flags, paired destination, clip-relative
`beat`, absolute `song-beat`, loop bounds, `looping?` and transport clock. Live's
time signature is also reported, with quarter-note beats per bar; for 7/8 that
value is 3.5 rather than an incorrect seven quarter notes.

All forwarded MIDI input carries `origin: "live-playback"`; existing common
recording logic suppresses re-recording of that origin. `tolglow-feedback` input
is discarded. This bridge never emits its clip-recording intents as MIDI wire
feedback as well.

## Verified API references and host qualification limits

The adapter uses these official Cycling ’74 pages, inspected on 2026-11-21:

- [Clip](https://docs.cycling74.com/apiref/lom/clip/): `add_new_notes`,
  `get_notes_by_id`, `apply_note_modifications`, MIDI/recording/loop properties,
  and nearest-palette `color` behavior; note editing APIs are Live 11+.
- [DrumPad](https://docs.cycling74.com/apiref/lom/drumpad/): `chains`, readable
  `name`/`note`, and the absence of a writable pad color property.
- [Chain](https://docs.cycling74.com/apiref/lom/chain/): writable `name`, `color`
  and `is_auto_colored`.
- [RackDevice](https://docs.cycling74.com/apiref/lom/rackdevice/): `drum_pads`
  and `has_drum_pads`.
- [Song](https://docs.cycling74.com/apiref/lom/song/) and
  [Track](https://docs.cycling74.com/apiref/lom/track/): transport, meter,
  session automation recording and track arm state.
- [JavaScript LiveAPI](https://docs.cycling74.com/apiref/js/liveapi/): object
  IDs, `get`/`set`/`call`, and the low-priority scheduling requirement.

`max_adapter.js` isolates the host dictionary codec. Its current implementation
passes a JSON dictionary string to note methods and accepts JSON or named Max
dictionary results. The official pages establish the dictionary parameter
schema, but an official JavaScript serialization example and an actual Max host
execution were not verified here. **That codec, active-recording note edits,
device freezing, playback/record timing, Drum Rack display behavior and physical
Push mode handoff remain real-host acceptance requirements.** The tests below
establish source/protocol behavior, not that Live accepted the host-specific
dictionary representation.

## Run the source tests

```sh
node --test src/tolglow/integrations/live_bridge/test/*.test.js
```

Tests inject only the external Live API boundary and exercise actual adapter
code. Separate tests open owned loopback HTTP servers to verify execution
acknowledgements, request idempotency, callback ordering, bounded input handling,
timeouts, late receipts and explicit shutdown. Patch structure checks validate
all object references and the `deferlow` path. No Live instance, hardware port or
external network endpoint is used.
