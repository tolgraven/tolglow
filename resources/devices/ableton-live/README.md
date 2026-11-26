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
4. Keep `live_api.js`, `live_session.js`, `parameter_observers.js`, `max_adapter.js` and `node_bridge.js` together beside the
   saved device, and include this folder in the device/project search path.
   `max_adapter.js` includes `live_api.js` and `live_session.js`; Node uses a relative `require` of it.
5. Save the actual Max for Live device from that Live-connected editing window
   as an `.amxd`. Use Max's dependency collection/device freezing workflow when
   packaging it, then verify all five scripts resolve from the packaged copy.
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

## Record and edit a lighting performance

The [pickup and loop qualification](../../../docs/live-routing.md#record-a-pickup-before-a-repeating-section) includes a playable MIDI clip, its cue and parameter mappings, and a way to inspect edits exported from Live.

1. Select a cue page in Studio. In **Connections → Ableton Live**, enable the
   bridge, choose **Bridge · confirmed clip writes** and pair the track with a
   Session clip slot or an Arrangement clip. Use zero-based indices. Save and
   start. The saved show includes the generated note numbers, CCs and pairing.
2. The 8×8 grid maps to Drum Rack pitches 36–99, with the upper-left pad at 92.
   Insufficient space and incompatible fields appear under **Unassigned cue
   and parameter mappings** after saving. Existing assignments reserve their
   pitches. Disable automatic mapping to maintain your own layout, or clear
   **Enabled** on an individual row to reserve it without responding. Moving an
   assignment changes which notes existing clips address.
3. Record or overdub the paired MIDI clip on its armed track. A cue launch adds
   a note. A held release sets that note’s length. Timed launches carry their
   musical duration. Explicit launch overrides and parameter changes add
   short notes on dedicated pitches. **Pitch selects the parameter, velocity
   supplies its value.** Velocity 1 is the low endpoint, 64 the midpoint and
   127 the high endpoint. Integers and enum choices snap, booleans switch at
   the midpoint. Note-off for a parameter sample leaves the last value in
   effect. A parameter arriving before its launch applies when the cue starts.
   Push encoder turns record each committed change. A temporary Accent turn
   also records the restored value when you lift, while the clip is recording.
4. Open the clip’s piano roll to move cue starts, resize held notes, edit
   velocities or draw additional samples. In Lighting mode the same recorded
   notes drive the common cue and parameter handlers when the clip plays.
   Native CC input continues to work with its original 0–127 range. Choose
   the separate **MIDI · delivery only** recording route for native CC
   automation, with a routed MIDI output and automation recording enabled.
5. After recording finishes enter an Arrangement start in quarter-note beats
   (beat 0 begins the song), then choose **Place clip in Arrangement**. The
   bridge copies the paired clip to the same track and refuses overlap with
   an existing Arrangement clip. The copy is independent of its source. To
   record against an existing Arrangement clip, pair **Arrangement clip** and
   enable global recording while that clip is playing on the armed track.
6. To follow an existing instrument part, route a copy of its MIDI to a
   separate monitored lighting track and assign that track's note and CC
   numbers in Studio. The music track can continue feeding its instrument.
   With stored lighting clips, use Live's ordinary Session or Arrangement
   transport. Each participating computer needs its own routed MIDI or
   companion callback. Beat synchronization alone carries no note data.

One bridge channel has 128 pitches. The 64 Drum Rack pitches leave 64 automatic
parameter lanes. Scalar numeric parameters need bounds or a recommended
range. Enum lanes support up to 127 choices. Color components, arbitrary
vectors and ranges need separate scalar controls. One note stream per cue
addresses the latest running activation for parameter editing. Simultaneous
independently edited voices of the same cue need separate cue mappings.
Clip note dictionaries use the track's routed MIDI channel.

## Follow automation in an existing Live Set

Select a device parameter, fader, pan control or send on a regular Live track.
Nested Rack devices work too. In Studio, connect the bridge and open **Live
automation sources**. Give the source a stable name such as `filter-cutoff`
and choose **Learn selected parameter**. In Lighting mode its native value
appears in the patchbay as `live/ableton-live/parameter/filter-cutoff`. Pick
that source, choose a cue parameter and set its range, response and envelope.

Studio saves the source name, canonical Live path, track/device/parameter
names, quantization and range. Reconnecting binds a fresh Live observer. An
unavailable or changed path is reported in the watcher receipt. A rename or
replacement retires the active subscription. Use **Forget**, select the correct
control and learn it again. Switching to Live mode suspends output. Returning
to Lighting mode publishes current values.

Disconnecting in Studio asks the companion to clear the live observers while
keeping the saved sources. Turning off the companion releases its observers
locally and restores normal MIDI pass-through. Reconnecting installs the saved
sources against the new sync session.

The observer follows changes exposed through `DeviceParameter.value` and
`automation_state` in Session or Arrangement. Relative clip modulation or
a plug-in's internal modulation may affect audio without changing that exposed
value. To compose a dedicated Session modulation lane in Live 12, put **CC
Control** before the bridge on the lighting MIDI track, assign one of its
twelve customizable dials a free CC, then map that CC to a cue parameter or
patchbay source. Draw or automate the dial in the clip and the bridge receives
the resulting MIDI CC. Existing instrument clips can feed a separate monitored
lighting track. For sound changes only available after processing, use an audio
return. [CC Control](https://www.ableton.com/en/manual/live-midi-effect-reference/#cc-control)
[DeviceParameter](https://docs.cycling74.com/apiref/lom/deviceparameter/)
[Live clip envelopes](https://www.ableton.com/en/manual/clip-envelopes/)

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
| `configure-destination` | Track index plus exactly one of `clip-slot-index` or `arrangement-index`, optional Rack index and expected names |
| `record-event` | Existing protocol-v1 `live/record-event` intent, optionally augmented with `song-beat` and `instance-id` |
| `describe-selected-parameter` | Read the selected control and its durable address |
| `watch-parameters` | `{"sources":{...},"generation":"<request-id>"}` replaces the observed source set and fences old callbacks |
| `place-in-arrangement` | `{"destination-id":"clip:42","song-beat":32}` |
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

Automatic setup, Capture and clip write operations (`inspect-set`,
`ensure-track`, `configure-track`, `write-clip`, `extend-clip`, `capture-midi`, `read-clip`,
`show-clip`, `fire-clip`, `stop-clips`) are specified on the Tolglow side in
[Capture into Live](../../../docs/live-routing.md#capture-the-last-phrase-into-live).
`live_session.js` implements them with these receipts. Keep it beside the other
scripts when saving the device; `max_adapter.js` includes it. The patch adds
thirty automatable `Tolglow lane k` parameters for controller lanes. Drop the
saved companion on a new, empty MIDI track once: setup adopts that track for
capture. It refuses a track that holds clips or other devices unless the
track's name starts with "Tolglow".

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

The Max JS adapter passes a JavaScript object as the note dictionary and reads
JSON or named Max dictionary responses. The JS host calling convention is
demonstrated in the [clip-generation example](https://adammurray.link/max-for-live/js-in-live/generating-midi-clips/)
and the [Arrangement clip reference example](https://cycling74.com/forums/syntax-for-duplicate_clip_to_arrangement).
Device freezing, playback timing, editing while actively recording, Drum Rack
display colors and physical Push handoff still require a Live installation.

## Run the source tests

```sh
node --test test/js/ableton-live/*.test.js
```

Tests inject only the external Live API boundary and exercise actual adapter
code. Separate tests open owned loopback HTTP servers to verify execution
acknowledgements, request idempotency, callback ordering, bounded input handling,
timeouts, late receipts and explicit shutdown. Patch structure checks validate
all object references and the `deferlow` path. No Live instance, hardware port or
external network endpoint is used.

The `tolglow-bus-send.maxpat` and `tolglow-bus-receive.maxpat` patches route audio to individual Drum Rack chains. See the [Live audio returns guide](../../../docs/live-audio-returns.md) for wiring and setup.
