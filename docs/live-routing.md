# Musical signal routing and Live host evaluation

The first patch-routing kernel is implemented in `tolglow.integrations.patch`.
It consumes observed values and explicit clocks, applies mappings and envelopes,
then returns ordinary `:param/set` commands. It neither executes those commands
nor opens Live, MIDI, audio or a network connection. The Ring host owns admission,
current activation bindings and the shared control reducer. A graphical patchbay
can edit the same saved description without introducing another effect engine.

## Implemented data contract

```clojure
{:version 1 :id :performance :revision 1
 :sources
 {"live/ableton-live/ch/1/note/36"
  {:kind :midi :range [0 127] :timeout {:beats 4} :min-confidence 0}}
 :destinations
 {:wash-level {:cue-id :wash :param :level :range [0 1]
               :base 0 :combine :max}}
 :routes
 [{:id :kick-to-wash :source "live/ableton-live/ch/1/note/36"
   :destination :wash-level :source-range [0 127] :range [0 1]
   :curve :ease-out :invert? false :priority 0
   :envelope {:attack {:beats 1/32} :decay {:beats 1/8}
              :sustain 0.5 :release {:beats 1/2}}}]}
```

`validate-patch` returns canonical data or throws before evaluation. Sources have
a declared kind (`:midi`, `:automation`, `:audio`, `:control`), calibrated range,
positive timeout and confidence floor. Kind describes the supplied observation;
a learned Live device parameter supplies an automation source. The graph is
bounded to 128 sources, 128 destinations and 512 routes. Routes connect declared
inputs directly to declared outputs; output-to-input edges are not accepted.

Mappings support clamping, inversion, existing tween curves, optional `:steps`
quantization (2–128), a narrower output range, `:smoothing` duration and the
existing musical ADSR implementation. Smoothing uses an exponential time
constant and explicit elapsed beats or milliseconds; a held target produces the
same result across frame cadences. The first observed value initializes it.
Envelope stages use one clock domain. Beats pause with the transport and bars
capture the meter through the shared envelope code. Milliseconds are available
when intentionally required for a physical response.

Each destination explicitly combines simultaneous contributors using `:max`,
`:min`, `:sum`, `:mean`, `:product` or `:priority`. Priority ties use route identity
deterministically. The result is clamped to the destination range; an idle
destination returns to its declared base. Routes cannot overwrite each other
according to whichever device callback happened to finish last. Contributions
to different captured activation IDs are kept separate, even if their logical
cue and parameter names match.

```clojure
(def captured (patch/new-state spec {:time-ms 0 :beat 0 :meter meter} show-epoch))
(patch/step
 spec captured
 {:clock {:time-ms 125 :beat 1/4 :meter meter}
  :epoch show-epoch
  :bindings {:wash "accepted-instance-id"}
  :active-instances #{"accepted-instance-id"}
  :envelope {:show-id show-id :epoch show-epoch :command-id "frame-42"}
  :observations
  [{:source-id "live/ableton-live/ch/1/note/36" :event-id "bridge-run/12"
    :epoch show-epoch :clock {:time-ms 125 :beat 1/4 :meter meter}
    :value 110 :gate? true :confidence 1.0 :origin :live-playback}]})
;; => {:state ... :commands [...] :values ... :diagnostics [...]}
```

Observations are bounded to 1024 per step. Unknown sources, earlier source
timestamps, future samples, old show epochs, duplicate event IDs and explicit
feedback origins are diagnosed. A short gate-on/off pair within one observation
batch still creates the correct release tail. A late callback cannot rewind an
already sampled envelope: it is applied at the earliest still-available owner
coordinate and reports `:late-sample-clamped`, retaining its original timestamp
for timeout evaluation. Source timeout begins release on the first owner step
that observes expiration; its precision is therefore bounded by owner cadence.

ADSR binds its exact activation at gate-on. Replacing a running cue does not
attach an older note or tail to the replacement. `validate-state` checks saved
progress against the exact patch revision; `restore-state` preserves envelopes,
smoothing, observations and receipt history while remapping captured activation
IDs into the new show epoch. Orphaned gates stay detached until a new gate-on;
restoring never replays old observation events or commands. `rebase-clock`
shifts musical anchors for seeks/loops, including negative historical beat
positions, while preserving all millisecond deadlines. Optional `:from-beat`
allows the whole-model owner to apply one shared delta even when routing's last
sample lags the transport. An intentional show restart may create fresh routing
state. Changed output values receive deterministic child command
IDs. Live-playback origin remains visible so the recording bridge can suppress
re-recording its own playback. Physical MIDI echoes cannot be identified solely
from bytes; wiring both ends of a loop still requires an explicit routing policy.

See [Live audio sends and returns](live-audio-returns.md) for the multichannel PCM carrier and post-FX return path.

## Existing device automation

With a connected bridge, select a Live device or mixer parameter and choose
**Learn selected parameter** in Studio Connections. The source appears under its
stable `live/<source>/parameter/<id>` ID in the patchbay. Device or mixer changes
and played-back automation feed the same graph in the parameter's native range.
The source reports whether Live is playing automation or has an override.
Its saved name and path can be restored across Live sessions. For gain, echoes
or relative modulation visible only in the resulting sound, use an audio return.
[Bridge setup and supported source types](../resources/devices/ableton-live/README.md)

## Sources available at the current boundary

The sync manager emits one `:sync/observation` event for each captured note or CC,
alongside any separately configured cue mapping. IDs are stable logical ports:
`midi/<source>/ch/<channel>/note/<note>`, `midi/<source>/ch/<channel>/cc/<cc>` and
equivalent `live/...` IDs. Channels are 1-based. Note-off and velocity-zero have
`:gate? false` and zero value; CC is continuous. Events include source/port kind,
capture timestamp, run/event IDs, confidence and origin. The host supplies its
authoritative epoch and musical clock; the adapter does not invent them.

The capture worker publishes low/mid/high RMS and monophonic 20–180 Hz bass pitch in addition to the broadband measurements. See [audio analysis](audio-analysis.md) for source IDs, units and window length. The audio boundary supplies measured features and analysis events with
source/confidence metadata. The host can register those as audio ports, using
explicit value ranges and confidence gates. Merely selecting an audio port does
not start capture. The GUI should show actually observed or configured sources,
the units/range, stale status and current activity. Configuring the same input as
both a cue mapping and a patch route is explicit; neither path silently disables
the other.

The companion receives MIDI routed through its track, including Session or
Arrangement clip playback. Learned device parameters publish native-range
automation values. MSP audio-feature emitters use the audio-return boundary.

## Record a pickup before a repeating section

A Session clip can start before its loop brace. Tolglow keeps that opening
section at its original beat and wraps the clip position when playback reaches
the right edge. The song beat keeps advancing. An edit received just after a new
Live status can use the preceding observation of the paired clip to locate the
gesture at its original time. [Live manual, Clip and Loop Region Settings](https://www.ableton.com/en/live-manual/12/clip-view/#clip-and-loop-region-settings)

1. Import [pickup-automation.mid](../resources/devices/ableton-live/examples/pickup-automation.mid) into a MIDI Session slot. Select 4/4 and 120 BPM. In Clip View set Start to **1.1.1**, enable Loop, set Position to **1.3.1** and Length to **1.0.0** (one bar). Standard MIDI files carry notes and timing. Set the loop brace in Live.
2. Pair the bridge with that slot in Lighting mode. Map MIDI channel 1, pitch **36** to a held cue, and assign pitch **100** as its editable velocity lane for a numeric `:level` parameter with range `[0 1]`. The first cue gate lasts 1½ beats. The next starts on beat 2 and repeats every four beats.
3. Play from the clip start. The level lane has velocities **33, 64** in the pickup and **96, 127** inside the loop. On the next pass only **96, 127** replay. Edit the first pickup velocity to **96** and replay from Start. The first level should change from `32/126` to `95/126`. The later pickup step remains `0.5`.
4. On a fresh pass, arm the track and overdub a level change halfway through the first beat. Inspect the added pitch-100 note in Live's note editor at **1.1.3**. Repeat the gesture halfway through the first beat *of the loop* and inspect **1.3.3**. Verify that ending the cue releases its held instance.

`lein run -m tolglow.qa.live-pickup` reads the supplied Standard MIDI File and prints the actual lighting commands for the first pass and one repeat. Pass a MIDI file exported from the edited Live clip to inspect its note timing and velocities under the same mappings. The headless suite also exercises the recording worker and checks the destination beat and velocity it sends for each adjustment. Live/Max playback timing, recording admission and physical output still need a licensed host run.

## Capture the last phrase into Live

Retrospective Capture (#671) and automatic Set setup (#680). The Tolglow
side, the Max for Live companion's operations (`live_session.js`) and their
HTTP contract are tested against a simulated Live object graph. Nothing in
this section has been run in a licensed Live Set; the hardware checklist is on
pull request #730.

**Capture buffer.** `tolglow.integrations.phrase-capture` keeps the last N
bars (default 8, plus one margin bar) of accepted cue launches, releases and
parameter writes. The host calls `observe-model` after each owner cycle; it
reads only the history entries after the newest one it has observed. Each
command's beat comes from its `:time-ms` against the committed transport. The
show stamps a command with the time of the owner cycle that drains it, so
capture resolves one owner cycle before the grid quantises it. While the
transport is stopped the beat stands still, so commands accepted during a stop
land on the stop beat and the phrase has no gap for the stop.
`capture` returns a *phrase* on the beat grid (default a sixteenth).
The phrase ends on the bar line nearest the Capture press. It holds one note
per activation (start, length, launch values) and one lane of points per cue
parameter. Beats are exact rationals relative to the phrase start. An
activation still running at the phrase start begins at beat 0 with its values
at that moment. One that is still running at the end is held to the end. A
backwards seek starts a new phrase. A show restart clears the buffer.
Commands whose source origin is `:live-playback` or `:tolglow-feedback` are not
captured, and neither is anything addressed to an instance those commands
launched, so replaying a clip does not record it again. Overlapping
activations of one cue are kept as separate notes on the same pitch, but MIDI
cannot say which release ends which note: a Standard MIDI File, Live's clip
editor and the playback decoder pair them first in, first out, so their
lengths are not preserved. One clip per run (#683) removes this.

The show runtime owns one buffer (`create-runtime` option `:phrase-capture`,
default `{:bars 8}`; `false` disables it) and folds every committed owner cycle
into it after the sync observers. Expired events leave a persistent queue from
the front: folding one command into 3.6k retained events took 5.3 µs in the
pooled REPL (192 µs with the earlier vector filtering). Over 4,000 owner
cycles of a show launching, moving and ending a cue every few cycles (about
330 activations running by the end), the fold averaged 75 µs per cycle, worst
1.6 ms with JIT and GC included (125 µs and 6.3 ms before the queue).
Most of the remaining cost is checking which captured activations are still
running. `(show/capture-action! rt :status {})` reports the running numbers.

```clojure
(show/capture-phrase! rt)            ; last 8 bars -> {:status :captured :phrase .. :notes n ..}
(show/capture-phrase! rt {:bars 4})
(show/capture-action! rt :status {}) ; buffer size, running activations, per-cycle cost
(show/capture-action! rt :smf {})    ; {:filename "tolglow-capture-1.mid" :bytes ..}
```

Studio's **Connections → Capture** panel runs the same operation
(`POST /api/capture/capture`), shows the summary and offers the last capture as
a MIDI clip (`GET /api/capture/last.mid`). The MIDI layout is kept for the
runtime's lifetime so later captures address the same pitches and controllers,
and it is saved with the show (see *Send a capture to Live* below).

**Clip encoding.** `tolglow.integrations.phrase-clip` allocates a layout once
and keeps earlier allocations, so clips recorded earlier still address the
same targets. Each cue gets one pitch (an existing Drum Rack pad when given).
Each parameter component gets a paired 14-bit controller: coarse on CC 1–31,
fine on CC n+32, skipping bank select (0) and data entry (6). That is 30
components per channel. Colours, aim vectors (`:vector` with `:size`) and
ranges use one pair per component; gradients are listed as unassigned. Cue
notes and controllers can use separate channels, which means separate capture
tracks. `phrase->clips` writes launch values at their note's start.
`clips->phrase` reads them back the same way. A later value attaches to the
latest running activation of its cue. A value with no running activation is
reported as an orphan and is not applied. The first encode quantises values to
1/16383 of their range. After that, decoding and encoding are a fixed point,
and decoded numbers stay inside their bounds. A value outside the encoded
range (possible when a parameter has only `:recommended` bounds) is reported
as skipped instead of being clamped.
`clip->smf`/`smf->clip` carry a clip in a Standard MIDI File (960 PPQ, tempo
and time signature, releases before controllers before note-ons within a
tick).

**Playback.** `clip/player` decodes the MIDI that Live plays from these clips
into `:cue/launch`, `:param/set` and `:instance/end` commands with origin
`:live-playback`. It binds each launch receipt to the note that started it.
Values or a release that arrive before the receipt are applied when it
arrives. Overlapping activations of one cue share its lanes, which address
the latest activation. Independent envelopes per overlapping run need the
one-clip-per-run tracks of #683. Live clips keep no MIDI channel, so the
companion tags clip playback with the track that played it and the player maps
that track to its layout channel (`live-connection/track-channels`). In the
show runtime the player is attached to the Live bridge (see *Send a capture to
Live* below).

**Connection boundary.** `tolglow.integrations.live-connection` sends data
operations over the existing companion transport shape (`:open!`, `:request!`,
`:close!`). Receipts are `executed`, `rejected` or `indeterminate`. Replies that crossed
the companion's JSON wire have their enumerations (`:kind`, `:monitor`,
`:code`) turned back into keywords and their clip beats into exact rationals;
`fake-live/json-transport` tests that path. Operations:
`inspect-set`, `ensure-track`, `configure-track`, `write-clip`, `extend-clip`, `capture-midi`,
`read-clip`, `show-clip`, `fire-clip`, `stop-clips`. Every write names the
track it expects. A non-executed receipt stops the sequence and is never
retried. `setup!` inspects the Set and plans only the missing pieces. It then
reads the Set back and checks every arm, monitor and input route. The
musician's own tracks are never touched, and duplicate capture names are
reported rather than guessed. Exactly one capture route is used per setup:
`:tolglow-buffer` writes the captured phrase as a new clip, and
`:live-capture-midi` feeds the monitored tracks continuously
(`clip/feed-messages` on each `capture/events-after`) and invokes Live's
Capture MIDI.

Choice of transport: the operations go to the **Max for Live companion** over
its loopback HTTP endpoint, which already has request identity, run ownership
and the three-state receipts. The operations use these documented Live Object
Model calls: `Song.create_midi_track`, `Track.arm`,
`Track.input_routing_type`/`_channel`, `ClipSlot.create_clip`,
`Clip.add_new_notes`/`get_all_notes_extended`, `Song.capture_midi` with
`can_capture_midi`, and `Song.view.detail_clip` with
`Application.view.focus_view`. `Track.current_monitoring_state`,
`Clip.automation_envelope`/`create_automation_envelope` and the
`AutomationEnvelope` methods (`insert_step`, `value_at_time`) are names from
Live's Python API. They are not in Cycling '74's Live Object Model reference
(checked 2026-11-26), so whether Max's `LiveAPI` reaches them is a question for
the licensed run. `resources/devices/ableton-live/live_session.js`
implements them behind the adapter's `deferlow` path. A Max device has to be in
the Set before it can answer, so an empty Set needs one manual step: drop the
companion on a new, empty MIDI track. Setup adopts that track as the cue
capture track (`ensure-track` with `:host? true`) and renames it; other capture
tracks are created. The companion adopts only a track that already has the
capture track's exact name, or an empty track (no clips, no device besides
itself) without a Tolglow name. Another Tolglow track has another role (the
cue launcher's `Tolglow launcher …`, a run track) and is refused with
`tolglow-track-in-use`, as `ensure-launcher` refuses Capture's track. A
musician's track is refused with `host-track-in-use`. Either way nothing is
renamed, armed or rerouted, and setup reports the refusal first in its
problems (`:refused` on the verdict). The host is always the track the device is on now, never a track
found by name, so after the device moves a rerun of setup configures its new
track. The companion caches its track id for MIDI events and lane moves and
refreshes it on every Set operation. A Python Remote Script that places the companion automatically would
remove this step. It is not written: Max cannot reach Live's browser, a Remote
Script can, and installing one is the separately packaged host step #680
allows. OSC through a Remote Script was not chosen: it needs the same
installation and has no receipts.

Echo rule: each capture track is fed on its own channel (16, 15, …), which clip
playback never uses. The companion drops MIDI arriving on its feed channel
(Tolglow's own monitored input) and forwards everything else as playback. It
reads the feed channel back from its track's input routing, so a reloaded or
moved companion keeps dropping the feed although a rerun of setup writes
nothing. The playback decoder drops events tagged with a track that is not a
capture track.

The loopback endpoint accepts request bodies up to 64 KiB. Controller lanes
travel as compact `[beat value]` points, and `write-phrase!` splits a clip's
notes and lane points into requests of at most 48,000 JSON characters: one
`write-clip`, then `extend-clip` requests that add to the same clip. An 8-bar
phrase with 30 lanes moving every eighth note (7,650 points) takes several
requests and reads back exactly. A clip is complete only when every one of its
requests executed. If a later request is refused or indeterminate, the clip
written so far is listed under `:partial-clips` and nothing is retried; delete
it in Live or capture again.

A refusal that never reached Max is `rejected` with its own code, so it is not
mistaken for a write that may have happened: `payload-too-large` (413),
`bridge-busy` (503), `malformed-request` (400), `bridge-session-mismatch` (409)
and `not-connected` (nothing listening). A timeout after delivery (504) and any
other transport failure stay `indeterminate`.

Controller lanes on the buffer route: the Live Object Model has no call that
writes MIDI CC events into a clip. The companion has thirty automatable
parameters, `Tolglow lane 1`–`30`, one per coarse controller. `write-clip`
turns each coarse/fine pair into a value in [0, 1] and writes it as a step of
that parameter's clip envelope
(`Clip.automation_envelope`/`create_automation_envelope`, then `insert_step`).
When the clip plays, the moving parameter is forwarded as its controller pair.
`read-clip` reports each lane's value at beat 0 and every change after it. It
scans with `value_at_time` every sixteenth (`:sample-beats`) and bisects each
change down to one tick (1/960 beat), so a step off the grid keeps its own
beat. A step that reverts within one scan step is not seen. Imports can then
round to the capture grid (`:grid-beats`, the capture's own rounding). A lane
value written while no activation of its cue runs belongs to that cue's next
launch, unless another value of the lane comes first, as a controller sent
before a note would. Both the clip decoder and the playback decoder apply this
rule, so a cue launched after the clip start keeps its launch values even
though its lane reads back from beat 0. An envelope is continuous: an
activation whose launch value equals the value the lane already holds reads
back without that launch value. Lanes need the
companion on the same track as the clip: a two-track layout needs a second
companion on the lane track, and the companion refuses controllers on any
other track. Capture MIDI remains the alternative route; the licensed run
decides between them.

**Send a capture to Live.** The runtime runs Capture, setup, the clip write
and the clip focus as one validated command, `show/capture-action!`, for the
REPL, Studio and the Push Capture button. Its shape is stable:

| Operation | Payload | Result |
| --- | --- | --- |
| `:status` | `{}` | buffer, cost, last capture and `:live` (below) |
| `:capture` | `{:bars n :end-beat b}` | capture summary with `:phrase` (REPL) |
| `:smf` | `{}` | `{:filename :bytes}` of the last capture |
| `:setup` | `{}` | `{:ready? :route :tracks [{:channel :track-id :name}] :problems :applied}` |
| `:send` | `{:bars :end-beat :capture? :name :slot-index :focus? :handoff?}` | capture summary plus `:live {:status :clips :partial-clips :setup :focus :capture-id}` |
| `:focus` | `{:clip-id :handoff?}` | `{:status :clip-id :view :code :handoff}` |

`live-capture/validate-command` checks the operation, the keys and each value
before anything runs: `:bars` a positive integer, `:end-beat` a non-negative
number, `:name` 1–128 characters, `:slot-index` 0–999, `:clip-id` a string or
integer, the `?` keys booleans. Anything else is refused with HTTP 400 (an
unknown operation with 404), from the REPL and from Studio alike. Studio posts
to `/api/capture/setup`, `/send` and `/focus`; the REPL helpers are
`show/setup-live-capture!`, `show/send-capture!` and `show/focus-capture!`.

```clojure
(show/send-capture! rt)                    ; last 8 bars -> Live, clip opened
(show/send-capture! rt {:bars 4 :name "Verse lift"})
(show/send-capture! rt {:capture? false})  ; send the last capture again
(show/focus-capture! rt {:handoff? true})  ; hand the surface to Live, then open the clip
(:live (show/capture-action! rt :status {}))
```

`:send` captures the last bars (unless `:capture? false`), then runs setup
over the running Live bridge (`live-bridge-connection`). Setup is idempotent:
it inspects the Set, creates or repairs only missing capture tracks, reads
everything back and verifies arm, monitor and input routing. The host track
is the companion's own track, as above. Then `:send` writes one clip per
capture track through the buffer route (`write-phrase!`) and, unless
`:focus? false`, focuses the first clip as `:focus` does. A capture with
nothing in it sends nothing (`:live {:status :nothing-to-send}`). Without a
connected bridge the capture is still kept and the result says so:
`:live {:status :not-ready :setup {:problems [{:code :not-connected ..}]}}`.
`:live :status` is `:sent`, `:partial` (a later request of a split clip was
refused; the clip so far is under `:partial-clips`), `:rejected`,
`:indeterminate`, `:not-ready` or `:unsupported`. Only the buffer route runs
from the runtime: Live's own Capture MIDI needs the continuous feed to a
monitored track, which the runtime does not send yet, so a saved
`:live-capture-midi` route answers `:unsupported` with `:route-not-wired`.

`:focus` selects the clip (default the first clip of the last send) and opens
Live's Clip View (`show-clip`). With `:handoff? true` the bridge first hands
the surface to Live: it releases held pad activations, records the mode and
sends the companion `handoff`. `:focus` waits for the companion's receipt for
that handoff (`live-sync/handoff-receipt`, the same wait as any bridge
operation). If the handoff is refused or never acknowledged, the clip is not
focused and the result carries the handoff's status and code. A refused or
lost handoff also takes the surface back: the bridge returns to lighting mode
(and after a lost one tells the companion so), so Tolglow and the companion
never disagree about who owns the surface. Only the receipt of the latest
handoff can do that, so a late receipt never undoes a newer handoff. `:handoff?` defaults to false. The
Push Capture button sends `{:handoff? true}`: Push then belongs to Live while
the performer edits.

Live mode: the companion then forwards only clip playback from the capture
host that setup adopted in its session, and only while a Session clip plays
there (`playing_slot_index` ≥ 0). A note heard while no clip plays (Live
auditioning notes as the performer draws them in Clip View, or a performer
playing the track) is not playback and stays in Live. The pad mappings make
no commands in Live mode.

Two limits of that rule:
- **Auditioning while the clip plays.** Live previews notes through the
  track's devices. While the capture clip plays, a previewed note looks
  exactly like playback, so it launches its cue briefly. Turn off Clip View's
  preview (the headphone button in the clip editor) while editing a looping
  capture clip, or expect short cue blips. Lighting mode behaves the same.
- **Arrangement playback.** Arrangement playback keeps `playing_slot_index`
  at -1, so capture clips recorded to the Arrangement do not return in Live
  mode. They return in lighting mode, and "Back to Arrangement" releases the
  Session runs.

The companion's status poll reports the capture host it knows and whether
its clip plays (`capture-host`, `capture-playing?`;
`live-sync/companion-capture`). A reloaded companion has forgotten its host.
Setup's `inspect-set` names the capture host (`capture-host-name`), so the
companion knows the host again without a write. Studio shows "playback
return active in Live mode" only while the companion's status agrees
(`:live :playback :live-mode-return?`, `:companion-host?`). Otherwise it says
the return is paused and asks for setup to run again.

When runs end:
- A handoff back to lighting, and the capture clip stopping, end the runs
  capture playback started. The companion reports that stop like a run-track
  stop, marked `capture?`.
- A handoff into Live mode keeps them only when the companion's last status
  confirms it forwards the playing bound capture clip, because the clip's own
  note-offs will still arrive. Otherwise it ends them too.
- Releases are queued in order with playback, and a run whose launch receipt
  has not arrived yet is ended when it arrives.

Playback return: the show's `sync-callbacks` give the Live bridge a
`:capture-playback!` callback (`show/live-capture-playback!`). For every
companion MIDI event, the bridge asks it first. An event with origin
`:live-playback` from a track that the last setup bound is claimed. The bridge
then leaves it out of its pad mappings, so a capture pitch that is also a pad
note launches its cue once. Claimed events wait in a bounded inbox of 512.
On overflow a controller value is coalesced first: the oldest value that a
later value of the same controller and channel replaces before any launch
goes. Then the oldest controller goes, and only then the oldest launch. A
release (note-off) is never dropped, so a launch is never lost to a
controller flood. Drops are counted in `:dropped`,
shown in Studio's status line and announced as a
`:live/capture-playback-dropped` event. The owner decodes the events at its
next cycle with `phrase-clip/player`, submits launches, values and releases
with origin `:live-playback`, and binds launch receipts on later cycles. An
end the owner queue refused is sent again while its instance runs, at most 8
times, as the run-clip player does. Events that arrived within 10 ms of each
other are decoded together, as one beat. The capture buffer never re-records
them. A later capture or setup rebinds the decoder
(`phrase-clip/rebind-player`) without dropping running runs. A restart forgets
runs of the old epoch. Live input on a capture track (origin `:live-input`)
and playback from tracks that are not capture tracks still go to the pad
mappings.

Bindings: track bindings come only from a setup whose read-back succeeded. A
setup that could not inspect the Set (a busy bridge, a reconnect) keeps the
previous binding. A read-back that no longer finds a bound track drops it and
releases every run. The companion tags each event with its track's name as
well as its ID, and every companion event comes from its own track. So an
event from another ID, or under another name, means the Set was reloaded, the
device moved or the track changed. The binding is then dropped, its runs are
released and `:live :playback` reports `:bound? false` with `:stale`. Clip
playback from a track named like a capture track that no binding holds
(after a Set reload, or after a show load before setup) is claimed and
dropped (`:unbound-dropped`), never handed to the pad mappings. `:setup` or
`:send` binds the tracks again by name.

Saved with the show: `capture-show` writes `[:integrations :live-capture]`:

```clojure
{:version 1
 :layout  {:layout/version 1 :channel 1 :cues {cue-id {:pitch n}}
           :lanes [{:cue-id :param :components [{:type :bounds|:values :cc}]}]}
 :desired {:route :tolglow-buffer
           :tracks [{:channel 1 :name "Tolglow Capture" :kind :midi :arm? true
                     :monitor :auto :input {:port "Tolglow" :channel 16}
                     :feed-channel 16 :host? true}]}}
```

That is the MIDI layout, the capture tracks with their arming, monitoring
and feed, and the playback returns (a track's name and channel). Live track
and clip IDs are session identities: saving keeps only the documented keys,
and a file whose capture data carries any other key (`:track-id`, `:clip-id`
or anything else) is refused. Loading a show runs `live-capture/migrate-saved`
first. It applies registered steps from older versions (refusing a step that
does not raise the version), and refuses data from
a newer Tolglow with a message that names both versions, rather than loading
the show and losing the layout on the next save. It then validates the data
(`live-capture/validate-saved`: no pitch or controller twice, channels 1–16,
tracks that match the layout's channels, one host track, feed channels apart
from playback channels) and adopts it. Clips recorded
before the save keep decoding to the same cues after the load, and later
captures add addresses without moving existing ones. The bridge's `:feed-port`
setting replaces the saved input port at setup. Nothing in this path saves a
Live Set or writes to the User Library.

Tests: `tolglow.integrations.live-capture-test` runs the command on a real
runtime with the Live bridge connected to the simulated Set
(`fake-live` behind the bridge's companion transport). It covers validation
from the REPL and Studio, a send without a bridge, then setup, write and
focus. Live's playback of the clip is sent through `/api/live`-shaped
callbacks: each cue launches once, from the capture decoder only, with its
launch value, its parameter move and its release. The same note played live
still launches through its pad. The test also covers the acknowledged focus
handoff from Studio, a refused handoff, a second send that writes no setup,
and the save, load and later capture with every address kept.

Regressions from the review of #817 run against the same harness. The
simulated companion forwards MIDI as the real one does in each surface mode
(`fake-live/companion-midi`) and reports capture-clip stops
(`fake-live/companion-events!`). The regressions cover:
- a refused inspection that keeps the binding;
- a read-back that drops it and releases its run;
- the launcher's track refused by setup, while its pad clip still plays
  through the pads;
- runs released on handoff, on clip stop and on return to lighting;
- capture playback forwarded in Live mode and auditioned notes not forwarded;
- a refused and a lost handoff rolled back;
- an overflow that keeps the note-off and announces the drop;
- a refused end sent again;
- a Set reload that drops the binding without reaching the pads, then
  setup binding it again;
- a loaded show dropping kept capture clips until setup;
- saved data with Live IDs or from a newer version refused.

The companion's side is in `live_session.test.js`. The Studio events and
status line are in `events_test.cljs`.

Tests (`tolglow.integrations.live-capture-roundtrip-test`) perform on a real
show runtime and capture. They set up a simulated Set with an armed musician
track, write two clips (cue track and lane track) and focus the first. Then
they move a note with its launch value and change a level in the simulated
editor, read the clips back, and play them through the simulated Live
playback into a fresh show. That replay reproduces the edited clip exactly
when captured and replayed at 70→120, 120→180, 180→70 and 120→120 BPM. The
Capture MIDI route captures the same phrase from the feed at 70, 120 and
180 BPM. `live-connection-test` covers empty and musical Sets, idempotent
reruns, lost acknowledgements, deleted and disarmed tracks, renamed tracks,
duplicates and Exclusive Arm. The simulated Set is test code
(`test/tolglow/integrations/fake_live.clj`). It does not claim real Live
behaviour where that is undocumented, for example how Capture MIDI picks the
clip length. Its `write-clip` refuses controller events unless a test creates
the Set with `:envelope-lanes? true`, which models companion lane parameters on
every capture track. `live-companion-contract-test` drives the real
`live-transport` HTTP client, `node_bridge.js` and `max_adapter.js` with
`live_session.js` against a simulated Live object graph
(`test/js/ableton-live/live_set_fixture.js`). It covers setup, rerun, the
envelope write and read-back, refusals and a stopped companion. The
companion's own tests are in `test/js/ableton-live/live_session.test.js`.

## Play a cue page from one Drum Rack track

Issue #228, the cue-launcher half of the Live milestone. The Tolglow side, the
companion operations and their HTTP contract are tested against the simulated
Set and a simulated Live object graph. None of it has run in a licensed Live
Set or on Push hardware; the operator checklist is on the pull request.

**Pads follow Push's 64 Pads layout.** A cue page cell `[x y]` (x left to
right, y top to bottom) becomes the Drum Rack note of the pad in the same place
on Push's 64 Pads drum grid. The grid is four 4x4 blocks: the left half holds
notes 36–67 (bottom block 36–51, top block 52–67), the right half 68–99
(bottom 68–83, top 84–99), each block rising left to right and bottom to top.
So the bottom-left pad is 36, the top-left 64, the bottom-right 71 and the
top-right 99. This is Push's 64 Pads note grid with the Drum Rack's pad
overview scrolled to row 9 (`RackDevice.View.drum_pads_scroll_position`, rows
of four pads, so note 36 is the lowest visible pad). It is still to be
confirmed on Push 3 hardware (step 3 of the host checklist on the pull
request).
- `ableton/pad-note` and `ableton/pad-cell` convert between a cell and a note
  for a layout (`ableton/pad-layouts`).
- The Live bridge setting `:pad-layout` chooses the layout for new automatic
  assignments: `:push-64-pads`, or `:rows` (the earlier rows of eight, bottom
  row 36–43, top row 92–99). Settings that name none get `:push-64-pads`,
  unless they already hold note mappings: a show saved before this keeps
  `:rows`, so one page never mixes the two. Existing assignments always keep
  their notes, because recorded clips address them; to move a page to the
  Push layout, remove its pad mappings and save again.
- Studio's **Connections → Ableton Live → Pad layout** sets it and always
  saves an explicit value.

**Pad hits launch cues through the shared control model.** Nothing new sits in
this path: the companion forwards each hit as Live bridge MIDI, and
`ableton/midi->commands` turns it into `:cue/launch` (and, for a held pad,
the release) with the pad note in the command source. A pad addresses a cue
*definition*; the hit launches an *instance* (the show's instance ID); the run
record keeps that run as a *take* (#683).

**Recording and replay without feedback.**
- While the launcher track plays no clip, the companion marks a hit
  `live-input`: a performance. It launches the cue, the run record keeps a
  take, and an armed launcher track records the hit in Live itself. The bridge
  never writes `live-input` back into Live.
- When a clip plays on the launcher track, the companion marks every note
  `live-playback`. The notes launch and release the same cues on the same
  beats, and make no takes and no clip writes. A looping pad clip therefore
  does not add takes each pass. Hits a performer plays on the launcher track
  *while a clip plays there* are playback too: they launch cues but make no
  takes. To record over a loop, play the loop on another track.
- `live-launcher/pad-clip` makes the same kind of clip from the run record:
  one note per released take of a cue that has a pad, at the take's start
  beat and held for its held beats, over the whole bars around the takes (or
  an explicit beat range). A note's velocity is how hard the pad was hit (the
  pad hit's velocity travels in the command source and the take); a take
  launched elsewhere gets the value its pad's `:velocity-param` received, or
  127. One pad plays one cue at a time, so a hit on a pad that is still held
  ends the earlier note there (reported under `:shortened`). Takes of cues
  with no pad, and running takes, are skipped and reported. Launch values
  and moves stay in the take.
- The known limits of #683's classification apply: a launcher track that
  holds Arrangement clips, or takes MIDI from another track, counts as
  playback, so a performer there is not recorded as a take.

**Live layout.** The launcher is the track of the companion that the Live
bridge (Studio's **Connections → Ableton Live**) talks to, because pad hits
reach the show through that bridge. Drop that companion on a new, empty MIDI
track once. One companion hosts either the launcher or Capture's cue track
(#680), not both: put a second companion on its own track for Capture.
- Setup and pad-clip writes go through the running Live bridge
  (`show/live-bridge-connection`): the same companion connection, under the
  bridge's sync session and in order with its status polls. The companion
  accepts operations only from the session of its last handshake, so a
  separate connection with its own session would lose the companion at the
  bridge's next poll (every 500 ms) and, while it held it, the bridge would
  drop the companion's pad hits. Connect the bridge first; without it setup is
  refused as `not-connected` and nothing is sent. An operation the bridge drops
  before sending it (the bridge stops or rebinds while it waits in the queue)
  is refused at once as `not-sent`; only one that started without a receipt is
  indeterminate. Studio waits long enough for three bridge operations and
  shows the launcher again after a failure.
- `:ensure-launcher` adopts the track only when it is a `Tolglow launcher …`
  track, an empty track, or holds only the companion and a Drum Rack. Any
  other Tolglow track, such as Capture's cue track, is refused with
  `tolglow-track-in-use`; a musician's track with clips or instruments with
  `host-track-in-use`. It names the track `Tolglow launcher <page-id>`,
  inserts a Drum Rack right after the companion when none follows it
  (`Track.insert_device("Drum Rack", index)`, documented since Live 12.3) and
  scrolls the pad overview to row 9. On an older Live a missing Drum Rack is
  refused with `drum-rack-creation-unavailable`; drop one after the companion
  and run setup again.
- `:write-pads` names and colours each mapped pad's first chain
  (`Chain.name`, `Chain.color`, `is_auto_colored` 0). An empty pad gets a chain
  with `RackDevice.insert_chain` and `DrumChain.in_note` (both since Live
  12.3); on an older Live it is reported as `chain-creation-unavailable`.
  `DrumPad` has no colour setter and its name is read-only, so nothing is
  written to the pad itself. Live keeps the nearest palette colour, so a chain
  whose colour is already close to the cue's (within 96 in RGB distance) is
  not rewritten: a rerun leaves the Set alone, while a changed cue colour is
  written. The read-back reports the colour and setup does not count a
  palette difference as a problem.
- `:inspect-launcher` reads the track, its input routing, the Drum Rack, the
  scroll row and every filled pad back, and `live-launcher/setup!` verifies
  them. Problems: a wrong track name, a missing Drum Rack, a wrong scroll
  row, a pad without its chain or name, and an input that cannot carry the
  performer's pads: none, Tolglow's own feed port (the bridge setting
  `:feed-port`, default `Tolglow`, matched anywhere in the port name, as in
  `IAC Driver (Tolglow)`), another track (MIDI From,
  or a track whose output names this one), or a single channel 2–16, which
  the companion drops as Tolglow's feed. Use All Ins or the controller's port.
  A filled pad on the grid that launches no cue is a `stale-pad` warning: it
  looks like a cue pad on Push and plays nothing. A rerun writes only what
  differs. A refused or indeterminate receipt stops setup and is never
  retried.
- `live-launcher/write-clip!` writes a pad clip into the launcher track's
  first empty slot (adding a scene when it is full) with the existing
  `:write-clip` operation.

```clojure
(show/live-launcher runtime :pads)                          ; the 64 pads Studio shows
(show/sync-action! runtime :start {})                       ; or Studio's Connect enabled adapters
(show/setup-live-launcher! runtime {:page-id :pads})        ; Studio: Set up in Live
(show/write-live-launcher-clip! runtime {:page-id :pads :from-beat 0 :to-beat 16})  ; Studio: Write pad clip
```

**Evidence.**
- `live-launcher-test` checks every cell of the layout against a literal 8x8
  note table, then performs on a real `show/create-runtime` whose Live bridge
  talks to the simulated Set:
  - setup before the bridge connects is refused from Studio and the REPL
    alike; after it, Studio's `POST /api/live-launcher/setup` and the REPL run
    the same operation through the bridge;
  - the bridge maps a page in Push's layout, and pad hits arrive exactly as
    Studio's `/api/live` receives companion callbacks, launch the pads' cues
    and become takes;
  - the simulated Live records the hits, velocity included, from the armed
    launcher track. That clip equals the pad clip Tolglow writes from the
    takes. Replaying either one launches the same cues on the same beats as
    `live-playback`, with no new takes, and the bridge receives no recording
    write. A separate negative control marks the replay `live-input` and
    shows it would record takes again.
- The same test reads the projection from the REPL and from
  `GET /api/live-launcher`, and covers an older Live (no Drum Rack insert, no
  chain insert), a performer's own Drum Rack with a stale pad, Capture's cue
  track (refused, untouched) and a launcher track listening to Tolglow's feed.
- `live-companion-contract-test` sets the launcher up through a running Live
  bridge over real HTTP (the bridge, `max_adapter.js` and `live_session.js`),
  with the bridge polling every 100 ms and once more between every
  operation, writes and reads back a pad clip, and shows that a connection
  with its own session is refused (`bridge-session-mismatch`) after the
  bridge's next poll. `live_session.test.js` covers the companion operations,
  their refusals, palette-snapped colours, input reporting and the
  input/playback marking on the launcher track.

## Mirror what ran as run tracks, clips and Group Tracks

Issue #683. The Tolglow side, the companion operations, the bridge and their
HTTP contract are tested against the simulated Set and a simulated Live object
graph. None of it has run in a licensed Live Set yet; the operator checklist
is on pull request #750.

**Run record.** `tolglow.integrations.live-runs` folds each committed show
model, as the capture buffer does, but keeps every run instead of the last
bars. The runtime owns one record (`create-runtime` option `:live-runs`,
`false` disables it).
- A cue definition gets a *channel* on its first launch. Later runs reuse it.
  A 64-cue page with two launched cues has two channels.
- Each activation is a *take*. It keeps:
  - start, release and end beats on the show transport, as exact rationals on
    the 1/960 beat grid;
  - the validated launch (mode, targets, duration, values);
  - the instance's effect revision and the cue's version;
  - any per-instance group placement;
  - parameter moves, the launching source and the instance ID.
  A take outlives its instance.
- Moves of one parameter on one beat coalesce; the last value wins. A take
  keeps at most `:max-moves` (default 512). Writes beyond that are counted in
  `:moves-dropped` and the take is marked `:truncated? true`. On this Mac, a
  2,000-write burst over 100 owner cycles folded in a mean of 87 µs per cycle,
  worst 0.3 ms, with the commands themselves included.
- The hold is transport time played forward. A seek or loop inside a run banks
  the segment before the jump, so it neither shortens nor lengthens the hold.
- A run holds the lowest *lane* of its channel that no other run of the same
  cue holds until its release. Successive runs reuse lane 0. Runs held at the
  same time get lanes 1, 2 and so on, each its own Live track, so they play and
  stop independently. A lane frees at the release command. A relaunch during
  the previous run's release tail therefore reuses the track, and the tail and
  the new run share that lane.
- A channel follows its cue's place in the manual effect-group tree (#553).
- A new show epoch ends every open take at the beat of the restart, within one
  owner cycle, and marks it `:interrupted :epoch`. Channels and finished takes
  are kept.

**Recording ownership.** A take is made only for a performance.
- Commands with origin `:live-playback` or `:tolglow-feedback` make no takes,
  and neither does anything addressed to an instance they launched.
- The companion marks MIDI it forwards `live-input` only when all of these
  hold:
  - its own track neither plays nor has launched a Session clip
    (`playing_slot_index` and `fired_slot_index` below 0);
  - the track holds no Arrangement clips;
  - the track takes no MIDI from another track (its input routing does not
    name another track);
  - nothing changed on those slots within the 250 ms after the change was
    noted;
  - no other MIDI track's output routing names the companion's track
    ("MIDI To").
  Everything else is `live-playback`, including when the state is unknown.
- The notification stores each observed value at once, as a plain
  assignment. Only reads that need LiveAPI are deferred. A launch is
  therefore seen at `fired_slot_index`, before a quantized clip starts. The
  250 ms window helps only notes handled after the notification stored the
  change. With launch quantization None, a note handled before that
  notification still reads the old slot and is classified as input; that
  ordering needs the host run.
- The Ableton bridge gives `live-input` commands origin `:live-input`. The run
  record and Capture keep them as performances, and they are never recorded
  back into Live, which already has its input.
- Everything else from Live, such as a looping launcher pattern (#228) or a
  Capture clip, stays `:live-playback`, so a looping clip does not add a take
  every pass.
- Known limits:
  - A performer playing on a companion track that holds Arrangement clips, or
    one that takes MIDI from another track, is not recorded.
  - Routing is detected by name, in both directions: an input port named
    exactly like another track, or another MIDI track whose output names the
    companion's track, counts as routing. A track named like a MIDI port
    therefore stops a performer being recorded. An undetected route from a clip would
    be recorded as performance, and a later Capture into Live would write back
    what Live played. Keep the companion on its own track with a MIDI port or
    keyboard input.

**Live layout.** `live-runs/sync!` inspects the Set, applies a plan and
inspects again.
- Each channel lane gets one MIDI track, named `Tolglow run <cue-id>` or
  `Tolglow run <cue-id> · 2` and so on.
- Each released take gets one non-looping Session clip (`Clip.looping` 0),
  named `<cue name> run <n> [<take-id>]`, holding one note (pitch 60) for the
  held length. A full track gets a new scene, and `Song.create_scene` adds an
  empty slot to every track in the Set.
- Each manual group that contains a channel gets a Group Track named
  `Tolglow group <group-id>`.
- Reconnection by name:
  - Tracks reconnect by name in any Set.
  - Clips reconnect by the take ID in their name, within one runtime. The
    record is not saved with the show yet, so after a restart the old run
    clips stay in the Set without takes, and playing them launches nothing.
  - A rerun writes only what is missing.
- A rejected step skips only its dependents. A lane track whose group could
  not be bound is created ungrouped and reported. An indeterminate receipt
  stops the run, nothing is retried, and the next sync finds by name a clip
  that was written after all.
- `:arrangement-offset-beats` adds a separate `place-clip` step per take
  (`Track.duplicate_clip_to_arrangement`), keyed by take ID:
  - a take already in the Arrangement is not placed again;
  - a failed placement is placed on the next sync;
  - a range another clip holds, for example after a loop, is reported as
    `placement-overlap` for that take only. Its Session clip is still written
    and the sync still completes.
- The performer's own tracks are never renamed, moved or written.

**Group Tracks: what is and is not documented.** The Live Object Model
documents `Track.group_track`, `is_foldable` and `is_grouped` for reading
membership. It has no call that creates a Group Track or moves a track into
one.
- `ensure-group` only finds a Group Track by name. A missing group is refused
  with `group-creation-unavailable`.
- `ensure-track` with `:group-track-id` inserts with
  `create_midi_track(index)`: just after the group's last direct member that is
  not itself a group, or just after the Group Track when it has none.
- Membership is read back. A track outside its group is reported as
  `not-in-group` and left where it is.

Whether that insert joins the group in real Live is the first group question
for the host run. A packaged template is the fallback creation path.

**Values in Live.** A run clip carries its timing. When its run track holds a
Tolglow parameters device, the clip also carries the take's launch values
and moves as envelopes of the device's slots (see
[Edit an effect's parameters from Live](#edit-an-effects-parameters-from-live)).
Without a device, launch values and moves stay in the Tolglow record and the
player replays them.

**Playback.** Playback is wired from Live to the show:
- The companion's `watch-run-tracks` observes `Track.playing_slot_index` on the
  run tracks, with reads deferred out of the notification.
- It emits each clip start and stop once. An event carries the clip ID, a
  serial and the companion's watch-session ID.
- `node_bridge.js` forwards these `run-clip` events to Studio's `/api/live`.
- The Live sync manager hands them to the runtime's `show/live-run-clip!`.
  The REPL can call the same function.
- `show/live-run-clip!` stamps each event with the show transport beat on
  arrival and only queues it (at most 256). Queued and dropped events are
  reported in the projection as `:playback-events`. This is the one place Live song
  time meets the show clock; the companion's `song-beat` is kept for
  diagnostics.
- The owner thread decodes the queue at its next cycle and submits the
  commands. The callback never waits for the owner, and the owner never waits
  for a callback; a two-thread test guards this.

`live-runs/player` decodes the events:
- A clip start launches the take's cue once with its recorded launch. A held
  launch replays as persistent, because the clip owns its release. The launch
  carries origin `:live-playback`, `:live/take-id` and the recorded
  `:live/effect-version`.
- The owner binds each receipt from the submitted command's reply. A refused
  launch is dropped, never left pending. The owner then restores a
  per-instance group placement, replays the recorded moves at their held-time
  offsets (a move after a seek keeps its offset into the run), and releases
  after the take's actual held length, also for a timed run stopped early.
  Each of these lands one owner cycle after it is due.
- An end the owner queue refused (for example because the queue was full) is
  sent again while its instance still runs, at most eight times, so a
  replayed cue cannot keep running.
- A playing entry whose instance already ended is dropped.
- A stop ends the instance, even before its launch is acknowledged.
- A repeat of the same watch session and serial launches nothing. A reloaded
  companion's new session is not taken for a repeat.
- Unknown clips are counted, and the last 16 kept.

Playback never makes a take, so a played clip cannot record another clip.

Playback does not reproduce:
- the effect revision: playback runs the cue's current definition;
- modulation or other writers active at the time;
- a clip edited in Live: the take's length is used, not the edited length;
- Arrangement playback: `playing_slot_index` reports Session slots only.

**Projection.** `show/live-runs`, Studio's **Connections → Live runs** and
`GET /api/live-runs?limit=n` list the newest takes, 256 by default and at most
4096. They report `:shown` and `:truncated?`, along with each take's clip and
Arrangement status.

**Evidence.**
- `live-runs-test` performs on a real `show/create-runtime`:
  - A held 1–4, B 2–7 launched from Studio, A 6–10, then two overlapping runs
    of A. Two A takes are on lane 0, one B take, and the overlapping runs are
    on lanes 0 and 1.
  - It mirrors them into the simulated Set with Group Tracks.
  - Failure and edge cases: groups that cannot be created, a lost
    acknowledgement, a deleted clip, a running take, Arrangement placement
    after a loop with a failed copy repaired, seeks, restarts, a 2,000-move
    burst, launch replay, a companion reload, origin marking and the projection
    limit.
  - It feeds a run-clip event shaped exactly as the companion sends it through
    `show/live-run-clip!` and lets the owner replay the move and the release.
- `live-sync-test` routes a `run-clip` callback to the show and gives
  `live-input` MIDI its origin.
- `ableton-test` checks that live input is not recorded back.
- The `live-companion-contract-test` case mirrors a record over real HTTP,
  with Arrangement copies, through the bridge, `max_adapter.js` and
  `live_session.js`.
- The JavaScript tests cover the bridge forwarding run-clip events, Group
  Track inspection, group placement, scenes, non-looping clips, `place-clip`,
  the playback watch with its session ID, origin marking and closing the
  watches when the device stops.

## Edit an effect's parameters from Live

Issue #625, first slice. The Tolglow side, the companion operations, the
in-device script and their HTTP contract are tested against the simulated
Set and a simulated Live object graph. Nothing here has run in a licensed
Live Set; the host checklist is on the pull request.

**One control projection.** `tolglow.authoring.parameter-controls` describes a
parameter's control: label, unit, hard bounds, readout decimals (five for a
normalized 16-bit channel on this patch, as Studio's readouts), input step
and control family (waveform selector, waveform glyph choices, choices,
enum, toggle, colour, vector, range, integer or number, with rotary, fader or
field). Studio's parameter rows and the Live device read it.

**Slots.** `tolglow.integrations.live-devices` gives every parameter component
of a cue channel a stable slot:
- numbers and integers with bounds (or a `:recommended` range) are one slot in
  their own units; choices, enums and booleans are enumeration slots;
- a colour is three slots, hue, saturation and brightness in [0, 1], edited
  through `afterglow.control.parameter-edit` as Push's encoders edit it; a
  vector (up to four components) and a range are one slot per component;
- gradients, unbounded numbers and longer vectors are listed under
  `:unsupported`.

Slots follow Studio's parameter order. A parameter added by a later revision
takes the next free slot, a removed one keeps its slot (shown as unused) and a
re-added one gets its old slot back, so automation recorded in Live stays on
its parameter. Layouts live in memory. On a channel's first sync after a
start, the layout is rebuilt from the slot names its devices show in the Set
(`seed-from-set`). Names are merged across every lane track's devices: a
configured name wins over an unconfigured `slot n` whichever device is read
first, and two devices naming one slot differently are reported as
`:slot-name-conflict` with the slot reserved. A name the current revision
still produces keeps its slot, any other configured name (an Unused slot, a
removed parameter) stays reserved, and only new parameters get new slots.
Labels two parameters share get the parameter ID appended (`Level [gain]`),
so their names do not depend on order. So a restart with a revision
that dropped a parameter does not move automation onto another one. A device
removed from the Set, or a Set opened without its devices, loses that memory.
The spec follows the revision a cue launches (an `:effect-version` pin), and
values decode through the running instance's own definition. A device holds 32 slots; a channel can use up to eight devices
(`Tolglow parameters`, `Tolglow parameters 2`, ...). Banks of eight follow
Studio's order, as on Push's encoders.

**The device.** `tolglow-parameters.maxpat` declares 32 automatable `live.dial`
slots (`slot-1` .. `slot-32`) after Live's Device On, and runs
`parameter_device.js`. The Live Object Model reads `DeviceParameter.name`,
`min` and `max` but cannot set them, and it inserts only native devices
(`Track.insert_device`, Live 12.3). So:
- the performer drops the device on a run track (or a template holds it);
- `configure-parameter-device` hands the slot layout to the device through
  Max's global `messnamed`; only the device whose Live id matches applies it,
  as `_parameter_longname`, `_parameter_type`, `_parameter_mmin`/`mmax`,
  `_parameter_steps`, `_parameter_units` (printf format with the unit) and
  `_parameter_range` (choices) messages to its dials;
- the next `inspect-set` reads every slot's name and range back, and the
  mirror reconfigures a device whose read-back differs.

**Sync.** `show/sync-live-runs!` (Studio: **Sync to Live**):
- refreshes each channel's layout and spec;
- finds the devices on each bound run track by name (a missing one is
  reported in `:parameter-devices`, not as a problem);
- writes each new run clip with `:parameter-envelopes`, one step per point,
  in the slot's own units, when every device the channel needs is present.
  Envelopes larger than one request are left out and reported as
  `:envelopes-too-large`; that take keeps replaying its moves itself;
- watches the bound run tracks (`watch-run-tracks`, also every slot's
  `value` and `automation_state`). `{:watch? false}` skips the watch.

A clip with envelopes (`Clip.has_envelopes` in the read-back) replays only
its launch and release from Tolglow: its moves come from Live automation, so
an envelope edited in Live is what plays.

**Values from Live.** Each slot change reaches the show as a
`parameter-device` callback through the bridge, then
`show/live-parameter-device-value!`, which only queues it (at most 1024). The
owner decodes the queue at its next cycle:
- the slot steers the instance its run track's playing clip launched, else
  the open run on that track's lane;
- `automation_state` 1 (automation playing) gives origin `:live-playback`:
  no take is made. Anything else is `:live-input`: a performance, kept as a
  take move;
- one `:param/set` per instance parameter per cycle, the last value winning;
  repeated serials are dropped;
- a slot whose parameter the instance's definition no longer has in that
  shape (removed, a number become a colour, a vector shortened) is dropped as
  `:retired` before decoding, and a value that cannot be applied is dropped as
  `:invalid`; nothing from Live can throw in the owner step;
- a report that only changes `automation_state` (an override, a re-enable)
  updates ownership and moves nothing;
- while a replayed run clip waits for its launch acknowledgement, its track's
  values wait too (up to 64) and apply to that replay once it exists; they
  are dropped if the clip stops first.

The slot watch reads each slot's value and state when it starts, so LiveAPI's
first callback is not taken for a performer's move.

**Tolglow's writes in Live.** Every accepted `:param/set` from Studio, Push,
the REPL or an agent on an instance shown on a run track is sent to its slot
(`set-parameter-values`) by the runtime's one writer thread: sends leave in
the order the owner accepted them, coalesced per track, and a slow Live never
adds threads. Only tracks whose device the last sync found are written. Each
write joins its slot's outstanding writes (up to 16, forgotten after 3 s); a
value Live reports that matches any of them (within 1e-5 of a continuous
range, half a step otherwise) is that write's echo and retires it and every
older one, so a drag's echoes never come back as performer moves. On an
integer, choice or toggle slot, and at either end of a range, only the newest
outstanding write may match, so a performer picking an earlier value is a
move. The companion remembers the value it last set on each watched slot
(compared as Live's 32-bit float) and does not report it, skips a slot
already showing the value, and lists those slots as `:own` in its receipt;
Tolglow then stops waiting for their echoes. The slot holds Live automation until
Live reports the automation playing again after an override, or a run clip
starts on that track. Nothing from Live (`:live-playback`, `:live-input`) or
`:tolglow-feedback` is written back. A send that did not execute forgets only
its own writes. Without a connected bridge the writes are counted as not
sent. A ramp's committed values are `:param/set` commands and are mirrored;
its per-frame values, like modulation, are not. A Tolglow colour write drops
the hue a previous Live turn cached.

**Cost.** The owner decodes at most 256 slot values per cycle and keeps the
rest queued. Classifying a slot is memoised per decode, and the waveform
catalog's index is built once per catalog change (it was rebuilt on every
question, several times per value). With the built-in waveform catalog in
the pooled REPL: 5 values take about 60 µs, 50 values about 210 µs, 256
values (the per-cycle cap) about 660 µs; collecting three writes about
25 µs. The projection reports `:cost` from the running show.
Sending, rendering and every Live request stay off the owner thread.

**Shared parameter frames.** `tolglow.visualization.parameter-frames` renders a
parameter's recent history once for every compatible view:
- a view is `{:instance-id :param :window-beats :style :width :height :dpr}`;
  views of the same instance, parameter, window, style and pixel ratio within
  10% of a size share one render key;
- one worker samples each shown parameter once per cycle (100 ms) from the
  committed snapshot, renders each key once with Java2D into a PNG (skipped
  when neither the beat nor the value moved) and puts it in each view's
  mailbox (one frame by default; a full mailbox drops its
  oldest frame and counts it). A lease not read for 5 s expires; the last view
  leaving ends the worker;
- Studio and the REPL use `show/parameter-frames-action!` and
  `/api/parameter-frames` (`/subscribe`, `/frame?lease=`, `/unsubscribe`).
  Leases are random UUIDs; an expired or ended lease answers 410 and Studio
  subscribes again.

Measured on this Mac under other lanes' load (headless JVM, Java2D, no GPU):
a 960x192 frame takes about 3.4 ms, 2.7 ms of it PNG encoding; a 320x64
frame about 0.7 ms. Shared, a cycle costs one render however many views; two
or four separate views would cost two or four. Sampling took about 80 µs per
cycle in the browser check, which joined a Live-device view to Studio's frame
at 10 renders per second and saw the render stop when the last view left.
Delivery across the Live host boundary (the companion fetching the PNG and a
`jsui` drawing it), Push mounting the frame and GPU rendering are not built
or measured.

Evidence: `live-devices-test` (projection parity, stable slots, every control
shape's round trip, envelopes, ownership, and a real runtime against the
simulated Set), `live-companion-contract-test` (configure and envelopes over
HTTP through `node_bridge.js`, `max_adapter.js`, `live_session.js` and
`parameter_device.js`), `test/js/ableton-live/parameter_device.test.js`,
`parameter-frames-test`, the Studio event tests and
`dev/studio/live-parameter-devices-browser-qa.mjs`.

## Verified Live APIs and limits

The current official Live Object Model supports observing clip playback state
and position. For MIDI/warped clips, position is in clip beats; unwarped audio
uses seconds. Observing `Clip.notes` signals an edited note list, not each played
note. A MIDI device in the actual routed signal path is the practical source for
performed note/velocity/gate events. Note-editing functions such as
`get_all_notes_extended` and `add_new_notes` operate on clip content; they do not
replace a playback event stream. [Clip reference](https://docs.cycling74.com/apiref/lom/clip/)

`Track.playing_slot_index` and `clip_slots` identify Session View activity.
Track meters can be observed, but `output_meter_level` is a held peak with a
one-second hold, while left/right meters are smoothed display values. They must
not be labeled instantaneous RMS, pitch or beat analysis.
[Track reference](https://docs.cycling74.com/apiref/lom/track/)

`DeviceParameter.value` and `automation_state` are observable and expose actual
parameter values/automation status. This is sufficient for parameter observation
but does not establish an arbitrary automation-clip editing API. Parameter
minimum, maximum and quantization metadata should determine a calibrated source
range. [DeviceParameter reference](https://docs.cycling74.com/apiref/lom/deviceparameter/)

MSP `plugin~` receives the host-routed audio stream. Audio analysis belongs in a
dedicated audio device/chain or explicit loopback capture, with measured window
latency. LiveAPI JavaScript must run in Max's low-priority context after device
initialization; it is not an audio-thread callback.
[plugin~](https://docs.cycling74.com/reference/plugin~/),
[LiveAPI threading](https://docs.cycling74.com/apiref/js/liveapi/)

## afterglow-max versus a separate engine

Inspected upstream commit `c72b78d882522d934449aec5b0a89ba0542505fc`, committed
2024-11-11. This is source inspection, not a Max/Live installation test.

The upstream package hosts Clojure objects through Max's `mxj` JVM. Its documented
objects expose cue control, variables, metronome, next-frame callbacks and REPL
evaluation. The README still includes Max 7 installation guidance and an old
Apple Java 6 requirement; those claims should not be treated as current host
qualification. [Inspected README](https://github.com/Deep-Symmetry/afterglow-max/blob/c72b78d882522d934449aec5b0a89ba0542505fc/README.md)

The project declares Clojure 1.12.0, historical Afterglow `0.2.6-SNAPSHOT`, Timbre
5.2.1 and provided `local/max 0.9`; the latter requires Max's own `max.jar`.
The package license is EPL-1.0. [Project manifest](https://github.com/Deep-Symmetry/afterglow-max/blob/c72b78d882522d934449aec5b0a89ba0542505fc/project.clj),
[license](https://github.com/Deep-Symmetry/afterglow-max/blob/c72b78d882522d934449aec5b0a89ba0542505fc/LICENSE)

Its initialization opens legacy MIDI inputs/outputs on a background future and
loads `init.clj`; cue wrappers access the old global show and register mutable
callbacks. Reusing that initialization would reintroduce the runtime architecture
being removed. [Initialization source](https://github.com/Deep-Symmetry/afterglow-max/blob/c72b78d882522d934449aec5b0a89ba0542505fc/src/afterglow/max/core.clj),
[cue wrapper](https://github.com/Deep-Symmetry/afterglow-max/blob/c72b78d882522d934449aec5b0a89ba0542505fc/src/afterglow/max/Cue.clj)

Current Cycling '74 documentation describes Java support as a separately
installed, open-source extension; modern `mxj` documentation discusses current
JVM installation, including M1 machines. Actual compatible Max/Java/macOS
versions still need measurement on the intended computers.
[Extending Max](https://cycling74.com/products/extend-max),
[mxj reference](https://docs.cycling74.com/reference/mxj/)

| Host option | Assessment for the new implementation |
| --- | --- |
| Separate Ring/JVM engine + small Max device | Current direction. Own JVM/runtime version, resource lifecycle, persisted model and REPL independently; bridge faults need not kill the lighting engine. Network/queue latency must be measured. |
| New thin `mxj` host | Feasible optional experiment, reusing the new engine and commands. Requires Max SDK/JVM packaging, callback-thread tests, explicit close/save/load behavior and no legacy initialization. Sharing Live's process increases the consequences of JVM/native faults. |
| Existing afterglow-max unmodified | Useful reference, unsuitable as the new engine host because it directly depends on the old runtime. No actual host-stability claim was established. |
| Python Remote Script | Alternative for controller/session observations. Live 11+ requires Python 3. Verify APIs and cleanup against exact Live versions; do not infer unsupported automation or audio DSP capabilities from Python access. |

The process-isolation preference above is an engineering judgment, not a measured
performance comparison. [Ableton's Remote Script installation guidance](https://help.ableton.com/hc/en-us/articles/209072009-Installing-third-party-remote-scripts)

## Link, AbleSet and two computers

Ableton Link is distinct from Pioneer Pro DJ Link/Beat Link. Link shares tempo
and a relationship between independent beat timelines; start/stop sharing is
optional. The currently published documentation also describes optional
arrangement/audio capabilities, so support must be qualified by SDK and actual
Live version. Captured session state is a natural immutable boundary; Link's
audio-thread and application-thread APIs have different blocking constraints.
No Link adapter is implemented by the current sync manager.
[Link concepts/API](https://ableton.github.io/link/)

The official SDK is C++ with a C wrapper under `extensions/abl_link`, licensed
under GPLv2-or-later or a separately obtained proprietary license. A native
binding/sidecar needs an explicit packaging/license choice and lifecycle tests;
adding Beat Link does not provide it.
[Official Link repository](https://github.com/Ableton/link)

AbleSet documents OSC control for Standard/Pro on UDP port 39051, with Bonjour
`_osc._udp.` discovery and `server=ableset` TXT metadata. Its documented commands
cover transport, tempo, setlist/section jumps and event subscriptions. No stable
public HTTP/WebSocket API was verified here. A future adapter should use the
documented OSC contract and saved explicit peer identity, not scrape the web UI.
[AbleSet OSC](https://ableset.com/docs/osc)

AbleNet's documented redundant playback configuration uses the exact same Live
Set on both computers, preferably Ethernet. It supports explicit peer addresses;
automatic drift correction uses phase nudging and is recommended on the backup
only, with audio clips warped. A two-computer setup with different performance
roles is not automatically the same as this redundancy model.
[AbleNet guidance](https://ableset.com/docs/ablenet)

For this project, save the role of each paired computer, selected musical source,
disconnect policy and the single lighting-output authority. Reconnect must not
silently transfer output to another discovered peer. A deliberate handoff should
invalidate old session callbacks and remap activation ownership. Acceptance needs
the actual two computers, exact Live/Max/Link/AbleSet versions, transport seeks,
tempo changes, disconnection/rejoin and proof that only one owner emits lighting.

## Validation evidence

The Connections page now mounts a re-frame patch editor with observed-source
selection, bounded scalar cue parameters, mapping curves/ranges/inversion,
quantization, independent musical ADSR stage units, smoothing and destination
mixing. It saves EDN through `POST /api/patch/save` so dynamic port IDs remain
strings. `GET /api/patch` reads settings/activity; `POST /api/patch/enabled` is
the separate explicit activation action. Opening or refreshing the panel never
starts a device. Local edits survive a delayed save response, and callbacks are
scoped to runtime identity, show epoch and request generation. A replaced show
invalidates its previous drafts. The frontend owner performs release-bundle and
viewport checks; server-rendered markup and event tests do not prove iPad Safari
or physical hardware behavior.

The pure patch suite passes **17 tests / 121 assertions**: calibrated
mapping, every combination policy, deterministic replay, frame-cadence smoothing,
musical pause/release, short same-batch gates, target replacement, separate tails,
confidence/timeout handling, feedback/dedup/epoch gating, captured-state restore,
activation remapping, musical seek rebasing and invalid descriptions.
No Live application, physical MIDI or audio device was used. Sync boundary and
companion-asset tests are documented separately in `sync-progress.md`; passing
these does not qualify complete Live/Link/AbleSet hardware interoperability.
