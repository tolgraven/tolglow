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
declaring an automation source does not create a Live observer. The graph is
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

## Sources available at the current boundary

The sync manager emits one `:sync/observation` event for each captured note or CC,
alongside any separately configured cue mapping. IDs are stable logical ports:
`midi/<source>/ch/<channel>/note/<note>`, `midi/<source>/ch/<channel>/cc/<cc>` and
equivalent `live/...` IDs. Channels are 1-based. Note-off and velocity-zero have
`:gate? false` and zero value; CC is continuous. Events include source/port kind,
capture timestamp, run/event IDs, confidence and origin. The host supplies its
authoritative epoch and musical clock; the adapter does not invent them.

The existing audio boundary supplies measured features and analysis events with
source/confidence metadata. The host can register those as audio ports, using
explicit value ranges and confidence gates. Merely selecting an audio port does
not start capture. The GUI should show actually observed or configured sources,
the units/range, stale status and current activity. Configuring the same input as
both a cue mapping and a patch route is explicit; neither path silently disables
the other.

Current Live MIDI capture is the companion device's routed MIDI stream, which
may include existing clip playback on that track. It is **not** a subscription
to every Session View track or Drum Rack chain. Session-wide note routing,
automation observers and MSP audio-feature emitters remain separate bridge work.
The patch kernel and source `:kind` fields do not imply those capabilities exist.

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
