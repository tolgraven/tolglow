# Parameter widget parity checkpoint

This is an implementation checkpoint, not a claim that the complete device or
web interface has reached parity. The current work is shared by the performance
board, waveform picker and passive Push display. Device capture throughput and
physical Push behavior require their separate validation lanes.

## Evidence and gaps

The prior in-session Push capture (`/tmp/studio-qa/push-display.png`) shows eight
120-pixel columns containing names, values and simple meters. It lacks visible
numeric endpoints, units, bipolar zero and numeric rotary geometry. The existing
web waveform picker already supported real server-sampled shape and derivative
charts, but only after disclosure, and refused effect parameter expressions in
waveform bindings. That prevented a live compound waveform with parameter
bindings from showing its current result.

| Behavior | Current source | Validation / remaining work |
| --- | --- | --- |
| Numeric min/max/current/unit | Shared SVG dial and exact-value web field, existing accessible range retained | Unit coverage for endpoints, bounds, units; new release and touch QA pending |
| Bipolar zero and integer steps | Zero marker, integer tick projection, clamped keyboard and pointer operations | Pure behavior tests pass; real dial pointer QA pending |
| Base versus modulated value | Optional base/effective markers; web reads actual modulation override projection | Push host must supply authoritative metadata; no invented ghost values |
| Choice and enum | Selected label, adjacent options and selection position | Numeric choice storage preserved; live Push capture pending |
| Boolean | Explicit switch state and On/Off label | Passive rendering; physical controller behavior remains separate |
| Color/vector/range | Color swatch/components, component tracks, explicit range endpoints | Three/four displayed components are bounded; richer component editors/pickers remain open |
| Resulting generated waveform | Always-mounted compact web chart, with full prior derivative/cycle/detail controls retained | Host live/context payload integration in progress; actual live parameter-change QA required |
| Push generated waveform | Shared renderer consumes supplied `:preview-samples` | Backend cache/projection and actual changing-shape capture pending |
| Captured audio waveform | Shared fixed-amplitude peak-window renderer with low/mid/high energy colors and supplied beat markers | Actual PCM history projection and web/Push mounting pending; no synthetic waveform fallback |
| Multiple running effects | All active instances have independent cards, parameters and system actions; compact launch rows preserve all grid coordinates | Five desktop browser workflows passed before newest dial changes; final phone/iPad/new-widget release QA pending |

The historical pre-migration waveform widget mentioned by the user has not yet
been identified in the available historical web sources. The checks above audit
the preceding implementation in this workspace. Historical interaction parity
must be verified from its actual source/capture before closing that requirement.

## Data and lifecycle contracts

The display consumes data only. `parameter-widgets/typed-display` accepts a
parameter map with `:schema`, `:value`, optional actual `:base-value`,
`:effective-value` and numeric `:preview-samples`. It does not sample oscillators,
advance the transport or acquire a device. The shared numeric web control emits
values through the existing parameter command path.

Live waveform requests use `:instance-id`, `:parameter`, `:input-key` and bounded
`:options`. The input key invalidates the client cache when the current effective
parameter inputs or definitions change; the host resolves authoritative data.
Draft requests use explicit `:selection` and `:context-params`. Expression bindings
are sent to the host without browser evaluation. Requests remain coalesced, with
one request in flight per picker and runtime/epoch stale-response guards.

Captured audio accepts at most 512 actual bins with `:min`, `:max`, `:rms` and
`:bands {:low :mid :high}` plus duration/sample-rate/source metadata and optional
beat markers. Peaks are plotted against fixed −1…1 amplitude. Band energy blends
blue, amber and pale colors; a silent bin stays silent. This is a frequency-band
visual convention inspired by DJ displays, not a claim to reproduce a proprietary
Rekordbox analysis algorithm. Missing samples display an explicit empty state.

## Verification checkpoint

The latest node suite passed **120 tests / 644 assertions**, with zero failures,
errors or compiler warnings. New tests exercise rotary bounds, integer steps,
bipolar zero, choice typing, real audio-bin validation/band color, live preview
invalidation during an in-flight request, and draft expression transport.
The optimized release and actual browser/device visual validation of the newest
shared widgets are still required. The prior optimized performance board passed
independent multi-instance edits, pause/manual edit/restore, musical envelope,
category targeting and complete-grid access in Chromium.
