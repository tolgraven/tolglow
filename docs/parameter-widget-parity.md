# Parameter widget parity checkpoint

This is an implementation checkpoint, not a claim of complete web or physical
controller parity. Studio and the passive Push display share Reagent/CSS widgets.
The host supplies authoritative values and sampled waveforms; a display never
advances an oscillator, opens a device, or creates synthetic readings.

## Implemented and verified

| Behavior | Current behavior | Evidence and remaining limits |
| --- | --- | --- |
| Numeric min/max/current/unit | Shared SVG rotary, endpoints, units, exact-value field and accessible range | Bounds, bipolar zero, integer steps, rapid keyboard repeats and real pointer operations pass |
| Base versus modulated value | Optional base/effective markers from actual modulation projection | No invented ghost values; Push needs the corresponding host metadata |
| Choice and enum | Selected label, adjacent options and selection position | Numeric choice storage preserved; optimized passive display checked |
| Boolean | Explicit switch and On/Off label | Rendering verified; physical controller behavior has its own lane |
| Color/vector/range | Swatch, bounded component tracks and range endpoints | Push shows the component controlled by its encoder; richer web component pickers remain open |
| Generated waveform | Always-visible compact result plus slope, angular velocity, jagginess, cycles and sample detail | Actual Ring preview changes when another live parameter changes; 513-sample detail and selector verified |
| Push generated waveform | Shared sparkline consumes supplied `:preview-samples` | Passive rendering verified; backend projection tests are separate from physical display evidence |
| Captured audio | Actual peak windows, fixed amplitude, low/mid/high energy colors and supplied beat/bar markers | Timed signed-16 PCM source exercised the production capture/analyzer pipeline, web scope and Push layout |
| Push audio mode | Full-width audio scope with eight compact encoder value columns | Actual 960×160 browser screenshot; capture data supplied from the real audio endpoint |
| Push navigation | Parameter-bank range, active-effect position/dots, fine mode, touched encoder and selected component | Bounded projection and optimized passive rendering verified; no device commands originate in the view |
| Multiple effects | Independent cards/parameters/system actions, compact/full grid with all coordinates | Six real instances exercised; desktop, iPad-width and phone-width checks pass |

The historical pre-migration waveform widget mentioned by the user has not been
identified in the available historical web sources. These checks audit the
preceding implementation in this workspace. Historical interaction parity still
requires its source or a reliable capture before that broader requirement closes.

## Data and lifecycle

`parameter-widgets/typed-display` accepts `:schema`, `:value`, optional actual
`:base-value`, `:effective-value` and numeric `:preview-samples`. Live controls
send validated semantic parameter commands. Keyboard repeats reduce against
current app-db values, so rapid events do not reuse an earlier rendered value.

Live waveform requests carry `:instance-id`, `:parameter`, `:input-key` and bounded
`:options`. The host resolves authoritative effective parameters and definitions.
The client invalidates when those inputs change, and includes the captured clock
only for definitions with time/beat/bar/phrase bindings. Draft requests carry
explicit `:selection` and `:context-params`. Expression data is never evaluated by
the browser. One request remains in flight per picker, with runtime/epoch guards.

Audio views accept actual `:min`, `:max`, `:rms` and `:bands {:low :mid :high}` bins,
plus duration/sample-rate/source metadata and optional beat markers. Peaks are
plotted at fixed −1…1 amplitude. Blue, amber and pale colors reflect measured band
energy, inspired by DJ displays; this does not reproduce proprietary Rekordbox
analysis. Missing capture data has an explicit empty state. Stopped capture is
labelled as stopped. Mounting or polling a display never starts capture.

The web scope owns one abortable request and a bounded timer. Teardown, show
replacement, process replacement, errors and stale responses are covered. The
Push view is entirely passive: the host pushes snapshots synchronously, with no
API polling, WebGL, editors, audio acquisition or lighting commands. Its eight
encoder columns are preserved in both parameter and audio modes. The host bounds
Push audio to 128 bins to respect the display capture message size.

Push navigation metadata is retained as supplied: `:parameter-bank`,
`:active-effect-navigation`, up to eight `:active-effects`, `:touched-parameter`
and `:fine?`. Parameter `:component-label` identifies what encoder rotation affects;
its color/vector/range rendering still shows the complete value. A replacement
snapshot clears prior touch, bank and component state.

## Verification, 2026-11-21

The frontend suite passed **135 tests / 713 assertions**, without failures,
errors or warnings. The optimized release compiled **147 files**, without
warnings, on Clojure 1.12.7, Java 25.0.5.1 and Node 26.13.1.

Actual Chromium checks against the Ring fixture host passed ten workflows:
current live waveform shape/detail, all three physical output delivery modes,
timed PCM capture and stop, the passive Push audio layout, separate application
agent connection and fresh Git workspace, incremental messages/command/file
cards, actual diff/check/candidate results, cancellation, and phone/iPad widths.
The application agent fixture is a real JSONL subprocess editing a real Git
worktree; no provider call is claimed. Checks/candidate readiness is shown in the
chat and does not mean that code has been activated in the running application.

The dedicated passive Push gate passed typed eight-column rendering with bank,
active-effect position, component/touch/fine feedback, supplied waveform samples,
snapshot replacement, exact 960×160 geometry and absence of API/graphics/editor
activity. The six-instance performance board passed eight workflows, including
independent controls, musical release, category actions and complete-grid access.

Session evidence is under `/tmp/studio-qa/`: `integrated-ui-results.json`,
`push-navigation-results.json`, `performance-board-results.json`, their images,
and the build/test logs. These temporary harnesses are session evidence, not
checked-in CI browser tests. Preview rendering was hidden for the control gates
so they do not establish graphics/control latency; the renderer worker has its
own integration gate. Chromium viewport emulation does not establish iPad Safari,
physical Push USB, or real audio-device behavior.
