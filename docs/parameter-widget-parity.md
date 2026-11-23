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
| Push generated waveform | Shared sparkline consumes supplied `:preview-samples` | Actual JVM helper → Node → Chromium → PNG capture changes the pulse shape after a live parameter command |
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
labelled as stopped. Capture starts through an explicit device action.

The web scope renders the bounded waveform from the visible Connections
subscription. A closed socket clears its displayed trace until delivery resumes. The
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

The frontend suite passed **136 tests / 717 assertions**, without failures,
errors or warnings. The optimized release compiled **147 files**, without
warnings, on Clojure 1.12.7, Java 25.0.5.1 and Node 26.13.1.

Composition slots resolve values in the same precedence as cue launches: effect
defaults, authored cue parameters, saved cue defaults, then slot overrides.
Component edits preserve the other saved components, and resetting a slot
reveals the saved cue values without changing that cue. A focused transition
regression covers this order; the backend saved-default integration has its own
host validation gate.

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

An additional actual `studio/push-browser-display` run submitted a real
`push/display-view` once, then changed pulse width from 0.25 to 0.75 through the
runtime command queue and tick. The generated waveform changed in the resulting
960×160 PNG. Switching the model's display setting to audio refreshed the same
view with live captured PCM, without another submission. The JVM/Node/Chromium
worker produced 23 captures with one submitted view and no dropped submissions;
this short correctness run is not a throughput benchmark. All owned resources
closed at the end. Physical USB output was replaced with an image capture sink.

Session evidence is under `/tmp/studio-qa/`: `integrated-ui-results.json`,
`push-navigation-results.json`, `performance-board-results.json`, their images,
`push-helper-capture.log`, `push-helper-live-first.png`,
`push-helper-live-changed.png`, `push-helper-audio-later.png`, and the build/test
logs. These temporary harnesses are session evidence, not
checked-in CI browser tests. Preview rendering was hidden for the control gates
so they do not establish graphics/control latency; the renderer worker has its
own integration gate. Chromium viewport emulation does not establish iPad Safari,
physical Push USB, or real audio-device behavior.

## Physical resolution and operator readouts

Studio inspectors and the passive Push 2 display resolve a control's physical channel width from its effect outputs, captured targets and current rig patch. For mixed targets the finest participating channel wins. For an eight-bit dimmer in normalized units, 127/255 reads **0.498** and 128/255 reads **0.502**. For a moving head with separate coarse and fine pan channels, 32767/65535 reads **0.49999** and 32768/65535 reads **0.50001**. Tilting uses the width of its own physical mapping. Fixture names and parameter names have no bearing on precision.

The controls carry `:dmx-bits` in their projected schemas. A declared unit range such as degrees determines how many decimal places distinguish a physical step. Free-running rates and other continuous parameters keep their native precision and use a compact readout, including scientific notation for very small values. Number fields expose their complete value while editing. Sending a command, evaluating effects, saving a show and packing DMX all use the original value.

The Push **Shift + encoder** gesture takes one physical code step for a parameter connected to an eight- or sixteen-bit channel. Ordinary turns retain the usual coarser editing speed. To reproduce the readout comparison on an isolated preview show, use the paired Afterglow revision pinned in CI, build Studio with `npm run studio:build`, then run `lein run -m tolglow.qa.parameter-precision target/precision`. It checks a real coarse/fine byte carry, captures the 960 × 160 display and edits a number in a live Studio inspector.
