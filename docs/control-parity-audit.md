# Control feature parity audit

The replacement does **not yet have full control parity**. A richer display does
not prove that the physical interactions behind it work. This audit compares
actual source on 2026-11-21; it is an acceptance checklist, not a hardware
verification report. Old source is a functional reference only. No runtime
compatibility adapter is proposed.

The main reference is
[the original Push 2 controller](../../afterglow/src/afterglow/controllers/ableton_push_2.clj).
The current adapter is
[the composable Push 2 profile](../../afterglow/src/afterglow/control/devices/push_2.clj).
Its declared capabilities are narrower than the original controller. Its
physical MIDI/display behavior still needs real-device verification.

## Behaviors that must be retained

| Behavior and source oracle | Current replacement evidence | Acceptance still required |
| --- | --- | --- |
| Cue pad toggles; a second press can force a releasing effect to end. Original cue-grid-pressed. | New press-pad emits instance-specific end/force commands; session tests exercise launch/end. | Real-device press/release and LED response. |
| Held cues release the exact launched instance. Original pad overlay keeps activation identity. | New bindings acknowledge launch IDs and queue late releases; session tests cover close/reconnect. | Physical pressure/release timing and display feedback. |
| Shift overrides a cue's held behavior. Original cue-grid-pressed tests Shift mode. | New profile does not track Shift. | Shared hold/latch override and concurrent-hold tests. |
| Velocity scales declared parameters; continued pressure updates velocity-controlled variables. Original overlay calls value-for-velocity. | New persistent launch includes velocity, but held launch does not pass it; Push 2 has no aftertouch input branch. | Velocity metadata/ranges and pressure ownership in both modes. MPE elsewhere does not satisfy Push 2 parity. |
| Several effects occupy display regions, with navigation through additional effects. Original room-for-effects and effect offsets. | Web board exposes concurrent cards; Push view projects one selected instance. | Push active-effect navigation and a legible overview without losing parameters. |
| Parameter banks scroll through every variable for each active effect. Original handle-scroll-cue-vars. | New encoders index the first eight alphabetically sorted parameter keys. | Ordered banks, selected-page feedback and tests for more than eight parameters. |
| Numeric gauge exposes range, centered/bipolar indication, current value and fine adjustments. | Numeric encoders clamp number/integer values; shared custom widgets are being expanded. | Schema order, units, precision, fine/coarse modifier and out-of-range indication in both view and input. |
| Dedicated boolean adjustment overlay. Original build-boolean-adjustment-overlay. | New Push encoders only accept number/integer. A passive display is insufficient. | Actual button/encoder selection command and feedback tests. |
| Color editing exposes hue/saturation gauges, pad-grid picker and touch-strip brightness. Original build-color-adjustment-overlay. | Web typed color controls exist; new Push input has no color overlay. | Shared color interaction, palette/grid selection, component edits and exact instance addressing. |
| Encoder touch opens detail; pitch-bend touch strip edits the selected variable with mode/LED feedback. Original display-encoder-touched, bend-variable-value and strip helpers. | New adapter has neither encoder-touch selection nor pitch-bend handling. | Overlay lifetime, strip normalization, LED mode and release behavior through composable controls. |
| Live waveform graph reflects current variables with a fixed now marker and colored beat/bar/phrase marks. Original draw-cue-visualizer. | Waveform picker now mounts a compact chart; effective-parameter backend projection is in progress. Push currently carries optional schema samples. | Changing resulting shape on both surfaces, phase alignment and musical markers, without extra stateful-effect evaluation. A name or static thumbnail fails parity. |
| Save current values as cue defaults and clear saved values. Original handle-save-effect and web save/clear endpoints. | Registry editing exists, but corresponding Push save/clear interaction is absent. | One shared operation, matching web/Push UX, revision/undo behavior and persistence. |
| Tap tempo, BPM adjustments, metronome view, beat/bar nudges and phrase reset. Original tempo/beat overlays and CC handler. | Web has BPM entry and play/pause. Push exposes master but no tempo/tap/beat controls. | Musical controls using authoritative clock and selected sync-source policy. |
| Explicit stop/run lifecycle. Original stop mode and web start/stop. | New CC85 toggles blackout; web play/pause controls transport. These operations differ. | Distinct, clearly labeled stop, blackout and transport pause; verify releases/output. |
| Optional linked scrolling between web and selected hardware controller. Original web link-controller and move listeners. | Web/page selection and Push origin are separate. | Shared viewport option with no feedback loop and preserved independent-device mode. |
| User-mode handoff restores palette/holds; reconnect reacquires state. | New session has suspend/resume, palette acquisition and cleanup tests. | Real Live/device handoff, no stuck notes and correct native display ownership. |
| Master encoder, LED state, palette preservation and acknowledged updates. | New adapter implements master, LED differences, palette restore and timeout handling. | Hardware throughput and missing-ack behavior; source-derived bytes are not hardware evidence. |
| Web launch modes, held release, contextual actions, copy/paste and composition slots. | Re-frame views/events include toggle/one-shot/held, radial actions, copying to empty cells and macro/sequence slots. Earlier browser gates exist. | Re-run after widget/layout changes; verify multitouch, scrolling and cancellation on real iPad Safari. |
| Web parameters/end controls remain usable with many effects. | Concurrent cards, per-instance and category controls now exist. | Browser acceptance for many effects, narrow layouts and edits without accidental launches. |

## Additions must not hide parity gaps

Enums, waveform selectors, vector/range parameters, categories, Push 3/MPE,
Seaboard, X-Touch/extenders, sensors and captured audio extend the original
feature set. Each needs both a common projection and working input mappings.
The numeric-only Push 2 encoder handler does not gain these interactions merely
because CSS can render them.

Captured audio must use actual bounded PCM history, musical markers and
documented frequency-color mapping. Generated waveform preview instead reflects
the selected signal and its current parameter context. Neither should advance a
stateful effect a second time to draw a picture.

## Follow-up order and verification

1. Close input/data gaps: ordered banks, typed editing, velocity, pressure,
   touch-strip/encoder touch and saved cue presets.
2. Restore timing/transport and effect navigation, then linked viewports.
3. Verify live generated-waveform shape and musical alignment on the optimized
   web build and actual offscreen Push capture.
4. Run shared-control transition regressions and browser interactions.
5. Perform physical Push/Live checks separately from captured MIDI, software
   WebGL and viewport emulation.

Current sources:
[performance board](../src/studio/tolglow/studio/performance.cljs),
[waveform widget](../src/studio/tolglow/studio/waveform.cljs),
[web events](../src/studio/tolglow/studio/events.cljs),
[original web routes](../../afterglow/src/afterglow/web/routes/show_control.clj),
[session tests](../../afterglow/test/afterglow/control/devices/session_test.clj).
This audit ran no hardware or new browser gates.
