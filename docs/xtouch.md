# X-Touch and extender MCU adapter

The new `afterglow.control.codecs.x-touch` and
`afterglow.control.devices.x-touch` modules implement a source-verified MCU-mode
subset for the full-size X-Touch and XT extender. The profile composes the shared
control model, bindings, session lifecycle and injectable MIDI transport. It has
not been tested on physical hardware. X-Touch Compact/Mini and XCTL Ethernet are
different protocol/mode targets and are not covered by this implementation.

## Inspected references and provenance

The requested Ableton remote script is
[Kik07L/Behringer-X-Touch-for-ableton](https://github.com/Kik07L/Behringer-X-Touch-for-ableton),
inspected at `7d31caf30cf4ffeeeb8369f1216436ff8789150b` on 2026-11-21. Its README
targets Live 12 and recommends release downloads because the main branch may
contain untested features. These findings describe that exact inspected source,
not a claim that Tolglow has run it inside Live.

Relevant files:

- [README/features](https://github.com/Kik07L/Behringer-X-Touch-for-ableton/blob/7d31caf30cf4ffeeeb8369f1216436ff8789150b/readme.md):
  native colors, color alternation, fader calibration, rack-chain behavior and
  extender installation.
- [consts.py](https://github.com/Kik07L/Behringer-X-Touch-for-ableton/blob/7d31caf30cf4ffeeeb8369f1216436ff8789150b/XTouch/consts.py):
  MCU note/CC addresses, main/XT IDs and feedback modes.
- [MainDisplay.py](https://github.com/Kik07L/Behringer-X-Touch-for-ableton/blob/7d31caf30cf4ffeeeb8369f1216436ff8789150b/XTouch/MainDisplay.py):
  two-row LCD text and native color messages.
- [ChannelStrip.py](https://github.com/Kik07L/Behringer-X-Touch-for-ableton/blob/7d31caf30cf4ffeeeb8369f1216436ff8789150b/XTouch/ChannelStrip.py):
  pitch-bend faders, capacitive touch and encoder-ring feedback.
- [MainDisplayController.py](https://github.com/Kik07L/Behringer-X-Touch-for-ableton/blob/7d31caf30cf4ffeeeb8369f1216436ff8789150b/XTouch/MainDisplayController.py):
  the actual color-alternation mechanism and timing behavior.
- [MackieControlXT.py](https://github.com/Kik07L/Behringer-X-Touch-for-ableton/blob/7d31caf30cf4ffeeeb8369f1216436ff8789150b/XTouchXT/MackieControlXT.py):
  extender strip offsets, touch and MIDI routing.

No explicit redistribution license was found in that inspected repository.
Several Python files retain embedded paths from Ableton's bundled scripts. The
new Clojure implementation is independently written from the documented protocol
facts; none of that Python or its bundled device assets is vendored.

Standard MCU addresses and the 56-column display layout were also checked
against [Control Surface](https://github.com/tttapa/Control-Surface) at
`0cfe26966c651c6dc4913e62acee2b99de449ea3`, specifically
[MCU.hpp](https://github.com/tttapa/Control-Surface/blob/0cfe26966c651c6dc4913e62acee2b99de449ea3/src/MIDI_Constants/MCU.hpp)
and [LCD.hpp](https://github.com/tttapa/Control-Surface/blob/0cfe26966c651c6dc4913e62acee2b99de449ea3/src/MIDI_Inputs/MCU/LCD.hpp).
That project carries GPL-3.0; it is a protocol reference, not a bundled dependency.

## Verified message subset

All data bytes are seven-bit unsigned values. The table uses hexadecimal bytes
and zero-based channel/strip indices.

| Function | Source-verified representation |
| --- | --- |
| Channel faders | Pitch bend `E0 + strip`, LSB, MSB; 14-bit value 0..16383 |
| Main master fader | Pitch-bend channel 8; extender ignores this control |
| Buttons | Channel 0 note-on/off; positive note-on velocity presses, note-off always releases |
| Fader touch | Notes `68..70` hex for eight strips plus main master |
| Encoders | CC `10..17` hex; low 6 bits magnitude and bit 6 direction |
| Jog | CC `3C` with the same signed-relative encoding; decoded but not assigned by default |
| Encoder rings | CC `30..37`, mode×16 + position 0..11 |
| Button feedback | Note-on velocity 0 off, 1 blink, 127 on |
| Text | `F0 00 00 66 device 12 offset ASCII... F7`; second row begins at offset 56 |
| Scribble colors | `F0 00 00 66 device 72 c0 c1 c2 c3 c4 c5 c6 c7 F7` |
| Device selection | `14` main, `15` extender |

Native color indices are black, red, green, yellow, blue, magenta, cyan and white
in that order. Active dark RGB values choose a visible nonblack approximation;
exact black and unused strips can remain black. Text replaces unsupported glyphs
with `?`; the adapter does not pretend the hardware accepts Unicode.

## What the color/PWM experiment actually does

The inspected script's color-mix mode repeatedly sends ordinary native palette
indices. It approximates additional colors by temporal alternation. Its README
and implementation describe a default two-second burst that freezes other
interface updates when Live's update loop is too slow. A documented experimental
Live `Options.txt` display-update setting allows the alternation to run in the
regular loop. The source's default alternation interval is 20 ms.

This is evidence for software temporal dithering, not a discovered arbitrary-RGB
or native PWM command. The README itself notes flicker and uncertain hardware
wear. Color appearance, firmware acceptance, rate limits and interaction latency
require physical measurements. Tolglow's ordinary adapter uses stable native
colors and never starts that cycling mode.

`codec/temporal-color-frame` is an explicit pure research helper returning one
palette index for an RGB duty sample. It has no timer, MIDI send or sleep. It can
be tested mathematically without implying that a given update rate produces a
pleasant or accurate physical color. Any future live experiment needs its own
bounded display scheduler, explicit user activation and output-load measurement;
it must not stall show/control processing as the reference's burst path does.

## Implemented controls and ownership

A profile maps an ordered channel vector to eight strips. A channel contains
`:cue-id`, optional `:label`/normalized display `:color`, and `:fader`/`:encoder`
bindings. A binding is a parameter keyword or `{:param key :component index}`.
Numeric, integer, boolean and enum controls have explicit schema conversions;
vector/color components preserve their other values. Default fader and encoder
bindings address `:level` on the latest activation of that cue.

- SELECT toggles a persistent activation; a second end on a releasing instance
  forces that instance's removal. MUTE requests release of the active instance.
- REC holds a cue and releases the exact instance recorded by its launch
  acknowledgement, even if the bank changes while held. SOLO selects a local
  strip focus; it does not invent a global solo policy.
- Encoder push restores the selected parameter's declared default. FLIP swaps
  fader/encoder bindings on the main unit. PLAY/STOP use the shared transport.
- Touch captures a fader's instance/parameter. Changing bank while a hand remains
  on the fader cannot redirect that gesture to another activation. Untouched
  fader input is ignored by default, preventing feedback echoes from being
  interpreted as intentional edits. Motor feedback is suppressed while touched.
- Feedback projects labels, values, native colors, rings and LEDs from actual
  model state and emits only changed messages. The main master fader maps linearly
  to lighting master intensity; Live-specific dB calibration is not applied.

The profile is pure. The existing session boundary owns MIDI, scheduling,
generation checks, acknowledgements and shutdown. `connect!` opens explicitly
selected ports or an injected transport; it never discovers a device on require.
Its default feedback scheduler is owned and stopped by the session. Pass
`:schedule! nil` for an externally driven refresh loop or tests.

`connect-group!` accepts an explicitly ordered `:units` vector with one main unit
and any extenders. Each has a distinct session ID and ports/transport. Offsets
follow the supplied physical order, including a main unit between extenders.
Main bank buttons move the whole group's width and channel buttons move one
slot. `bank-group!` provides explicit host control. `close-group!` closes every
session; a later startup failure closes earlier opened units. No port discovery
or cross-device global atom is involved.

```clojure
(require '[afterglow.control.devices.x-touch :as x-touch])

;; model!, dispatch! and now-ms! come from the shared host. dispatch! must return
;; a synchronous accepted/rejected acknowledgement, as for other controllers.
(def surface
  (x-touch/connect!
    {:session-id "xt-main" :input-index 0 :output-index 1
     :model! model! :dispatch! dispatch! :now-ms! now-ms!
     :channels [{:cue-id :ember :label "Ember" :color [1 0 0]
                 :fader :level :encoder {:param :color :component 1}}]}))

;; Close through afterglow.control.devices.session/close! when finished.
```

The indices above are placeholders for explicitly selected local ports, not
discovery results. A host must retain its show epoch in callbacks, as the studio
does for other controllers, and reject stale input across full-show restores.

## Verification and remaining work

Protocol tests use fixed independent byte examples taken from the cited protocol
facts, including both main and extender SysEx. These are source-derived fixtures,
not fabricated recordings from hardware. Session tests exercise the actual
control reducer with injected transports: launch/end, exact held identity,
touch ownership across banking, feedback diffs, typed/component parameters,
independent units, shared group banks, malformed input and partial-open cleanup.

The final focused run passed **10 tests / 138 assertions** with reflection
warnings enabled for the new codec/profile namespaces and no new warnings.
The inherited transport namespace still reports pre-existing reflection warnings
when loaded under that setting; it was not changed by this lane.

Physical MIDI/USB, firmware versions, motor calibration, actual display legibility,
multiple real extenders, Live handoff, group reconnection UX and temporal color
appearance remain unverified. The adapter is a reusable lighting-control subset,
not a complete reproduction of every Live remote-script feature. Rich device
widgets/category tools and wider automapping remain separately tracked in
[expressive performance](product/expressive-performance.md).
