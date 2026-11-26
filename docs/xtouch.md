# X-Touch and extender MCU adapter

The new `afterglow.control.codecs.x-touch` and
`afterglow.control.devices.x-touch` modules implement a source-verified MCU-mode
subset for the full-size X-Touch and XT extender. The profile composes the shared
control model, bindings, session lifecycle and injectable MIDI transport.
Scribble text, colours, motor faders and linked banking were run on an X-Touch
and X-Touch-Ext (firmware 1.22) on 2026-11-26; touch, encoders and button input
from a hand on the hardware are not yet verified. X-Touch Compact/Mini and XCTL Ethernet are
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

Afterglow's `connect-group!` still accepts an ordered `:units` vector with one
main unit and extenders, but Tolglow no longer uses it. Tolglow attaches each
unit as its own surface and links them (below). With `:model-bank` and
`:bank-command` a host owns the bank: every unit reads the committed window
and the main unit sends bank and channel buttons (SHIFT for first and last) as
commands instead of changing its own bank.

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

## Surfaces, links and the session box

Each X-Touch or X-Touch-Ext connected with `show/connect-device!` attaches as
its own control surface (`tolglow.surface.link`, descriptor
`:behringer/x-touch` or `:behringer/x-touch-ext`). A link joins the `:strips`
viewport of several surfaces into one logical surface: **extend** places the
next unit one bank to the right (or left, up, down), **mirror** shows the same
window on both. The window (the session box) is show data: the main unit's
BANK and CHANNEL buttons, SHIFT+BANK for the first and last page, Studio's
session box buttons and `show/bank-surface!` all commit the same
`:control-surface/bank` command, and every member follows on its next feedback.
Linked members show the first member's strips, so an extender needs no strip
configuration of its own.

```clojure
(show/connect-device! runtime :x-touch {:session-id "xt-main" :input-index 0 :output-index 6
                                         :effect-group-view :manual})
(show/connect-device! runtime :x-touch {:session-id "xt-ext" :input-index 5 :output-index 11
                                         :extender? true})
@(show/link-surfaces! runtime "desk" ["xt-main" "xt-ext"])
@(show/bank-surface! runtime "desk" :last)
```

Link modes, all saved with the show as
`{:mode :members [{:surface-id :viewport :offset}] :direction :window}`:

- **extend** (`:direction` `:right`, `:left`, `:down` or `:up`) packs each
  member beside the previous one by its actual width, so a 4-strip unit next
  to an 8-strip one leaves no gap or overlap. The window pages by the whole
  span (16 for main plus extender).
- **mirror** shows the leader's window on every member and pages by the
  leader's width.
- **follow** keeps each later member `:offset` pages after the leader (for
  example a Studio page one page to the right of a Push) and pages by the
  leader's width, so following never makes the leader skip pages.

Members link when their descriptors declare the same content axis for the
named viewports (`:channels` for X-Touch `:strips` and Push `:encoders`), not
when the viewport names match. Everything that fits starts at strip 1.
Otherwise windows stay on multiples of the leader's width, so with 20 strips
the second page shows strips 17–20 on whichever unit sits at the window's left
edge.

While a member is unavailable (disconnected or handed to Live) the extend
layout is repacked over the attached members only: the window, paging,
`:last` and reveal cover just the units that are there, so a lone main unit or
a lone extender pages through every strip from 1. The full layout returns when
the unit reattaches under the same session ID. A connected surface belongs to
its device: only its own connection can attach over it or remove it, and
detaching or removing an unknown surface is refused. Links and windows are
saved with the show; surfaces load detached. Touch ownership, held cues and
motor feedback stay per unit: a fader held across a bank change keeps writing
the strip it touched. See
[REPL and Studio parity](repl-studio-parity.md#link-control-surfaces-and-move-their-session-box)
and issues #705 and #706.

### Hardware run

`lein run -m tolglow.qa.xtouch-link-hardware` attaches the two units on this
Mac, links, banks, sets one fader per unit, mirrors, and leaves both units at
rest (faders at 0, rings and LEDs off, LCDs blank, scribble colours black). It
opens only the two MCU ports and has no rig output. Run on 2026-11-26 with Live
quit; see the PR for the receipt and the timetable of what the units showed.

### Committed effect group levels

In the Tolglow REPL, opt in when connecting an MCU unit to the running show.

```clojure
(require '[tolglow.show :as show])

(show/connect-device! runtime :x-touch
  {:session-id "xt-mix" :input-index 0 :output-index 1
   :effect-group-view :manual
   :channels [{:cue-id :ember :fader :level :encoder {:param :color :component 1}}]})
```

Manual groups occupy strips in authored order, with children immediately after
their parent. Use `:effect-group-view :overlay` to bank the current fixture,
category, or cue-page overlay. Configured cue channels follow the group strips.
Group slots stay in place when instances launch or release. Each group shows its
name, committed level, and whether it has active members. Manual roots use cyan,
children use blue, and overlay groups use magenta.

The fader commits the same `:effect-groups/level-set` command used by Studio.
Group and cue faders in this view require touch. A held fader keeps the channel
it was touching across bank changes, then returns to the current bank's
committed value on release. Flip and encoders apply to the cue channels. Use
`(show/effect-group-channels runtime)` to inspect membership and the effective
levels of member instances. Studio's command endpoint updates the same levels.

Suspend and reconnect clear touches. The bank is the surface's committed
window, so a reconnected unit returns to it.

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
