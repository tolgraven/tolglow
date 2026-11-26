# Push 3 and X-Touch qualification on Naut

This kit closes the hardware gates of #219, #543, #553 and #310 and records
the tethered-display receipt for [Push display packaging](../push-display-packaging.md).
It also gives the display route that #529, #547, #552 and #530 need. Each check
is a command or REPL session that writes an EDN receipt under
`target/qa/naut-hardware/`. Every scripted check also runs in
`tolglow.qa.naut-hardware-test` against a virtual Push 3 or X-Touch at the same
MIDI transport boundary. The physical device is the only untested part.

## Operator script (Live quit, about 10 minutes)

Run from this worktree. Every command prints its receipt path; answer each
`[y/n]` question in the terminal. Nothing here touches lighting output, Live or
the FT232R. Each `lein run` takes about 15 s to start.

```sh
export AFTERGLOW_ROOT=/Users/tol/CODE/pixTol/tolglow/.claude/worktrees/afterglow JVM_OPTS=-Xmx3g
K="lein run -m tolglow.qa.naut-hardware"
```

| # | Command | You do | You look at |
| --- | --- | --- | --- |
| 1 | `$K motor x-touch` | Hands off, then answer y/n | All nine faders (master included) travel to 3/4, 1/4, 1/2, the top and the bottom, stopping at each |
| 2 | `$K motor x-touch-ext` | Same | The eight extender faders do the same |
| 3 | `$K lcd x-touch` | Answer y/n | Top row `XTouch1`…`XTouch8`, bottom row colour names; backlights step off, red, green, yellow, blue, magenta, cyan, white, then match the names; REC, SOLO, MUTE, SELECT rows light in turn; rings sweep. Ends blank and white |
| 4 | `$K lcd x-touch-ext` | Answer y/n | The same with `XT1`…`XT8` |
| 5 | `$K rate x-touch` | Wait 20 s, answer y/n | The last 5 s alternate red/blue every 20 ms: a steady magenta, or visible flicker? |
| 6 | `dev/qa/naut-hardware/push3-display.sh show-chart 12` | Wait about 25 s for the claim, then 12 s of chart; say what you saw in chat | `RED` `GREEN` `BLUE` `WHITE` left to right, `BOTTOM RIGHT` in the corner, a one-pixel outline, nothing wrapped or cut off |
| 7 | `$K scenario x-touch/strips` | Follow the prompts (touch and move faders, SELECT, MUTE) | The motor step: strip 1 moves to 3/4 by itself |
| 8 | `$K push-3-mode user` then `$K capture-layout 1` | Press each prompted control once (8 pads, 9 encoder touches and turns, arrows, Mute, Select, touch strip) | Nothing on the display; the prompts |
| 9 | `$K scenario push-3/pads target/qa/naut-hardware/push-3-layout-<time>.edn` then `$K push-3-mode live` | Follow the prompts: tap, hold, lean on a pad, Select+pad, encoder, Mute twice | Nothing; the receipt records each step |

Steps 1 to 7 fit in ten minutes. Steps 8 and 9 add about five. `push-3-mode live`
restores Live Mode and MPE aftertouch as found; run it even if step 9 is skipped.
The automatic checks (`$K probe all`, the unattended motor, LCD and rate
receipts and the chart receipt) already ran on 2026-11-26 and need nobody.
Check 8 (display-dependent Push 3 features) ran unattended on 2026-11-26 with
`dev/qa/naut-hardware/push3-display.sh show-features 15` (results below); what
it showed still needs the operator's eyes.

## Hardware results, 2026-11-26 (Live quit)

Live 12.4.5 was quit by the operator before these runs, so its Push3 helper
held nothing and Live sent nothing. Afterglow `de5a9f52`, libusb 1.0.30
(Homebrew), JDK 25, CoreMIDI4J 1.6. Receipts are the EDN files named here,
under `target/qa/naut-hardware/` on Naut (not committed).

### Probes (`probe all`, fresh JVM, exit 0)

| Probe | Port | Verdict | Reply (hex) | Latency |
| --- | --- | --- | --- | --- |
| `push-3/identity` | Push 3 User Port | answered | `F0 7E 01 06 02 00 21 1D 69 32 03 00 01 00 5B 00 xx xx xx xx 00 03 01 14 04 F7` | 0.7–1.7 ms |
| `push-3/identity-live-port` | Push 3 Live Port | answered | the same bytes | 0.7 ms |
| `x-touch/firmware` | X-Touch INT | answered | `F0 00 00 66 14 14 56 31 2E 32 32 F7` (`V1.22`) | 0.8 ms |
| `x-touch-ext/firmware` | X-Touch-Ext | answered | `F0 00 00 66 15 14 56 31 2E 32 32 F7` (`V1.22`) | 0.8 ms |
| `x-touch/device-query` | X-Touch INT | answered | `F0 00 00 66 14 01` + 11 ASCII bytes + `F7` | 0.8 ms |
| `x-touch-ext/device-query` | X-Touch-Ext | answered | `F0 00 00 66 15 01` + 11 ASCII bytes + `F7` | 0.9 ms |
| `x-touch/identity`, `x-touch-ext/identity` | | no-reply (1.5 s, no input at all) | | |

`xx` and the 11 ASCII bytes are unit serial numbers, recorded verbatim in the
local receipts and left out here. Push 3 decodes as Ableton (`00 21 1D`), family
`69 32`, member `03 00`, software 1.0 build 91, which matches Push3.log's
"Firmware 1.0 Build 91". After the four serial bytes Push 3 sends
`00 03 01 14 04`, one byte more than Push 2's single board-revision byte; `03`
matches the logged board revision 3 and `01 14` may be pmc 1.20, but neither is
documented. Push 3 answers `F0 7E 7F …` and `F0 7E 01 …` alike, on either port,
and nothing on the External Port. The X-Touch and XT in MCU mode ignore
Universal Identity (and the Xctl keepalive `F0 00 20 32 58 54 00 F7`) and answer
the MCU device query with a host connection query (`01`); the kit never sends
the `02` reply, and the units keep answering afterwards.

**Why the earlier probes failed.** The `:no-reply` receipts from 05:13 UTC were
taken while Live and its Push3 helper still ran (Live was quit later). Those
452 ignored Push 3 messages were not logged; with Live quit the User and Live
ports carry only Active Sensing (`FE`) at about 3.7 per second, so the 452 were
Live's traffic, plausibly the `38 0D`/`38 18` LED SysEx stream another capture
attributes to Live. The X-Touch `:no-reply` was correct: it does not implement
Universal Identity. The two firmware probes did not reach the device: the kit
listed ports through Afterglow's shared scan, which raises a retryable
`:midi/discovery-pending` (wrapping `TimeoutException`) when the first
CoreMIDI4J scan in a fresh JVM takes over 1.5 s. The kit now joins that scan for
up to 20 s, lists ports once per command, and `probe!` returns `:verdict :error`
with the exception instead of throwing. Every receipt now summarises the other
input (counts per status byte and a sample).

### X-Touch and XT, no human present

- **Motor (`motor`, receipts `x-touch-motor-…052429Z`, `x-touch-ext-motor-…052438Z`):**
  every fader commanded to 0.75 (`E0 7F 5F`, raw 12287, the check 4 target),
  0.25, 0.5, 1.0 and 0, 1.5 s apart. Verdict `:no-position-readback`: neither
  unit sent a single message while its motors moved (no pitch bend, no touch
  note). MCU faders report position only while touched, so the travel itself
  can only be confirmed by eye (operator script 1–2). Both units answered the
  firmware query afterwards.
- **LCD, colours, LEDs (`lcd`, 223 and 222 messages):** both rows of text, all
  eight backlight colours, per-strip colours, the REC/SOLO/MUTE/SELECT LED rows
  and a ring sweep, all with the unit's own device ID (14 main, 15 extender).
  Firmware answered afterwards. Appearance needs eyes (operator script 3–4).
- **Rate (`rate`, receipts `x-touch-rate-…052622Z`, `x-touch-ext-rate-…052642Z`):**
  top-row text at 10, 25 and 50 Hz for 5 s each and the 20 ms red/blue
  alternation, scheduled on fixed deadlines: achieved 9.98, 24.99, 49.94 and
  49.97 Hz (main) and 9.98, 24.95, 50.00 and 49.97 Hz (XT). Each send returned
  in at most 0.72 ms. The firmware query answered after every stage in
  0.59–1.17 ms, the same as at rest, so neither unit showed a backlog at
  50 messages per second. Flicker needs eyes (operator script 5).
  The first run slept a fixed period after each send and reached only 9.3, 21.2
  and 36.7 Hz: `Thread/sleep` overshot by about 7 ms per call on this loaded
  Mac. The reference script's 20 ms colour cycle would get the same overshoot.
- **Operator-observed (2026-11-26, during 06:24:20–06:25:01 CET):** relayed
  from chat, "The x-touch and extender did a little sequence of moving faders
  and blinking colors and animating knob led rings". That window is the
  unattended `motor-check!` (receipts `x-touch-motor-…052429Z`,
  `x-touch-ext-motor-…052438Z`) and `lcd-check!` (`x-touch-lcd-…052454Z`,
  `x-touch-ext-lcd-…052501Z`) runs on both units, the REPL equivalent of
  `motor x-touch --unattended` and `lcd x-touch --unattended` for each unit. So
  motor moves, backlight colour changes and ring LEDs are seen to respond on
  both units. Positions, text and per-colour accuracy were not judged; operator
  script 1–5 asks for that.
- **End state:** every fader (master included) at zero, both LCD rows blank,
  backlights white, rings and strip LEDs off (`rest-state`), sent again at
  05:36:11 UTC (`x-touch-rest-…053611Z`, `x-touch-ext-rest-…053611Z`); both units
  answered the firmware query afterwards.

### X-Touch target: the operator's custom remote script, not plain MCU

Product direction (operator, 2026-11-26): X-Touch functionality should follow
the operator's fork of the custom Live X-Touch remote script
(`Behringer-X-Touch-for-ableton`, upstream cited in [X-Touch](../xtouch.md)),
not the basic MCU feature set. These runs use MCU-mode wire messages only
because that is the transport both the script and the hardware speak; they
qualify the transport, not a feature set. What the run touched, mapped to the
script's behaviours:

| Script behaviour | Exercised here | How |
| --- | --- | --- |
| Coloured scribble strips (8 native colours, black only on purpose) | Wire only | `lcd`: all 8 indices on all strips, then one per strip (`F0 00 00 66 dd 72 …`) |
| Colour mix mode (rapid colour cycling, flicker warning, 2 s bursts) | Rate only | `rate`: 5 s red/blue every 20 ms, 50 Hz achieved; flicker not yet judged |
| Two-row 56-character display text | Wire only | `lcd`, `rate`: rows 0 and 1 (`… 12 …`) |
| Encoder rings (pan/send modes) | Single mode only | `lcd`: positions 0–11 in single mode; boost-cut/wrap/spread not run |
| Faders at 0 dB with calibrated zero point | No | Raw positions only; the script's dB mapping is not in the kit |
| Clipping level meters, metronome blink, settings menu, chains-as-groups, single-send mode | No | Not in the kit |

A separate lane is writing the feature-map issue. Nothing in this kit assumes
plain MCU is the product target.

### Push 3 tethered display

- **Operator-observed (2026-11-26, 06:16:17–06:17:14 CET):** during the
  orchestrator's `show-chart 30` with Afterglow's own CLI, the operator
  confirmed in chat that the Push 3 screen showed the test chart ("the screen
  write worked"). Unit: Push 3, firmware package 1.5.4 (1.0 build 91), Afterglow
  `de5a9f52`, libusb 1.0.30. That CLI exits 0 and prints nothing on success.
- **Receipt path (`display-chart`, now behind `push3-display.sh show-chart`):**
  10 s run `push-3-display-chart-…052808Z`: 194 frames, 194 header and 194
  payload transfers, 63,573,024 bytes, no libusb errors, one open and one close.
  Payload transfer (327,680 bytes) 11.27–12.85 ms, median 11.69; header
  0.18–1.30 ms. Refresh achieved 19.3 Hz. The 8 s run through the wrapper
  (`…053201Z`) gave 157 frames at 19.6 Hz, median payload 11.48 ms, exit 0.
- **Claiming takes 25 s.** Stage timing: chart 0.5 ms, open and claim 25,019 ms,
  first frame 16 ms later. Timing each libusb call put all of it in
  `libusb_init` (25,022 ms; open 1 ms, descriptor checks 1.3 ms, claim 0.6 ms).
  `LIBUSB_DEBUG=4` shows `darwin_check_configuration` stalling 5.00 s on each of
  five unrelated devices (two `2109:2822` hubs, `214b:7250` and `214b:7260` hubs,
  a `04e8:61fb` drive), not on the Push 3. Afterglow's `list`, `inspect` and
  every display open each create a fresh libusb context, so each pays the 25 s.
- **Refresh is about 19.5 Hz, not 30.** Afterglow's sink schedules with a fixed
  33 ms *delay*, after a header plus an 11.5 ms payload transfer, so the period is
  about 45 ms plus sleep overshoot. The frame was static, so this is not
  visible here; it bounds animated content.
- Push 3 MIDI answered the identity probe normally after each display session.

### Push 3 display-dependent states (check 8)

`push3-display.sh show-features 15 4310` (`tolglow.qa.naut-display-features`),
2026-11-26, Live quit. Studio's production build served `/push-display` on
127.0.0.1:4310; headless Chrome 155 (no `--use-angle`, so WebGL may be
software) captured it and the frames went to the Push 3 through one USB claim
(25.1 s, all `libusb_init`; one open, one close). Operator input came from a
virtual Push 3 at the MIDI boundary with the kit's byte encoding and synthetic
addresses (no real layout is captured yet), so no Push 3 MIDI port was opened
and nothing was sent to the controller. Receipt
`push-3-display-features-…060356Z.edn` plus one PNG per state (the exact image
sent) in `check-8-…060020Z/`, not committed. 3,736 header and 3,736 payload
transfers, 1,224,272,256 bytes, no libusb errors; payload transfer
10.82–15.69 ms, median 10.96.

| Local time (CET) | State | Issue | Presented view (from the receipt and PNG) |
| --- | --- | --- | --- |
| 07:00:45–07:01:02 | Parameter page | #219 | Ember: period 1 (dial), level 0.7, tint swatch with H/S/V bars; cue row Ember, Merlot, Orchid… |
| 07:01:02–07:01:18 | Full cue variations | #547 | "Full cue variations", Saved 1–8 over the top row, 56 cells. A pad pressed before the overlay reached the screen changed nothing (no ack); the same pad after it was presented was accepted and changed level 0.7→0.369, period 1→3/4 and tint |
| 07:01:18–07:01:26 | Cue parameter rows | #547 | "Parameter variations", rows period, level, tint (24 cells, 3 rows); Up again closed it |
| 07:01:26–07:01:42 | Beat-ratio chooser | #529 | Touch encoder 1: "Beat ratios · Ember · period · 3/4 · Preferred", two pad rows 1/4…4 with 3/4 selected. Pad 1/4 accepted: period 3/4→1/4 |
| 07:01:42–07:01:50 | Chooser closed | #529 | Encoder lifted: parameter page again, period 0.25 |
| 07:01:50–07:02:08 | Saved shows | #552 | Content-browser session (Push session replaced): "Saved shows", Naut rehearsal and Naut tour, "Latest named" |
| 07:02:08–07:02:16 | Show preview | #552 | "Saved show preview · Naut rehearsal · Latest named" |
| 07:02:16–07:02:24 | Back | #552 | The list again with Naut rehearsal still selected |
| 07:02:24–07:02:39 | Registered effects | #552 | Next library: Amber … Moon, page 1 of 2, Version 1 |
| 07:02:39–07:02:56 | Room preview | #530 | Display mode simulator: the rig scene, "LIVE · Fit rig · 8 / 8 fixtures" |
| 07:02:56–07:03:11 | Room, one touch | #530 | Encoder 2 touched (device session), scene unchanged: the Room projection carries no touched parameters at this revision |
| 07:03:11–07:03:26 | Room, two touches | #530 | Encoders 2 and 3 touched, scene unchanged, as above |
| 07:03:26–07:03:41 | Parameters, one touch | #530 | Level slot highlighted |
| 07:03:41–07:03:56 | Parameters, two touches | #530 | Level highlighted; tint fans out across slots 3–8 (swatch, full-width H/S/V, RGB readout) |

Each state is held 15 s (8 s for the follow-on states) from the moment its
view was presented (43–2,999 ms after its input). The Push 3 answered the
identity probe afterwards and interface 0 was free again.

Findings from this run:

- **Fixed here:** Push browser projection dropped the device view's metadata
  in parameter, audio and simulator modes, so presentation receipts never
  reached the session outside pad overlays. After the #547 overlay closed, the
  session kept its overlay generation and encoder touch could no longer open
  the #529 chooser. `show/push-browser-display` now keeps the metadata
  (regression in `variation-control-test`).
- **#530 is not implemented for the Room preview:** the simulator projection
  returns only the scene, so touching encoders changes nothing there. The
  touch layouts exist on the parameter page only.
- Not exercised: the #552 checkpoint restore, real pad, encoder and button
  addresses (check 2), and anything the operator has not yet confirmed seeing.

### Push 3 MIDI mode without Live

`push-3-mode user` sends `F0 00 21 1D 01 01 0A 01 F7` (echoed on both the Live
and User ports) and `… 1E 01 F7`; the aftertouch query `… 1F F7` then answers
`… 1F 01 F7` (Poly). Before, it answered `… 1F 02 F7`, Live's MPE setting.
`push-3-mode live` sends `… 1E 02 F7` (Push answers `B0 7B 00`, All Notes Off,
on the Live port) and `… 0A 00 F7`; the query reads `02` again. Both were run
and the Push 3 was left in Live Mode with MPE, as found. Pads were not pressed,
so User-Mode pad output is still unverified.

The inventory below was read on 2026-11-26 from the IORegistry, CoreMIDI and
Live's logs while Live 12.4.5 owned the devices. No MIDI port was opened for
output, no USB interface was claimed and Live was not contacted.

## Inventory, 2026-11-26

### USB (IORegistry, read only)

| Device | VID:PID | bcdDevice | Speed | Location / address | Configuration |
| --- | --- | --- | --- | --- | --- |
| Ableton Push 3 | `2982:1969` (10626:6505) | 1.00 | high (USB 2.0) | `0x01112400` / 12, libusb bus 1 address 12 | 1 of 1, class EF/02/01 (composite with IAD) |
| Behringer X-Touch | `1397:00B1` (5015:177) | 1.22 | full | `0x01114300` / 17 | 1 of 1 |
| Behringer X-Touch-Ext | `1397:00B6` (5015:182) | 1.22 | full | `0x01114130` / 14 | 1 of 1 |
| FTDI FT232R USB UART | not read further | | | `0x01111110` | Apple `AppleUSBFTDI` serial driver bound; **never opened** |

Push 3 interfaces and the processes holding them:

| # | Name | Class/sub/proto | Endpoints | Held by |
| --- | --- | --- | --- | --- |
| 0 | Ableton Push 3 Display | 255/255/255 | 2 | `Push3` (Live's Push 3 helper) user client |
| 1 | Ableton Push 3 Audio | 1/1 (control) | 1 | `usbaudiod`, Apple audio control nub |
| 2 | Ableton Push 3 Audio Out | 1/2 (streaming) | 0 in alternate 0 | `usbaudiod` |
| 3 | Ableton Push 3 Audio In | 1/2 (streaming) | 0 in alternate 0 | `usbaudiod` |
| 4 | Ableton Push 3 MIDI | 1/1 | 0 | |
| 5 | Ableton Push 3 MIDI | 1/3 (streaming) | 2 | `MIDIServer` |
| 6 | Ableton Push 3 xPort | 255/255/255 | 2 | nobody |

Interface 0 matches the name, class and endpoint count Afterglow requires
(`afterglow.control.devices.push-3.screen`). The IORegistry does not show endpoint addresses or packet sizes. Afterglow `inspect` records them.
The `Push3` process also holds a device-level user client. The X-Touch and
XT each expose an audio-control interface 0 and a MIDI-streaming interface 1
held by `MIDIServer`.

Live's Push 3 helper log (`Push3.log`) reports a tethered Push 3 hardware revision B, board revision 3,
firmware package 1.5.4 ("Firmware 1.0 Build 91"), pmc 1.20, apc 1.9, xySensor 0.13
and imagery 0.7. The log shows no compute module and no battery. The helper opened the display at startup, then logged
"Push is go".

### CoreMIDI (read without creating a client)

`swift dev/qa/naut-hardware/coremidi-inventory.swift push` and `... x-touch`
print this. Unique IDs are CoreMIDI's persistent IDs on this Mac.

| Device (unique ID) | Endpoint | Source ID | Destination ID |
| --- | --- | --- | --- |
| Ableton Push 3 (-9800989) | Live Port | 1266943001 | 1371440370 |
| | User Port | -2019063041 | 1999968494 |
| | External Port | -1504852924 | 792274261 |
| X-Touch (765594123) | INT | 908879555 | -908474505 |
| | EXT | 2084307619 | 2125235552 |
| X-Touch-Ext (-245385523) | X-TOUCH_INT | 1472828095 | 608317290 |

All six endpoints were online. Live lists only the Push 3 User and External
ports. The Live Port belongs to the Push 3 helper.

The project JVM sees each endpoint twice. CoreMIDI4J lists it as
`CoreMIDI4J - Ableton Push 3 User Port`, and the JDK lists it as `User Port` with
description `Ableton Push 3 User Port`. The JDK names are ambiguous: a connected
Push 2 also has a `User Port`, and the X-Touch has `INT`. Afterglow's
`:expected-input-name` compares only these names, so it cannot tell the units
apart. The kit resolves the CoreMIDI4J entries by full name: `CoreMIDI4J - Ableton Push 3 User Port`,
`CoreMIDI4J - X-Touch INT` and `CoreMIDI4J - X-Touch-Ext`. Run
`lein run -m tolglow.qa.naut-hardware ports` to see the current indices. They
change when devices are connected or removed.

### Live 12.4.5 (Log.txt and preference files, read only)

- Control surfaces: slot 4 `XTouchXT`, input and output `X-Touch-Ext (X-TOUCH_INT)`. Slot 5
  `XTouch`, input and output `X-Touch (INT)`. Slot 7 is `None` with ports `FromPush`/`ToPush`.
  Slots 1–3 and 6 (`Move`, `Push2`, `LK`, `BLOCKS`) have no ports. Push 3 is not in a
  script slot. Live's Push 3 helper owns the device directly.
- MIDI ports: Push 3 User and External ports have Track, Sync and Remote off. The
  `X-Touch-Ext (X-TOUCH_INT)` input has Track on.
- Link: each Live start in the current log prints `Link: Disabled`. The last one is at
  2026-11-22T16:57:43, and no later line changes it, so Link is off in the running set.
- Push preferences: `aftertouch_mode` is `mpe`. The Push 3 profile in this kit needs Poly
  Aftertouch.
- Since startup, `Push3.log` has repeated `MidiHub: Failed to push MIDI message to outgoing
  queue` at about 800 per minute: 6.2 million repetitions by 05:18 on 2026-11-26. The same message
  appears in the 2026-10-01 log. It is recorded here, not diagnosed.

### Link, Afterglow descriptors and automatic probes

- **Link: no readings.** Tolglow joins Link only through Carabiner
  ([Ableton Link](../ableton-link.md)). Carabiner is not installed (`which carabiner`, `~/bin`, Spotlight),
  and no other Link implementation exists in Tolglow or Afterglow. Even with a peer,
  Live's Link is disabled, so Live would not appear in the session. Getting a reading needs two changes:
  operator approval to download the Carabiner 1.2 release binary, and enabling Link
  (and Start Stop Sync) in Live's preferences, restored afterwards.
- **Afterglow `list`/`inspect`: run after libusb 1.0.30 was installed and Live quit**
  (results above). At inventory time no libusb was installed. Source review: `list` initializes libusb,
  enumerates cached descriptors and frees them, without opening a device. On macOS, current libusb
  builds the descriptor from IORegistry properties and opens a device only as a fallback.
  `inspect` calls `libusb_open`. On macOS that calls `USBDeviceOpenSeize`, which asks other
  clients of the device (here Live's Push 3 helper) to close. Run `inspect` only after Live
  has released the device. `push3-display.sh` enforces this.
- **Automatic MIDI probes:** refused by the permission system at inventory time; run on
  2026-11-26 after Live was quit (results above).

## Before you start

1. Write down the preferences you will change and restore each one at the end:
   Live control-surface slots 4 and 5 as listed above, and Push 3's Aftertouch Mode (`MPE`).
   Do not save the Live set.
2. Work in this branch's worktree with `export JVM_OPTS="-Xmx3g"`. All commands below run
   from the repository root. Receipts land in `target/qa/naut-hardware/`.
3. No lighting output is involved. The scenarios run an in-memory show with no sink.

### Handing a device to the kit and back

| Device | Take from Live | Give back to Live | Confirm |
| --- | --- | --- | --- |
| X-Touch and XT | Live → Settings → Link, Tempo & MIDI: set Control Surface slots 4 (`XTouchXT`) and 5 (`XTouch`) to **None** | Set slot 4 back to `XTouchXT` and slot 5 to `XTouch`, with their inputs and outputs as listed above | `Log.txt` prints a new `Midi Remote Scripts:` block with both scripts. The LCD shows Live's track names again |
| Push 3 MIDI | With Live running: switch Push 3 to **User Mode** ([Ableton manual, User Mode](https://www.ableton.com/en/push/manual/#user-mode)) and set **Aftertouch Mode** to **Poly** in Push's Setup. With Live quit: `lein run -m tolglow.qa.naut-hardware push-3-mode user` | Return to Live mode and Aftertouch Mode **MPE** (`push-3-mode live` with Live quit) | Pads launch Live clips again. `PushPreferences.json` shows `"aftertouch_mode": "mpe"`; `push-3-mode` receipts read the aftertouch mode back |
| Push 3 display | **Quit Live.** Live's Push 3 helper holds interface 0 even in User Mode | Start Live again and reopen the set without saving | `dev/qa/naut-hardware/push3-display.sh status` lists `Push3` as interface-0 holder again |

## Checks needing the operator

The times assume the operator is at the controllers with this page open.

| # | Check | Issues | Needs | Time |
| --- | --- | --- | --- | --- |
| 1 | Identity and firmware probes | #219, #310, #553 | Live quit (Push 3 did not answer with Live running) | 1 min |
| 2 | Push 3 layout capture | #219 | User Mode, Poly Aftertouch | 4 min |
| 3 | Push 3 pads, pressure, encoder, modifier | #219, #543 | Captured layout from 2 | 3 min |
| 4 | X-Touch strips, group fader, motor | #553, #310 | Slots 4 and 5 set to None | 3 min |
| 5 | X-Touch-Ext strips | #553, #310 | Slots 4 and 5 set to None | 2 min |
| 6 | LCD/color rate and flicker | #310 | Slots 4 and 5 set to None | 2 min per unit |
| 7 | Tethered display chart and descriptor receipt | #219, push-display-packaging | libusb; Live quit | 2 min |
| 8 | Display-dependent Push 3 features | #529, #547, #552, #530 | Check 7 passed; Live quit | 10 min |

Including the handoffs, the full pass takes about 40 minutes. Checks 1–6 work without libusb and without quitting Live.

### 1. Identity and firmware probes (automatic once started)

```sh
lein run -m tolglow.qa.naut-hardware probe all
lein run -m tolglow.qa.naut-hardware probe push-3/identity    # or any single probe
```

- Each probe sends one query and waits 1.5 s for the reply. Push 3: the Universal Identity
  Request `F0 7E 7F 06 01 F7` on the User Port (`push-3/identity`) and on the Live Port
  (`push-3/identity-live-port`). X-Touch and XT: the MCU version query `F0 00 00 66 dd 13 00 F7`
  and device query `F0 00 00 66 dd 00 F7` (`dd` = 14 main, 15 extender), plus Universal
  Identity, which these units ignore.
- Receipt: `:verdict` (`:answered`, `:no-reply` or `:error` with the exception), reply bytes and
  latency, decoded fields, and a summary of any other input (counts by status byte and a
  sample). Results on this Mac are under "Hardware results" above. With Live running, the
  Push 3 did not answer within 1.5 s (receipt from 05:13 UTC); quit Live first.

### 2. Push 3 layout capture (#219)

Hand the Push 3 MIDI to the kit (User Mode, Poly Aftertouch), then run:

```sh
lein run -m tolglow.qa.naut-hardware capture-layout      # all 8 pad rows
lein run -m tolglow.qa.naut-hardware capture-layout 2    # bottom two rows only
```

- The kit opens only the Push 3 User Port **input**. Its transport has no output.
- Follow the prompts: each pad from the bottom row up, then for each encoder touch and one clockwise click, the master encoder,
  the arrows, Mute and Select, and the touch strip. The first matching message is learned. Repeats of an address that is already captured are ignored.
  A control that sends nothing within 30 s is listed under `:skipped`.
- Receipt: `push-3-layout-*.edn` with `:layout` (the captured note and CC addresses that
  `afterglow.control.codecs.push-3/compile-layout` accepts), every sample's bytes and `:skipped`.
  This is the controller-mode address map #219 asks for. Attach it to the issue.

### 3. Push 3 pads, pressure, encoder and modifier (#219, #543)

```sh
lein run -m tolglow.qa.naut-hardware scenario push-3/pads target/qa/naut-hardware/push-3-layout-<time>.edn
```

Prompts in order. Each step passes when the committed show model shows the
expected result within 30 s.

1. Tap bottom-row pad 1 briefly. Expect cue `:tap` running (persistent tap).
2. Tap it again. Expect that exact instance to end.
3. Hold pad 2 for a second and lift. Expect `:hold` running while held and released after lift (temporary hold).
4. Tap pad 3, then press it again and lean on it. Expect `:expr` still running with its level following pressure above 0.6
   (holding a running cue targets expression, and release leaves it running).
5. Hold Select and touch pad 3. Expect `:expr` selected for editing with no launch or end.
6. Touch encoder 1 and turn it. Expect `:expr` level changed by the encoder.
7. Press Mute twice. Expect blackout on, then off.

Receipt: per-step verdict and latency, the session status, and every MIDI byte
received with a timestamp. The session sends no bytes to the Push 3. The test checks this.

### 4. X-Touch strips, group fader and motor (#553, #310)

Set slots 4 and 5 to None, then run:

```sh
lein run -m tolglow.qa.naut-hardware scenario x-touch/strips
```

The show has a manual group `All` with the child group `Flat`. Group strips come
first, followed by the cue strips `Tap` and `Expr`.

1. Look at the LCD and answer y/n: strips read `All`, `Flat`, `Tap`, `Expr`, with cyan, blue,
   red and green strips.
2. Press SELECT on strip 3. Expect `:tap` running.
3. Touch the strip 1 fader, pull it down and release. Expect the committed level of group `All` below 0.05.
4. Hands off. The kit commits group `All` = 0.75. Expect the fader feedback pitch bend to be sent,
   then answer y/n whether the motor moved the fader to about three quarters.
5. Touch the strip 3 fader and push it to the top. Expect `:tap` level above 0.95.
6. Optional: move the strip 3 fader without touching its cap. Expect no change (untouched input is ignored).
7. Press MUTE on strip 3. Expect `:tap` released.

### 5. X-Touch-Ext strips (#553, #310)

```sh
lein run -m tolglow.qa.naut-hardware scenario x-touch-ext/strips
```

1. Extender strip 1 shows `Spare` (channel offset 8). Answer y/n.
2. SELECT on extender strip 1 launches `:spare`.
3. Hands off. Answer y/n whether extender fader 2 stayed still (no channel is assigned).
4. MUTE on extender strip 1 releases `:spare`.

The receipts carry all LCD, color and fader bytes. Every SysEx message uses device ID 15 on the extender
and 14 on the main unit.

### 4a. Motor sweep and LCD/LED pattern (#553, #310)

```sh
lein run -m tolglow.qa.naut-hardware motor x-touch      # motor x-touch-ext for the extender
lein run -m tolglow.qa.naut-hardware lcd x-touch        # lcd x-touch-ext
lein run -m tolglow.qa.naut-hardware rest x-touch       # faders to zero, LCD blank, LEDs off
```

`motor` drives every fader to 0.75, 0.25, 0.5, 1.0 and 0 and records any message
the unit sends back; MCU units send none for motor moves (`:no-position-readback`),
so the y/n answer is the travel evidence. `lcd` writes both rows, steps the
backlight colours, lights the REC/SOLO/MUTE/SELECT rows and sweeps the rings.
Both end at `rest-state` and probe firmware afterwards. Add `--unattended` to
skip the y/n question.

### 6. LCD/color rate and flicker (#310)

```sh
lein run -m tolglow.qa.naut-hardware rate x-touch
lein run -m tolglow.qa.naut-hardware rate x-touch-ext
```

The kit sends top-row LCD text at 10, 25 and 50 Hz for 5 s each, on fixed deadlines.
It then runs a 5 s red/blue native-color alternation every 20 ms, the reference
script's default, and finishes at rest (blank, white). The firmware query runs
before, after each stage and at the end; each send is timed. Answer y/n whether the alternation looked
stable. Receipt: achieved message rates, both probe latencies, the answer and
`:responsive`/`:unresponsive`. This is the bounded, explicitly started experiment
that [X-Touch](../xtouch.md) requires. The normal adapter never cycles colors.

### 7. Tethered display chart and descriptor receipt (#219)

Needs libusb 1.0 (Homebrew 1.0.30 is installed on Naut).

```sh
dev/qa/naut-hardware/push3-display.sh status      # IORegistry only
dev/qa/naut-hardware/push3-display.sh list        # safe with Live running
# Quit Live. Then:
dev/qa/naut-hardware/push3-display.sh status      # interface-0 holder must be "none"
dev/qa/naut-hardware/push3-display.sh inspect > target/qa/naut-hardware/push-3-descriptor.edn
dev/qa/naut-hardware/push3-display.sh show-chart 12
```

- `inspect` and `show-chart` refuse while any process holds interface 0 (exit 4), and refuse
  without libusb (exit 3).
- `show-chart` runs `tolglow.qa.naut-hardware display-chart`, which prints and writes a receipt:
  stage timing, first-frame time, frame and transfer counts, bytes, per-transfer durations,
  refresh rate, libusb errors and the verified descriptor. Expect about 25 s before the
  first frame on Naut (libusb initialisation, see "Hardware results").
- During the 12 s chart, photograph the display. Check the red, green, blue and white labels left to right, the
  `BOTTOM RIGHT` mark, the one-pixel outer frame, and that the frame is not cut off or wrapped and the idle screen does not return.
- Unplug while streaming once, then run `status` again. The address may change.
- Receipt: the descriptor EDN, the photo and the observed errors. Then start Live again.

### 8. Display-dependent Push 3 features (#529, #547, #552, #530)

These run on the tethered display, so Live stays quit. Start the Tolglow REPL
(`lein repl`) and connect the Push 3 with the layout from check 2 and the
display address from check 7:

```clojure
(require '[tolglow.show :as show] '[clojure.edn :as edn]
         '[afterglow.control.devices.transports :as transports]
         '[tolglow.qa.naut-hardware :as kit])
(def runtime (show/create-runtime {:model (kit/qualification-model) :workspace-options nil}))
(def ticker (future (while true (show/tick! runtime) (Thread/sleep 10))))
(def layout (:layout (edn/read-string (slurp "target/qa/naut-hardware/push-3-layout-<time>.edn"))))
(def display (show/push-browser-display runtime
               {:device :push-3 :usb {:bus 1 :address 12}  ; from push3-display.sh status
                :browser-executable "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
                :url "http://127.0.0.1:4310/push-display"  ; Studio started on port 4310
                :presentation-receipts? true}))
(def push (show/connect-device! runtime :push-3
            {:session-id "naut-push-3-display" :user-mode? true :expression-mode :poly-aftertouch
             :cue-gestures? true :cue-variation-pads? true :display display :layout layout
             :midi (kit/input-transport :push-3)
             :encoder-bindings {:encoder-1 {:param :level} :encoder-2 {:param :count}}
             :button-bindings {:select :cue-gesture :up :variation/cycle :right :page-right :left :page-left}}))
;; ... exercise as below, then:
(show/disconnect-device! runtime "naut-push-3-display") (future-cancel ticker) (show/stop! runtime)
```

| Issue | What to do | What to record |
| --- | --- | --- |
| #547 | Launch Tap, press Up (variation cycle), choose a pad variation, then press a pad before the overlay appears | Photo and clip of the overlay, the receipt showing the early press refused, the level change |
| #529 | Touch encoder 1 on a running cue and wait for the chooser, select, lift | Clip of the chooser frame and the return to the cue grid |
| #552 | Disconnect `push`, then connect through `tolglow.controllers.content-browser-display/connect-device!` with `:catalog-controls` on captured unused buttons, including `:next-kind` ([Browse saved shows](../push-display-packaging.md#browse-saved-shows)). Browse effects and cues, then open a working-recovery checkpoint | Photo of the list, selection preserved on Back, the restore receipt |
| #530 | Press Layout for the simulator, touch one encoder, then two | Still and clip of the untouched, single-touch and two-touch layouts |

**Scripted run.** With `node_modules` and a Studio production build in the
worktree, and Live quit:

```sh
dev/qa/naut-hardware/push3-display.sh show-features 15 4310   # seconds per state, Studio port
```

It claims the display once (about 25 s), then holds each state in the order of
the table under "Push 3 display-dependent states" and writes one receipt with a
per-state entry (input, presented view, frames captured, transfers, PNG and its
SHA-256). Input is a virtual Push 3 with synthetic addresses, so the operator
only watches. `tolglow.qa.naut-display-features-test` runs the same sequence
with a replay backend and an injected capture. The REPL session above remains
the way to drive the same states from the real controls once check 2 has a
layout.

## Not covered

- **#532:** Push and X-Touch have no effect Solo/Mute adapter at this revision. The X-Touch SOLO
  button only selects a local strip focus. No hardware check can be written until an adapter exists.
- **#545:** the Select and Reorder grids are implemented for Push 2 only. Push 3 needs the
  adapter first.
- MPE expression (the operator's current Push setting) is not part of this kit. The kit uses
  Poly Aftertouch, which the User Mode profile supports.

## Findings from building the kit

- (2026-11-26, hardware) Afterglow's shared MIDI scan gives up after 1.5 s; a
  fresh JVM's first CoreMIDI4J scan can exceed that, which is what made the
  firmware probes throw. The kit joins the scan for up to 20 s.
- (2026-11-26, hardware) Every Afterglow libusb entry point creates its own
  context, and `libusb_init` takes 25 s on Naut because of unrelated USB devices.
  Reusing one context per process, or doing the descriptor read once, would
  remove it for every reconnect. This is an Afterglow change.
- (2026-11-26, hardware) Afterglow's display refresh uses a fixed delay, so it
  runs at about 19.5 Hz rather than the documented "about 30 times per second".
- (2026-11-26, hardware) MCU motor moves produce no MIDI from the units, so the
  motor step of check 4 can only assert that feedback was sent, not that the
  fader arrived.

- `open-push-3!` adds a refresh cadence only when a `:display` is given. Without one,
  a MIDI-only Push 3 session never reaches its hold deadline, so with `:cue-gestures? true` a
  long hold behaves like a tap. The kit passes `:schedule!` itself. The replay test
  fails without it.
- Pressure that arrives before the hold deadline does not change the held cue. Real pads send
  aftertouch continuously, so the replay sends pressure again after the deadline.
- `transports/midi-transport` checks only `MidiDevice$Info.getName`, which is ambiguous
  for the JDK's macOS entries (see CoreMIDI above).

## Hardware-free verification

```sh
lein test :only tolglow.qa.naut-hardware-test tolglow.qa.naut-display-features-test
```

This runs every probe, the full 89-control layout capture, the Push 3 pad scenario on the captured layout, both
X-Touch scenarios and the rate check against virtual peers. `dev/qa/naut-hardware/push3-display.sh status`
and its refusal paths were run against the live IORegistry.
