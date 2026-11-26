# Push 3 and X-Touch qualification on Naut

This kit closes the hardware gates of #219, #543, #553 and #310 and records
the tethered-display receipt for [Push display packaging](../push-display-packaging.md).
It also gives the display route that #529, #547, #552 and #530 need. Each check
is a command or REPL session that writes an EDN receipt under
`target/qa/naut-hardware/`. Every scripted check also runs in
`tolglow.qa.naut-hardware-test` against a virtual Push 3 or X-Touch at the same
MIDI transport boundary. The physical device is the only untested part.

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
- **Afterglow `list`/`inspect`: not run.** No libusb 1.0 dylib is installed (Homebrew
  prefixes and Spotlight checked). Afterglow's driver loads it through JNA.
  `brew install libusb` needs operator approval. Source review: `list` initializes libusb,
  enumerates cached descriptors and frees them, without opening a device. On macOS, current libusb
  builds the descriptor from IORegistry properties and opens a device only as a fallback.
  `inspect` calls `libusb_open`. On macOS that calls `USBDeviceOpenSeize`, which asks other
  clients of the device (here Live's Push 3 helper) to close. Run `inspect` only after Live
  has released the device. `push3-display.sh` enforces this.
- **Automatic MIDI probes: prepared, not run on hardware.** The identity and MCU firmware
  queries are below. The session's permission system refused the first run, so they wait for
  the operator.

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
| Push 3 MIDI | Switch Push 3 to **User Mode** ([Ableton manual, User Mode](https://www.ableton.com/en/push/manual/#user-mode)) and set **Aftertouch Mode** to **Poly** in Push's Setup | Return to Live mode and set Aftertouch Mode back to **MPE** | Pads launch Live clips again. `PushPreferences.json` shows `"aftertouch_mode": "mpe"` |
| Push 3 display | **Quit Live.** Live's Push 3 helper holds interface 0 even in User Mode | Start Live again and reopen the set without saving | `dev/qa/naut-hardware/push3-display.sh status` lists `Push3` as interface-0 holder again |

## Checks needing the operator

The times assume the operator is at the controllers with this page open.

| # | Check | Issues | Needs | Time |
| --- | --- | --- | --- | --- |
| 1 | Identity and firmware probes | #219, #310, #553 | Approve MIDI output; Live may stay connected | 1 min |
| 2 | Push 3 layout capture | #219 | User Mode, Poly Aftertouch | 4 min |
| 3 | Push 3 pads, pressure, encoder, modifier | #219, #543 | Captured layout from 2 | 3 min |
| 4 | X-Touch strips, group fader, motor | #553, #310 | Slots 4 and 5 set to None | 3 min |
| 5 | X-Touch-Ext strips | #553, #310 | Slots 4 and 5 set to None | 2 min |
| 6 | LCD/color rate and flicker | #310 | Slots 4 and 5 set to None | 2 min per unit |
| 7 | Tethered display chart and descriptor receipt | #219, push-display-packaging | Approved libusb install; Live quit | 8 min |
| 8 | Display-dependent Push 3 features | #529, #547, #552, #530 | Check 7 passed; Live quit | 10 min |

Including the handoffs, the full pass takes about 40 minutes. Checks 1–6 work without libusb and without quitting Live.

### 1. Identity and firmware probes (automatic once started)

```sh
lein run -m tolglow.qa.naut-hardware probe push-3/identity
lein run -m tolglow.qa.naut-hardware probe x-touch/firmware
lein run -m tolglow.qa.naut-hardware probe x-touch-ext/firmware
lein run -m tolglow.qa.naut-hardware probe x-touch/identity
```

- Each probe sends one query and waits 1.5 s for the reply. Push 3: the Universal Identity
  Request `F0 7E 7F 06 01 F7` on the User Port, which Live ignores (Track, Sync and Remote
  are off). X-Touch and XT: the MCU version query `F0 00 00 66 dd 13 00 F7` (`dd` = 14 main,
  15 extender). The operator's Live script sends the same query and parses the same reply,
  so running it with Live connected changes nothing in Live.
- Receipt: `:verdict :answered`, reply bytes, decoded manufacturer, family, model and version
  (identity) or firmware ASCII (MCU). `:no-reply` is also a result. The X-Touch might not
  implement Universal Identity.

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

### 6. LCD/color rate and flicker (#310)

```sh
lein run -m tolglow.qa.naut-hardware rate x-touch
lein run -m tolglow.qa.naut-hardware rate x-touch-ext
```

The kit sends top-row LCD text at 10, 25 and 50 Hz for 5 s each. It then runs a 5 s
red/blue native-color alternation every 20 ms, the reference script's default, and finishes on white.
The firmware query runs before and after. Answer y/n whether the alternation looked
stable. Receipt: achieved message rates, both probe latencies, the answer and
`:responsive`/`:unresponsive`. This is the bounded, explicitly started experiment
that [X-Touch](../xtouch.md) requires. The normal adapter never cycles colors.

### 7. Tethered display chart and descriptor receipt (#219)

Needs `brew install libusb`, which needs operator approval.

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

Check 8 is a REPL session, not a scripted replay. The logic it exercises has its own
virtual replays: `tolglow.authoring.variation-control-test` for #529 and #547, and the content-browser
display tests for #552. The session itself has not run here. Studio must be running on a port
from this lane's block for the browser capture.

## Not covered

- **#532:** Push and X-Touch have no effect Solo/Mute adapter at this revision. The X-Touch SOLO
  button only selects a local strip focus. No hardware check can be written until an adapter exists.
- **#545:** the Select and Reorder grids are implemented for Push 2 only. Push 3 needs the
  adapter first.
- MPE expression (the operator's current Push setting) is not part of this kit. The kit uses
  Poly Aftertouch, which the User Mode profile supports.

## Findings from building the kit

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
lein test :only tolglow.qa.naut-hardware-test
```

This runs every probe, the full 89-control layout capture, the Push 3 pad scenario on the captured layout, both
X-Touch scenarios and the rate check against virtual peers. `dev/qa/naut-hardware/push3-display.sh status`
and its refusal paths were run against the live IORegistry.
