# 2023 gig rig

The rig from the operator's last real gig (August 2023, re-edited July 2024)
is a loadable venue in `src/tolglow/venues/gig_2023.clj`. Its data existed only
as uncommitted edits to the legacy `tolglow/config.clj` and is quoted in
[issue #720](https://github.com/tolgraven/tolglow/issues/720). The namespace
uses the same portable fixture types and patch groups as the
[showcase rig](showcase-show.md) and [patch groups](fixture-patch-groups.md).

![Room preview of the 2023 gig rig](https://raw.githubusercontent.com/tolgraven/tolglow/media/gig-rig-720/gig-rig-720/room-audience.png)

The image is a headless Chrome render (Metal GPU) of the released Room bundle
with one static colour look on every fixture.

## Load it

```
lein run --config tolglow.venues.gig-2023/config
```

The config is the shipped `resources/tolglow.edn` with this rig, its room and
truss, and the gig's faster output response (150 ms linear attack, 200 ms
release). It starts a 40 Hz preview show with Studio on 127.0.0.1:8500 and no
physical output. From a REPL:

```clojure
(require '[tolglow.venues.gig-2023 :as gig] '[tolglow.show :as show])
(def runtime (show/start! {:model (gig/model)}))
(show/stop! runtime)
```

To add only the types and patch to a running configuration, pass
`gig/fixtures-config` as the `:fixtures` module config. That path keeps the
existing room.

## Fixture types

| Type ID | Mode | Footprint | Channel source |
| --- | --- | --- | --- |
| `:cameo-thunder-wash-600-rgbw` | 7-channel 2: dimmer, strobe, flash duration, R, G, B, W | 7 | Cameo CLTW600RGBW manual REV 04, *DMX control*, 7-CH Mode_2; OFL `cameo/thunder-wash-600-rgbw` mode `7ch2` |
| `:cameo-thunder-wash-600-rgb` | 6-channel 2: dimmer, strobe, flash duration, R, G, B | 6 | Cameo Thunder Wash manual (CLTW100W/100RGB/600W/600RGB), 6-CH 2 Mode; OFL `cameo/thunder-wash-600-rgb` mode `6ch2` |
| `:rgbw-60-moving-beam-15ch` | 15-channel | 15 | Legacy `:rgbw-60-mh` only. **Not verified** |
| `:rgbw-60-moving-beam` | 13-channel | 13 | `tolglow.fixtures/rgbw-60-moving-beam` (shared with the showcase) |
| `:rgbw-12-12-moving` | 16-channel | 16 | `tolglow.config/fixture-data` HY12x12 map |
| `:rgbw-7-12-moving` | 14-channel | 14 | `tolglow.config/fixture-data` (shared with the showcase) |

Both Thunder Wash manuals give the same strobe channel: 0-5 open, 6-10
closed, 11-250 strobe slow to fast and 251-255 open. The RGB manual rates it
0-30 Hz and the RGBW manual 1-30 Hz. Flash duration is 0 (or 1) to 510 ms.
Unless an effect sets it, the flash duration channel stays at 0, the shortest
flash, whenever the fixture's own strobe runs. The gig had the same default;
how visible that is on the hardware has not been checked.
Beam angles and power are the manuals' figures: 75° and 110 W for the RGB
model, 104° and 160 W for the RGBW model. OFL lists 75° and 130 W for the RGB
model and 75° for the RGBW model.

No manufacturer table, OFL definition or QLC+ definition was found for the
60 W beam head's 15-channel mode. The type restates the legacy map: pan and
tilt with fine bytes on 1-4, dimmer 6, shutter 7 (closed, open, then strobe
from 16 at 0.2-10 Hz) and RGBW 8-11. The legacy map declared channels 5 and
12-15 as unused dummies, so the footprint reserves them and nothing writes
them. The 13-channel mode puts movement speed on 5, auto on 12 and control on
13. The gig patched the 13-channel builder, so the venue does too; the
15-channel type is published but not patched.

Lens radii, and the HY12x12's beam angle and wattage, are estimates for the
preview. They do not change DMX output.

## Patch

Universe 1 throughout. Group names and list order are the legacy ones, so the
fixture IDs are those of the legacy show.

| IDs | Type | Addresses | Position (m) |
| --- | --- | --- | --- |
| `:moving-1` | HY12x12, rotated Y 180° | 1-16 | 1.5, 1.2, -2.5 |
| `:moving-2` | 60 W beam, 13-channel | 17-29 | 0.2, 0.2, -2.0 |
| `:moving-mini-1` to `-3` | 7x12 W moving head | 100, 114, 128 | x -3, 0, 3 at 3.0, -2.0 |
| `:moving-mini-4` | 7x12 W, rotated Y 180° | 142 | 0.2, 0.0, -4.0 |
| `:wash-1` to `:wash-8` | Thunder Wash 600 RGBW | 301, 308 ... 350 | x -4.2 to 4.2 every 1.2 at 4.0, -2.0 |
| `:wash-2-1`, `:wash-2-2` | Thunder Wash 600 RGB | 357, 363 | 0, 4.0, -2.0 and -0.5, 4.0, -1.5 |

The room is 10 m wide, 4 m high and 8 m deep, with the legacy main truss at
2.5 m.

## Corrections to the legacy data

| Legacy | Here | Why |
| --- | --- | --- |
| `:moving` group `:offset 0` | `:offset 1` | Addresses run 1-512. The legacy patcher passed 0 to Afterglow's `patch-fixture!`, whose precondition rejected the HY12x12; `catchall` swallowed the error. It then advanced by 16 and patched the beam at 16-28. With offset 1 the HY12x12 is at 1-16 and the beam at 17-29. Which address the beam head was really set to is unknown. |
| Walls at x ±2.5 | Room 10 m wide | The mini heads at x ±3 were already outside those walls. Only the truss and ceiling heights were edited for the gig, and the same edit set the pointing ranges to ±5 m. |
| Washes at x -6, -4, -2, 2, 4, 6, 8, 10 | Evenly spread, -4.2 to 4.2 | The legacy row spans 16 m. Patch order, left to right, is kept. |
| Z measured from the main truss (stage wall -2, rear wall 6) | Z moved 2 m upstage | The Room preview centres the room on the origin. |
| `:fixtures :types` names `:moving-mini-1` to `-8` | Four mini heads | Only four were patched; the other names referred to nothing. |
| RGB strobe a plain channel; RGBW strobe from 11 (`:end 250` was ignored by the builder) | The manuals' strobe map | The legacy builders had no open/closed bands. Channel offsets are unchanged. |

The legacy map did not rotate the washes, so they hang pointing straight down.
The legacy patch also did not say how the 3 m mini heads were hung.

## Checks

`tolglow.venues.gig-2023-parity-test` (parity lane) expands the legacy maps
quoted in the issue with the original `tolglow.fixtures/create`, compares
footprints, calibration and channel offsets with the portable types, and runs
the legacy group patcher `tolglow.util/patch-cfg!` on the legacy patch table
in a headless Afterglow show (nothing is opened or sent). Every attribute of
every fixture outside the `:moving` group lands on the same universe and DMX
channel as in the legacy show. The test pins the `:moving` correction: the
legacy patcher rejected the HY12x12 at 0 and placed the beam at 16, and the
venue places the beam one channel later. Run with the corrected `:offset 1`,
the legacy patcher produces the venue's whole DMX map.

`tolglow.venues.gig-2023-test` (pure lane) pins the address table, keeps every
fixture inside the room, checks the Thunder Wash strobe bands through
`tolglow.fixtures.shutter`, and starts the documented config and the
`:fixtures` module config.

## Not carried over

The gig also raised the default colour saturation to 0.83 (maximum 0.92),
desaturated sparkle colours by 0.15 instead of 0.20, and ran Pro DJ Link as the
clock source with a Push 2. Those belong to the legacy runtime's colour and
controller settings and are not part of this venue.
