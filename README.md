# Physical darkness for inverted DMX channels

Afterglow `0c3c6852e11e8403298eb72fec3b75be2c470aa4` adds shared physical off bytes to the compiled patch, based on Afterglow master `e0df95534806540e2bcd1d7881d23041313befd7`. The publisher uses them while waiting and during blackout. Tolglow’s shutdown and repatch retirement use the same bytes. The candidate merges cleanly with the local #446 interpolation candidate `0986a868`. [Issue #524](https://github.com/tolgraven/tolglow/issues/524)

The new packet tests cover 8-bit dimmers and emitters, both full and partial inversion, 16-bit coarse/fine channels spanning universes, physical function-range packing, blackout and reopening, and retirement across both active and withdrawn universes. Retired slots yield to newly mapped channels and reserved footprints. Fog keeps its declared physical off endpoint, and idle mechanical bytes stay at zero. The explicit renderer blackout still retains authored mechanical values.

| Verification | Result |
| --- | --- |
| New regressions on the base revision | 2 tests, 18 passes, 7 expected failures, 0 errors |
| Afterglow focused | 120 tests, 4,364 assertions, 0 failures or errors |
| Afterglow parity | 687 tests, 10,670 assertions, 0 failures or errors |
| Tolglow scheduler | 11 tests, 65 assertions, 0 failures or errors |

[Baseline](afterglow-before.log) · [Focused](afterglow-focused.log) · [Parity](afterglow-parity.log) · [Tolglow scheduler](tolglow-scheduler.log)

## Captured Tolglow output

Tolglow `3fc425ff5c71dd24035d6df48704c0f4cd943bf6` issued real cue, master and rig commands with an injected publisher clock and in-memory sink. The normal sequence used 25 ms buffered latency. Coarse and fine are universe 1 slot 512 and universe 2 slot 1. Fog is universe 1 slot 2.

| Time (ms) | Packet | Coarse | Fine | Fog |
| ---: | --- | ---: | ---: | ---: |
| 0 | Inverted patch, waiting | 255 | 255 | 196 |
| 25 | Inverted lamp, live | 0 | 0 | 196 |
| 50 | Blackout | 255 | 255 | 196 |
| 75 | Reopened, waiting | 255 | 255 | 196 |
| 100 | Reopened, live | 0 | 0 | 196 |
| 125 | Lamp withdrawn | 255 | 255 | 196 |
| 150 | Normal dimmer reuses both addresses, waiting | 0 | 0 | 196 |
| 175 | Normal replacement lamp, live | 255 | 255 | 196 |
| 200 | Inverted lamp restored, waiting | 255 | 255 | 196 |
| 225 | Inverted lamp restored, live | 0 | 0 | 196 |
| 250 | Show shutdown | 255 | 255 | 196 |

The capture records all 512 bytes per universe. Actual fixture response and live DMX transport timing remain unmeasured. [Capture source](capture.clj) · [Packet data](captured.json)

## Separate expired-slot behavior

After the normal sequence, a second run patched a fully inverted dimmer into the addresses formerly used by a normal dimmer and added a fog channel. Packing the first publication under that patch exceeded its slot by 1 ms. Tolglow’s optional safety callback sent a copy of the prior wire packet at 76 ms with current fog off byte 196 and stale dimmer bytes `[0 0]`. Those dimmer bytes request full drive under the new mapping. This callback receives prior wire state by design and applies current fog cutoffs. Its policy for newly mapped light channels needs separate coverage. Legacy uncompiled `blackout-universe` also has its own buffer policy. [Capture log](tolglow-capture.log) · [Tolglow safety callback](https://github.com/tolgraven/tolglow/blob/3fc425ff5c71dd24035d6df48704c0f4cd943bf6/src/tolglow/show.clj#L833-L850)

The PR candidate `1b1c09369f1d793e279c3780dce89f8134955921` includes the independently merged buffered publisher. The final three-namespace run passed 88 tests and 1,175 assertions. The still shows the exact ordinary capture. Its 7.3-second playback reveals those packets at 24× slowdown with a final hold. [Still](idle-output.png) · [Playback](idle-output.mp4) · [Final focused tests](afterglow-merged-base-focused.log)
