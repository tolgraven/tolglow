# Actual Afterglow publisher commit

This compares the committed implementation at Afterglow `28616f127bf47afae1c98010ce5d154bbb5b3710` with its parent's original publication wake function, in the same process on the M4 Air. Tolglow is `d5664f52`. Each arm runs six seconds of warmup and 600 nominal slots. This run includes the committed conservative recovery for failed and expired sends.

| Arm | Sent / nominal | Current / held | Shortest / longest sink gap (ms) |
| --- | --- | --- | --- |
| Source commit 1 | 600 / 600 | 148 / 452 | 15.28 / 41.53 |
| Baseline 1 | 597 / 600 | 150 / 447 | 15.12 / 71.95 |
| Baseline 2 | 600 / 600 | 173 / 427 | 16.25 / 34.37 |
| Source commit 2 | 600 / 600 | 166 / 434 | 14.61 / 36.09 |

The committed publisher delivered all 1,200 slots without incomplete packets. The lowest measured gap exceeded the 12.5 ms minimum. It delivered 314 current sources and 886 held sources. All producer computations exceeded 25 ms. The simulated clock again reproduced the delayed recovery at 57.5 ms for slot 2. This capture used an in-memory ArtDMX sink.
