# Recover the next nominal DMX slot after a late successful send

A prior profile recorded 14 absent publication rows across four weighted M4 samples. Replaying the clock calculation from the saved timestamps attributed 13 skips to the half-period handoff guard and one to an overrun. The candidate lets the same spacing guard open inside the next nominal slot. Its fake-clock example sends on slots 1 and 2 at 44 and 57.5 ms, where the old policy skips to slot 3 at 75 ms.

The timed comparison uses Tolglow `d5664f52`, Afterglow `7a54b5d5`, physical M4 Air and the same bounded host JVM. Four arms measured 1,000 nominal slots after six seconds of warmup. The receiver is the in-memory ArtDMX sink.

| Arm | Sent / nominal | Current / held | Shortest / longest sink gap (ms) |
| --- | --- | --- | --- |
| Baseline 1 | 998 / 1000 | 285 / 713 | 15.10 / 46.80 |
| Candidate 1 | 1000 / 1000 | 270 / 730 | 18.25 / 31.77 |
| Candidate 2 | 1000 / 1000 | 277 / 723 | 15.28 / 37.78 |
| Baseline 2 | 997 / 1000 | 278 / 719 | 15.34 / 47.84 |

All delivered packets were complete. The tested candidate preserved the 12.5 ms minimum spacing. The producer still exceeded 25 ms for every render, and more than two-thirds of the candidate's slots held a previous source.

The Afterglow source change retains the existing conservative grid recovery after failed or expired sends. Its simulated-clock tests verify the next-slot recovery, actual handoff spacing, expired frame handling and partial transport failure. The output/Art-Net loopback tests passed 33 tests and 222 assertions. The pure suite passed 645 tests and 9,982 assertions. The timing table is the earlier scratch run. This records no hardware DMX test.
