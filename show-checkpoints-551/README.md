# Studio checkpoint verification

Tolglow `b05402d3691aefbafe05e39b2211650b514f2ce5` was checked in Chromium 156 against a running capture-only JVM with Afterglow `99f61c5bed2850daaeb87bc5b84ddf0d3c60bf64`. Studio served byte-for-byte matches for the six freshly built JavaScript files and the tracked stylesheet. The optimized Studio bundle's SHA-256 was `cfbe3b0754ed337c59cfbb9074b76ea0419fbcda66a8c54363404efbe4f3c910`. [Verification receipt](receipt.json)

| Chromium viewport | Observed result |
| --- | --- |
| Desktop 1440 × 900 | Restored the named checkpoint and displayed 50% master |
| Phone 390 × 844 | Restored working and previous-session checkpoints at 30% master, then discarded the previous-session checkpoint |
| Tablet 820 × 1180 | Displayed the two remaining checkpoints and disabled previous-session restore |

When a peer restored the working checkpoint first, Studio displayed 30% master and reported that the show changed. The stale local request returned HTTP 400. A current JSON request restored the named checkpoint. Stale JSON and Transit with the wrong show-ID type returned HTTP 400. Canceling a confirmation sent no write, Escape returned focus to Restore show, and Chromium reported no page exceptions. [Browser capture log](browser-capture.log) · [Peer race log](browser-race.log)

The focused Studio events suite passed 74 tests and 401 assertions. The tests cover new rig edits with the HTTP receipt arriving before or after the streamed show, and draft preservation when a peer restore wins. The definition-spacing check passed across 530 source files. [Studio events log](studio-events.log) · [Definition-spacing log](definition-spacing.log)

The 39.12-second clip is a 390 × 844 Chromium compositor recording encoded as H.264. [Phone clip](checkpoint-phone.mp4) · [Desktop still](checkpoint-desktop.png) · [Phone still](checkpoint-phone.png) · [Tablet still](checkpoint-tablet.png)
