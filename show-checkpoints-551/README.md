# Integrated Studio checkpoint verification

Tolglow `ebeef160d3126c8f27d13baa57e1e5c80980e15d` was built from a source export against Afterglow `99f61c5bed2850daaeb87bc5b84ddf0d3c60bf64`. The production build included the preview bundles, compute dispatcher and both Shadow CLJS release targets. Studio and the compute worker compiled with zero warnings. The server's responses matched the freshly built six JavaScript files and the tracked stylesheet byte for byte. The Studio bundle's SHA-256 was `fbdbc659e341cd41f9f0a601d1d00fe85ccd6e386791c943166aa4b8379026d7`. [Digests and test receipt](receipt.json)

| Chromium viewport | Observed result |
| --- | --- |
| Desktop 1440 × 900 | Restored the named checkpoint and displayed 50% master |
| Phone 390 × 844 | Restored working and prior checkpoints, each at 30% master, then discarded the prior checkpoint |
| Tablet 820 × 1180 | Displayed the two remaining checkpoint timestamps and disabled prior restore |

The browser ran against the actual preview-only JVM, with keyboard focus restored after Escape. Canceling a confirmation sent no write. Real checkpoint POSTs used the typed Transit show ID and contained no path. The direct JSON route accepted the current identity and restored the named checkpoint. It rejected stale JSON and the wrong Transit show-ID type with HTTP 400. Chromium reported no page exceptions. The focused Studio events suite passed 71 tests and 387 assertions. Its development compile reported the existing cue-pad type-inference warning.

The clip is a 23.56-second phone-width Chromium compositor capture encoded as H.264. [Phone clip](checkpoint-phone.mp4) · [Desktop still](checkpoint-desktop.png) · [Phone still](checkpoint-phone.png) · [Tablet still](checkpoint-tablet.png)
