# Queued Studio checkpoint restore

Tolglow `7241be015e97d77f633f5e0aebab1cb3f3534803` and Afterglow `16efb8b525e01d02a27897a389960da755247d13` ran with a capture-only show owner and Chromium 156. Both production targets built with zero warnings. The Studio bundle served to Chromium had SHA-256 `e2453f1e2c9c16906027ea152b66dada643bb7e7f30c7ded8f0735a5a6f50043`. [Verification receipt](receipt.json) · [Release log](release.log)

Two Studio clients requested the same named checkpoint while the active show was held at 30% master. They received pending HTTP replies after 5.617 s and 5.343 s. Advancing one owner frame restored the first request to 50% master and rejected the second as stale. Each client displayed its own result and could use the checkpoint panel again. Each client made one catalog request before the commit and one refresh after it. [Queued still](checkpoint-queued.png) · [Accepted still](checkpoint-accepted.png) · [Rejected still](checkpoint-rejected.png)

The eight-second H.264 clip replays three frames from that live Chromium run with timing and outcome labels. [Checkpoint restore clip](checkpoint-queued-restore.mp4)

The focused JVM checks passed 13 tests and 148 assertions, including receipt replay on a new WebSocket. Studio events passed 78 tests and 437 assertions. The spacing check passed across 532 files. [Host checks](host-checks.log) · [Studio events](studio-events.log) · [Definition spacing](definition-spacing.log)
