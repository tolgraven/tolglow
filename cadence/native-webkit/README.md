# Native Studio cadence on the M4 MacBook Air

This is an actual AppKit window and the bundled Ring host, using the release Studio UI and Room worker from the hosted build of `38d19ea2`. Both packaged preview bundles matched the audited draft byte for byte. The artifact identifies its hosted merge revision and paired Afterglow. The scratch shell combines the one-time startup focus fix from #520 with the preview's existing WebKit factory. It is ad hoc signed under `se.tolgraven.tolglow.studio`. The older package's four dangling Native Image symlinks were removed in accordance with #515. The packaged application JAR and Java executable were unchanged.

The only diagnostic code in the window records accepted DOM bitmap presentations and display RAF timestamps. It does not intercept shaders, modify worker code, request extra frames, set WebKit's first responder, or enable elapsed GPU timer queries. A scoped native helper activates the test app once. The expanded run uses the normal Room expander and scrolls its canvas into view. A 40 Hz loopback demo drives eight fixture contributions, with no physical or network DMX output. Fog is 0.25 for the fixed case or 0.7 for moving cases. Work light is zero and the fixtures use interpolated updates. Each case warms for 15 seconds and measures for 20. The plume case requests a fresh preview plume every four seconds.

The WebKit worker reports `Apple GPU`, while host hardware inventory identifies an Apple M4 MacBook Air. The first compact live receipt used an incorrect Chrome-specific GPU string gate. The raw receipt is preserved and an explicit independent verifier rechecks the correct renderer identifier, software-renderer flag, accepted worker frames, display rate, camera and OS focus/lock gates. No application result is inferred from the older shader compilation instrumentation.

The Linux- or cloud-hosted Chrome figures cannot substitute for these macOS results. The local Chrome and Studio measurements also have different canvas sizes and Auto renderer budgets. Their rates are independent measurements of physical presentation. Neither this demo nor its graphics timings verify the DMX publisher's deadline.

The compact Room used an 816 × 500 backing raster. Its worker reported eight lit fixtures, and the camera, controls and window focus held for all three samples. All 110 OS checks verified an unlocked Mac. The app shut down its loopback listener after recording.

| Compact scene | Room presentations/s | Display RAF/s | p95 presentation interval (ms) |
| --- | ---: | ---: | ---: |
| Fixed lights and haze | 59.95 | 60.00 | 23.34 |
| Moving heads and haze | 52.20 | 60.00 | 23.86 |
| Moving heads and repeated plumes | 58.80 | 60.00 | 23.82 |

The expanded Room reached 2180 × 972. Its first sample lost document focus, paused production presentation and reported the backgrounded state. The subsequent sample never regained visible Room status before its timeout. The failure receipt is included to distinguish this interruption from a performance measurement. Expanded and 120 Hz qualification remain open.

The `recorder` directory holds the revised local instrumentation. Its expanded option records per-frame worker timing. The compact receipt predates that addition and has per-second worker samples. To reproduce, use the branch with the #520 startup focus view and the #489 WebKit factory combined, substituting the included diagnostic factory and bundling `RoomCadence.js` in a scratch app. Copy the matching Room cadence and desktop-session helpers from `38d19ea2`. The `window-id.swift` helper targets the newly launched process. Pass the scratch app, source checkout, receipt directory and compiled window helper to `run.mjs` through `tolgraven-cpu runtime`. Add `--expanded` to exercise the larger Room. For a saved result, decompress `room.json.gz` alongside its matching metadata, then run `node summarize.mjs path/to/receipt compact` or supply `expanded` instead.
