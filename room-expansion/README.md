# Studio Room expansion

A real release UI booted from the embedded loopback JVM package. The probe drives the semantic Expand and Compact buttons in an owned tab of the shared Chrome testing profile. The profile zoom makes the reported CSS viewport larger than the requested emulation dimensions. The table uses `innerWidth` and `innerHeight` measured in the page.

| CSS viewport | Expanded canvas visible in stock build | Expanded canvas visible after repair |
| --- | ---: | ---: |
| 1138 × 733 | 72% | 100% |
| 1311 × 811 | 0% | 100% |
| 1600 × 1000 | 0% | 100% |

The candidate also returned the compact canvas to view at all three sizes. The rebuilt UI matched the digest fetched by the test runner. Each case used the same bundled backend and an isolated show. The probe records layout geometry and closes its own tab and server.

The packed baseline is from the `38d19ea2` release, and the candidate is the rebuilt release of `a1fea5a5` on current master. This verifies Expand and Compact layout behavior. Worker presentation timing is measured separately on the physical display.

[Issue #525](https://github.com/tolgraven/tolglow/issues/525) · [Packaged baseline](https://github.com/tolgraven/tolglow/actions/runs/45516334993)
