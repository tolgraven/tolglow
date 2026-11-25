# Check the actual Afterglow change

Afterglow `b7f830cd131c39c4f96fab6c81288f7316466c9f` and its parent `99f61c5bed2850daaeb87bc5b84ddf0d3c60bf64` were compared with Tolglow `ec569ed0e46faee18c65a1aec1be4d62d79f1aac` on the M4 Air. The comparison temporarily loads the original two affected functions for control arms. The candidate runs the committed implementation. The JVM and weighted in-memory fixture use the preceding benchmark configuration, with six seconds of warmup and eight seconds of live measurement per arm.

| Arm | Render median / p95 (ms) | Completed | Current / held | Sent / nominal |
| --- | --- | --- | --- | --- |
| Source commit 1 | 44.86 / 53.82 | 137 | 118 / 202 | 320 / 320 |
| Baseline 1 | 50.52 / 59.74 | 120 | 100 / 219 | 319 / 320 |
| Baseline 2 | 52.58 / 65.22 | 111 | 91 / 228 | 319 / 320 |
| Source commit 2 | 42.92 / 54.50 | 142 | 123 / 195 | 318 / 320 |

Each side delivered 638 of 640 slots in aggregate. The two candidate samples used more scheduled sources and all delivered packets were complete. Every render still exceeded 25 ms.

The source commit matched all 24 fixed-step model, intent and compact frame pairs and every byte of 3,792 ArtDMX packets after a mid-run fixture placement edit. The optics/spatial/HDR tests passed 36 tests and 399 assertions. The full pure suite passed 645 tests and 9,994 assertions.

To reproduce, copy the three Clj scripts into `target/weighted-product-optical-reader` in the matching Tolglow checkout. Set `AFTERGLOW_ROOT` to the candidate commit and run `lein with-profile +benchmark run -m clojure.main target/weighted-product-optical-reader/confirm.clj` through the bounded host wrapper.
