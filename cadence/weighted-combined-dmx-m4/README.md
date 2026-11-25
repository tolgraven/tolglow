# Combined Afterglow changes on the physical Air

This is the weighted 20,000-pixel and 64-moving-head in-memory ArtDMX comparison from Tolglow `d5664f52`. The local Afterglow merge `442a835e5997dadf5eae82fc66cd33b2a48afd25` combines [#121](https://github.com/tolgraven/afterglow/pull/121) commit `b7f830cd` with [#122](https://github.com/tolgraven/afterglow/pull/122) commit `28616f1`. Its tree is `fe92914834d1f00994af0354a79b5aa977ad61a3`. The controls restored the three original affected functions in that same JVM. Each arm warmed for six seconds and measured 600 nominal slots.

| Arm | Sent / nominal | Current / held | Render median / p95 (ms) | Shortest / longest sink gap (ms) |
| --- | --- | --- | --- | --- |
| Combined 1 | 600 / 600 | 167 / 433 | 52.30 / 58.40 | 14.42 / 35.66 |
| Baseline 1 | 597 / 600 | 182 / 415 | 51.79 / 60.69 | 19.26 / 42.56 |
| Baseline 2 | 595 / 600 | 182 / 413 | 52.40 / 61.38 | 14.94 / 83.27 |
| Combined 2 | 600 / 600 | 202 / 398 | 48.23 / 57.67 | 16.24 / 34.89 |

The combined arms delivered 1,200/1,200 slots against 1,192/1,200 in the controls. All packets were complete, shortest measured gaps exceeded 12.5 ms and the two combined p95 render timings were lower. The combined arms supplied 369 scheduled sources and held 831. Sustained fresh output remains open because every render took more than 25 ms. Exact fixed-step replay again matched model, intent and Compact state over 24 frames, including a fixture placement change and every byte of 3,792 ArtDMX packets.
