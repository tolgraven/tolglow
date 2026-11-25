# Longer publication comparison with the integrated preview revision

These controls use Tolglow `d5664f52` and the committed Afterglow specialization at `b7f830cd`, comparing the original and specialized functions in the same process. Each arm warmed for six seconds and then measured 1,000 nominal slots (25 seconds). The M4 and bounded host JVM configuration stayed the same. The DMX transport is the in-memory sink.

| Arm | Render median / p95 (ms) | Completed | Current / held / late | Sent / nominal |
| --- | --- | --- | --- | --- |
| Baseline 1 | 55.04 / 63.99 | 343 | 294 / 704 / 0 | 998 / 1000 |
| Candidate 1 | 50.85 / 61.09 | 360 | 316 / 679 / 1 | 996 / 1000 |
| Candidate 2 | 46.02 / 56.61 | 418 | 352 / 644 / 0 | 996 / 1000 |
| Baseline 2 | 51.15 / 61.79 | 365 | 291 / 705 / 0 | 996 / 1000 |

The candidate delivered 668 scheduled sources versus 585 across the controls. It missed eight slots in total against six in the controls and delivered one packet late. Each delivered frame was complete. The largest observed sink interval was 51.21 ms with the candidate and 43.49 ms with the control. These samples establish higher producer throughput. Publication efficiency and the 25 ms producer budget remain open. The fixed-step replay again matched every byte of 3,792 packets and all 24 model, intent and Compact frame pairs, including a fixture placement edit.
