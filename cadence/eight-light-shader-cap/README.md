# Eight-light fog shader capacity probe

Chrome ANGLE Metal on the M4 rendered four separate eight-head cases with the same per-frame input at each comparison. Replacing the two temporary ray arrays and the matching loops' upper bound with eight left every sampled RGB8 output and half-float volume value identical, including motion samples.

| Scene | Auto GPU ms, original / cap 8 | Desktop GPU ms, original / cap 8 |
| --- | --- | --- |
| Narrow fresh plume | 3.04 / 3.26 | 7.41 / 8.28 |
| Three plumes across the room | 3.21 / 3.97 | 7.76 / 9.69 |

Timing brackets both the volume integration and depth-aware composite. The candidate added GPU time in all four cases, so the renderer continues to use its existing loop capacity.

Run the script from `target/bloom-air/` in a built checkout of `38d19ea2` using the shared Chrome testing profile. Pass the output path and `--motion`.
