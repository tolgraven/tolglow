# M4 Studio Room cadence repeat

Production Studio, Tolglow `25714194abf931f2a84d29a451ca0e47ac97bfe2`, the #489 preview merged with its contemporary master. Companion Afterglow `7e399c58c5e2d66f5a2722be48e6d3cf33af9c00`. Each scenario sampled accepted DOM bitmap presentations for 20 seconds in one foreground Chrome tab. All 71 OS console checks reported unlocked. Camera and controls were stable during the samples. Browser display callbacks stayed near 60 Hz.

| Scene | Presented Hz | p95 frame interval | Gaps over 25 ms |
| --- | ---: | ---: | ---: |
| Fixed heads, ambient haze | 57.00 | 25.40 ms | 60 |
| Moving heads, dense haze | 54.20 | 31.98 ms | 112 |
| Moving heads, active plumes | 46.35 | 35.84 ms | 271 |

The actual eight-head demo ran the Ember cue in all scenarios and added Sweep for the two moving scenarios. The third scenario accepted five local fog plumes during measurement. Auto used a 914 × 410 scene, a 301 × 135 fog target with 24 base steps, one shadow, and a 2806 × 1260 backing canvas. Fixture interpolation was enabled and the work light was zero. The browser reported ANGLE Metal on Apple M4.

Use the dated output directory in a built checkout with the paired Afterglow available. This test uses a preview-only show on an ephemeral localhost port and closes its owned tab and server. It requires an unlocked foreground Chrome development window on the real display.

```sh
ROOM_LIGHTING_MODE=interpolate ROOM_WORK_LIGHT=0 ROOM_PLUME_SCENE=1 ROOM_WARMUP_MS=12000 \
  /Users/tol/.local/bin/tolgraven-cpu runtime -- \
  node dev/preview/measure-room.mjs http://127.0.0.1:9223 target/room-m4-repeat
```

`room.json` contains program hashes, view and worker settings, acceptance times and OS checks. The result is a valid production measurement and misses the sustained display-cadence goal. The Room display clock and the DMX output publisher have separate budgets.

## Matched graphics pass comparison

The same renderer drew the moving eight-head haze scene in five 20-second blocks. Auto was frozen at the same raster, fog target, base steps, one shadow, and 60 Hz target as above. The comparison held the camera and controls fixed, omitted diagnostic GPU timer queries, and passed all 117 desktop unlock checks. The diagnostic worker verified which known stages actually ran for each accepted bitmap.

| Pass | Presented Hz | p95 frame interval | p95 GPU fence wait |
| --- | ---: | ---: | ---: |
| Full renderer before | 53.10 | 31.88 ms | 20.34 ms |
| Re-present a cached bitmap | 59.70 | 20.71 ms | 9.92 ms |
| Bypass fog volume and depth composite | 58.54 | 22.41 ms | 12.29 ms |
| Reuse the shadow map | 48.35 | 35.18 ms | 20.94 ms |
| Full renderer after | 48.50 | 35.56 ms | 20.94 ms |

The fog bypass ran faster than both full controls. Reusing the shadow map was within 0.15 Hz of the adjacent full control. The two full controls vary, so use the comparison to locate the cost and retain both baselines. The pass comparison uses dispersed room haze. It does not isolate the cost of active plume subdivision. The presentation figure counts accepted DOM bitmaps, and fence wait includes polling and GPU completion. The bypass removes visible fog and is a diagnostic.

Repeat the comparison with the same command above, omitting `ROOM_PLUME_SCENE` and setting `ROOM_WARMUP_MS=20000`, `ROOM_SCENE=dense` and `ROOM_PASSES=full,present-cached-frame,skip-fog-raymarch,cached-shadows,full`. Extract the full receipt with `gzip -cd attribution.json.gz`.
