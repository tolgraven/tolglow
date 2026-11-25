# M4 Room bitmap and fence comparison

The live Room worker on the physical M4 rendered eight moving heads in maintained haze at the same camera, interpolated fixture updates, zero work light and a fog setting of 0.7. Every twenty-second sample used the same Auto floor. The scene rendered at 914 × 410, fog at 301 × 135 with 24 base ray steps, and the canvas presented at 2806 × 1260. Desktop lock, tab focus, controls, GPU renderer and viewport checks passed. The run used a loopback demo with physical output disabled.

| Pass | Accepted presentations/s | Display callbacks/s | p95 interval (ms) | Median fence polls/frame |
| --- | ---: | ---: | ---: | ---: |
| Full control | 55.45 | 60.00 | 27.92 | 21 |
| Transfer bitmap before fence | 56.05 | 60.00 | 26.34 | 21 |
| Poll fence more frequently | 56.00 | 60.00 | 29.20 | 69 |
| Full control | 56.30 | 60.00 | 27.73 | 21 |

The early-transfer variant still waits for GPU completion before publishing and closes an aborted bitmap. The polling variant raises the hardware cooperation budget from 16 to 64 task yields. Both retain the single-frame guard and all graphics passes. Diagnostic GPU timers stay disabled in every arm. The bitmap call takes approximately 0.02 ms at the median, and the timing covers JavaScript return only. Fence and bitmap times combined are 9.30–9.36 ms in the controls and 9.38 ms with early transfer. The new presentation rates lie within control drift. Neither variant is in production.

The entry point serves its opt-in worker from a separate Ring resource root scoped to the demo. It compares the served worker digest to the emitted file. Chrome's owned benchmark tab and the private server close at the end. The results cover actively moving fixtures with dispersed haze. Dense emitted plumes and macOS WebKit need separate qualification.

Run in a built checkout of Tolglow `38d19ea24a5e9766ed8572bb96cef23a42456204` with the paired Afterglow `bcf63ec2fe920543fd72e61e8a961cbf765be433`, an unlocked Mac and the shared Chrome testing session.

```sh
mkdir -p target/room-presentation-probe
cp /path/to/prepare.py /path/to/summarize.mjs target/room-presentation-probe/
python3 target/room-presentation-probe/prepare.py
AFTERGLOW_ROOT=/path/to/afterglow ROOM_SCENE=dense ROOM_QUALITY=auto \
ROOM_LIGHTING_MODE=interpolate ROOM_WORK_LIGHT=0 ROOM_PASSES=full,early-bitmap,eager-fence,full \
tolgraven-cpu runtime -- node target/room-presentation-probe/measure-room.mjs \
http://127.0.0.1:9223 target/room-presentation-device
node target/room-presentation-probe/summarize.mjs target/room-presentation-device/room.json
```
