# Native Push 2 waveform test capture

The still and four-second replay show the actual in-memory Wayang output after the injected send callback. The first frame uses width 0.25. The update frame follows the show owner's accepted runtime parameter change to width 0.75. Playback timing is for inspection.

- Tolglow build `eb62b8fc349a00d382d3433ca5e844f0e65c8108`, based on `0938af8bdc69d5b91f34a511b357749629331e02`
- Paired Afterglow build `16efb8b525e01d02a27897a389960da755247d13`
- Linux, OpenJDK 21.0.13, headless AWT, 960 × 160 RGB Wayang image, in-memory display and injected MIDI
- The startup change is in the test fixture. The scratch render initializes AWT fonts before the worker's first-frame wait. The measure is submission to the completed in-memory Wayang render and fixture send callback.

| Cache and workload | Initial scratch draw (ms) | Submission to first worker frame (ms) | Repeats passed |
| --- | ---: | --- | ---: |
| Cold writable cache | 6462 | 1388, 391, 866 | 3/3 |
| Warm writable cache | 3329 | 861, 261, 374 | 3/3 |
| Unwritable cache, two CPU peers on the same two logical CPUs | 9786 | 1514, 391, 238, 197, 433 | 5/5 |

Each row started a fresh JVM and repeated the test within that JVM. The probes ran during concurrent JVM load. The original fixture under the unwritable cache and pinned contention completed its initial projection in 441 ms, then entered AWT. No frame had reached the send callback after approximately 7 seconds in the draw.

The post-fix `lein -o test-pure` run passed 1,020 tests and 9,976 assertions.
