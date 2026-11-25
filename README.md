# Beat-locked pulse replay

Tolglow commit `09ebea3c880b0872e845a5c5bb494d6b549175ff` captures two typed parameter values for the certified pulse expression. Every evaluation selects an endpoint using the current beat phase and the same floating-point operations as the direct sampler. Show identity, epoch, effect revision, waveform, destination schema and authored binding scope the retained plan. Meter, source and the private or shared launch clock drive phase on every frame. Custom expressions and oversized destinations use the direct evaluator. [Issue #509](https://github.com/tolgraven/tolglow/issues/509)

The mixed venue includes 2,048 RGBW pixels, 64 heads, eight washes, a projected chart, four spatial volumes, two effect workers and 16 pulse banks. The banks attach 31 beat- or bar-driven pulses and one sine to exercise the fallback alongside the optimized path. The runtime advances at 25 ms logical steps and sends packed frames to an in-memory ArtDMX sink. A captured clock sample enters the normal source boundary and jumps backward from 5.705 to 0.375 beats. The trace also changes tempo, meter, duty, phase, master and blackout. [Replay harness](replay.clj) · [Master reference](reference_parameter_sources.clj)

Each pass warms both runtimes for 50 frames and then pairs 150 measured frames. The observed controls, acknowledgments, intents and packet hashes match at every paired sample. The raw first-pass streams have the same SHA-256 `be23cb4146e0dfdfb08a14d3eb2f76b73b5214340ae76c6d083c16384ff87851`. The measured sample includes 4,649 retained pulse evaluations, one changed-binding rebuild and 150 direct sine evaluations per pass. [Direct packets](reference.artdmx) · [Captured cycle packets](replay.artdmx) · [Timing log](benchmark.log)

The still and video render the two actual packet streams against the fixture positions. The video plays each captured logical frame at its recorded phase. [Still](pulse-cycle-replay.png) · [Video](pulse-cycle-replay.mp4)

## Reproduce

Use the candidate Tolglow checkout and Afterglow `a2b945327973a1d6dd391929c91e1d02a7b433dc`. The reference sampler was taken from Tolglow `ae7f2b6c000008644d4e5658f4fb2a8ee97acdd5`, with its namespace changed so both samplers can run in one JVM. All lighting output is routed to the in-memory sink.

```sh
export AFTERGLOW_ROOT=/path/to/afterglow
export JAVA_TOOL_OPTIONS='-Xmx1024m -XX:ActiveProcessorCount=3'
lein run -m clojure.main /path/to/replay.clj /path/to/artifacts 2048 150 50 2
python /path/to/render_capture.py /path/to/artifacts
lein test-pure
lein check-definition-spacing
```
