# GitHub-built Studio on the physical M4 Air

The `Tolglow-Studio-arm64` artifact came from [Packaged Apple Studio run 45529197893](https://github.com/tolgraven/tolglow/actions/runs/45529197893). The PR head is `ec569ed0e46faee18c65a1aec1be4d62d79f1aac`. GitHub built the synthetic PR merge `c9af924740375af72d0a61d40aca25056b4a99bb` with Afterglow `e1eb9ff50985a9e3e304b54ec7a97db735956c5a`.

The downloaded archive SHA-256 was `f3f7634300bfbdc2239cc1fdb593d27d9f9d4130a3aff1f5d1da39e53e34590c`. The app passed `codesign --verify --deep --strict`, had no broken symlinks and declared `se.tolgraven.tolglow.studio`. The production preview scripts had the same hashes across the app JAR, its running localhost server and the local release at the PR head.

| Asset | SHA-256 |
| --- | --- |
| `preview-worker.js` | `2af5a972e065ac97b739b07c84d5ad8688534d37bbd2f8a948b5d2cc96369332` |
| `preview.js` | `0e2c71f5b0bdd8f54b8e00443ad871d57abb5611d2bfc91fa3cfb527aa8e46e7` |

The original, unchanged package ran with `--qualify-metal` and a disconnected Ember and Sweep show on 2026-11-25 UTC. Its focused Room showed an 816 × 500 canvas. The worker was ready, presented a frame with eight active lights and reported Apple GPU. The observer recorded 29 successful link events, 58 compiled shaders with translated MSL and a real draw. The packaged observer provided a single observed launch. A control run is absent from this receipt. Instrumentation may stall for the driver, so sustained frame rate and visual fidelity remain unmeasured here.

The receipt omits the ephemeral loopback URL. The test app and its loopback listener stopped at the end.
