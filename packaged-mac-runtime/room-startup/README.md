# Packaged Mac Room startup

This paired physical M4 check used the same signed local bundle layout, the `se.tolgraven.tolglow.studio` identifier, and the same production Studio JAR and worker from [the ea993d3c hosted package](https://github.com/tolgraven/tolglow/actions/runs/45290490338). Both runs started the disconnected demo with Ember and Slow sweep, opened the actual AppKit window, and scrolled Room into view without clicking it. The probe only sampled when the native window was key and the Room canvas intersected the viewport. It took a sample every 500 ms.

In the baseline’s first eight samples, WebKit did not have keyboard focus and the background worker stayed idle. A later diagnostic focus transfer provided a positive control. In the candidate, the production AppKit view claimed initial focus and the worker was ready by the second sample. The candidate presented the eight-head show on the Apple GPU and exposed 29 linked programs and 58 Metal shader stages. Diagnostic focus was disabled for the entire candidate run.

The production first-responder change is `37472e048b57a0d5154bc595bb25648abc0e92c9`. Both test apps and loopback show hosts closed after capture. These are startup and shader-correctness receipts.
