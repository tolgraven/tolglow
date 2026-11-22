# Preview timing review

The local full browser suite observed 47 main-thread animation frames in ten seconds, with a 2049.9 ms maximum gap. The worker-only retry observed 75 with a 1183.3 ms maximum gap. Both missed the 1000 ms ceiling. The preceding main-thread timer checks passed in both runs, so the observed failure was specifically the paint heartbeat. Later assertions about GPU cadence and context disposal have no result from these failed runs.

The regression exercises real Chromium with the SwiftShader CPU backend, eight fixtures, a 20 ms incoming update interval and a separately measured browser compositor heartbeat. The render loop has explicit GPU fences and acknowledges one pending presentation at a time. Neither a UI timer nor a JS render duration alone proves a paint occurred. This workload is separate from the live frame coordinator, UDP sinks, physical GPU and hardware frame rate.

The worker, its timing test and JS preview sources matched e87e43f during these two runs. That baseline's dedicated GitHub worker/preview step passed in 87 seconds. On the shared local host four CPU cores were available by cgroup quota. Between approximately 20:22:14–20:23:00 UTC, CPU consumed 183.4 core-seconds and quota throttling occurred in 456 of 459 periods. This measurement was taken during concurrent compilation after the two failures. It is evidence of current contention, not a historical per-frame CPU trace.

[Baseline Studio job](https://github.com/tolgraven/tolglow/actions/runs/45059030276/job/137320570625)

The interim remediation PR also passed the dedicated Chromium preview/worker CI step on a5b41f1f138642099da008ca316333c496b87ece in 75 s. [Interim Studio job](https://github.com/tolgraven/tolglow/actions/runs/45065966730/job/137342094908)
