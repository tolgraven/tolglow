# Grouping independent effect work

In the first complete replay of 48 disjoint effects, the existing two-worker
executor finished 55 of 80 frames within 25 ms. Grouping the same effects into
two worker submissions finished 54. Serial evaluation finished 56. All three
sent identical ArtDMX, advanced the same effect instances and produced the
same ordered control, clock and lifecycle events. The two-worker production
policy continues to submit each effect individually.

## What can run together

Afterglow already evaluates separate instances on bounded workers. Both see
the same immutable frame. The coordinator restores priority and launch order,
commits each effect’s own state, and resolves native lighting layers. Grouping
work changes the number of executor tasks and how long each worker stays busy.

The offline planner describes a write by fixture ID and lighting attribute.
Two washes on the same pixel conflict. A wash and a dimmer on that pixel can
share a phase if their calculations only read the frame snapshot. Multiple
readers of the same beat can also share a phase. A writer waits for earlier
readers and writers of its field. Unknown callbacks, previous-layer reads,
dynamic target selection and a hot-reload candidate fence both sides of their
own job. The planner only trusts the known effect ID and revision in this
benchmark. It retains the indices of reusable groups while their instance
signature remains stable. It takes new state from the current frame every time.

Each phase divides its jobs across the two workers. An early large chunk can
leave its sibling idle. A work item is still evaluated by the standard
Afterglow runtime with the same fault isolation. The production registry does
not have to trust a new declaration about arbitrary user code.

## Workload and oracle

The rig has 1,024 RGBW pixels and 128 moving heads over 11 universes. Thirty-two
washes calculate color on groups of 32 pixels, while 16 effects aim groups of
eight moving heads at beat-driven world-space positions. The mixed scene adds
a dimmer on the first stripe, an overlapping color wash, a transform reading
the previous layer, and a deliberately faulty effect. Each run changes the
master, ends the first wash and relaunches it. This invalidates the dispatch
plan and exercises the real command path.

Three otherwise-identical shows run in rotating order at each logical 40 Hz
sample. Every frame checks all ArtDMX bytes, instance IDs, phases, native
parameters, private state and clocks, effect intents and the ordered observer
events against serial evaluation. Fault provenance and stack frames identify
the Java worker that caught the deliberate failure and are excluded from the
cross-executor comparison. The mixed show quarantines it once. The other
effects and commands continue.

After 32 warmup frames, each mode renders 80 measured frames. Whole-frame
wall time includes command drain, worker evaluation, joining, packing and
the ArtDMX capture sink. Worker-stage wall time covers the executor mapping.
CPU sums the owner and live effect-worker threads. Percentiles are nearest
rank. The benchmark does its oracle checks after the measured frame.

| Scene and dispatcher | Frames within 25 ms / 80 | Whole-frame p95 | Worker-stage p95 | Owner + worker mean CPU |
| --- | ---: | ---: | ---: | ---: |
| 48 effects, serial | 56 | 63.37 ms | 29.86 ms | 13.26 ms |
| 48 effects, per effect | 55 | 46.87 ms | 32.50 ms | 12.85 ms |
| 48 effects, grouped | 54 | 53.54 ms | 30.52 ms | 13.44 ms |
| 52 effects, serial | 12 | 124.09 ms | 58.38 ms | 14.31 ms |
| 52 effects, per effect | 7 | 175.68 ms | 48.91 ms | 15.40 ms |
| 52 effects, grouped | 2 | 117.42 ms | 68.79 ms | 15.94 ms |

A second capture on the same host gave 79 of 80 disjoint frames within 25 ms
with grouping and 77 with per-effect dispatch. Their p95s were 21.57 and
19.75 ms. For the mixed rig, grouping finished 74 and per-effect dispatch
75 within 25 ms. Both repetitions produced the same reference byte digests,
five beat events, one bar edge and three accepted commands in each scene.

The recorded four-vCPU Linux host had competing work. The higher whole-frame
p95 for per-effect dispatch in the mixed scene coexists with more frames
meeting the deadline than grouped dispatch. Grouping reduced steady submissions
from 48 to two and from 52 to five. The repeat showed a small change in the
opposite direction on the disjoint rig. The current evidence gives no
repeatable improvement in the full-frame deadline count. The paired CI job repeats the comparison and retains
per-frame wall timings, worker CPU, sampled RGBW bytes and emitted events.

```sh
AFTERGLOW_ROOT=../afterglow lein run -m tolglow.perf.independent-effects \
  /tmp/independent-effects.edn 32 80
```

The two reference digests over the 112 warm and measured frames are
`9ce2b382230a0c905a0920736df587cc8d3bc787aef669eacfd591a7ae84fc6b`
for disjoint effects and
`727cb71d6b7d201696e93e89bb045645d6a453a92717b757c20a9fc1869340f6`
for the mixed scene. Recorded source revisions are in the EDN.
