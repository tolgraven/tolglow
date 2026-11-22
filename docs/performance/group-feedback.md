# Fixture-group feedback performance

Group feedback reads the committed bytes of its selected fixtures. A cached
reading remains valid only while the patch, group membership and all affected
universe bytes match. A changed fine byte forces a new measurement. Groups on
unrelated universes can retain their last reading. A steady target preserves the
model and instance identity. The controller still observes changed target
variables and operator settings on each frame.

Reproduce the software comparison from the project root.

```sh
lein run -m tolglow.bench-group-feedback 2048 60
```

Two warmed preview owners ran the same generated look on 2,048 RGB heads. One owner kept the group feedback enabled. The other continued the
same owned cue at its settled opacity. Sampling order alternated and the final
DMX universes had to match byte-for-byte. On the shared JDK 21 host, the
difference between the two median owner-thread CPU readings fell from 0.39 ms
to 0.10 ms after reusing unchanged measurements and publishing the observed show
variables. The corresponding median allocation difference fell from 8,192 to
2,952 bytes per frame. With reuse, wall p50/p95 were 4.06/26.10 ms with feedback
and 3.77/14.64 ms with the same look
alone. The benchmark includes the complete software tick, runs unpaced, and
reports CPU/allocation for the owner thread. It does not measure physical
output or universal 40 Hz delivery. A first scene still compiles the generated
effect and prepares its calibrated meter.
