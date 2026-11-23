# When to sample inputs for a lighting frame

An operator moves an XY pad at 18 ms. The next physical output slot is at 25 ms. An effect that sampled the pad at 0 ms misses the gesture for that slot. An effect that samples at 15 ms also misses it. The next eligible slot will capture it. Delaying the sample improves freshness up to the point where doing so risks missing publication.

At 40 Hz there are 25 ms between output slots. If recent *whole frames* need 8 ms and the scheduler reserves another 2 ms for variation and handoff, it can wake around 15 ms into the interval. Those figures illustrate the policy. The mixed-rig stress benchmark missed every 25 ms deadline on its shared host. A scheduler can only spend spare time that the workload actually has. [Measured frames](https://github.com/tolgraven/afterglow/pull/88)

## Frame context

The physical output deadline uses a monotonic clock. Map that instant to show time and its beat, bar and phrase using the same transport snapshot for every effect in the frame. Keep render start, input sample times, completion, and actual send time separately observable. A seek, show replacement or clock discontinuity invalidates work for the old timeline.

Snapshot the latest completed sensor or simulation samples once at render entry. A bouncing ball can contribute its position, velocity, sample time and sequence. A pure effect receives that snapshot and its previous committed state and returns new state and lighting values. A worker never blocks the output thread waiting for a WebGL readback. If the latest observation is old, its age is visible to the resolver.

Two workers can process disjoint head and channel sets from the same snapshot. Shared variable reads and writes, previous-layer reads, native blends, and ordered effects introduce dependencies even when final DMX addresses differ. Validate parallel results against a serial replay, including state and event order. Commit the next state once.

The browser can advance fog, the camera, and presentation-only motion at display refresh. When a show subscribes to a browser-generated value, deliver timestamped changes at the requested sampling rate. Local events can update app-db and let re-frame reactions feed the affected components. Releasing the last consumer should stop any dedicated sampling or readback. A browser paint never establishes the physical publication clock.

## Running the 40 Hz capture

The release host starts Afterglow’s producer and publisher on a common physical
clock. It maps the time remaining to each slot onto the current show clock
under the owner lock. Every effect in that frame sees the same input map and
target beat. A command waiting when the render slot expires stays queued for
the next frame. A Link seek, pause, resume or show replacement invalidates
previously prepared source frames.

Inspect timing from the REPL with `(afterglow.output.scheduler/status
(:publisher running))`. The `:render-timing` and `:output-timing` maps give
nominal slot, actual render entry, input capture start, show time, beat,
handoff, actual send call, return and lower source input age at the sink.

```sh
AFTERGLOW_ROOT=../afterglow lein with-profile +benchmark run -m \
  tolglow.perf.output-deadlines /tmp/output-deadlines.edn 120 240
```

The EDN contains every render, physical publisher wake, transport call, sampled
fixture bytes and cold and warm statistics. It compares measured lead against
reserving a full frame period for every render. The eight-moving-head scene
runs Ember and Slow Sweep with a six-millisecond injected wait in Ember to
exercise a known lower bound. The other scene patches 1,024 RGBW pixels and
128 movers and runs spatial wash, world-space aim and pulse without a delay.
Each case uses two effect workers and full universe buffers in a capture
sink. Actual Art-Net or OLA interface latency requires a separate run.

### Recorded local result

On the 1,024-pixel, 128-mover rig, 215 of 240 nominal slots produced a full
render within 25 ms. The sender visited 237 slots and handed the current
slot's sample to its capture sink 205 times. The other sends held the last
eligible source. The two-worker mixed rig ran for six measured seconds after
three seconds of warmup. Percentiles use the nearest rank. Render duration
includes render start through universe-buffer freezing and handoff.

| Rig and lead policy | Complete within 25 ms / 240 | Current source sent / 240 | Render p95 | Median input age at target | Lower source age at actual send p95 |
| --- | ---: | ---: | ---: | ---: | ---: |
| Eight movers with the six-millisecond Ember wait, measured lead | 128 | 121 | 120.45 ms | 12.17 ms | 989.06 ms |
| Eight movers, full-period lead | 203 | 202 | 16.47 ms | 24.37 ms | 75.62 ms |
| 1,024 pixels and 128 movers, full-period lead | 202 | 199 | 21.67 ms | 24.50 ms | 74.73 ms |
| 1,024 pixels and 128 movers, measured lead | 215 | 205 | 21.75 ms | 16.33 ms | 61.85 ms |

Host contention varied sharply between the four sequential runs on this
four-vCPU Linux environment. For example, the eight-mover measured run took
53.91 ms at p95 for publication including packing, while the dense rig took
1.87 ms. These wall times include scheduling pauses. The isolated paired CI
job records its own traces. The replay checks that every transmitted lighting
source completed by its deadline, source times stayed ordered, and physical
send calls remained at least half a nominal frame apart, allowing 0.01 ms
for floating-point rounding. The raw trace retains cold startup.
[Measured timeline and replay files](https://github.com/tolgraven/afterglow/pull/89)

## Measuring the scheduler

Start with an explicit reserve on a cold show. Adjust it using observed completion costs, worker and packing time, and wake-up jitter. Take more lead after a miss and recover gradually. If the work cannot fit, skip expired scheduling slots and apply the configured late-frame policy. Prevent a late completion or an old show epoch from overwriting newer output.

Record input age at publication, slack at completion, deadline misses and actual send jitter. Check tempo changes, seek, pause, restart, clock discontinuities, slow workers and sudden changes in input cost with an injected clock. Compare against immediate-start scheduling on the same physical machine and timeline.

The MacBook Air qualification runs CPU geometry, completed native GPU shader readback, and the complete show with native materials as three independent workloads. The returning reports identify the machine, workload, output equivalence and missed deadlines. A Studio venue preview measures a fourth workload in its own browser context. [Pinned qualification kit](https://raw.githubusercontent.com/tolgraven/afterglow/eb1d5ba2cdec73c2637ba08c6af376e4547443e5/pr-88/macbook-air-lighting-benchmark.zip)
