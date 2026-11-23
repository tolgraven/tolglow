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

### Dedicated Ubuntu runner

On the 1,024-pixel and 128-mover rig, **238 of 240 slots completed a full render
within 25 ms** on the paired CI runner. With measured lead the render took
7.12 ms at p95, 226 slots sent their own sample, and median input age at the
nominal target was 7.24 ms. Reserving the full period gave 237 current-source
sends and 24.84 ms median input age. When a source missed its slot the direct
policy held the last one. The resulting p95 age at actual send was 31.65 ms
with measured lead and 26.92 ms with fixed lead.

With the known six-millisecond Ember wait, the measured policy completed all
240 renders in 25 ms and sent 236 current sources. Render p95 was 9.61 ms.
Median input age at the slot was 10.02 ms versus 24.82 ms with fixed lead.
The fixed run completed 237 renders within 25 ms and sent 237 current sources.
The rendering and sender p95s reflect software execution through the memory
sink. The performance artifact includes the raw observations and cold startup
for all four original cases. [Paired Ubuntu job](https://github.com/tolgraven/tolglow/actions/runs/45191695719/job/137751416392)

The original dense-rig trace had nine late frames by less than one
millisecond. Two others ran late by 1.07 ms and 2.46 ms and one by 16.72 ms.
If each measured render had taken the same time in the available idle window,
1.5 ms more lead would have covered ten of the twelve misses. The developer
replay now runs a separate measured-lead policy with 6% of a physical period
of extra reserve after cold startup. It records real output at that lead so
the candidate can be compared with the production and fixed-period policies.

In a separate six-second local run, the dense rig completed **234 of 240**
frames within 25 ms with extra reserve and sent 222 current samples. With
the production lead it completed 229 and sent 212. Render p95 was 12.46 ms
and 12.55 ms, respectively. The extra reserve increased median input age
at the target from 10.79 to 11.92 ms and p95 lower-source age at actual send
from 37.52 to 37.80 ms. In the eight-mover scene it completed 235 frames
and sent 230 current samples, versus 232 and 218 with production lead. The
extra reserve remains a developer benchmark option.

The next dedicated Ubuntu run completed **238 of 240** dense frames within
25 ms with production lead and sent 229 current sources. The extra reserve
completed 239 and sent 237. Render p95 was 6.06 ms and 5.90 ms, respectively.
Median input age at the target rose from 6.31 to 7.67 ms, and p95 age of the
lower source at the sink rose from 8.78 to 10.19 ms. Admission preceded the next deadline in every one of those six cases. [Six-policy Ubuntu job](https://github.com/tolgraven/tolglow/actions/runs/45199722459/job/137779636192)

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
for floating-point rounding. The earlier shared capture also recorded two
previously unseen sources handed to the sink a whole slot late, 35.8 ms
after the dense rig's deadline and 88.4 ms after the eight-mover deadline.
The sender now checks expiry again after packing. The release replay verifies admission time, records the callback timestamp
inside the capture sink, and reports expired or closely spaced entries separately. The raw trace retains cold startup.
[Measured timeline and replay files](https://github.com/tolgraven/afterglow/pull/89)

A separate capture of the atomic mailbox and source retention revision
`84b3821` completed **141 of 240** dense frames within 25 ms, with 112
current-source sends and 39.45 ms render p95 under shared-host load. The
publisher dropped three dense production-lead attempts after packing. Across
all six cases, 33 attempts expired before admission. At eight-mover slot 346,
the sender admitted its packet 23.50 ms after the target. The test sink
entered 5.23 ms later, 28.73 ms after the target. Another eight-mover pair
entered the sink 12.38 ms apart. The dense rig had neither condition. Each
raw capture records both Git revisions.

Of the 240 nominal slots in the dense case, 169 had a render. Of these,
141 took at most 25 ms, and 151 finished before their own deadline. A frame
can start more than one period early. The sender visited 177 slots, sent
112 current and 62 held frames, and dropped three attempts after packing.
The p95 render time uses completed renders as its denominator. Slot 142
finished rendering 20.56 ms early. The publisher woke 17.83 ms late and
spent another 53.54 ms before rejecting that packet. Slot 143 woke 21.73 ms
late and its 62.15 ms render missed its target. Slot 145 held slot 141’s
source. Slot 148 supplied the first new source.

Future captures pair admitted and delivered packets by order and frame ID.
They report `:late-sink-entries`, `:short-sink-gaps`, and
`:meets-sink-timing?` using the timestamp inside the sink. A missing sink
cannot count as a fresh send. The sender now prepares retired-fixture and
fog-channel output before its final admission check and spaces the next
attempt from transport return. An OS pause after the check can still delay
the downstream sink. [Full slot chart and raw captures](https://github.com/tolgraven/afterglow/pull/89)

## Measuring the scheduler

Start with an explicit reserve on a cold show. Adjust it using observed completion costs, worker and packing time, and wake-up jitter. Take more lead after a miss and recover gradually. If the work cannot fit, skip expired scheduling slots and apply the configured late-frame policy. Prevent a late completion or an old show epoch from overwriting newer output.

Record input age at publication, slack at completion, deadline misses and actual send jitter. Check tempo changes, seek, pause, restart, clock discontinuities, slow workers and sudden changes in input cost with an injected clock. Compare against immediate-start scheduling on the same physical machine and timeline.

The MacBook Air qualification runs CPU geometry, completed native GPU shader readback, and the complete show with native materials as three independent workloads. The returning reports identify the machine, workload, output equivalence and missed deadlines. A Studio venue preview measures a fourth workload in its own browser context. [Pinned qualification kit](https://raw.githubusercontent.com/tolgraven/afterglow/eb1d5ba2cdec73c2637ba08c6af376e4547443e5/pr-88/macbook-air-lighting-benchmark.zip)
