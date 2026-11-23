# Live publication in the weighted venue

This probe starts a real Tolglow show at 40 Hz with two effect workers. The
renderer and the independent physical publisher run on their production clock.
The transport encodes ArtDMX datagrams into memory. It measures delivery
through the driver and encoder. Network transmission, fixture electronics
and the browser preview are outside this run.

The default rig combines 20,000 RGBW pixels, 64 moving heads, eight opaque
washes, the mixed chart projection and four moving volumes. The fixture poses,
UV placement, wash priorities, chart material and volume falloff match the Mac
benchmark kit. Position- and beam-based membership are both active. The Mac
report advanced a synthetic clock after the preceding render. This probe keeps
the physical clock advancing when a render overruns. [Mac rig and original measurements](macbook-air.md)

## Reproduce a session

Run from the checked-out Tolglow project with the paired Afterglow checkout
in the sibling directory. The optional arguments are pixel count, warmup slots
and measured slots. At 40 Hz the default 240 measured slots last six seconds.

```sh
AFTERGLOW_ROOT=../afterglow lein with-profile +benchmark run \
  -m tolglow.perf.weighted-live target/live-weighted-venue.edn 20000 160 240
```

From a REPL loaded with the benchmark profile, call
`(tolglow.perf.weighted-live/capture {:pixels 20000 :warmup 160 :slots 240})`.
The result includes both source revisions. Show startup supplies its default
adaptive keyframes. A separate control source computes the four positions for
the estimated next slot and sends public cue parameter commands. It waits for
all four receipts before admitting a new set. The recorded pose at each
completed render shows what the renderer actually received. Eight named washes
remain active alongside the original spatial color, gain, accent and movement
cues.

## Read the result

- `:nominal-slots` counts clock opportunities. `:completed-renders` counts
  renders that produced a ready timestamp. `:complete-within-25-ms` counts
  those taking at most one frame period from render start to output ready.
  `:ready-for-deadline` counts renders ready by their individual target.
- `:source-counts` distinguishes current delivery, repeated held output,
  newly delivered older source, initial idle output and delivery after slot
  expiry. Each admitted record has a monotonic driver-entry timestamp, an
  ArtDMX handoff timestamp after encoding, and a return timestamp. Skipped
  renders, missing publisher records and explicit pre-send expiry are separate
  counts. An absent publisher record does not establish its cause.
- `:sink-gap-ms` includes the preceding warmup handoff where available.
  `:lower-source-age-at-sink-p95-ms` measures input age through the memory
  transport. The driver retains a nonzero RGBW pixel witness and a checksum of
  the captured coarse and fine pan/tilt bytes across all 64 heads. The first
  head also has its four actual bytes in each receipt.
- `:effect-sampling` counts actual direct, future endpoint and interpolated
  instance frames. Each render carries its per-instance call count. The four
  committed pose counts, rejected controls and faulted instances reveal an
  inactive workload. `:show-output-exercised?` requires delivered pixel light,
  changing delivered aim and changes to all four volume positions.
- Allocation is JVM-wide over the recorded start/end sample, including
  controls, packing and observation. CPU time is process-wide. Collector time
  uses JVM management counters. These counters cannot assign cost to an
  individual effect. The observer checks packet bytes before returning from
  the sink, so the sender's return includes that inspection.

The final safety blackout runs after the scheduler drains and after the capture
is frozen. Deadline misses remain in the report.
