# Initial legacy renderer stage baseline

This is a **single-threaded stage microbenchmark**, not a measurement of a running
show. It establishes a repeatable starting point for examining ordered assigner
reduction and DMX channel packing. It proves neither end-to-end frame rate nor
an acceleration speedup. The new functional kernel is not being measured here.

## Reproduce

In the sibling Afterglow checkout, with Java and Clojure CLI installed:

```sh
./bin/bench --heads 100,1000,10000 --warmup 30 --samples 100 > renderer-baseline.edn
```

The executable is `afterglow/bin/bench`; the implementation is
`afterglow/dev/afterglow/bench_render.clj`. The wrapper reads application
dependencies through `dev/legacy_deps.clj`, without running Leiningen plugins,
documentation builds, or application startup. As with the legacy test lane, it
substitutes released Beat Link 8.0.0 for the unavailable historical snapshot.
First use requires dependency downloads. The measured loop has no device I/O:
it does not create or start a show, open controllers, or send DMX/Art-Net.

The wrapper fixes JVM initial/max heap to 256 MiB/1 GiB and runs headless. Pass
positive integer counts. A shorter development run can use
`--heads 100 --warmup 10 --samples 20`; its tail estimates are even less stable.
Keep each emitted EDN report with its source revision and machine information
when comparing changes. The report includes every measured sample, not only
summary statistics.

## Workload and measurement boundary

Each synthetic head represents four consecutive RGBW output channels. Channels
are packed into sequential 512-byte universes, with unused trailing channels
left zero. For each channel the harness prebuilds three real legacy
`Assigner` records: set a deterministic level, add 20 capped at 255, then scale
by 0.75. Base levels vary with pixel index and component; inputs are unchanged
between iterations. This describes output layout only: the benchmark does not
exercise the engine's RGB-to-RGBW conversion or fixture photometry.

Every timed iteration clears local universe buffers, runs the actual public
`afterglow.effects/run-assigners` reduction for each channel, and calls the actual
`:channel` implementation of `resolve-assignment` to pack results. This includes
legacy dispatch, reduction, assignment allocation, rounding/clamping and byte
writes. It does not replace those operations with an approximate implementation.

Construction and warmup are outside the timing interval. A sum of all unsigned
buffer bytes is computed after every measured iteration, outside its timing
interval, and must equal the warmed output checksum. This consumes the output
and catches inconsistent repeated results; it is not a substitute for the
characterization suite's independent expected-byte assertions.

Excluded work: effect construction/generation, assigner grouping, worker pool
scheduling, clock snapshots, geometry/aiming, movement history, color conversion,
network transmission, Art-Net packet creation, GPU work and readback. Production
Afterglow parallelizes independent targets; this harness intentionally measures
the serial stage. Neither observed stage time nor its reciprocal is a complete
show throughput estimate.

## Recorded run

Measured on 2026-11-21 with engine source revision
`16b0b4cac70433e4118038dfbfd304e22d1d9018` of the fork's `master` branch and the
new benchmark harness in this change. Runtime metadata:

[Raw EDN samples and metadata](benchmarks/2026-11-21-assignment-packing.edn)
are retained with this document. This is master's implementation; `tolmaster`
already contains typed byte-write changes and other experiments described in
[branch compatibility](branch-compatibility.md). Those changes have not been
benchmarked here, so these numbers must not be attributed to the user's active
fork configuration.

| Setting | Value |
| --- | --- |
| JVM | OpenJDK 64-Bit Server VM 21.0.13 |
| Clojure | 1.11.4 |
| OS | Linux 6.18.57, amd64 |
| CPU | AMD EPYC 9V74 80-Core Processor, verified using `lscpu` |
| JVM available processors | 4 |
| Container CPU quota | `400000 100000`, equivalent to 4 CPUs |
| Container memory ceiling | 16 GiB |
| JVM heap | 256 MiB initial, 1 GiB maximum |
| Collector | G1 |
| Warmup/measured iterations | 30 / 100 for each size, ascending size order |
| Allocation measurements | Not collected |

The JVM could not read the optional `/proc/cpuinfo` field in this sandbox, so
`:cpu-model` is `nil` in the EDN report; the CPU model above came from a separate
shell inspection. The harness records unavailable optional fields as `nil`.

All values below are milliseconds per measured stage iteration. Percentiles use
nearest rank. With 100 samples, p99 is the second-largest observation; these are
observations from one process, not confidence bounds or latency guarantees.

| RGBW heads | Channels | Universes | p50 | p95 | p99 | Maximum | Checksum |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 100 | 400 | 1 | 3.634 | 9.899 | 36.720 | 36.989 | 38,900 |
| 1,000 | 4,000 | 8 | 36.110 | 38.513 | 44.532 | 48.459 | 444,686 |
| 10,000 | 40,000 | 79 | 366.670 | 407.237 | 414.963 | 443.986 | 4,401,380 |

The process was running in a shared development container without CPU pinning or
an isolated performance host. JIT, garbage collection, container scheduling and
other development work can influence the observations. No allocation rate,
profiler attribution, SIMD execution, GPU result, or speedup was measured.

## Next measurement work

1. Capture profiles and actual allocation measurements for this same workload
   before selecting an optimization. Keep the source, checksum and workload
   fixed across candidate comparisons, and use independent JVM runs.
2. Add separate benchmarks for assigner creation/grouping, color conversion,
   movement/geometry, serial and bounded parallel evaluation, and transport
   packing. Instrument stage boundaries so a reduction in one stage cannot hide
   increased work elsewhere.
3. Add a headless full-frame harness using captured show definitions and a
   recording output adapter. Record total frame latency, p95/p99, allocation,
   scheduling overhead and exact DMX output parity before claiming renderer gains.
4. Evaluate primitive arrays/batching and then SIMD/GPU backends against the same
   CPU reference. GPU submission, synchronization, readback and packet packing
   belong in an end-to-end comparison. Keep browser preview performance separate
   from the server's physical-output timing budget.

See [rendering architecture](architecture/rendering.md) for the wider performance
plan and [testing](testing.md) for behavioral gates.
