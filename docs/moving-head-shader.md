# Calibrated moving-head aim on a shader

World-to-head transforms batch well. Each head reads its own mount, world-space aim and previous pan, then writes two calibrated axes. The WebGL preview can consume those results on the GPU. Sending pan and tilt to fixtures adds a transfer back to the output host and requires matching the fixture's actual channel bytes.

## Reproduce

Install the project's Clojure CLI and npm dependencies before running the profiles.

```sh
npm ci
bin/profile-mover-shader 64 80
bin/profile-mover-shader 128 80
```

Set `AFTERGLOW_ROOT` to the companion checkout and `CHROMIUM_PATH` to a Chromium executable if they differ from `../afterglow` and `/usr/bin/chromium`. The default uses SwiftShader so CI can run the same corpus. Set `MOVER_USE_HARDWARE=1` to let Chromium select a graphics device and inspect the reported renderer. Set `MOVER_REQUIRE_EXACT=1` to require identical bytes at every address. The command prints a JSON report, exits unsuccessfully on a numerical regression, and opens an isolated graphics context without starting a show.

## Workload

The JVM generates phase-shifted world-coordinate sine and cosine trajectories for an array of truss-mounted heads. Each head receives a distinct target at each successive sixteenth of a beat. The fixture rows include standing and inverted mounts with yaw, roll and pitch variations. The patch mixes angle-range and center/half-circle calibration, per-axis output inversion and coarse/fine channels that cross universe boundaries. The full rig participates each frame.

Afterglow compiles the patch once. Its ordinary `resolve-frame` receives one `:aim` assignment per head per frame and history from its own last frame. The experiment uses the resulting normalized pan, tilt, reachable flag and every byte of every universe as its reference. [Corpus and oracle](../dev/tolglow/mover_oracle.clj)

The shader uploads mounting data once and receives each frame's world targets and its own previous pan. One vertex invocation per head applies inverse Z, Y and X rotations, computes yaw and elevation, tests the calibrated wrap candidates, picks the legal candidate nearest the previous pan and clamps unreachable axes. Transform feedback captures the results into a buffer with rasterization disabled. The benchmark copies this buffer into a staging buffer, waits asynchronously for its fence, reads it into CPU memory, then performs 1-based patch addressing, channel inversion and fractional-255 coarse/fine packing. [Shader](../resources/gpu/mover-aim.vert) · [Measurement harness](../dev/gpu/mover-shader.mjs)

## Measurement boundaries

Eight warmups precede 80 measured frames per rig. Timing begins before building the per-head upload and ends when the CPU has the complete DMX buffers. It includes command submission, browser scheduling, fence wait, readback and packing. CPU oracle generation, target choreography, rig preparation, shader compilation, IPC between a browser and a show host, scene drawing, transport and physical output are outside that interval. The fence measures elapsed time until the CPU can use a result. Browser timer wakeups typically take about 4 ms here. The driver reported a read-buffer shadow-copy warning, retained in the reports.

The ordinary CI gate asserts zero reachability differences, normalized axis error below 0.00001 and a maximum error of one unit of Afterglow's fractional-255 packing. It compares complete universes, including unaddressed slots, and requires that every different byte be attributable to a pan/tilt channel. CI reports timing without using it for pass/fail. Opting into the exact gate rejects any byte differences. Independent history on the two sides lets a wrong wrap choice propagate into the comparison.

## Result

Chromium 156 used ANGLE/Vulkan SwiftShader on an AMD EPYC 9V74 running Linux. The reference was `tolgraven/afterglow` commit `36abc94562329d8cb6c62c8916629b3e62fe2a21`.

| Heads | Universes | Axes compared | Coarse/fine byte differences | Frames byte-exact | Total p50 / p95 / p99 | Frames over 25 ms |
| ---: | ---: | ---: | ---: | ---: | --- | ---: |
| 64 | 3 | 10,240 | 1 / 9 | 71 of 80 | 5.46 / 9.72 / 16.89 ms | 0 |
| 128 | 4 | 20,480 | 1 / 19 | 62 of 80 | 6.09 / 25.46 / 31.21 ms | 5 |

Both sets agreed on reachability for every head. The largest difference in the emitted unsigned 16-bit pan or tilt word was **1**. The 128-head corpus exercised 1,409 wrapped pan solutions and 623 unreachable head/target pairs. Fence waits dominated the median, at 4.17 ms for 64 heads and 4.29 ms for 128. A sample of 80 makes p99 the largest observation. The raw report includes per-stage timing distributions and independent rolling FNV-1a digests over all DMX universes. [Measured results](benchmarks/2026-11-21-moving-head-shader.json)

For the preview, keep this computation on the same graphics device as the scene. For a physical-output backend, an equivalent native host benchmark on the target Mac needs to include readback synchronization, stale-frame handling and final DMX parity. These software measurements leave hardware acceleration and end-to-end speedup open. The existing CPU renderer continues driving physical output.
