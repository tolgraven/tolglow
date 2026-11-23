# Adaptive scheduling and visual fidelity

The existing predictive sampler provides origin, target and interpolated output. The display and cue-grid samplers have their own consumers. Use full-rate runs of the identical seeded show as the reference for proposed scheduling changes. [Musical keyframes](../effect-keyframes.md) · [Presentation and physical deadlines](../output-presentation.md)

| Workload | Candidate experiment | Compare with the reference |
| --- | --- | --- |
| Slow washes, dimmers and continuous color | Adapt per-effect and per-preview sampling to temporal change and measured cost. Interpolate every presentation frame. | Linear-light RGB error and brightness peaks, frame latency percentiles and extreme samples. |
| Geometric motion | Use angular/angular-velocity or projected footprint error to bound keyframe intervals. | Direction error in degrees, beam-edge displacement at venue distance, wraparound and target changes. |
| Sparse flashes, strobe, shutter and wheel functions | Preserve timestamped transitions and discrete holds. Invalidate a predicted interval on a human command, tempo jump or hot reload. | Missed/doubled transitions, maximum timing offset, pause/restart behavior and immediate human override. |
| iPad, Push and background Studio | Stop zero-demand samplers, reduce cosmetic keyframe frequency first, sustain presentation refresh separately from sampling. | Subscription count, capture CPU/GPU, physical output p95 while previews start or stop, and offscreen work. |
| Entering/leaving overload | Reserve headroom, reduce load in steps, and recover with hysteresis. Compare against fixed-cadence and existing visual-first policies. | Time spent congested, emitted fallback frames, recovery time and oscillation under realistic bursts. |

Report limits and measured error alongside time saved for each device. A faster animation that misses a one-shot, or a high refresh number produced by repeating aged data, must be distinguishable in the results. Physical rendering, shared geometry and GPU program optimization are coordinated with the [work-elimination lane](render-work-elimination.md).

## Measured decisions

* **Give visible controls their own sampling budget.** Studio and Push 2 share
  musical cue-color keyframes for mounted cells. The host releases work when
  the final surface leaves. Sustained pressure first widens presentation
  sampling, while visual interpolation and physical output keep their own
  clocks. Browser and captured Push 2 LED evidence accompany the implementation.
  Push 3 currently exposes a semantic data view. Its physical pad LED writer
  and device measurements are still open.
  [Mounted pad previews](https://github.com/tolgraven/tolglow/pull/117)
* **Keep short flashes at source rate.** Uniform four-frame evaluation missed
  11 of 16 flash frames and introduced five false ones in a two-bar replay.
  Selecting intervals per effect preserved all flashes and all 19 shutter
  changes. The automatic scheduler now protects clock-driven discontinuities
  while allowing predictable washes to slow. In 320 output frames with
  controlled overload and recovery, it inspected two definitions, matched
  every reference flash byte, and returned the wash from stride eight to one.
  [Reference replay](multirate-quality.md) · [Recovery test](../effect-keyframes.md#reproduce-the-transient-and-recovery-test)
* **Admit effects using evidence from both worker stages.** On 1,024 RGBW pixels
  and 128 moving heads, stride two saved 0.27–0.66 ms per wash frame and cost
  0.20–0.24 ms per shared-aim frame. The default threshold kept the aim at
  full rate. A zero threshold admitted it in both repeat runs.
  On 512 movers a shallow spatial aim lost time at stride two and saved
  time at four in both trials. [Admission and interval measurements](keyframe-admission.md)

* **Count the whole interpolation window.** Cheap interpolation frames can hide
  expensive endpoint preparation. On a 1,024-pixel and 64-mover show,
  four-frame spacing reduced worker CPU for a deep RGB wash but increased it
  for the aim program. The saved reports separate full, endpoint and
  interpolation work. [Worker-stage experiment](multirate-quality.md#worker-cpu-cost-of-direct-endpoint-and-interpolated-frames)

* **Read packed movement endpoints directly.** On 512 movers and 512 pixels,
  four-frame aim keyframes consumed 1.34–1.36 ms of worker CPU per frame when
  geometry validation expanded both numeric columns to assignment rows.
  Checking each aligned XYZ triplet directly brought it to 0.53–0.72 ms.
  A separate comparison found identical native aim, RGB, shutter, cue lifetime,
  intents, and all DMX universe bytes for 160 frames.
  [Measured Afterglow change](https://github.com/tolgraven/afterglow/pull/76)

* **Measure motion error where the beam lands.** At four-frame intervals,
  the same near-target movement displaced a 5° spot by 1.299–1.300 spot radii at
  p95 and a 40° wash by 0.206 across two calibrated Afterglow packers. Choosing a stride per distance and beam
  width from one bar met a 0.25-radius budget on the held-out bar. A faster
  sine sweep exposed the limit of testing the midpoint alone. All 40 intervals
  passed that check, yet intervening frames drifted by up to 10.82 radii.
  The live scheduler still needs a reliable curved-motion admission rule.
  [Calibrated footprints, fork revisions and replay](multirate-quality.md)

* **Reuse display work between lighting commits.** In a 64-pad replay,
  returning 486 MIDI acknowledgements previously caused 566 whole-grid
  projections across 80 lighting frames. Reusing the committed grid reduced
  this to 80 and preserved all 3,598 palette writes. With 32 cues active, a
  separate replay compared each MIDI message and display publication while
  Afterglow shared a sorted cue index inside each device refresh. Session CPU
  averaged 5.87–6.30 ms per lighting frame, down from 14.51–17.61 ms.
  Both captures used an immediate-acknowledgement MIDI emulator.
  [Physical-session projection](https://github.com/tolgraven/tolglow/pull/143) · [Active cue indexing and reproducible comparison](https://github.com/tolgraven/afterglow/pull/80)

The paired master profile retains `multirate-quality.edn`,
`transient-scheduling.edn`, `sampling-cost.edn`, `beam-fidelity.edn` and both
source revisions.
Its first complete CI run measured the three replays in 26, 16 and 20 seconds
respectively. [CI evidence](https://github.com/tolgraven/tolglow/actions/runs/45152359329)

Physical GPU timing, iPad power and display latency, and a live angular-error
ceiling for strongly curved movement remain open. The native GL reference
replay used Mesa software rendering with deadline fallbacks.
