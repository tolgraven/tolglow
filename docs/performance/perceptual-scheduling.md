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
