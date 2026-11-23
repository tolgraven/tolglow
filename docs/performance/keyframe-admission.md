# Measuring the cost of a predictive interval

A cheap interpolation can still make an effect more expensive. At a stride of two, one worker frame builds the next endpoint and one blends toward it. Comparing the mean cost of this pair with two direct evaluations answers whether lowering this effect's sample rate bought worker time.

## A mixed pixel and moving-head show

The release show ran at 40 Hz and 120 BPM with two real effect workers, 1,024 RGBW pixels and 128 moving heads. A wash applied three deep spatial fields to the strip. All movers aimed at a shared target. Both contained an affine beat ramp so each interval had an exact interpolant. A clock-driven pulse and recurring quarter-beat shutter one-shots exercised the fixed-rate paths while the CPU work changed.

The first pass calibrated stride one and two on **both** eligible effects, twice in opposite orders. Each run warmed up for 48 frames and measured the next 80. A worker-thread CPU timer enclosed the complete direct, endpoint or interpolation operation. A nested timer measured the release sample callback. Time spent waiting to run can increase the latter's **wall-time** cost. The report includes its CPU time as well. The probe adds two clock reads around each sample and records each row after stopping the enclosing timer.

| Effect | Direct worker CPU (ms) | Endpoint worker CPU (ms) | Blend worker CPU (ms) | Mean at stride two (ms) |
| --- | ---: | ---: | ---: | ---: |
| Spatial wash | 1.33–1.55 | 1.59–1.89 | 0.25–0.27 | 0.93–1.07 |
| Shared aim | 0.16–0.18 | 0.63–0.74 | 0.12–0.14 | 0.38–0.43 |

The second pass held the calibration results fixed and measured each decision **individually**. Six fresh show runtimes replayed the identical input, first in the order full, wash only, aim only, then in reverse. The wash saved **0.27 and 0.66 ms** of worker CPU per output frame. The aim added **0.20 and 0.24 ms**. The wash, movement, pulse and shutter continued to run at the original presentation rate.

## What the governor chose

To isolate admission from timing noise, three synthetic 32 ms pressure readings carried the measured direct sample wall times into Afterglow's release cadence function. A two-frame pressure window and maximum stride of two allow one slowdown. The comparator used the same minimum-cost threshold and ranked the candidates by measured whole-worker savings. It could also keep both at full rate.

* With both effects eligible, both rules chose the wash in each calibration round.
* When only the aim was eligible, the default 0.75 ms threshold kept it at full rate in both rounds. Lowering the threshold to zero made the current rule slow the aim. Both estimates of whole-worker savings rejected that change.

This experiment supports keeping the current default. A lower custom threshold can admit effects whose endpoint costs erase the gain from skipping samples. A live savings policy would need a bounded way to learn stage costs after edits, rig changes and overload, and enough evidence to distinguish noise from a real difference. The timings here exclude DMX packing, worker queue delay, presentation and physical hardware.

## Reproduce

Use the paired Afterglow revision `3fd49ef6877cac57dbc5f0e185fedd2dc7607f39`. The standalone process instruments two functions while it owns the show workers.

```sh
AFTERGLOW_ROOT=../afterglow lein with-profile +benchmark run \
  -m tolglow.perf.admission-cost target/profiles/admission-cost.edn 1024 128 48 80
```

Every measured run compared all universe bytes against the full-rate show. Projected linear RGB and native 3D aim agreed within 1e-9. All eight lit pulse frames, ten shutter transitions, one-shot lifetimes and emitted intents matched. The EDN includes counts, mean, p50, p95 and maximum stage cost, both decision pairs, holdout measurements and output traces. [Captured report](../benchmarks/master-performance/admission-cost.edn)
