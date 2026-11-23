# Measuring the cost of a predictive interval

A cheap interpolation can still make an effect more expensive. At a stride of two, one worker frame builds the next endpoint and one blends toward it. Comparing the mean cost of this pair with two direct evaluations answers whether lowering this effect's sample rate bought worker time.

## A mixed pixel and moving-head show

The release show ran at 40 Hz and 120 BPM with two real effect workers, 1,024 RGBW pixels and 128 moving heads. A wash applied three deep spatial fields to the strip. All movers aimed at a shared target. Both contained an affine beat ramp so each interval had an exact interpolant. A clock-driven pulse and recurring quarter-beat shutter one-shots exercised the fixed-rate paths while the CPU work changed.

The first pass calibrated stride one and two on **both** eligible effects, twice in opposite orders. Each run warmed up for 48 frames and measured the next 80. A worker-thread CPU timer enclosed the complete direct, endpoint or interpolation operation. A nested timer measured the release sample callback. Time spent waiting to run can increase the latter's **wall-time** cost. The report includes its CPU time as well. The probe reads both clocks before and after each sample and records each row after stopping the enclosing timer.

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

Every measured run compared all universe bytes against the full-rate show. Projected linear RGB and native 3D aim agreed within 1e-9. All eight lit pulse frames, ten shutter transitions, one-shot lifetimes and emitted intents matched. The EDN includes counts, mean, p50, p95 and maximum stage cost, both decision pairs, holdout measurements and output traces. [Captured report](captures/admission-cost.edn)

## Can a losing two-frame trial hide a useful interval?

Measure the finest interval that actually saves worker time **within the effect's permitted maximum**. At 40 Hz and 120 BPM the tested intervals of one, two, four and eight output frames correspond to 1/20, 1/10, 1/5 and 2/5 beat. An artist can cap an effect at any of them. A movement or pulse fidelity constraint can lower that cap before any cost decision. [Floor-footprint bounds for predictable aim](aim-curvature.md)

Let `D` be the cost of evaluating directly, `E` the cost of a complete endpoint frame, and `I` the cost of an in-between frame, all in worker CPU milliseconds. If the two stage costs hold at stride `s`, the per-frame cost is `(E + (s − 1) × I) / s`. The experiment checks each permitted stride and chooses the first one with a cost below `D`. It keeps full rate when every permitted interval ties or costs more. It also records what happens if a losing stride-two trial immediately keeps the effect at full rate.

Each scenario first calibrates the two stages at stride two, then commits its choices for all four caps. Independent show runtimes measure the actual worker costs at strides one, two, four and eight in forward and reverse order. One report uses the measured means. A second uses the stage medians weighted by their measured frequency, which describes a typical sampling cycle and resists individual timing spikes. Both remain available in the capture.

The spatial fields change by fixture position. Their motion over the measured beat range is affine, so each schedule can be checked against exactly the same full-rate scene. Predictive scheduling affects one effect at a time while a pulse and shutter one-shots continue on the output clock. The spatial wash runs across the strip, and the aim cases use either one shared target or separate shallow and deep expressions at each moving head.

The table gives the first profitable stride under an eight-frame cap. Each pair shows the two held-out runs in opposite orders. A value of one keeps direct evaluation. Both rigs had 1,024 RGBW pixels.

| Effect | 128 movers, typical cycle | 128 movers, mean | 512 movers, typical cycle | 512 movers, mean |
| --- | --- | --- | --- | --- |
| Spatial RGB wash | 2 / 2 | 2 / 2 | 2 / 2 | 2 / 2 |
| Shared aim target | 1 / 1 | 1 / 1 | 1 / 1 | 1 / 1 |
| Shallow spatial aim | 1 / 8 | 1 / 8 | 4 / 4 | 4 / 4 |
| Deep spatial aim | 8 / 8 | 4 / 1 | 2 / 2 | 2 / 2 |

**Stopping after stride two missed a reproducible saving at stride four.** On 512 movers the shallow spatial aim took 0.264 and 0.295 ms per direct frame. At stride two its measured means rose to 0.344 and 0.386 ms. At stride four they fell to 0.239 and 0.251 ms. A two-frame artist cap keeps this effect at full rate. A four-frame cap permits the saving in both runs.

The deep aim on 128 heads reached its first typical-cycle saving at stride eight in both runs. Its mean changed enough to make that choice uncertain. The two-stage model picked the measured finest profitable stride in 12 of 16 held-out typical-cycle comparisons under the eight-frame cap. It picked it in 11 of 16 comparisons using means. A live policy needs confirmation of the saving at the chosen interval. The release governor continues to use its current timing policy.

Both runs held the eight lit pulse frames, ten shutter changes, full instance maps and emitted intents against the full-rate shows. All universe bytes agreed. Native XYZ aim and linear RGB agreed within 1e-9. The timing boundary covers the effect worker. The smooth affine fixture fields exercise this cost comparison on the sampled timeline. Curved paths, moving optical boundaries, discrete events and human retargeting still need their own fidelity limits.

Use Afterglow `14abdf9630baba74bbf70029a07609a14686eb01`. Its source tree matches the measured PR head `57c0056cc77cfb0c9230bc1906c784032275ec61`.

```sh
AFTERGLOW_ROOT=../afterglow lein with-profile +benchmark run \
  -m tolglow.perf.stride-break-even target/profiles/stride-128.edn 1024 128 48 80
AFTERGLOW_ROOT=../afterglow lein with-profile +benchmark run \
  -m tolglow.perf.stride-break-even target/profiles/stride-512.edn 1024 512 48 80
```

[Raw reports with all stage distributions, choices and output traces](captures/stride-break-even.zip)
