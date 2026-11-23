# Adaptive render quality

The show delivers DMX at 40 Hz. Presentation sampling can take a different
rate, and predictable effects may evaluate expensive endpoints less often than
their results are presented. A controller needs both time and output-error
measurements to choose a rate responsibly.

The ongoing quality lane owns replay, perceptual and discrete fidelity, timing
headroom, quality ceilings, and slow recovery after a pressured period. The
existing profiler owns spatial candidate pruning and GPU fusion. The viewport
renderer owns antialiasing. Shared live scheduling policy changes follow
measurements against a full-rate reference.

## Existing behavior

* Afterglow's keyframe runtime evaluates a future private clock, caches typed
  endpoints and interpolates native RGB, aim and other continuous attributes.
  Discrete assignments, emitted intents and stateful evaluations fall back to
  the live path. An artist can choose a musical interval and custom easing.
* The release host measures last-frame wall time and worker effect evaluation
  cost. Sustained pressure doubles one eligible effect's spacing. Quiet frames
  shrink spacing in steps. The current cadence cost estimates do not contain
  color, angular or transient-fidelity measurements.
* The cue-page sampler in Afterglow #71 and Tolglow #117 gives mounted visual
  sampling first claim on quality reduction. Window, page and device lifetime
  decide what presentation work exists. The lighting output cadence remains
  independent of pad presentation.
* The existing spatial-field adaptive benchmark verifies exact ArtDMX bytes for
  linear time terms. Its reference cannot reveal how sampled intervals alias a
  nonlinear pulse. The mover performance benchmark verifies layered transforms
  at full rate. This replay combines those two questions.

## Work in progress

At 120 BPM a 40 Hz lighting frame spans one twentieth of a beat. Compare full
rate, uniform skipped evaluations and per-effect sampling on the same fixture
patch and clock. Measure emitted RGB channel error, world-space aim error in
degrees and destination meters, discrete shutter state and edges, short visible
pulse overlap and lifecycle intents. Report timing of the real show tick apart
from validation work. Uniform sampling is a comparison lane. The proposed
selective lane leaves short pulses and shutter events at source rate while
interpolating slow continuous color and aim. The signal generators and the
sampled outputs are deterministic. Wall time depends on host resources.
