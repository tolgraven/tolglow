# Graphics experiments

These namespaces are Quil/Processing and GPU experiments, not the production
browser simulator. Inspect top-level forms before requiring a sketch: avoid
opening windows or starting render loops in headless tests.

The planned renderer and rig editor consume a shared scene description with
explicit meter/radian units, fixture/head IDs, calibration, and physical output
channels. Preserve Afterglow's audience-facing axes (X right, Y up, Z toward
the audience); convert coordinates only at adapter boundaries. Processing's
inverted Y convention must not leak into fixture configuration.

Do not couple the browser preview frame rate to DMX output. Simulator quality
must scale down for iPad/phone GPU and thermal limits. Compare simulated beams
and colors against fixture output data, then add measured optics and spatially
varying fog; a uniform glow or postprocess bloom is not a fog model.

GPU compute work requires a CPU reference and end-to-end timing including
upload, synchronization, readback, and output encoding. Browser WebGL preview
and authoritative show computation are separate deployment concerns. Record
fallback behavior and do not claim AVX/GPU speedups without measurements.
