# Venue fog control

Afterglow owns the mass balance, optical coefficients, physical DMX inversion and fixture off bytes. Use the committed atmosphere and its delivery timestamp for feedback. A setpoint and an observation can refer to different times. Submitting a cue or evaluating a frame does not confirm a physical send.

Keep sensor-independent policy in `control.clj`. Ordinary cues take ownership ahead of automatic feedback, and a scan can operate the pump itself. Run motion, color and lighting energy through the existing renderer. Route automatic nozzle deadlines through the independent publisher in `deadline.clj` so a retained frame cannot outlive its duty limit.

Use the in-memory capture tests in `tolglow.atmosphere.control-test` and `tolglow.venue-fog-test` when changing this path. Their reversed-range, cross-universe fixture should receive its exact off bytes during a stall and an active-show restore.
