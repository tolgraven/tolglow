# Deterministic automation

Seeds, clocks, registry/rig snapshots and ownership enter explicitly. Functions
return values and commands; no atoms, I/O, wall-clock reads or global `rand`.
Randomization is opt-in per schema, stays within hard bounds and avoids unbounded
rejection sampling. Parameter order must be deterministic.

Auto mode keeps selection and emission companions within the configured scope.
Intentional darkness bypasses emission prerequisites explicitly. Use resolved
output when predicting emission; a dimmer cue alone cannot illuminate a black
RGB fixture. Prediction is planning work, outside the render deadline. Keep
unknown/invalid fixtures visible instead of silently assuming illumination.

Record accepted instance IDs before cleanup so auto mode cannot stop a human's
replacement cue. A new show epoch discards pending ownership and old commands.

Derive a substream (`random/derive-seed`) from the identity a choice belongs to
(decision, cue, parameter, section boundary) instead of threading one stream
through unrelated choices. Section lengths are musical only. Use the launched
cue's chosen parameters in emission prediction. A refusal met while planning
drops that cue for the decision; it must not escape and stall the show tick.
Operator behavior and limits: [auto show](../../../docs/auto-show.md).
