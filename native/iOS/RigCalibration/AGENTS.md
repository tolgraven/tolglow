# Native rig capture

Follow native/AGENTS.md and docs/rig-calibration.md. This is actual ARKit IO, not
an alternative fixture engine or OSC server. Require camera permission, LiDAR,
normal tracking, bounded queues and deliberate output arming. Backgrounding,
interruption and failed transport stop capture/heartbeats; the host lease owns
lighting cleanup. Do not retry an ambiguously accepted output command.

Keep lens observations separate from beam landing spots. Preserve original image
intrinsics, depth confidence, AR world identity and capture timestamp. New AR
worlds need new rig alignment. Swift source/static checks do not establish an
Apple SDK build or hardware accuracy; run native/Tests/check_macos.sh on macOS
and record real device observations separately.

Measure passive room receivers from tracked plane boundaries in the same aligned
AR world. Freeze the outline for review and send only the selected rectangle.
Revision checks guard remeasurement. Keep passive entry available on a preview
host and invalidate local geometry when the alignment or tracking world changes.
