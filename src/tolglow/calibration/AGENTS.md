# Rig calibration

Read the root/JVM instructions and [calibration contract](../../../docs/rig-calibration.md).
Keep geometry, pattern planning, identity matching and proposal generation pure.
Camera/AR objects and sink resources never enter a portable session. Evidence
must come from actual successful publisher receipts, not admitted commands.

Use the real renderer as the aim/calibration oracle in tests. ARKit depth is
camera-plane distance, not ray length. Rig/AR transforms are proper rigid
right-handed matrices in metres; preserve fixture IDs, channels, addresses and
known axis calibration. Mounting rotation and axis reversal are ambiguous unless
mechanical calibration is verified. Missing/hidden lenses and weak geometry
remain unresolved candidates rather than confident generated placements.
