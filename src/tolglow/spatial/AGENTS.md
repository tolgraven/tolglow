# Spatial show actions

Keep fixture search, late binding, ranking and contact-owned actions in Tolglow. Afterglow owns geometry, body motion, fixture membership and the rendered HDR contacts. Use show commands for each launch and keep the owning instance IDs in portable model data. An unrelated manual launch can coexist with a contact action. Reuse the same selector contract from Push, Studio, saved cues and headless callers.

Test membership changes, empty selections, cancellation, restore ownership and paused clocks. Hardware-free host regressions live in `test/tolglow/spatial_host_test.clj`. [Show contract](../../../docs/spatial-show.md)

`cues.clj` owns grid registration, live parameter sampling, member ownership and
the association between each instance and its native-layer mask. Use the ordinary
registry, launch and restore operations. Afterglow resolves per-head optical
weights and blends native groups. The host supplies immutable previous-frame
observations and keeps the optical reader out of saved state. A faulty shape
affects only its own cue. [Volume cue regressions](../../../test/tolglow/spatial_cues_test.clj)
