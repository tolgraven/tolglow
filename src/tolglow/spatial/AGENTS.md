# Spatial show actions

Keep fixture search, late binding, ranking and contact-owned actions in Tolglow. Afterglow owns geometry, body motion, fixture membership and the rendered HDR contacts. Use show commands for each launch and keep the owning instance IDs in portable model data. An unrelated manual launch can coexist with a contact action. Reuse the same selector contract from Push, Studio, saved cues and headless callers.

Test membership changes, empty selections, cancellation, restore ownership and paused clocks. Hardware-free host regressions live in `test/tolglow/spatial_host_test.clj`. [Show contract](../../../docs/spatial-show.md)

`cues.clj` owns grid registration, live parameter sampling, member ownership and
the association between each instance and its native-layer mask. Use the ordinary
registry, launch and restore operations. Afterglow resolves per-head optical
weights and blends native groups. The host supplies immutable previous-frame
observations and keeps the optical reader out of saved state. A faulty shape
affects only its own cue. [Volume cue regressions](../../../test/tolglow/spatial_cues_test.clj)

A static sphere or rounded-box cue may declare a `:personality` input. Resolve
it from the frame's model with `tolglow.personalities/input-choice` and keep
the authored shape when it returns nil. The draw uses the cue ID, never an
instance ID, so a restored show renders the same bytes.
[Personality input regressions](../../../test/tolglow/personalities_host_test.clj)

`sampling.clj` owns the show's field-sampling workers. It slices a large patch
into contiguous ranges, runs Afterglow's public samplers on each range and
replays the hits in the serial insertion order. Keep per-head arithmetic in
Afterglow. Every task finishes before `sample` returns, and a failure is
rethrown as the worker threw it. Compare any change with the serial sampler
for entries and iteration order. [Sampler regressions](../../../test/tolglow/spatial/sampling_test.clj)
