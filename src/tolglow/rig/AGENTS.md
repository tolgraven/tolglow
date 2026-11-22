# Rig configuration and observations

This directory owns show-facing rig operations. Afterglow owns fixture geometry, physical channel encoding, plume transport and the numerical calibration fit. Studio and the iPhone send the same operations available from the REPL. Keep Ring, camera APIs and display state at their respective boundaries.

- Surface positions, distances, lens apertures and sightlines use metres. A fixture's beam angle is its full included angle in degrees. Apply receiver tracking to the committed native values so a moving leader can have another head follow its lit edge.
- A fixture brightness measurement uses the physical publisher's confirmation of the dark and full-white states. A fog scan observes an ordinary cue or a manually operated burst. Use a stable lit reference and linear image contrast. The two procedures can have different timing and ownership requirements.
- Fog emission comes from the DMX the sink successfully sent. Measure physical time on the publisher's monotonic clock. Preview-only playback uses its committed output and show clock. A failed send leaves the previous pump level in effect.
- Fit scan data on the submitting thread. Admission on the show thread checks the epoch, session, rig and readings that produced the fit. Apply stores the combined clearance in the room and any measured mixing time on the selected nozzle. Preserve measurements and saved fog quantities in Transit show runs.
- Use hardware-free regressions first. Run `lein test tolglow.fixture-photometry-test tolglow.venue-fog-test tolglow.fog-calibration-test tolglow.reflected-surfaces-test` against the paired Afterglow revision. Record physical venue or mobile qualification separately from deterministic capture-sink and browser results.
