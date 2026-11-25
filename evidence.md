# Ableton Session encoder recording

Tolglow `904b7cbd` and its released Afterglow pin `8b0e005f` ran the shared show owner, Ring callback, Node transport and Max adapter against a behavioral Live object graph. The clock is manual and the demo's eight fixtures are rendered headlessly. Licensed Live, physical Push and physical output require a separate run.

The recorder wrote the cue note at beat 2 and extended it to 1.25 beats. The Ember level notes used velocities 33, 64, 96, 127 and 96 at beats 2, 2.25, 2.5, 2.75 and 3. A temporary Accent gesture contributes the last two notes, which capture the temporary level and its restoration.

The manual edit moves the third level note from beat 2.5 to 2.625 and lowers its velocity from 96 to 1. Replay reaches zero at the edited beat. The two later notes restore their original captured levels, and the release reaches the exact replayed cue instance. The test checks that duplicate and stale gesture inputs, an accepted no-op, playback while armed, show import, and passive snapshots add no clip notes. A second armed destination remains empty after import.

The still and replay visualization use the note dictionaries and output frames in `trace.json`. The visual is a measured headless replay capture.
