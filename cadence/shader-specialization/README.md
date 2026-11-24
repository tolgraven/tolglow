# Fog shader specialization probes

Scratch GPU experiments used the integrated `3fbe1612` room on Apple M4 with Chrome ANGLE Metal. Each comparison interleaved the unmodified shader and candidate on the same frozen dense plume or uniform haze field. The receipt records GPU elapsed time for the volume pass after draining upstream work. Pixel comparisons compare both linear fog and the final display at gain 1 and 8.

The zero-ambient candidate only applies when both ambient vectors are exactly zero. It left every captured display pixel unchanged. The median remained approximately 1.30 ms for Auto haze, 2.90 ms for Auto plume, 4.9 ms for Desktop haze, and 7.1 ms for Desktop plume. The uniform-haze candidate only applies with empty detail bounds. It also left all displayed pixels unchanged. Its median was 1.51 → 1.44 ms at Auto and 5.11 → 4.40 ms at Desktop. Neither variant changes the authored lighting data, work-light control, or sampled scene resolution.

These are isolated timings. No variant is integrated into the application. The small Auto change does not address active plume cadence.
