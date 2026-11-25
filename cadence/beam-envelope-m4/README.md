# Exact fog beam-interval envelope on the M4 Air

This scratch shader skips the inner light loop for a ray segment if it lies entirely outside the union's enclosing interval. Inside the interval, the original sampling and accumulation remain identical. Tests used Chrome 158 and ANGLE Metal with the physical Apple M4. The test compared post-tonemap RGB8, volume RGBA16F radiance and opacity at normal exposure and 8× output gain. All four pairs matched the unchanged shader pixel for pixel.

| Quality | Scene | Median GPU fog-pass ms, baseline → candidate |
| --- | --- | --- |
| Auto | Room haze | 1.31 → 1.34 |
| Auto | Fresh plume | 2.92 → 2.91 |
| Desktop | Room haze | 4.84 → 4.71 |
| Desktop | Fresh plume | 7.20 → 7.79 |

The added per-ray bookkeeping brought no useful Auto gain and slowed the Desktop fresh-plume case. No renderer changes were made.
