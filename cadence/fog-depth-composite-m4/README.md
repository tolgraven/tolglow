# Uniform-depth fast path in the M4 fog composite

The prototype uses a hardware bilinear volume lookup only when the four neighboring full-depth samples exactly match the center. Other pixels still use the original depth weighting and thin-surface integration. Each pair ran in Chrome 158 on ANGLE Metal, with one shader substitution in the production fixture. GPU timer queries wrapped the composite pass. The original shader repeated pixel for pixel.

| Quality | Scene | Median composite GPU ms, baseline → candidate | Maximum RGB8 difference |
| --- | --- | --- | --- |
| Auto | Room haze | 1.43 → 1.43 | 3 |
| Auto | Fresh plume | 3.02 → 3.04 | 3 |
| Desktop | Room haze | 5.34 → 5.38 | 17 |
| Desktop | Fresh plume | 7.37 → 7.61 | 17 |

The fast path produced no useful timing gain and introduced differences at a few Desktop pixels. It remains out of the renderer.
