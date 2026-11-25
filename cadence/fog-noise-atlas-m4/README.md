# 3D fog noise texture experiments on the M4 Air

The prototype filled a 128³ GPU texture with the original GLSL lattice hash, sampled it with cubic-weighted trilinear interpolation, and evaluated the original expression at lattice coordinates outside the texture. Geometry, quality settings and ray steps were fixed within each pair. Chrome 158 used ANGLE Metal with an Apple M4. Each pair compared its own baseline and candidate. The shader-fog pass times exclude the rest of the preview; the maximum peak RGB8 difference is across the fixed and moving images.

| Storage | Quality | Plumes | Median GPU ms, baseline → candidate | Max RGB8 difference |
| --- | --- | --- | --- | --- |
| R16F | Auto | Narrow | 3.03 → 2.92 | 10 |
| R16F | Desktop | Narrow | 7.43 → 7.05 | 9 |
| R16F | Auto | Three | 3.30 → 3.13 | 28 |
| R16F | Desktop | Three | 9.64 → 7.85 | 32 |
| R32F | Auto | Narrow | 3.25 → 2.76 | 10 |
| R32F | Desktop | Narrow | 8.10 → 7.06 | 10 |
| R32F | Auto | Three | 3.61 → 3.45 | 18 |
| R32F | Desktop | Three | 7.75 → 7.72 | 32 |

Repeating the baseline produced identical pixels. Peak differences persisted with 32-bit storage and could introduce isolated brightness changes in moving fog. The prototype stays out of the renderer. The gain is variable and the figures make no claim about native Studio frame rate.
