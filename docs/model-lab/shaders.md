# GLSL examples for a local model

The shader exporter pairs complete WebGL2 fragment programs with a named lighting task,
typed inputs and an example fixture. It covers modulation in world coordinates,
beat-driven tweening, the Studio display color transform, blur along a lighting buffer
and a palette driven by beat or fog absorptance. Native light values enter as uniforms.
The programs can run on an isolated Studio render target. Installing an effect in the
show still uses its registry and supported expression or shader APIs.

| Split | Upstream family | Examples |
| --- | --- | ---: |
| Training | Ashima Arts and Stefan Gustavson’s classic, simplex and cellular spatial noise | 8 |
| Training | glsl-easings curves for fixture keyframes | 16 |
| Training | Three.js r187 linear-light and sRGB transfer | 2 |
| Validation | Jam3 separable Gaussian lighting-buffer filters | 3 |
| Test | Erkaman cosine palette, driven by beat phase, rig height or venue transmission | 3 |

An upstream and its whole family stay in one split. Palette examples share a function
and test three different signal inputs. Those three examples do not provide three
independent shader-family measurements. The holdouts measure transfer across
filter/palette families, with no overlap of upstream shader source between splits.

## Reproduce the corpus

From the Tolglow project root, run the exporter with Python 3. It verifies the checked-in
source and license SHA-256 digests and writes UTF-8 JSONL without network access.
Each row includes a stable ID, family, mode, source URL at an exact commit, notice,
attribution, typed fixture values and system/user/assistant messages. Original notices
and attribution travel inside each shader so distributing just its answer retains them.

```sh
python3 dev/model-lab/shaders/export.py target/model-lab/shaders
node dev/model-lab/shaders/check.cjs target/model-lab/shaders /usr/bin/chromium
```

The GPU check uses the project’s installed Playwright Core and an installed Chromium.
All 32 programs compiled, linked and rendered into a floating-point WebGL2 target in
Chromium SwiftShader. The checks exercise spatial variation and dry gains, keyframe
endpoints, independent transfer and palette calculations, and impulse-response weights
in both blur directions. These are shader behavior checks. The size of this collection
supports a small focused evaluation.

The manifest records the exact SHA for every selected upstream file and license.
Downloaded source bytes stay untouched under `upstream/`. The exporter removes the
glslify export directive, the Three.js JavaScript template wrapper and GLSL 1.20
version directives where necessary. It maps `texture2D` to the WebGL2 `texture`
built-in. The generated wrapper supplies a fragment output and concrete lighting inputs.
Curves with undefined negative-base powers or incorrect endpoints were omitted.

[Source manifest](../../dev/model-lab/shaders/sources.json) · [Reproducible exporter](../../dev/model-lab/shaders/export.py) · [GPU checks](../../dev/model-lab/shaders/check.cjs)
