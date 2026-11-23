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

## Three.js renderer source index

For the combined assembly, install the dependencies in the root
`package-lock.json` and add `--with-references`. The exporter reads the existing
`three@0.187.0` package. You can also point `--three-root` at an installation of
that exact release. Each of 127 upstream ES modules is matched against its official
Git blob ID at the pinned r187 commit before the exporter unwraps its GLSL strings.

```sh
python3 dev/model-lab/shaders/export.py target/model-lab/shaders --with-references
node dev/model-lab/shaders/check.cjs target/model-lab/shaders /usr/bin/chromium
```

The combined export produces **162 training rows**, **three validation rows** and
**three test rows**. Training includes the 26 numeric training examples above
and **136 retrieved source units**. The independent `shader-reference.jsonl`
contains just those 136 units, with `mode: shader-reference`. This is an index
of 101 distinct GLSL chunks and 35 distinct material vertex or fragment stages.
Each body appears once. Stages sharing identical GLSL were deduplicated. Three.js
source and the two color-transfer anchor tasks share the same training family.
The separate blur and palette holdouts retain their original membership.

Each source record lists the include names, preprocessor feature symbols, shader stage,
lighting topic and consuming material programs. The exporter follows the transitive
include graph through the pinned material library. For instance, the shadow shader
records identify the relevant material stages and renderer macros, and the environment
and fog records can be retrieved alongside their consuming shaders. Source links,
digests, upstream MIT terms and per-file comments accompany the records. The exporter omits standalone retrieval of chunks already covered by the anchors and
selected snippets pointing to separately authored implementations.

The associated chat records use a small set of source-retrieval templates. One theme
asks for a material pass, one for a guarded include and another for an include at a
particular consumption site. This measures retrieval coverage. The numeric WebGL2
checks apply to the 32 standalone anchor tasks. Renderer snippets rely on Three.js
for bound attributes, uniforms, feature flags, varying declarations and include
expansion. No standalone compile or numerical claim is made for those 136 snippets.

[Verified Three.js Git blobs](../../dev/model-lab/shaders/three-r187.json) · [Renderer source extraction](../../dev/model-lab/shaders/reference.py)
