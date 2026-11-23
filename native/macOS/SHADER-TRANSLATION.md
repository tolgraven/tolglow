# Native Studio shader translation

[Issue #493](https://github.com/tolgraven/tolglow/issues/493) tracks qualification
of the native Mac application's existing production renderer. `StudioWindow`
embeds the actual Studio in `WKWebView`; Three.js submits its GLSL ES shaders to
WebKit. ANGLE's Metal backend translates those shaders into Metal Shading
Language. Its [upstream diagnostic documentation](https://chromium.googlesource.com/angle/angle/+/main/doc/MetalBackendShaders.md)
describes both MSL inspection and the WebKit GPU-process environment variable
`__XPC_ANGLE_METAL_PRINT_MSL_ENABLE=1`.

There is no second shader renderer or offline translated demonstration here.
The opt-in qualification observes the original linked programs, including the
named production preview worker, and writes their original GLSL, translated
source, compile/link results, context renderer strings and presented-preview
telemetry to a local JSON receipt. Normal app launches install no hooks.

For a packaged app, append `--qualify-metal /absolute/path/receipt.json` to its
ordinary `--open-show /path/to/show` launch. The destination's parent must exist.
Keep the Room preview visible for the check. For an already running loopback
Studio host, the native verification runner uses the same webview configuration
and navigation policy as the shipped Mac window:

```sh
native/Tests/qualify-metal http://127.0.0.1:PORT/ target/native-metal
node --test native/Tests/shader-qualification.test.mjs
```

The runner first checks the uninstrumented renderer in one owned AppKit window,
saves `native-control.json`, then reloads that same window with observation and
requires the rendering route to remain the same. It closes the window after
the receipt is written. It starts no JVM and changes no show configuration or
physical output.
It observes for up to 60 seconds and bounds retained diagnostics to 160 events
and 16 MiB. It imports the original preview worker module from an instrumented
bootstrap, buffering startup messages until the original module installs its
listener, without replacing shaders, changing rendering quality, forcing a
fallback or bypassing frame acknowledgements. Shader-status queries can wait
for the driver, so this is a correctness/translation qualification, **not a
performance measurement**.

`qualified` requires a ready production preview, a presented worker bitmap or
completed main-thread draw, an observed WebGL draw,
successful linked programs and recognisable translated MSL from a compiled
shader. The receipt records exactly how many linked programs and MSL shaders
were observed. Renderer names alone do not prove shader translation. The
[Khronos debug-shaders extension](https://registry.khronos.org/webgl/extensions/WEBGL_debug_shaders/)
may be hidden by the platform; absent/empty/non-MSL translated source produces
`unqualified`, not a claim that rendering failed. Runner exit codes are 0 for
qualified, 2 for unqualified and 1 for a runner/receipt error. Visual parity,
GPU benefit, sustained cadence and every possible effect remain separate checks.

On 2026-11-23 the native runner qualified the production Studio assets at
`21424806404a872686820972290289434574da37` on the M4 Mac: macOS 27.2.1
(`26B131`), WebKit `22625.2.8.11.9`, Apple GPU, eight active demo heads running
Ember and Sweep. The uninstrumented control and observed run both used the
worker at an 816×500 display canvas. The observed run presented frames without
fallback, linked 29 programs and captured 58 compiled MSL shader stages. The
receipt, control and exact served-asset/native-source hashes are retained under
`target/native-metal/final/`. The four focused observer tests passed, and all
Mac app Swift sources compiled. This exercised the shared native WebKit setup;
it did not build or sign a complete distributable app bundle. The packaged
capture workflow now retains the same qualification receipt when it runs.

The show's optional native computation backend is a separate Afterglow JOGL
GL3 transform-feedback pipeline. Its context, resident program cache, owned
buffers, captured frame inputs, deadline cancellation and CPU fallback are not
changed by this work. [glslang](https://github.com/KhronosGroup/glslang) can compile
GLSL to SPIR-V and [SPIRV-Cross](https://github.com/KhronosGroup/SPIRV-Cross) can
translate SPIR-V into MSL. A direct Metal computation backend would also need
resource binding/reflection, command submission, asynchronous result ownership,
cache/lifecycle integration and CPU/GPU output parity through Afterglow's real
compute API. Merely emitting an MSL file would not deliver that backend.
