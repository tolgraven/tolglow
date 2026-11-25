# Room shader startup qualification for #550

Room already ships the GLSL templates in its browser and worker bundles. A new WebGL context still generates the exact variant and links a program for it. This probe exercises a saved source plan across independent browser processes and measures those costs separately. It also tests restoring a different show and epoch through the Room instance that is already open.

## What Room does today

1. Studio lazily imports `/studio/preview.js`, then constructs `RoomPreview` and supplies the current projection. On the worker path, the module worker signals protocol readiness before it creates a renderer. The active state message creates a WebGL2 context and initializes materials, render targets, and the procedural PMREM environment. [Studio mount](https://github.com/tolgraven/tolglow/blob/56a785c527034a5914f0f1833c10741c2f4c8282/src/studio/tolglow/studio/interop.cljs#L178-L214) · [Worker startup](https://github.com/tolgraven/tolglow/blob/56a785c527034a5914f0f1833c10741c2f4c8282/js/preview/worker.js#L63-L128) · [Scene initialization](https://github.com/tolgraven/tolglow/blob/56a785c527034a5914f0f1833c10741c2f4c8282/js/preview/scene-renderer.js#L83-L129)
2. Three r187 uses shader chunks bundled by esbuild plus Room’s fog, glass, film and optics sources. For example, Room’s optics hook changes the spotlight shader and binds the aperture uniforms. Three generates prefixes, expands chunks, substitutes light and clipping counts, unrolls loops, and supplies the GLSL 300 ES interface before passing the strings to WebGL. The linked-program map belongs to one renderer and is keyed with context-local stage IDs for custom shaders. [esbuild entries](https://github.com/tolgraven/tolglow/blob/56a785c527034a5914f0f1833c10741c2f4c8282/js/preview/build.mjs#L1-L11) · [Room optics](https://github.com/tolgraven/tolglow/blob/56a785c527034a5914f0f1833c10741c2f4c8282/js/preview/optics.js#L14-L31) · [Three source assembly](https://github.com/mrdoob/three.js/blob/r187/src/renderers/webgl/WebGLProgram.js#L752-L820) · [Program lifetime](https://github.com/mrdoob/three.js/blob/r187/src/renderers/webgl/WebGLPrograms.js#L642-L677)
3. Room draws the opaque scene and shadows, executes fog and postprocessing, waits for the worker GPU fence, transfers an image bitmap, and accepts the frame only when the show, runtime and epoch match. The worker message named `ready` opens the mailbox. The proxy’s `ready` marks the accepted bitmap. [Room passes](https://github.com/tolgraven/tolglow/blob/56a785c527034a5914f0f1833c10741c2f4c8282/js/preview/scene-renderer.js#L445-L474) · [GPU fence and transfer](https://github.com/tolgraven/tolglow/blob/56a785c527034a5914f0f1833c10741c2f4c8282/js/preview/worker.js#L14-L55) · [Proxy readiness](https://github.com/tolgraven/tolglow/blob/56a785c527034a5914f0f1833c10741c2f4c8282/js/preview/worker-proxy.js#L97-L145)

## Measurement

Four paired runs per mode all presented the same RGB pixels. Each restored plan reused all 28 assembled program variants. Six were requested during initialization and 22 during the first Room draw. The serialized plan occupied 1,546,087 bytes, or 223,178 bytes gzip.

| Stage | Ordinary first load | Saved GLSL |
| --- | ---: | ---: |
| Assemble GLSL variants | 80.7 ms | 0 ms |
| Variant lookup | 0 ms | 13.4 ms |
| Fetch and parse gzip plan | 0 ms | 109.6 ms |
| Submit shader source, compile and link calls | 40.8 ms | 45.5 ms |
| First use, including deferred driver waits and reflection | 3,688.2 ms | 2,862.9 ms |
| Worker GPU fence on first Room draw | 8,095.0 ms | 7,721.8 ms |
| First accepted frame on the DOM canvas | 12,704.0 ms | 12,321.2 ms |

Values are medians. The baseline assembly range was 47.3–93.0 ms. First accepted frames ranged from 11.18–17.90 s on the ordinary path and 7.98–20.74 s with saved GLSL. Individual pairs differed by several seconds in both directions. This experiment establishes the amount of source assembly avoided and the remaining linked-program work. It did not establish a first-frame speedup.

The unmodified release worker produced the same canvas as the instrumented paths. A stale build reused none of the 28 variants and reproduced the release pixels. Changing the rig from eight fixtures to three reused 22 variants, assembled six and reproduced the changed rig’s ordinary-path pixels.

Reusing an open Room immediately after its first draw presented the new show and epoch in 5.17 s. The second draw requested two additional depth-material variants, with spotlight count changing from zero to eight. This is a first-to-second draw control. After two ordinary same-show warm-up draws in a separate run, the retained Room presented the restored show in **3.64 s with zero new shader programs**. All 24 live programs remained available. The first restored draw took 16.0 ms to submit and 3.59 s to complete its software-GPU fence. [Retained-context trace](retained.json)

The exercised show is a deterministic browser projection with eight illuminated moving heads, a truss, software auto-low quality, one shadow, fog density 0.35, default bloom and a 640 × 360 display canvas with a 480 × 270 internal scene raster at DPR 1. Reduced motion fixes the animation phase. The Room worker and the unchanged release proxy render real pixels and the main thread presents the bitmap. A standalone test page supplies the projection. This times Room startup and excludes JVM startup, storage of show snapshots, Studio layout and network synchronization. Headless Chromium 156 on Linux used ANGLE SwiftShader with the GPU shader disk cache disabled and a fresh browser process for every first frame. Those timings represent a controlled software-GPU test.

Assembly measures Three’s prefix generation, chunk expansion and GLSL conversion. The existing material hook runs in both paths and is measured separately. The WebGL submission bucket covers shader uploads, compile, attach, attribute binding and link calls. The first-use bucket includes deferred driver waits, status/log checks and uniform/attribute reflection. These are browser wall-clock observations. WebGL may defer translation, compilation and pipeline work and does not expose a separate driver compile duration. The GPU fence covers submitted rendering work, and the accepted-image clock runs from test-page module entry through actual canvas presentation. The constructor and draw totals already include their shader timings.

## Saved plan and follow-up

- The prototype indexes fully assembled vertex/fragment GLSL by a release fingerprint, ordered defines, runtime variant fields and exact pre-assembly shader strings after the material hook. Process-local stage IDs and uniform values do not select the source. Each new WebGL context still calls compile/link and reflection. The content fingerprint covers the pinned release and Three source. The restore uses a same-origin gzip response from a plan generated by the harness. [Plan identity](plan.mjs) · [Capture and load](probe.mjs)
- Portable show data can carry an optional, versioned plan reference with a build ID and variant manifest. Store any executable source reuse payload in a bounded cache populated by the trusted runtime or signed release assets. Include shader-generating material and patch revisions, compile-time definitions, light/shadow layouts, shader language, precision, output-format and color-space choices and effective capability inputs. Hash the entry contents for corruption detection. Match the current variant, keep fresh uniforms and buffers, atomically replace a complete cache, and fall back to current generated source on missing, invalid or rejected data. An old show must restore with no plan. The existing show epoch remains the frame-presentation gate.
- Prioritize keeping a live Room context and its linked-program map across compatible restores. For fresh contexts, first reproduce the timings on target hardware and test whether loading an exact variant earlier enables useful work before its first draw. `KHR_parallel_shader_compile` can surface completion without forcing a blocking query. A prewarm candidate needs the actual scene, shadow, fog, glass and postprocessing material/target variants. Judge readiness by the first accepted current-epoch frame and measure first cue/input responsiveness. A fresh context, context loss, hidden Room, or unknown variant still follows ordinary startup. [Parallel shader compile extension](https://registry.khronos.org/webgl/extensions/KHR_parallel_shader_compile/)
- The native macOS Studio hosts a WKWebView. WebGL has no public program binary serialization API. A native Metal renderer could use `MTLBinaryArchive` with a device/GPU and OS/driver compatibility key plus metallib/compiler and pipeline-descriptor digests. Archive misses fall back to normal native pipeline creation. That path would need its own hardware and native-renderer qualification. [Native macOS host](https://github.com/tolgraven/tolglow/blob/56a785c527034a5914f0f1833c10741c2f4c8282/native/macOS/TolglowStudioApp.swift#L92-L107) · [WebGL2 context API](https://developer.mozilla.org/en-US/docs/Web/API/WebGL2RenderingContext) · [Metal binary archives](https://developer.apple.com/documentation/metal/mtlbinaryarchive)

## Reproduce

Use the existing local Tolglow checkout, cached npm dependencies, installed Chromium and Python Playwright/Pillow. The build exports the pinned source into scratch space and instruments the worker copy there. The runner starts an ephemeral loopback server and owns all browser instances.

```sh
node --test plan.test.mjs
scratch=$(mktemp -d)
node build.mjs /workspace/tolglow /workspace/worktrees/tolglow-fixture-fin-batch/node_modules "$scratch"
python measure.py "$scratch/www" . --pairs 4
python measure.py "$scratch/www" . --retained-only
```

[Raw measurements and build digests](raw.json)
