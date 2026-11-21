# Lighting simulator scene contract

This replaces the visual-quality assumptions of the first analytic room preview.
The existing screenshots show a diagnostic prototype: flat room planes, spherical
fixture markers and solid box trusses. They do not satisfy the requested game-level
lighting simulator. Functional rendering and a successful shader compile are not
visual acceptance. The high-fidelity simulator remains in progress.

## Ownership and data

One `RoomPreview` instance owns one Three.js WebGL2 renderer, scene graph, opaque
HDR/depth targets and post-processing chain. Re-frame owns UI state and controls;
the engine is specialized graphics interop. The JVM remains authoritative for
fixtures, positions, orientation/calibration and committed emitted output. No
second browser lighting engine may independently advance effects.

The existing `RoomPreview(canvas, status)`, `update(snapshot)`, `setFog(value)`,
`burst()`, `resetCamera()` and `dispose()` contract remains callable through the
engine loader. Additional explicit camera/quality/preset methods may be exposed
through Reagent controls. Shader hot reload must compile a candidate first and
retain the last working program on rejection. Rendering errors remain visible.

`room_renderer_fog_fixture_optics` owns the complete preview: scene construction,
fixture meshes, light/shadow management, fog field/pass, compositing and visual
verification. Frontend controls remain with the Studio UI owner. There is one
graphics owner and no overlapping fog or renderer implementation lane.

Coordinates are metres in the existing right-handed rig space: Y is up, X spans
the room width and Z its depth. No decorative offset may move the simulated beam
away from the actual fixture origin. Prefer the resolved world-space direction
and aim. The CPU oracle's elevation convention makes tilt zero horizontal and
-90 degrees downward; pan/tilt fallbacks use that convention and declared
calibration/fixture rotation. Test numerical aim
against the renderer's CPU oracle. RGBW's resolved emitted RGB controls light
colour. Exposure/tone mapping are display transforms, not emitted DMX values.
Missing photometry uses a documented visual estimate, never a claim of calibrated
lux, IES behaviour or exact matching to a real fixture.

## Scene and illumination

The default environment must read as an actual small venue: plausible proportions,
concrete or timber surfaces with normal/roughness detail, structural metal, stage
and restrained architectural context. Use actual lattice truss geometry and
articulated fixture base/yoke/head/lens meshes. Fixture and truss placement follows
saved rig data; venue decoration must not imply additional controlled fixtures.
Library asset sources and licences belong beside imported assets.

Use physically based materials, linear HDR light accumulation, shadowed surface
lighting, controlled bloom and filmic display mapping. Bloom reflects bright
emitters; it must not wash the whole image into a pastel blur. Ambient/work light
is explicitly a preview setting, with a dark-show setting. Blackout cannot leave
coloured decorative beams. The room should remain spatially legible through
exposure and material response, not arbitrary glowing outlines.

Fog is a spatial density field emitted at a named machine position, retaining
history independently of HTTP snapshot arrival. A fresh burst begins dense and
local, is advected with visible structure, expands/diffuses and contributes to
room haze. Step the field with a bounded local clock and stable transport through
pause, hidden views and long frame gaps; never simulate an enormous catch-up step.
Volumetric integration uses opaque scene depth, extinction, directional phase
scattering and light visibility. Geometry occludes beams. At zero density there
is no illuminated volume in free space, while lit surfaces still receive light.
No solid transparent cone is an acceptable substitute for the density field.

## Interaction and quality tiers

First frame: an understandable view from within or just outside a cutaway venue,
not a tiny floating dark box. Named views include audience, rig overview and
fixture focus. The camera can orbit/pan/pinch, has keyboard equivalents and reset,
and does not unexpectedly jump when a parameter changes. Keep controls and exact
fixture labels in Reagent/DOM, outside the GPU image. Existing 2D rig placement
remains available; later GPU picking must identify the same saved fixture IDs.

Allow an expanded/fullscreen simulator as well as a compact performance view.
Touch controls need reachable targets without consuming the entire preview. The
same engine serves desktop, iPad and phone with bounded quality tiers. Scale fog
target resolution, ray steps, shadow-map sizes and shadow-casting light count;
never silently change authoritative fixture output. Display any fixture/quality
limit. Instancing/batching is preferred for repeated meshes. Rebuild static scene
geometry on structural rig changes, not each frame or poll.

Only one preview scene is active. Lazy-load its code/assets, cap pixel ratio and
stop GPU/fog updates when hidden or offscreen. Dispose geometry, materials,
textures, observers, controls and event handlers on teardown; recover WebGL
context loss. Reduced-motion mode retains control and direct camera views while
limiting idle animated atmosphere. A failed renderer shows a clear unavailable
state plus the existing 2D rig view, not an overlapping pretend preview.

## Acceptance evidence

Expose an explicit ready signal only after scene assets, shader compilation and
the first real frame succeed. Capture the actual canvas, never a generated mock.
Review desktop expanded, normal performance layout, iPad landscape/portrait and
phone. Keep baseline and revised screenshots together with deterministic rig,
effect snapshot, camera, exposure, fog seed/time and quality settings.

Required scenes: blackout; no-fog surface lighting; one narrow spotlight; several
crossing saturated/desaturated beams; a moving head and shadow-casting truss;
fresh dense fog plume; the same plume dispersed later. Check occlusion, aim,
colour, beam shape, material detail, readable depth and absence of obvious ray
banding. User-visible quality must improve in actual frames before calling the
replacement an improvement; the original AAA ambition stays open until the
assets, simulation and reviewed result support that description.

Measure CPU updates, GPU/frame timings where timer queries are available, target
sizes, shadow/fog budget and input responsiveness. Software Chromium rendering
proves shader execution and images, not iPad frame rate or thermal behaviour.
Physical mobile and desktop GPU performance are separate gates. Preserve the
render-independent show-control deadline even if the preview reduces quality.
