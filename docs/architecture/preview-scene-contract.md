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

Studio selects up to 128 emitting targets from the whole output patch. A sparse
frame also supplies a small idle sample. A rotating solo can reach any patched ID
across universes. The browser reserves geometry for the selected rig fixture and
assigns up to 16 persistent spotlights to visible emitters. Its shadow slots keep
their textures across cue changes. Camera tours and room fog use the allocated
lights. Both Transit state pushes and Ring inspection use the same projection.
The device-tier budgets and selection order are in `js/preview/README.md`.

Where OffscreenCanvas workers are available, the one scene implementation runs
in a worker. The main thread owns OrbitControls and visibility. Only one state
message may await acknowledgement. Intermediate snapshots coalesce to the latest
projection. The hardware worker can retain two frames total, with a fence per
submitted draw. It publishes in order after GPU completion. After the first
worker frame the proxy hands the visible canvas to the worker where the browser
allows it. The worker then presents each completed frame itself and reports it,
so page-thread work cannot delay the room. It refuses frames from a show the
page has left and clears the canvas when told the identity changed. Browsers
without a transferable canvas keep the acknowledged bitmap path, with one
transferred bitmap awaiting acknowledgement. Both threads sample the display
rate. In Blink, where worker animation callbacks follow the display, the worker
uses the higher value. Elsewhere it uses the page's. Page-side readers of a
handed-over canvas run two animation frames after its report, when the page's
copy holds that image. When the show identity, scene and controls stay
the same, new light and haze samples feed the next draw while the previous
frame's fence is pending. Scene or control changes drain submitted work before
applying the new state.
Pacing includes this GPU wait, since transfer/command submission alone can still
build a compositor backlog and starve the UI. Bitmaps from an old show/epoch are
closed without display. The DOM canvas stays with the page until a worker frame
exists, allowing startup failure to use the same scene implementation on the
main thread before presentation is committed. An error after rendering starts retains the
last image and reports failure; an active worker stall never switches rendering
onto the main thread. Worker disposal terminates its complete GPU lifetime.
Capability fallback is not evidence of Safari support or smooth physical-device
performance. Cold shader compilation must not poison later rendering cadence.

The existing `RoomPreview(canvas, status)`, `update(snapshot)`, `setFog(value)`,
`burst()`, `resetCamera()` and `dispose()` contract remains callable through the
engine loader. Additional explicit camera/quality/preset methods may be exposed
through Reagent controls. Shader hot reload must compile a candidate first and
retain the last working program on rejection. Rendering errors remain visible.
An engine uses the globals of the window whose loader imported it (document,
focus, animation frames, pixel ratio, observers). When the Room is popped out,
Studio loads the module in that window and constructs the engine on a canvas
created there; a canvas is never moved between documents (see
`docs/studio-ui.md`, Room in its own window).

`room_renderer_fog_fixture_optics` owns the complete preview: scene construction,
fixture meshes, light/shadow management, fog field/pass, compositing and visual
verification. Frontend controls remain with the Studio UI owner. There is one
graphics owner and no overlapping fog or renderer implementation lane.

Coordinates are metres in the existing right-handed rig space: Y is up, X spans
the room width and Z its depth. No decorative offset may move the simulated beam
away from the actual fixture origin. Project each visible moving head with
`afterglow.render.frame/fixture-pose`. The calibrated mechanical angles and
world direction in `:pose` reflect the committed pan and tilt, including end
stops and wraps. Requested aim stays available to the editor. While smoothing
frames, interpolate the mechanical angles only across continuous travel.
With incomplete servo data, the preview can use a supplied world direction.
The CPU oracle's elevation convention places zero along +Z and −90° down.
RGBW's resolved emitted RGB controls light
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

### Live spatial volumes

Launching a spatial cue draws its active volume in the Rig and Perform previews.
Saved bouncing bodies also follow the positions reported by the spatial runtime.
The visible canvas leases the `[:spatial/scene]` stream. Scrolling it out of view,
switching tabs or leaving the window releases that lease and the cached field data.
The show continues to run the cue while both previews are closed.

The glass pass reads the fogged, linear HDR room and opaque scene depth before
bloom and tone mapping. Transmitted light bends with the surface normal. A depth
test preserves fixtures and trusses crossing in front of the volume. The surface
reflects the room and the four brightest allocated spotlights. The preview work
light affects those room reflections. A small ripple follows the local rendering
clock, and a bounced body uses the host's contact deformation and position.

Each canvas shows up to twelve bounded live surfaces. Primitives use analytic mesh
dimensions. Constructive solids sample the shared Afterglow signed-distance field
on a 30³ grid, once per distinct local shape. Moving, stretching the whole shape,
or changing its paint reuses that mesh. Very thin details may fall between samples.
Refraction samples the room already visible on screen. Off-screen objects and
recursive transmission through another glass volume need additional rendering.
The material is a visualization of a cue's region of influence and uses no DMX
channels. Physical iPad and MacBook GPU timings remain to be measured.

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

## Display refresh and committed lighting

Camera movement and the atmospheric pass use local display time. The mobile,
desktop and high quality tiers request display callbacks up to 120 Hz.
The low tier retains a 10 Hz budget for software rendering. Actual cadence
depends on the display, GPU completion time, graphics settings and browser.
Auto quality reduces target dimensions after repeated over-budget frames and
recovers gradually with sustained headroom. The worker uses completed GPU work
for this decision. The direct renderer samples GPU elapsed-time queries or asynchronous completion fences. Manual quality keeps
its configured resolution. The fog field keeps its bounded simulation timestep
across display rates.
Workers use display callbacks when supported and an elapsed-time deadline on
browsers that only provide worker timers. Hardware may start the next draw
while the prior frame is completing or awaiting canvas acknowledgement.
Software rendering and explicit paused draws keep one frame in flight.

The default **Smooth · one frame delay** blends the two latest consecutive
committed frames over their timestamp difference, normally 25 ms. Only linear emitted RGB,
dimmer, intensity and resolved pan/tilt travel can blend. Axis interpolation
uses fixture calibration and the backend's chosen physical travel. Missing axes
and half-turn jumps follow the latest resolved direction. Fixture functions,
shutters, gobos, focus and zoom take their new value on the original frame.
A shutter transition, tagged fixture cut, or active strobe snaps the fixture to its current result. The preview holds the latest
sample when updates stall. A reconnect with a new runtime or epoch, rig edit,
command revision, blackout transition, observed lifecycle cut or skipped frame
starts a fresh pair. Authored steps on continuous parameters can still look
smoother in this mode. **Follow show frames** presents each committed fixture
output on arrival so you can inspect its exact timing.

The canvas consumes the acknowledged server projection. The worker receives the
rig once per change and receives new output independently from camera input.
Display callbacks perform graphics work locally. Tests exercise frame sampling,
120/60 Hz clocks and worker/fallback rendering. Physical 120 Hz MacBook Pro and
iPad Pro performance requires measurement on those devices.

### Presentation resolution and highlight range

The scene, scattering integration and output canvas each have a pixel budget. The
canvas follows the panel’s physical pixels, up to UHD. Geometry is multisampled
where the GPU supports the same sample count for RGBA16F color and 24-bit depth.
At most 16 million colour/depth samples are allocated. Low quality leaves that
memory for lighting. After fog and glass, bloom runs on scene-linear HDR. The final
pass resolves remaining edges with FXAA, then dithers and adds adjustable grain at
presentation resolution. The editor draws on top. **Image** in the Perform preview
adjusts exposure, bloom strength and grain independently.

On a display advertising high dynamic range, Studio checks for floating-point
canvas storage and extended compositor tone mapping. A direct WebGL canvas preserves
highlights up to four times diffuse white. Ordinary displays and bitmap-transfer
surfaces use ACES and sRGB. `getTelemetry().displayRange` reports the negotiated
result. Software screenshots exercise SDR. Physical highlight brightness and sustained
refresh on an HDR Mac or iPad require a device measurement.

Auto starts with the detected device class. Warm renders of the actual rig form
a continuous workload benchmark. Four consecutive over-budget samples reduce the
scene raster. Sixty comfortably under budget allow recovery, then a higher tier.
A one-second settling period follows each change. A low-cost preview may reach
the full 4K scene budget. Fog, glass, lighting and the display clock remain local
to the currently visible preview.

### Suspension and resume

The canvas starts its graphics context after its first positive intersection
with the viewport in a visible, focused window. Scrolling it completely out of
view, hiding its tab, blurring the window or leaving the page stops the camera
and display callbacks. Resizing or entering fullscreen still respects all four
conditions. Show output and other Studio controls continue running.

A suspended preview retains the latest show projection. The worker drains
submitted GPU work and discards bitmaps from the expired visibility generation.
No further preview states, camera messages or frames are sent
while suspended. Resume reapplies the current rig, latest fixture values and
viewport, and resets the elapsed time for fog, camera and interpolation. Unmount
disconnects viewport observation, event handlers, renderer and outstanding
callbacks. Remount creates a fresh owner.

The original tolglow-web preview paused its rendering loop at zero intersection
and on component unmount. [Reference implementation](https://github.com/tolgraven/tolglow-web/blob/2b58843/src/cljs/tolglow_web/three.cljs#L560-L640).
The combined activity signals have deterministic tests. The Chromium camera
suite scrolls the real canvas, enters and exits fullscreen, checks that suspended
input produces zero frames, and mounts a second hidden preview for both render
paths. Its mixed-gate check dispatches window blur/focus events. The unit suite
simulates tab visibility, window focus and page lifecycle changes. Physical
macOS and iPadOS window switching and display refresh need device verification.

## Comparing the preview with a private camera reference

Use the operator's privately supplied iPhone Cinematic footage as a continuing visual reference for changes to the room renderer. Review the original locally. Keep the footage, extracted stills, identifying metadata and recreations of private artwork out of repositories, CI artifacts and review posts. Public evidence can use a synthetic venue with the same lighting conditions. The supplied clip has been decoded and inspected privately. The matched renderer comparison and hardware measurement remain open.

1. Select a short passage and record its start and end presentation timestamps in the private notes. Preserve the source timebase and variable frame timing where available. If timing or optical metadata is absent, mark it unknown.
2. Compare scene properties first. Where the reference makes a property visible, inspect the emitting aperture, footprint and edge softness, lit surfaces, extinction through the room and how those change in motion. Compare the neutral render before adding a camera treatment.
3. Treat the phone image as a camera rendering. Record known lens or zoom selection, focus changes, exposure and tone mapping separately. Cinematic focus blur or exposure adaptation can change apparent beam width and brightness. Keep uncertain observations explicit before using them to tune a fixture or the fog model.
4. Compare a matched still **and** moving playback over the same interval and framing. Look for temporal aliasing, unstable edges, steps in motion and exposure drift that a still cannot reveal. For the rendered side record revision, rig and cue, hardware, browser, display refresh, output dimensions, internal raster dimensions and quality settings. Report actual presentations per second and frame-interval spread over the passage. Record the physical fixture output cadence separately.

For the next renderer review, retain the private time range and observations locally. Publish only non-identifying observations and evidence from the synthetic rig, including the rendered still, recording and measured performance. Update the result when a change improves one property at the expense of another. The existing Air measurements provide a reproducible performance comparison. [Physical MacBook Air qualification](../performance/macbook-air.md)
