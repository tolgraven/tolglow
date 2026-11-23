# Preview atmosphere

`fog.js` is the local, display-only atmosphere pass used by the single Three.js
preview owner. It never advances a show effect, sends a control command or writes
DMX. The scene owner renders opaque linear HDR colour and perspective depth first,
then calls the fog composite before bloom and display tone mapping.

## Contract

`new VolumetricFog(renderer, {quality})` accepts `phone`, `mobile`, `desktop` or
`high`. The owner calls `setRoom([width, height, depth])`, `setSize(width, height)`,
`setAmount(0..1)`, `update(seconds, lights)` and
`render(sceneTarget, camera, outputTarget)`. All positions and distances are metres
in the rig's Y-up coordinate system. `burst(options)` starts a bounded local fog
machine pulse; `dispose()` releases owned targets, texture and materials.

Each light supplies world `position`, normalized `direction`, linear emitted
`color`, half-angle `angle`, `penumbra`, visual `intensity`, maximum `distance` and
optional Three `SpotLight.shadow`. Colour already includes the authoritative
dimmer and master. The owner renders opaque shadows before calling `render`;
the module reads its comparison depth texture and matrix using a GLSL
`sampler2DShadow`. Three r187 moved PCF storage out of the former packed RGBA
attachment. The forward-depth comparison contract is validated explicitly;
the clear-path regression requires shadowed and unshadowed radiance to match
exactly before adding an occluder. The penumbra uses
Three's inner angle `angle * (1 - penumbra)`.

Finite apertures add `virtualPosition` and `apexDistance` while preserving
`position` as the optical lens origin. The virtual apex is behind the lens by
`apertureRadius / tan(angle)`; ray/cone intersections are clipped to the optical
lens plane. Attenuation and shadow maps share the scene owner's virtual source.
This truncated-cone approximation gives a finite lens footprint without moving
the authoritative fixture. It is not an area-light integral or measured optics.

`setAmbient({color, groundColor, intensity})` supplies a deliberately small
hemisphere estimate of work-light and reflected-light illumination. It defaults
to zero and must be zero for a completely unlit, dark-show scene. This is an
explicit approximation, not global illumination or calibrated photometry. It
scatters only where density exists; it cannot create a beam in empty air.

`stats()` reports the target size, light/shadow/ray budgets, field mass/density,
simulation steps and dropped elapsed time, plus the CPU update duration. GPU
timing belongs to the scene owner. CPU duration does not include GPU rendering.

## Density and integration

`fog-field.js` keeps a bounded 3D density and temperature grid. A source injects
a warm, shaped jet. Semi-Lagrangian transport carries it through a simple curl
velocity field, buoyancy lifts warm material, stable diffusion broadens it, and
cooling/ventilation dissipate it. Fixed 50 ms steps and at most two steps per
update prevent a hidden tab or long frame gap from causing unbounded catch-up.
This is a visual fluid approximation, not a pressure-projected CFD solver.

The half-float density texture is sampled in a depth-limited ray march. Opaque
surfaces terminate the ray. Exponential extinction attenuates the background;
single scattering uses a normalized isotropic bulk phase (1 / 4π), cone profile,
distance falloff and four-tap light visibility. Coherent detail modulates
existing density rather than manufacturing fog. Heat-local curl and lower
explicit diffusivity retain fresh jet folds. A bounded three-sample canopy
estimate shades dense plume interiors under ambient illumination; its bright
multiple-scatter floor is an approximation, not a full radiative-transfer solver.
Both the hemisphere and directional preview worklamp contribute to the ambient
estimate, so water fog does not become artificially blue/dark beneath warm light.

Each view ray also intersects each light's positive cone analytically. Lighting
integrates the cone's actual overlap with each march segment. This prevents a
narrow shaft near its lens from disappearing whenever it falls between jittered
samples. Camera-path extinction still uses the bounded global march. Local
constant-density quadrature, limited shadow resolution and single scattering are
approximations; multiple scattering and resolved incoming-light optical depth
remain future work. The low-resolution result is upsampled with opaque-depth
weights to avoid dragging background fog across foreground edges. Where no neighbor represents a thin surface at its opaque depth, the compositor integrates the medium in front of that pixel using the same density, incident light and shadows.

Amount zero copies the source HDR surface exactly. A zero-density field has
zero in-scattering even with lights or ambient illumination. With no light or
ambient, fog attenuates visible surfaces without generating coloured radiance.

## Budgets and evidence

| Tier | Density grid | Output scale | Ray steps | Shadowed lights |
| --- | --- | --- | --- | --- |
| phone | 20 × 12 × 20 | .33 | 24 | 1 |
| mobile | 24 × 14 × 24 | .40 | 32 | 2 |
| desktop | 32 × 18 × 32 | .50 | 48 | 4 |
| high | 48 × 24 × 48 | .67 | 72 | 4 |

At most 16 supplied lights and four active machine sources are retained. The
scene owner may expose fewer tiers and must display its selected limits. These
are bounded work settings, not measured phone/iPad frame-rate guarantees.

Run `node --test test/js/preview/fog.test.mjs` for deterministic field,
cone-membership, diffusion, cooling, extinction and long-gap tests. The test logs
informational Node CPU field timings without unstable timing assertions.

Run `node --test test/js/preview/fog-gpu.test.mjs` for a real isolated WebGL2
context. `CHROMIUM_PATH` defaults to `/usr/bin/chromium`; a missing browser skips
this explicit integration gate rather than claiming GPU validation. The fixture
bundles the production module, renders real half-float/depth/shadow targets and
reads their pixels. It checks exact empty/disabled preservation, opaque-wall
occlusion, extinction, shadow attenuation, blackout and narrow-shaft coverage.
The WebGL2 checks use Chromium's SwiftShader renderer. The operator guide shows
the complete Studio scene with the same lighting during and after a fog cue.
Physical iPad performance and thermal stability require separate measurement.
[Compare the clear room, nozzle plume and mixed haze](../../docs/studio-atmosphere.md#reading-the-room-preview).

## Fog from the show

When a snapshot supplies `:output :atmosphere`, Studio follows the fog output
that Afterglow accepted for the physical rig. Each source supplies its nozzle
position and direction, present flow in m³/s, cumulative emitted equivalent
m³ and the equivalent m³ still concentrated near that nozzle. The host also
supplies the volume that has mixed throughout the room and its concentration.
The slider adjusts preview visibility. The flow readout displays m³/min.

A rise in cumulative output deposits fresh material. The renderer carries up
to eight local parcels per active fogger. They travel along the nozzle axis,
rise, spread and mix. It normalizes each deposit over the receiving voxels so
that integrating density over cell volume reproduces the local equivalent m³.
The homogeneous background uses mixed equivalent m³ divided by room volume.
These are manufacturer-rated output equivalents. Real optical extinction also
depends on fluid, particle size, ventilation and surfaces.

A stationary show sample emits once, independent of display FPS. When the
window resumes or an epoch changes, the renderer reconstructs the local
plumes from the current quantities. A fully dispersed show can resume with
uniform haze and quiet nozzles. A configured, empty atmosphere renders clear
air. The manual preview plume remains available for rigs without show fog data.

The direct surface beam retains its calibrated angle and finite lens. In the
volume, material along the incident path attenuates the light. An additional
soft, density-dependent lobe represents scattering beyond the bright shaft.
It widens away from the physical aperture and uses the shadow on the edge of
the shaft, preserving opaque occlusion and the dry-room appearance. The direct
and scattered light scale with the fixture's calibrated brightness.

The show's `:extinction-per-m` is the optical extinction coefficient of the
rated output at unit density. The default is 1.25 m⁻¹. At unit preview strength
the transmission of a homogeneous layer with density `d` and length `L` is
`exp(-extinction-per-m × d × L)`. Viewer, lamp and reflected-light paths use
the same coefficient. A different fluid calibration changes apparent opacity
without changing emitted or mixed volume.

### Orbit qualification

Run `FOG_ORBIT_OUTPUT=target/fog-orbit node --test test/js/preview/fog-orbit.test.mjs` with Chromium available. The test renders the release WebGL fog shader at nine angles with a fixed source, density, exposure and eye distance. It measures both sides of the composite pass, then restricts diagnostic in-scattering to a stationary 25 cm chord. This separates a source viewed down the length of its cone from an equal mass of fog seen from the side. A thin foreground surface also exercises depth reconstruction. At a 15 × 9 fog raster, a narrow beam between sample centers can lose contrast. The output includes this separate subpixel sweep and its full-raster reference.
