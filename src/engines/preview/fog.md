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
single scattering uses a forward Henyey–Greenstein phase function, cone profile,
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
weights to avoid dragging background fog across foreground edges.

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

Run `node --test src/engines/preview/fog.test.mjs` for deterministic field,
cone-membership, diffusion, cooling, extinction and long-gap tests. The test logs
informational Node CPU field timings without unstable timing assertions.

Run `node --test src/engines/preview/fog-gpu.test.mjs` for a real isolated WebGL2
context. `CHROMIUM_PATH` defaults to `/usr/bin/chromium`; a missing browser skips
this explicit integration gate rather than claiming GPU validation. The fixture
bundles the production module, renders real half-float/depth/shadow targets and
reads their pixels. It checks exact empty/disabled preservation, opaque-wall
occlusion, extinction, shadow attenuation, blackout and narrow-shaft coverage.
The available Chromium renderer is SwiftShader: this proves shader execution and
pixel behaviour, not hardware acceleration, mobile thermal stability or AAA
visual quality. Full-scene screenshots and physical device performance remain
separate acceptance gates.
