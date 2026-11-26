# Video on 3D surfaces

Place a video source on a plane, a wavy sheet or a sphere in the room. Every head near the surface takes the colour of the picture where the surface passes it. The result is an ordinary effect output, so a surface cue composes, fades and stops like any other cue. [Issue #674](https://github.com/tolgraven/tolglow/issues/674) builds on the sources and sampler of [video on LED strips](video-strips.md), which stays an independent 2D workflow.

## Surfaces

`[:settings :video :surfaces]` holds up to 16 surfaces by id. They are saved with the show.

```clojure
{:id :sheet :source :grid :shape :wave
 :position [0 2 -3.2] :rotation-deg [0 0 0]          ; venue metres; [pitch yaw roll] degrees
 :size-m [6.4 3.2]                                   ; plane and wave; a sphere takes :radius-m
 :wave {:amplitude-m 0.3 :wavelength-m 2.0 :speed 0.25 :axis :u}  ; speed in cycles per beat
 :crop {:x 0 :y 0 :width 1 :height 1}                ; the part of the source shown, in source fractions
 :contact {:distance-m 0.2 :feather-m 0.05 :curve :smoothstep}
 :footprint {:width 0.0 :height 0.0}                 ; fractions of the surface
 :strips {:row-1 {:height 0.25}}                     ; source height one strip averages
 :filter :box}
```

**Surface coordinates.** `u` runs 0→1 across the surface and `v` runs 0→1 down it, like the source: `(0, 0)` shows the crop's top-left corner. A plane or wave spans local x (`u`) and local y (up, so `v` runs downward); its face points along local +z. The wave displaces the sheet along +z by `amplitude · sin(2π(s / wavelength + phase))`, where `s` is local x for `:axis :u` or local y for `:axis :v`. A sphere's `u` is longitude and `v` latitude. Its local +z meridian shows the middle of the crop, `v = 0` is the top pole, and the seam (`u = 0 ≡ 1`) is the local −z meridian. Turn the sphere to move its seam. World position is `position + R · (scale · local)` with `R = Ry(yaw) · Rx(pitch) · Rz(roll)`.

**Wrapping and seams.** Sheets do not wrap. A head whose nearest point lies off the sheet's edge is never in contact. A sphere wraps in `u`: a head just either side of the seam reads the first or the last column, and a sampling footprint that straddles the seam continues on the other edge of the crop instead of reading black.

**Contact.** The contact distance is the unsigned distance from a head to the surface, so both faces count. It is exact for a plane and a sphere. For a wave it is the first-order normal distance `|z − d(s)| / √(1 + d′(s)²)`: exact on crests and troughs and within a few percent while `amplitude · 2π / wavelength` stays below ½. A head within `:distance-m` takes full weight. Over `:feather-m` the weight falls to zero with the spatial volumes' `:linear` or `:smoothstep` curve ([spatial cues](spatial-show.md)). The weight is the head's write opacity, so the look below shows through the feather. Candidate heads come from the spatial rig index used by volumes, then each one is measured exactly.

**Sampling footprint.** Each head averages a small box of the source in linear light, with the 2D sampler's filters. A pixel of a strip averages its share along the strip: the spacing to its neighbouring pixels in contact, measured on the source and the short way across a seam. Across the strip it averages the strip's `:strips` height, or else the surface `:footprint :height`. Choose how much source height feeds a strip there: 0 reads a single line, 0.25 averages a quarter of the surface height. A head outside a pixel layout averages `:footprint :width` × `:height`. Sizes are fractions of the surface, so zooming the crop zooms what each head averages.

## A surface cue

```clojure
{:op :video :surface :sheet
 :placement {:crop-x {:op :param :key :slide} :phase {:op :beat}}}
```

`{:op :video :surface id}` feeds `:color` (or a scalar with `:channel :luma`). Each placement value is a numeric expression, so parameters, modulation, macros and the musical clock can move it:

| Key | Effect | Identity |
| --- | --- | --- |
| `:crop-x`, `:crop-y` | Slide the crop over the source, in source fractions. The surface stays put and the picture moves across it. | 0 |
| `:crop-scale` | Zoom the crop about its centre. | 1 |
| `:x`, `:y`, `:z` | Move the surface in metres. The picture travels with it and different heads come into contact. | 0 |
| `:scale` | Scale the surface about its position. | 1 |
| `:pitch`, `:yaw`, `:roll` | Turn the surface, in degrees, added to `:rotation-deg`. | 0 |
| `:amplitude`, `:wavelength` | Scale the wave. | 1 |
| `:phase` | Add wave cycles to `:phase + :speed × elapsed beats`. | 0 |
| `:contact` | Scale the contact distance and feather. | 1 |

The wave's travel follows the cue's elapsed beats, so it keeps tempo and freezes when the transport stops. **Make a cue and launch it** in Studio, or `{:op :surface-cue :surface :sheet}` from the REPL, publishes an effect with one parameter per key at its identity value, places the cue in the first free cell of the **Video** page like a spawned volume cue, and launches it.

The compiled output evaluates the placement and samples once per lighting frame. It records the surface it evaluated in the instance state. The Room and the REPL read that record; they never evaluate a placement, modulation or the beat clock a second time. A stopped transport keeps the last record, and a saved show restores it with the instance.

**Overlaps.** Two surface cues, on the same or different sources, compose like any two cues: by priority and launch order, with their blend. Where both touch a head, the higher cue's colour wins (or adds, with `:blend :add`). A head only one surface touches keeps that one.

## Studio

**Rig & space → Video surfaces** lists the surfaces with how many heads each touches at its saved pose. Add a plane, wavy sheet or sphere on the first source. The editor sets shape, position, rotation, size or radius, the wave, the crop, contact distance and feather, and the head footprint. It lists the heads in contact by pixel layout, with a source height field for each strip. **Remove surface** deletes it; a source a surface reads cannot be removed.

The Room draws every running surface from its recorded pose, textured with the latched frame (refreshed up to twice a second). The rig editor's Room also draws saved surfaces nobody is running. A ring marks each head in contact and brightens with its contact weight. The disc inside the ring shows the colour that head is actually emitting in the committed output, so it can be read against the picture behind it. The Room receives committed values for the 128 brightest heads only, as for every fixture; a contact outside those keeps an empty ring.

## REPL

```clojure
(require '[tolglow.show :as show])

@(show/video-action! runtime :edit {:op :put-surface
                                    :surface {:id :sheet :source :test-grid :shape :plane
                                              :position [0 2 -3.2] :size-m [6.4 3.2]
                                              :contact {:distance-m 0.2}}})
@(show/video-action! runtime :edit {:op :surface-cue :surface :sheet :priority 10})

(get-in (show/video-action! runtime :status {}) [:surfaces :running 0 :count])  ; heads in contact
```

`(:surfaces (show/video-action! runtime :status {}))` has `:authored` (each saved surface at its saved pose) and `:running` (each running surface output with `:instance-id`, `:resolved` pose and `:contacts`: id, surface uv, distance and weight). `tolglow.video.surface` holds the shared geometry: `resolve-surface`, `project`, `world-point`, `contacts`, `footprints` and `mesh`.

## Verification

- `tolglow.video-surfaces-test` covers coordinates round-tripping on every shape, either side of the contact threshold, the feather curve, crop versus surface motion, a travelling wave, the sphere seam (colours and a straddling footprint), per-strip sampling height and the compiled output.
- `tolglow.studio.video-surfaces-host-test` runs a real show: captured DMX for the labelled grid either side of 0.25 m, the feather blend, parameter-driven crop and surface moves, a wave crossing a tube on the beat and freezing on stop, a sphere seam, two feeds overlapping and changing priority, the Room projection agreeing with captured DMX, and save/reload restoring the same pose, contacts and values.
- `dev/studio/video-surfaces-browser-qa.mjs` against `tolglow.qa-video-surfaces` drives the optimized Studio in headless Chrome with the Metal GPU. It is not iPad Safari or a foreground display.

**Cost.** On the M4 Pro with other agents loading the machine, 2,000 strip pixels over a 1024 × 576 frame: sampling a cached plan takes 0.06 ms; a moving surface rebuilds contacts and plan every frame, 4.3 ms median. A stationary surface reuses its plan. The rebuild is plain Clojure maps per head and is the first place to optimise for larger rigs.
