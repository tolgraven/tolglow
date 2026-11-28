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

**Contact.** The contact distance is the unsigned distance from a head to the nearest surface point, in venue metres, so both faces count. A negative scale mirrors the surface; distances stay unsigned and a zero scale touches nothing. For a wave, `|z| − |amplitude|` first rejects far heads, then the nearest point is searched. It lies within half a wavelength of the head along the wave (a farther curve point has a copy one wavelength nearer at the same height), so the search samples that window every wavelength / 32 — at most 33 samples — and Newton steps refine the best sample and every crest, trough and crossing of the head's height in it, where a steep wave's nearest point hides between samples. The work per head does not grow with contact distance over wavelength. Against a dense brute-force reference the search agrees to within 1e-6 m on gentle and steep waves (amplitude 0.05–0.6 m, wavelength down to 0.02 m), and contact classification near the threshold matches it. A head within `:distance-m` takes full weight. Over `:feather-m` the weight falls to zero with the spatial volumes' `:linear` or `:smoothstep` curve ([spatial cues](spatial-show.md)). The weight is the head's write opacity, so the look below shows through the feather. Candidate heads come from the spatial rig index used by volumes, inside a box of `|scale| · (half diagonal + |amplitude|) + contact + feather`; each candidate is then measured exactly.

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

The wave's travel follows the cue's elapsed beats, so it keeps tempo and freezes when the transport stops. **Make a cue and launch it** in Studio, or `{:op :surface-cue :surface :sheet}` from the REPL, publishes an effect with one parameter per key at its identity value, places the cue in the first free cell of the **Video** page and launches it, through the same `tolglow.cue-spawn/place-and-launch` transaction as a spawned volume cue. `:launch false` places it without launching.

The compiled output evaluates the placement and samples once per lighting frame. It caches contacts by the surface's pose, shape, size, wave and contact, and the sampling plan by the contact set, crop, footprint and frame size, so a sliding crop keeps its contacts and a travelling wave whose contact set holds keeps its plan. It records the surface it evaluated in the instance state. The Room and the REPL read that record; they never evaluate a placement, modulation or the beat clock a second time. A stopped transport keeps the last record, and a saved show restores it with the instance.

**Overlaps.** Two surface cues, on the same or different sources, compose like any two cues: by priority and launch order, with their blend. Where both touch a head, the higher cue's colour wins (or adds, with `:blend :add`). A head only one surface touches keeps that one.

## Studio

**Rig & space → Video surfaces** lists the surfaces with how many heads each touches at its saved pose. Add a plane, wavy sheet or sphere on the first source. The editor sets shape, position, rotation, size or radius, the wave, the crop, contact distance and feather, and the head footprint. It lists the heads in contact by pixel layout, with a source height field for each strip. **Remove surface** deletes it; a source a surface reads cannot be removed.

**Move in the Room** opens the rig editor's **Move surface** tool. Each saved surface shows three handles: drag its centre to move it in the editing plane (floor, front or side, with the editor's snap), the cone above it to raise or lower it, and the ring beside it to turn it about the vertical. The Room draws the surface at the dragged pose at once; the heads in contact update when the show commits the drag. A drag is one `{:op :put-surface}` edit and one undo step. The tool's panel also has numeric position and yaw, and **Undo**/**Redo**, shared with the editor here and with the REPL. Size, pitch and roll stay numeric.

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
@(show/video-action! runtime :edit {:op :undo})   ; undo the newest surface (or strip) edit
```

`(:surfaces (show/video-action! runtime :status {}))` has `:authored` (each saved surface at its saved pose) and `:running` (each running surface output with `:instance-id`, `:resolved` pose and `:contacts`: id, surface uv, distance and weight). `tolglow.video.surface` holds the shared geometry: `resolve-surface`, `project`, `world-point`, `contacts`, `footprints` and `mesh`.

## Verification

- `tolglow.video-surfaces-test` covers coordinates round-tripping on every shape, either side of the contact threshold, the feather curve, crop versus surface motion, a travelling wave, the wave distance against a brute-force reference off the crest and on steep waves, far heads with short wavelengths, a negative scale (also from an authored `{:op :sin}`), the sphere seam (colours and a straddling footprint), per-strip sampling height, one sample per output per step and the compiled output.
- `tolglow.studio.video-surfaces-host-test` runs a real show: captured DMX for the labelled grid either side of 0.25 m, the feather blend, parameter-driven crop and surface moves, a wave crossing a tube on the beat, freezing on stop and resuming without catch-up, a sphere seam, two feeds overlapping and changing priority, two feeds summing with `:blend :add`, the Room projection agreeing with captured DMX, and save/reload restoring the same pose, contacts and values.
- `dev/studio/video-surfaces-browser-qa.mjs` against `tolglow.qa-video-surfaces` drives the optimized Studio in headless Chrome with the Metal GPU. It is not iPad Safari or a foreground display.

**Cost.** `lein run -m tolglow.bench-video-surfaces` times one compiled surface output step: 2,000 strip pixels in contact with an 8 × 4 m wave plus 2,000 heads far from it, 1024 × 576, box filtered, medians on the M4 Pro with other agents loading the machine. Before the split caches (43399af4) and after:

| Frame | Before (43399af4) | After |
| --- | --- | --- |
| Surface still | 1.26 ms | 1.37 ms |
| Wave phase moving | 5.30 ms | 3.63 ms |
| Crop sliding | 5.42 ms | 2.46 ms |
| Whole surface moving | 4.89 ms | 5.11 ms |

The after column was measured at load average 5.5, which also moved the unchanged still frame by 0.1–0.2 ms between runs. The same harness times the wave search's worst case: 2,000 heads just above the crests of a 0.3 m wave with 0.25 m contact take 3.5 ms at wavelength 0.1 m, 3.5 ms at 0.02 m and 3.3 ms at 0.001 m.

The still frame is the ordinary per-target assignment path every effect pays; sampling a cached plan takes about 0.06 ms of it. A moving surface rebuilds contacts (1–2 ms, the exact wave search included) and its plan (about 1.2 ms). A whole surface moving every frame costs about what it did before the fixes, because the exact wave distance replaced a cheaper but wrong estimate. Heads beyond `|amplitude|` plus the contact distance cost one comparison.
