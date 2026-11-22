# Aim at a surface and follow a beam edge

A fixture's `:beam-angle` is the full included beam angle in degrees. A fixed optic keeps that angle as the show runs. For a zoom lens, set `:zoom-range-deg [narrow wide]` to use the actual zoom output. `:lens-radius-m` sets the radius where the light leaves the fixture. Add `:pseudo-fixture true` to expose the receiving surface as a source of reflected light in Studio.

```clojure
{:id :left :name "Left spot" :position [-2 3 -1]
 :universe 1 :address 1
 :channels {:pan [1 2] :tilt [3 4] :red 5 :green 6 :blue 7}
 :beam-angle 20 :lens-radius-m 0.08 :pseudo-fixture true}
```

Studio's rig editor can aim selected fixtures together at a point on a room surface. Use edge-to-edge to place successive spots against the previous spot. For an already animated leader, use follow mode and leave its aim under cue control. Release returns control to the ordinary show effects. A saved rig can express the same relationship:

```clojure
{:edge-tracking [{:leader :left :follower :right :surface-id :floor :along [1 0 0]}]}
```

List leaders before their downstream followers. The tangent is in world coordinates on the chosen receiver. Each frame the solver uses the mounted, calibrated beam, then accounts for both cone angles, lens radii and the oblique intersection. If a follower cannot place its spot without overlap within its limits and the chosen receiver, it dims for that frame. The next feasible position restores the authored light. Real motors, optical penumbra and DMX resolution affect the observed edge on stage.

`(:output (tolglow.show/snapshot runtime))` includes `:edge-tracking` results and the selected heads' `:beam-footprint` in `:values`. A footprint identifies the receiving surface, centre-ray hit, ellipse centre, two axes and radii in metres. Use `tolglow.rig.reflections/footprint` on a committed target row to query a head outside the bounded Studio selection. The nearest surface hit by the centre ray determines the reported footprint. An ellipse extending past the rectangle has `:clipped? true`. Studio uses the patch's light color and the surface to illuminate nearby objects. This derived source has no DMX address.
