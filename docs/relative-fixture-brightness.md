# Measure relative fixture brightness

In the iPhone companion, connect to the running Studio and open **Scan rig → Measure fixture brightness**. Select one representative of each type. The first is the reference. Place the phone one metre from each lens, on the centreline, using the same orientation, zoom, fixture zoom and ambient conditions. Keep the camera settings locked through the entire comparison. Reposition the phone for each fixture.

Arming uses the running show's configured output. Each white phase drives the selected fixture's visible emitters and dimmer to full, including separate red, green, blue, white and amber channels. During measurement other light emitters are dark. UV and strobe stay at zero. Move or focus the fixtures before measuring. The show continues evaluating its cues and releases the override on apply, cancel, rig replacement, or loss of the 15-second session lease.

The phone takes a dark and white reading for each representative after the output publisher confirms the requested bytes. A command acknowledgement alone cannot confirm light at the lens. The saved value is the fixture's background-corrected linear reading divided by that of the reference, which is 1. This measures relative on-axis brightness at the test zoom. Beam spread, lens position and camera spectral sensitivity influence the reading. The result has no absolute lumen or lux units.

The show writes `:measured-relative-brightness` on patched instances of the measured type and publishes a new version of its registered fixture definition when present. An untyped fixture carries its own measurement. These values travel with the normal Transit show save. Loading a saved show always releases any measurement output.

Headless applications use `tolglow.show/submit!` with the `:photometry` operation. The Ring endpoint `GET /api/photometry` returns runtime and epoch identity, output availability and candidate IDs/types. POST to the same route with the runtime-id, expected-epoch and unique session-id. The operations are `start` with armed? and ordered fixture-ids, `step` with fixture-id and dark or white phase, `heartbeat`, `apply` with distance-m 1 and ordered fixture-id/dark/white readings, and `cancel`. Heartbeat reports the current phase and whether its output was recently verified. The hardware calibration path requires an independently scheduled, configured output sink.

## Trim the color of an individual fixture

Use `:color-correction` on a patched RGB or RGBW fixture when its authored colors need a different emitter balance from other fixtures. Each gain runs from zero to one and omitted emitters use full gain. White gain scales the white extracted by `:rgbw-policy :extract-min`. An explicit emitter assignment, such as a full-white measurement step, directly controls its channel.

```clojure
(require '[tolglow.show :as show])

(let [rig (:rig (show/snapshot runtime))]
  (show/submit! runtime :rig
    (update rig :fixtures
      (fn [fixtures]
        (mapv #(cond-> % (= (:id %) :front-wash)
                 (assoc :color-correction {:red 0.90 :blue 0.82}))
              fixtures)))))
```

Select a cue that writes `:color` and use **Global** brightness in the Studio group scene to match the corrected look on those fixtures. The same target and source readings are available via `:lighting-scene` and `(show/source-brightness runtime [:front-wash])`. The matching calculation accounts for configured trims against the full-white reference. The fixture meter's hue ceiling describes raw emitter headroom.

Set the trims from a color measurement of the specific fixtures and mode you use. A relative white measurement supplies a brightness scale. Color correction requires separate channel or color-reference readings, ideally from a colorimeter. [Group lighting scenes](group-lighting-scenes.md)
