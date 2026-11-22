# A DMX fogger in the venue

Add the fogger to your fixture definition, or patch it as you would a dimmer with a `:fog` channel. Enter the manufacturer's **rated fog volume** in m³/min. The direct show map below describes a 60 m³/min nozzle in a 200 m³ room.

```clojure
{:room {:width 10 :height 5 :depth 4
        :air-changes-per-hour 2 :fog-half-life-s 180}
 :fixtures [{:id :floor-jet :position [1 0.4 -2] :universe 1 :address 120
             :channels {:fog 1}
             :fogger {:flow-m3-min 60 :dmx-range [0 255] :mixing-seconds 8
                      :direction [0 0.25 1]}}]}
```

The show holds the last successfully delivered DMX level for the elapsed physical time. At full flow for four seconds this machine emits four cubic metres of *equivalent rated-output volume*. The initial plume is concentrated around the nozzle. It mixes into the venue while both deposition and the configured air exchange reduce the total. A zero output stops emission immediately; fog already released keeps dispersing, including during a musical pause. In a preview-only show the committed DMX buffer and the show clock drive the same model. A real sink's successful send confirms software delivery; a machine's heater, duty cycle, fluid choice and actual air currents still affect the room.

Read `(get-in (tolglow.show/snapshot runtime) [:output :atmosphere])` for per-source emitted and local volumes, current flow, room volume, mixed volume and mixed density. The model measures relative optical density; a value of 1 represents one room volume of fog at rated output density. This value can exceed 1, and is independent of the light output percentage. Saving a show records the quantities. Restoring rebases the physical clock and starts the pumps' estimated flow at zero until another frame is presented.

## Hold a venue fog level

In Studio → Connections → Venue fog, choose the desired light loss along the reference sightline, the band on either side, and the individual pumps. Arm automatic fog control to begin feedback. A target of 30% with a five-point band starts a demand below 25%, continues through the band and ends it at 35%. These are switching thresholds. Lingering plumes can carry the room above the upper threshold after a pump stops. Feedback also forecasts the highest attenuation the already released local fog can reach as it mixes and clears. If those plumes can reach the upper threshold, the controller holds further pulses, even while the current room reading is low. After a pulse each machine rests before it can pulse again.

The sightline level is `1 − exp(−extinction × mixed density × reference path)`. Afterglow calculates it with the room's measured extinction and reference distance when available. It describes the well-mixed room along the reference path; a person in a concentrated jet can see substantially greater attenuation. Open the live status in Connections to see the current estimate, band and each pump's state.

At the REPL, submit the same configuration to the normal show coordinator. Headless code constructing a show can call `tolglow.atmosphere.control/configure` with its model and these values.

```clojure
(tolglow.show/submit! runtime :settings
  {:scope :fog-control
   :values {:enabled? true :target 0.30 :hysteresis 0.05
            :fog-fixture-ids [:floor-jet]}})
```

Read `:fog/level` and `:fog/target` in model `:variables`. A reaction can change the target. Subscribe to category `:atmosphere` or one of `:fog/below-target`, `:fog/in-band`, `:fog/above-target`, `:fog/target-changed`, `:fog/demand-started` and `:fog/demand-ended`. Subscribers get the level, target and band position with the normal stamped event. The Studio Debug event catalog includes each type.

To set manufacturer duty limits, add `:duty {:max-on-s 0.4 :min-off-s 6 :max-level 0.5}` to the fixture's `:fogger` map. These are the defaults. A pulse ends on its own publisher-clock deadline, including when rendering is stalled and the publisher repeats the last frame. The capture reports delivered DMX, so a lost connection suspends feedback for that pump once its last delivery is older than 500 ms. A real machine can continue to hold its last received DMX setting during a transport failure.

An authored cue can take over a pump. Music-triggered pulses use the remaining pumps, with the same device caps on profiled machines. Pause, blackout, a zero master or calibration suspends autonomous demand. A calibration operator can still drive an ordinary cue. The profile maps demand into the physical DMX range, including reversed ranges and coarse/fine inversion. When a lease expires, the publisher sends the configured off bytes for that pump. Loading a show preserves the target and selected fixtures and disarms its automatic controller.
Fog is off at its declared physical DMX endpoint when no cue assigns the
channel, during blackout, while the independent publisher waits for a fresh
frame, and on shutdown. When repatching, the output continues to send that
endpoint on the old addresses until another channel or fixture footprint owns
them. This includes coarse/fine pairs spanning two universes.
