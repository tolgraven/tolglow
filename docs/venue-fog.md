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

## Calibrate a room with the iPhone

Mount the phone with a fixed view through the room to a stationary, steadily illuminated bright patch and an adjacent dark patch. Measure the distance to the patches. Start the scan in the iPhone rig screen, capture clear air, run a normal fog cue, then switch the cue off and continue recording while the room clears. The phone locks exposure and white balance. The scan endpoint does not issue a fog cue. Leave the illumination, doors, ventilation and camera position steady for that measurement.

The **fit** operation compares the loss of image contrast with flow from the selected fogger at the actual successful DMX send times. It estimates total clearance half-life, the time for the nozzle plume to mix, and how much light a unit of this fog extinguishes per metre. The fit runs off the live render thread. An unmetered burst can yield a clearance half-life from the clearing tail. A change of fluid or ventilation calls for a new scan. Review the fitted parameters and score, then **apply** to store them in the rig and serialized show. Cancel leaves the previous calibration in place. Raw camera frames stay on the phone.

The room's measured half-life includes both fluid losses and ventilation. Each render uses that total once. Read `(get-in (tolglow.show/snapshot runtime) [:output :atmosphere :fog-level])` for the estimated fraction of contrast lost in the mixed room across the measured sightline. For example, a level of 0.3 means a target retained 70% of its clear-air contrast. The near-nozzle plume can be much denser. The same projection carries `:mixed-density`, `:extinction-per-m`, `:reference-path-m`, and the timestamp of the last presented fogger value.

The native app uses the Ring endpoint `/api/fog-calibration`. A GET returns the runtime, show epoch, selectable nozzles and the current session. Headless commands go through `tolglow.show/submit!` with type `:fog-calibration`, operation `:start`, `:samples`, `:fit`, `:apply` or `:cancel`, and the same session ID, runtime ID and expected epoch. Sample timestamps are elapsed milliseconds from capture start. Sending an identical batch twice is supported. Saved shows retain the applied venue and fixture calibration. In-progress capture belongs to the particular room, physical clock and phone session.

During acquisition, a stable clear reference also publishes the latest direct observation as the show variable `:fog/observed-level`. `:fog/observation` supplies its sightline length, phase, scan ID, show epoch and monotonic sample time so code can distinguish a fresh reading from a historical one. The simulator and setpoint controller use the evolving room model between observations.
