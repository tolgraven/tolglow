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

Fog is off at its declared physical DMX endpoint when no cue assigns the
channel, during blackout, while the independent publisher waits for a fresh
frame, and on shutdown. When repatching, the output continues to send that
endpoint on the old addresses until another channel or fixture footprint owns
them. This includes coarse/fine pairs spanning two universes.
