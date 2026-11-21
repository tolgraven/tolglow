# Live audio sends and returns

A send turns the current numeric parameter of a lighting cue into a continuous mono sine. Its pitch follows the parameter logarithmically. A return analyzes a separate mono PCM channel after Live's effect chain. Pitch, level or fitted-sine residual can control the return cue parameter. Return level independently scales that cue's opacity, so a quiet delay tap or reverb decay can produce a correspondingly dimmer image.

## Connect a Live track

1. Choose a virtual audio device that both the Studio JVM and Live can open, such as a multichannel loopback interface on a Mac. A pair of virtual devices also works. Identify independent channel numbers for dry and processed audio. If both ends use the same loopback device, allocate different channels for the two directions.
2. In **Connections → Signal patchbay → Live audio sends & returns**, list or enter mixer names. Enter the channel count the driver exposes. Add a lane, choose the source cue and parameter, then choose a separate return cue and parameter. Select the physical send and return channels. They are numbered from 1. Save the routing.
3. In Live enable the corresponding mono channels in the audio preferences. Set an audio track's **Audio From** to the dry channel and **Monitor** to **In**. Insert Delay, Echo, Reverb, EQ or the effect chain you want. Route its **Audio To** to the processed channel. Send the carrier only to the virtual interface.
4. Set a delay to wet-only to hear its repeats in the lighting after the source ends. Turn on **Connect audio** in Studio and launch the source cue. Its original fixtures can occupy another group or have different outputs from the return cue. Use **Disconnect audio** to release the return voices and close both streams.

The default rate is 48 kHz with 1,024 sample blocks (21.33 ms). Each port uses signed 16-bit little-endian PCM with independent interleaved channels. Available formats and channel counts depend on the Java Sound mixer provider. Mixer enumeration gives names and direction. Opening both requested lines verifies the format. The control panel reports a failed open without starting an alternative device.

## Drum Rack chains

Use a distinct send/return pair for every simultaneous parameter. Live can route individual chains to or from dedicated tracks. The editable Max patch sources in `src/tolglow/integrations/audio_bridge` provide a mono bus for cases where the chain routing chooser does not expose the needed tap.

- Put **tolglow-bus-send.maxpat** in a Max Audio Effect on a track receiving Studio's dry channel. It accepts the left input and silences the device output. Set **Audio bus** to an unused number.
- Put **tolglow-bus-receive.maxpat** in a Max Instrument at the start of the destination Drum Rack chain and select that bus. It emits the mono signal into both channels. Put your Live audio effects after the receiver.
- Place another bus sender after the effects and a receiver on a monitored return track, using a **different** bus number. Route that track to the virtual interface return channel. Keep chains/receivers processing during echo gaps. If Live suspends an unused chain, use an always monitored audio track for that lane.
- Save the devices in your Live library and their bus assignments in the set. A bus number is shared across loaded devices. Use a unique bus number for each leg of each lane. Carrier generation and return analysis run in Studio independently of the MIDI clip bridge. Host operation of the editable patchers still requires qualification in Max for Live.

The patchers use `plugin~`, `plugout~`, `send~`, `receive~` and a stored Live parameter. Signal buses may incur an additional signal-vector delay depending on host scheduling. [Cycling '74 send~](https://docs.cycling74.com/reference/send~/) · [receive~](https://docs.cycling74.com/reference/receive~/) · [plugin~](https://docs.cycling74.com/reference/plugin~/) · [plugout~](https://docs.cycling74.com/reference/plugout~/)

## Mapping and tails

Each active return exposes four source ports in the patchbay: `live-audio/<lane>/rms`, `pitch-hz`, `pitch-confidence` and `spectral-residual` under the same prefix. Residual is measured against the dry carrier frequency. Gain changes alone leave little residual. Pitch shifts, modulation or extra harmonics can increase it. Monophonic pitch has a confidence threshold, and ambiguous pitch closes its gate. The ordinary patchbay also supports response curves, range, ADSR and combining these ports with other inputs. Routing a processed feature back to its own dry source by hand creates a feedback loop.

A lane tracks the latest running dry cue activation. After it ends, the return retains the original ID through quiet gaps until the configured musical deadline, sustained silence or a physical upper limit. An echo can create a new *owned return voice* after the dry instance has completely gone. Human instances keep their own IDs and values. A new dry activation immediately suppresses an older wet voice. Its carrier waits for the previous return to drain before reusing that physical channel. Use independent lanes to overlap polyphonic parts. A stalled or disconnected stream retires its voice within 500 ms of the last captured window at the next Studio frame. Show import preserves lane settings and discards derived wet activations. Reconnection is explicit.

## REPL and persisted configuration

```clojure
(def config
  {:output-mixer "Live send" :input-mixer "Live return"
   :sample-rate 48000 :channels 4 :block-frames 1024
   :lanes [{:id "kick" :send-channel 1 :return-channel 3
            :dry-cue :ember :dry-param :level :wet-cue :spot :wet-param :level
            :feature :pitch :frequency-range [120 1800] :amplitude 0.5
            :curve :smoothstep :max-tail {:beats 8} :confidence-min 0.85
            :silence-hold-ms 1200 :max-tail-ms 15000}]})
@(tolglow.studio/submit! runtime :settings {:scope :audio-returns :values config})
(tolglow.studio/audio-action! runtime :return-start {})
(tolglow.studio/audio-action! runtime :return-status {})
(tolglow.studio/audio-action! runtime :return-stop {})
```

The settings writer runs on the normal owner queue. A manually ticked host must tick before dereferencing its receipt. `:max-tail` accepts beats, bars, phrases or a physical duration. The maximum and silence hold are measured in captured sample time. Dry and wet parameter ranges come from their registered schemas. Integer return parameters are quantized after the response curve. Choose a window long enough for at least 2.1 periods of the lowest carrier frequency. Extremely low frequencies increase latency. A pitched carrier feature is independent of the separate capture analyzer's sub-bass port.

Embedding without Ring uses `tolglow.integrations.audio-duplex/create`, `start!`, `frame-input!` and `stop!`, with `tolglow.integrations.audio-return/advance` on the control owner. The duplex boundary owns the device lines and three daemon workers for encoding, capture and analysis. Capture replaces a queued, obsolete full window if processing falls behind. The render owner sees only the latest analyzed values and the dropped-window count. Window duration is only part of total latency; actual driver, Live/plugin latency compensation, DSP processing and frame timing need measurement on the performance rig.

The integrated tests send real PCM16 samples through the actual workers with injected audio lines, then inspect rendered DMX after a dry cue ends. They also exercise mixer failure, capture retirement, channel isolation, confidence, musical tail expiry and restoration. Real CoreAudio routing, Max host execution and hardware timing remain unmeasured.
