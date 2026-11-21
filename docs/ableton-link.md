# Ableton Link and lighting during a pause

Enable **Link** and **Start Stop Sync** in Live’s Link settings. Run a local Carabiner 1.2 daemon on the Studio host. Carabiner participates in the Link session over the network, so a second computer can run Studio while the performance computer runs Live. The default local TCP port is 17000. [Carabiner protocol and downloads](https://github.com/Deep-Symmetry/carabiner/blob/0479f5934e60c28b0184bf5b7cc362794f834fbc/README.md#protocol)

In **Connections**, enable **Ableton Link**, keep **Follow Link play and stop** checked, and choose **Connect enabled adapters**. Select **Ableton Link** under **Clock source**. Starting and stopping shared playback in Live now controls Studio. While paused, repeated Link status messages preserve the frozen show beat. On resume, Studio reacquires Link phase and rebases active musical timers. The Link session beat expresses shared musical phase. To follow a particular song or clip playhead, select the Ableton Live bridge.

Turn off **Follow Link play and stop** before connecting to keep local transport control while receiving tempo and phase. Link peers must share transport changes for following to work. Studio connects to Carabiner at 127.0.0.1. Configure, save and import only store the pairing. Connect opens the socket. Disconnect closes its reader. Zero peers suspends the selected clock. If the daemon exits, start it again and disconnect/reconnect in Studio.

## Paused lighting

Under **Connections → Paused lighting**, choose the behavior that fits the room. The same setting applies when playback stops locally or through a followed clock.

| Mode | Emission while the show is paused |
| --- | --- |
| Use the running cues | Continue evaluating cues with the musical transport frozen. This is the default for new shows and existing show files. Explicit wall-time effects can continue. |
| Blackout | Set emitter intensity to zero for the duration of the pause. Resume returns authority to the running cues. |
| Ambient color | Apply the selected color and brightness to the rig. The initial setting is warm amber at 8%. Direct emitter writes from running effects are overridden and the strobe channel is set to zero. Pan, tilt, focus and other fixture controls continue following the show. |

The fixture’s RGBW policy controls white extraction. Fixtures with a dedicated dimmer apply the ambient brightness there. RGB fixtures without a dimmer scale their emitters. A fixed-color dimmer retains its physical color. Master attenuation and the show blackout apply to the paused look. The preview and physical sinks consume the same resolved frame.

The settings operation accepts ordinary show data.

```clojure
(studio/submit! runtime :settings
  {:scope :output
   :values {:pause-look {:mode :ambient :color [0.8 0.2 0.1] :level 0.1}}})

(studio/sync-action! runtime :configure
  {:config {:selected-source :ableton-link
            :link {:enabled? true :port 17000 :follow-transport? true}}})
```

Show files persist the output mode, color, level, selected clock, daemon port and follow preference. The runtime owns the TCP socket and the currently observed peer state. Studio receives shared playback notifications as they arrive and periodically requests the phase for freshness and stale-source detection. This connection runs on its own reader thread.

The automated gate uses a loopback TCP peer speaking Carabiner’s documented, newline-delimited EDN protocol, the actual Studio clock and captured RGBW DMX. It exercises independent play, pause, resume, peer loss and saving a pause look. The physical qualification gate is running Live with Start Stop Sync enabled on a second computer, comparing phase across tempo changes and reconnecting the network.
