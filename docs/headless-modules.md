# Show APIs for code and controllers

A show file, a MIDI surface and the web controller work with the same show model. Require the namespace for the operation in a REPL or script. Each caller owns the managers it creates and closes them at shutdown.

| Operation | JVM namespace | Entry points |
| --- | --- | --- |
| Accept paired phone or watch observations | `tolglow.sensors` | `create-manager`, `ingest!`, `advance!`, `close!` |
| Encode, decode and receive the sensor wire protocol | `tolglow.sensors.osc` | `encode`, `decode`, `open!` |
| Configure capture and process sample blocks | `tolglow.audio.capture` | `validate-config`, `create-manager`, `start!`, `stop!` |
| Sample authored waveforms or summarize PCM | `tolglow.waveforms` | `preview`, `new-capture`, `capture-block`, `capture-view` |
| Inspect expressive mappings for a device and cue | `tolglow.show` | `expressive-preview` uses the active show's device profiles and cue registry |
| Read and write complete show documents | `tolglow.persistence` | `freeze`, `write-string`, `read-string`, `thaw` |
| Configure providers, conversations and credentials | `tolglow.agents`, `tolglow.agents.credentials` | `configure!`, `start-chat!`, `history`, `cancel-all!` |
| Work with fixtures, patching and RDM | `tolglow.rig`, `tolglow.rig.rdm`, `tolglow.fixtures.library` | Rig transitions and explicit service managers. The optional `:rdm` module selects OLA or a direct Art-Net gateway |
| Connect show signals, compose cues and publish DMX | `tolglow.routing`, `tolglow.composition`, `afterglow.output.scheduler` | Patchbay transitions, composition and output sinks |
| Queue a cue at a musical boundary | `tolglow.show` | `submit!` with a launch grid, `quantized-cue-status` |

For example, record a bounded waveform summary directly from a PCM source:

```clojure
(require '[tolglow.waveforms :as wave])

(def capture (wave/new-capture {:max-bins 128 :bin-ms 20}))
```

Use the same expressive preview as Studio from a REPL with an active `runtime`.
Use the device's mapping ID, or its session ID when no mapping ID is configured.

```clojure
(require '[tolglow.show :as show])

(show/expressive-preview runtime {:device-id "roli-policy" :cue-id :ember})
```

A connected device supplies its axes and mapping defaults. For an offline model,
use `tolglow.expressive/preview` with a declared device kind.

The clock supplies musical time to effects. Device reads, network publication and credential access belong to their explicit owners. Web-specific routes, browser subscriptions and display projections bind these APIs to Studio. Afterglow owns the clock reducer, renderer and controller mapping used by those APIs.

The original show authoring namespaces remain available, including `tolglow.config`, `tolglow.setup`, `tolglow.fx` and `tolglow.fixtures`. See the [source-linked behavior inventory](feature-parity-inventory.md) for their configuration structures.

## Quantize a live cue launch

```clojure
(require '[tolglow.show :as show])

(def id (str (random-uuid)))
(show/submit! runtime {:op :cue/launch :cue-id :ember :command-id id
                       :grid {:beats 1/4} :source {:adapter :repl :session-id "desk"}})
(show/quantized-cue-status runtime id)
```

Use a positive single-unit grid such as `{:beats 1/4}`, `{:beats 1}`, `{:bars 1}` or `{:phrases 1}`. A grid request at the boundary can fire in that owner cycle after pending release and cancellation requests have been processed. Tempo changes leave the due beat intact. Pausing holds the queue. A restart or discontinuous source change marks queued requests missed, and loading a show starts with an empty queue. Omit the grid or pass `:off` to launch immediately. Blackout and the ordinary end commands remain immediate.

The intake promise reports `:pending` for a grid, while `quantized-cue-status` reads its latest receipt. The receipt distinguishes raw observation, request, due beat and actual admission. Supply `:raw {:time-ms ... :beat ...}` for known coordinates in the show clock domain, or supply just the one coordinate observed at the input. When `:raw` is omitted, the owner observation is used. Cancelling with `{:op :cue/cancel-launch :launch-command-id id :reason :operator-cancel …}` requires the same originating source. A held release uses `:held-release` and targets the resulting instance if the cue has already fired.

Studio has a Launch grid selector and a cancellable list of queued cues. For a Push 2 or Push 3 session, add `:launch-grid {:beats 1/4}` to the existing device connection options. The Live bridge accepts the same setting for playback with per-note overrides. By default these launch immediately. LFO phase is a separate setting on a parameter source or effect block. See [modulation](modulation.md) and [Live synchronization](sync-progress.md).

## Start a calibrated performance from configuration

The default application configuration registers optional performance modules after fixture patching and show restore. Enable them in the same EDN file that defines the rig. The following entries assume the rig contains a profiled fogger named `:stage-jet` and a group of lights named `:front`.

```clojure
{:lighting-capacity {:enabled true}
 :lighting-budget {:enabled true :global {:limit 0.55 :max-active 4 :standby-level 0.025}}
 :output-response {:enabled true :global {:attack {:beats 1/4} :release {:beats 1}}}
 :lighting-activity {:enabled true}
 :fog-control {:enabled true :target 0.30 :hysteresis 0.035 :fog-fixture-ids [:stage-jet]}
 :lighting-scenes {:enabled true :groups [{:group-id :front :target 0.45}]}}
```

The lighting-capacity module measures the rig. The fog controller uses the calibrated reference sightline and the selected nozzle's delivered flow. Set the manufacturer's pulse and rest limits on the fixture profile, for example `:fogger {:flow-m3-min 60 :mixing-seconds 3 :duty {:max-on-s 0.4 :min-off-s 6 :max-level 0.5}}`. The duty values limit the length and strength of each pulse even when a render is late.

The lighting-scenes module chooses and parameterizes an emission effect for each group. For a later change, call `(tolglow.show/submit! runtime :lighting-scene {:op :vary :group-id :front :target 0.45})`. Audio energy events can request the same operation. [Group scenes and event bindings](group-lighting-scenes.md) · [Fog calibration and control](venue-fog.md)

Read `:fog/level-change-per-s` and `:fog/clearance-half-life-s` under the show’s `:variables` to drive effects directly, including while automatic pumping is disarmed. From the REPL, use `(select-keys (tolglow.show/variables runtime) [:fog/level :fog/level-change-per-s :fog/clearance-half-life-s])`. A rate of `0.03` means the sightline is losing three more percentage points of contrast per second. A negative rate means the room is clearing. The half-life describes the loss of suspended material, including ventilation. It comes from the applied scan, or from the configured ventilation and fluid decay when the rig uses manual settings.

During a stationary scan the actual camera also publishes `:fog/observed-level` and `:fog/observed-change-per-s`. The latter uses the last two distinct capture timestamps. `:fog/observation` contains their latest reading, phase, monotonic sample time, sightline length, session and epoch. These values retain the last camera observation when the scan ends. An unmetered machine can provide these direct readings and a clearance half-life.

The optional `:autopilot` and `:hdr` modules accept the same signal bindings, timing and fixture scopes as Studio. [Energy autopilot and HDR](energy-autopilot.md)
