# Show APIs for code and controllers

A show file, a MIDI surface and the web controller work with the same show model. Require the namespace for the operation in a REPL or script. Each caller owns the managers it creates and closes them at shutdown.

| Operation | JVM namespace | Entry points |
| --- | --- | --- |
| Accept paired phone or watch observations | `tolglow.sensors` | `create-manager`, `ingest!`, `advance!`, `close!` |
| Encode, decode and receive the sensor wire protocol | `tolglow.sensors.osc` | `encode`, `decode`, `open!` |
| Configure capture and process sample blocks | `tolglow.audio.capture` | `validate-config`, `create-manager`, `start!`, `stop!` |
| Sample authored waveforms or summarize PCM | `tolglow.waveforms` | `preview`, `new-capture`, `capture-block`, `capture-view` |
| Inspect mappings for the selected device and cue | `tolglow.expressive` | `preview` reads the cue registry and shared Afterglow mapper |
| Read and write complete show documents | `tolglow.persistence` | `freeze`, `write-string`, `read-string`, `thaw` |
| Configure providers, conversations and credentials | `tolglow.agents`, `tolglow.agents.credentials` | `configure!`, `start-chat!`, `history`, `cancel-all!` |
| Work with fixtures, patching and RDM | `tolglow.rig`, `tolglow.rig.rdm`, `tolglow.fixtures.library` | Rig transitions and explicit service managers |
| Connect show signals, compose cues and publish DMX | `tolglow.routing`, `tolglow.composition`, `afterglow.output.scheduler` | Patchbay transitions, composition and output sinks |

For example, record a bounded waveform summary directly from a PCM source:

```clojure
(require '[tolglow.waveforms :as wave])

(def capture (wave/new-capture {:max-bins 128 :bin-ms 20}))
```

The clock supplies musical time to effects. Device reads, network publication and credential access belong to their explicit owners. Web-specific routes, browser subscriptions and display projections bind these APIs to Studio. Afterglow owns the clock reducer, renderer and controller mapping used by those APIs.

The original show authoring namespaces remain available, including `tolglow.config`, `tolglow.setup`, `tolglow.fx` and `tolglow.fixtures`. See the [source-linked behavior inventory](feature-parity-inventory.md) for their configuration structures.

## Start a calibrated performance from configuration

The default application configuration registers three optional modules after fixture patching and show restore. Enable them in the same EDN file that defines the rig. The following entries assume the rig contains a profiled fogger named `:stage-jet` and a group of lights named `:front`.

```clojure
{:lighting-capacity {:enabled true}
 :fog-control {:enabled true :target 0.30 :hysteresis 0.035 :fog-fixture-ids [:stage-jet]}
 :lighting-scenes {:enabled true :groups [{:group-id :front :target 0.72}]}}
```

The lighting-capacity module measures the rig. The fog controller uses the calibrated reference sightline and the selected nozzle's delivered flow. The lighting-scenes module chooses and parameterizes an emission effect for each group. For a later change, call `(tolglow.show/submit! runtime :lighting-scene {:op :vary :group-id :front :target 0.72})`. Audio energy events can request the same operation. [Group scenes and event bindings](group-lighting-scenes.md) · [Fog calibration and control](venue-fog.md)
