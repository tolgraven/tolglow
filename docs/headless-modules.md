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
| Connect show signals, compose cues and publish DMX | `tolglow.routing`, `tolglow.composition`, `tolglow.output` | Patchbay transitions, composition and output sinks |

For example, record a bounded waveform summary directly from a PCM source:

```clojure
(require '[tolglow.waveforms :as wave])

(def capture (wave/new-capture {:max-bins 128 :bin-ms 20}))
```

The clock supplies musical time to effects. Device reads, network publication and credential access belong to their explicit owners. Web-specific routes, browser subscriptions and display projections bind these APIs to Studio. Afterglow owns the clock reducer, renderer and controller mapping used by those APIs.

The original show authoring namespaces remain available, including `tolglow.config`, `tolglow.setup`, `tolglow.fx` and `tolglow.fixtures`. See the [source-linked behavior inventory](feature-parity-inventory.md) for their configuration structures.
