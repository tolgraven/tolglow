# External integration adapters

Read the repository instructions and `docs/implementation-contract-v1.md`.
These modules translate data at the shared-control boundary; they never select
a global show, open a device, or execute provider-generated tools on require.

Keep fixture source/provenance and unsupported-field diagnostics. An import
with errors has no publishable `:definition`; its `:candidate` is editor data.
Never evaluate imported Clojure or enable XML DTD/external-entity expansion.

Live/Drum Rack messages are protocol intents until a Live-side bridge has
executed and acknowledged them. Preserve command identity, epoch, origin and
held-instance ownership; do not claim MIDI can edit arbitrary clips or colors.

Audio FFT/YIN work runs outside fixture rendering. Arrays are owned by the
current call. Preserve phase, measured window latency, confidence gates, bounds,
and the captured instance identity of audio tails. Do not auto-route analyzed
returns back into the encoder or infer plugin settings from residual energy.

Provider credentials remain separate from show/config exports. Only a boundary
transport may make HTTP requests. Consume complete SSE records, keep fragmented
tool arguments as data, and report truncated streams. Tools still need schema
validation and the same control reducer as human input before execution.

Tests live under `test/tolglow/integrations`. Use synthetic audio and captured
protocol strings; no real provider keys, Live instance or hardware are required.
Report separately which protocol tests pass and which integration/hardware
acceptance remains unverified.
