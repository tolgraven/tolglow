# Changing the energy autopilot

Tolglow chooses looks and setpoints. Afterglow owns native composition, spatial spill, shutter handling and DMX packing. The performer configures the same headless settings from a cue, the REPL or Studio. [Operator guide](../../../docs/energy-autopilot.md)

## Follow a tick

1. In `tolglow.autopilot/prepare`, resolve the selected fixtures and modulation inputs from the show snapshot. Derive cadence and waveform phase from the transport. Keep the authored-output forecast lazy so a tick with no decision can skip that render.
2. In `controller`, compare the rolling observations with the effective targets. Account for recent corrections while the window catches up. `target-response` slews the requested targets. The output-response stage separately shapes how the fixtures change. Preserve these two clocks when changing a response curve. [Response settings](../../../docs/output-response.md)
3. In `candidates` and `actions`, admit a look using the actual patch, light-producing channels, open shutter ranges and scope. Track ownership by instance. A human edit or a new session can end the conductor’s claim. The host submits authoring work after frame presentation and validates the returning proposal against the current request.
4. Render the model once for presentation. Apply configured output response and hard caps before `observe` measures delivered channels. Keep calibration, blackout and pause behavior in the host. `statistics` weights readings by elapsed musical or physical time. A duplicate tick must not count a second flash. Store portable feedback state in the model and keep compiled patch plans and frame buffers in the frame owner.

## Check a change

Run these commands from Tolglow with the matching Afterglow checkout available through `AFTERGLOW_ROOT` or `../afterglow`.

```sh
lein test tolglow.autopilot.statistics-test tolglow.autopilot.signal-test tolglow.autopilot.target-response-test
lein test tolglow.autopilot.controller-test tolglow.autopilot.actions-test tolglow.autopilot-host-test
LIBGL_ALWAYS_SOFTWARE=1 xvfb-run -a lein with-profile +native-gpu test tolglow.native-show-test
lein with-profile +benchmark run -m tolglow.bench-autopilot
```

The host test reads captured DMX, and the native suite compares actual shader output with CPU output. For changes to rendering or budgeting, use the mixed-rig benchmark’s default 2,048 RGBW pixels and 64 moving heads. Report decision ticks and ordinary ticks separately, the number over the 25 ms deadline, the sink used and any CPU fallback. Compare full-frame captures before drawing conclusions from a faster stage. For changes confined to spill geometry, run `lein run -m afterglow.bench-hdr` from Afterglow. [Related lighting and opacity benchmarks](../../../docs/performance/lighting-controls.md)
