# Studio application boundary

The application uses Ring on the JVM and re-frame/Reagent in `src/studio`.
Keep HTML to the document mount. WebGL and Neovim have isolated interop engines;
application state, subscriptions, events and views belong in ClojureScript.

- `studio.clj` owns the immutable show model, command admission, frame commits,
  resource lifetime and REPL facade. Workers return data. Never run device IO,
  provider requests or subscriber callbacks inside `swap!` or effect workers.
- `web.clj` is a portable Ring application with explicit service callbacks.
  Keep server adapters separate from routing and domain operations.
- Beats (including fractions), bars and phrases are primary authoring units.
  Milliseconds remain explicit audio, physical and wall-time options. Do not
  turn musical durations into fixed millisecond deadlines at launch.
- `persistence.clj` validates full schema-2 show files before publication. Restore
  captured effect state, transport position, registries, composition and auto
  ownership, settings and integration data. Remap active IDs and invalidate old
  epochs. A load must not advance effect state just to recreate the saved image.
- Agent configuration and chat history are show data. Credentials live in the
  separate local credential store; show files contain references. Rebuild live
  connections and reactive registrations through their owned boundary services.
- Reactive registration source and captured data must be explicit and portable.
  Do not attempt to serialize arbitrary Java closures, workers or sockets.
- Old engine code is a feature/parity reference only. Do not introduce an old
  runtime compatibility adapter or silently maintain two implementations of the
  same new operation.
- Tests exercise actual compiler/engine/render/command paths and local HTTP or
  injected transports. No physical lighting, audio input, provider request or
  MIDI activation is required to test the application.
- Effect faults are per-instance model data, not whole-show stop commands.
  Use engine rejection of renderer-attributed evaluations; never rerun healthy
  effects to diagnose another effect. Only committed fault events enter the
  bounded asynchronous logger. Retry uses :instance/retry; active fault identity
  remaps on restore while historical events remain evidence.

Track ownership and evidence in `docs/work-items.edn`. Run `bin/test pure` and
`bin/test parity`; frontend and browser checks are separate documented gates.
