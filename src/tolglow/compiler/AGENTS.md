# Typed authoring compiler

Read ../AGENTS.md and ../../../docs/rendering-progress.md. `batch.clj` consumes the
shared `tolglow.compiler/compile-plan`; do not duplicate authoring validation.

- Compile against a captured immutable rig. The host owns bounded caches and
  rebuilds them on rig changes. No hidden atom, clock, RNG or worker pool belongs
  inside an effect/compiler plan.
- Preserve definition/output selector intersection and per-target output order.
  Duplicate attributes create new layers; grouping cannot move writes across
  another contribution that affects the same target/attribute.
- Keep response and envelope semantics identical to the scalar compiler. State
  and canonical model parameters remain caller-owned; only the per-frame copy
  changes. Do not evaluate the effect on both backends to hide disagreements.
- Scalar numeric semantics are the reference. Keep world coordinates signed and finite. Preserve explicit
  exact-number and tween fallbacks and explain them through function metadata.
  Unsupported query/IR nodes must fail shared validation, never arbitrary eval.
- New arrays are privately owned until step return and read-only thereafter.
  Pure groups bind with `:shared-outputs? true`, so one evaluated array reaches
  every instance of the compiled group and every later frame with the same
  inputs (always, on a stopped transport). Never pass it to an in-place writer:
  not Afterglow `fields/scale-field!`, not `aset`/`aset-double`, not
  `System/arraycopy` into it. Write scaled or changed values to a new array, as
  `finish-fields` does. `tolglow.shared-fields-host-test` checks this through
  `show/tick!` against the scalar runtime. A binding keeps its last output
  while its program is cached, so the show records each compiled program's
  pure bindings and, after every frame, calls `reuse/clear!` on those of
  programs that frame no longer ran: a released or replaced cue retains nothing.
  Use runtime/append-contributions to flush ordinary assignment tails before
  typed blocks. Kernel priority/order, not worker completion, determines merging.
- Test actual model/engine/render transitions and byte output. Benchmark cached
  compilation plus the real capture boundary; report excluded host/network/UI
  work. CPU batching is not SIMD or GPU execution.
- A native-enabled compiler prepares owned field descriptions with pure Afterglow
  functions. The show binds Afterglow’s render compute API. Keep native queue,
  draw, wait, calibration and fallback code in the fork.
  [Native show configuration](../../../docs/native-shader-show.md)

The show supplies `:bind-pure` when compiling a rig. For pure outputs derive
parameter, position and clock paths from the validated expression. Apply current
response and modulation inputs first. Apply envelopes to the frame's owned
arrays after the pure field evaluation. Upstream layer values are inputs to
any compound transform that uses them. Rebind on definition or rig changes.

Positional numeric eligibility uses the intersection of the effect and output
targets and the axes that the particular output reads. Fixtures outside that
intersection must not force a spatial pixel or moving-head effect into scalar
evaluation. Check exact values on selected heads against the scalar compiler.

A fixture coordinate entered as a small integer can use a typed field when
its immediate expression operation converts it to floating point. Preserve
the scalar oracle for exact arithmetic, ratios and coordinates outside double
integer precision. Check every use of an axis, including nested operations
and operations with more than two arguments.

Compile authored block bindings to the shared Afterglow IR before scalar or batch selection. Keep source variables and cue inputs available as dry values. Inspect a captured pure plan on demand. The effect deadline must never allocate a visual trace for every fixture. [Effect building blocks](../../../docs/effect-blocks.md)

Capture fixture `:surface-uv [u v]` in target order beside world positions. Uncharted heads use their individual world X/Y. Verify scalar, typed, deferred and target-local trace agreement whenever a material can read both spaces.
