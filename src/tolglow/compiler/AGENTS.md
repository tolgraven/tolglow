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
  Use runtime/append-contributions to flush ordinary assignment tails before
  typed blocks. Kernel priority/order, not worker completion, determines merging.
- Test actual model/engine/render transitions and byte output. Benchmark cached
  compilation plus the real capture boundary; report excluded host/network/UI
  work. CPU batching is not SIMD or GPU execution.
- A native-enabled compiler prepares owned field descriptions with pure Afterglow
  functions. Open the session and evaluate requests only in the owning show.
  [Native show configuration](../../../docs/native-shader-show.md)
