# Implementation contract v1

This is the shared first working slice for parallel implementation, 2026-11-21.
It narrows the larger architecture documents into callable interfaces. The
coordinator and adapters are explicit I/O boundaries; effect evaluation is pure.

## Control model

`afterglow.control.model/new-show` takes an optional map of initial values.
The model contains `:show-id`, integer `:epoch`, integer `:revision`,
`:time-ms`, `:registry {:effects {} :cues {} :fixtures {}}`, `:instances {}`,
`:pages {}`, `:variables {}`, `:master` in [0,1], `:blackout?`, and
`:transport {:bpm 120 :playing? true}`. Extra bookkeeping is private to control.

`apply-command [model command]` returns `{:model ... :events [...] :ack ...}`.
Commands carry `:command-id`, `:show-id`, `:epoch`, `:source`, `:op` and explicit
`:time-ms` for time-dependent operations. No reducer reads a clock. Rejection
returns the unchanged model and a structured acknowledgement. Accepted writes
advance revision. Duplicate commands return their cached acknowledgement without
performing the action again. Epoch changes invalidate earlier commands.

Implement `:cue/put` (`:cue`), `:cue/delete` (`:cue-id`), `:cue/launch`
(`:cue-id`, `:launch {:mode :persistent|:held|:timed|:one-shot :duration-ms ...
:params {...}}`), `:instance/end` (`:instance-id`, optional `:force?`),
`:param/set` (`:instance-id`, `:param`, `:value`), `:master/set` (`:value`),
`:blackout/set` (`:value`), `:show/restart`, and registry publication operations.
Additional operations can be added without replacing this envelope.

Instances have `:id` (string), `:cue-id`, `:effect-id`, `:effect-version`,
`:params`, `:started-at-ms`, optional `:release-at-ms`/`:duration-ms`, `:state`,
`:priority`, `:order`, and `:phase` (`:running` or `:releasing`). An end command
addresses an instance, never merely a cue, so stale releases cannot stop a new
launch. Effect definition data is captured as `:definition` for revision-safe
evaluation. Registry entries use maps with stable `:id` and integer `:version`.

## Authoring

`tolglow.registry/validate-definition [kind definition]` returns a vector of
path/message error maps (`[]` means valid). `publish [registry kind definition]`
returns the next registry or throws `ex-info` with validation errors; kinds are
`:effects`, `:cues`, `:fixtures`. Parameters are maps keyed by keyword, with
`:type`, `:default`, `:bounds`, optional `:recommended`/`:random` metadata.
Cues contain `:id`, `:effect-id`, `:params`, `:priority`, optional `:duration-ms`.
Effects contain `:id`, `:version`, `:params`, optional `:duration-ms`, and `:outputs`.

`tolglow.compiler/compile-effect [definition]` returns a pure step function.
The step accepts `{:frame {:time-ms ... :delta-ms ... :fixtures [...] ...}
:instance <instance-map> :state ... :assignments [] :intents [] :done? false}`.
It returns updated state, vectors of assignments/intents, boolean `:done?`.
The interpreter must not read wall time, call random globals or perform I/O.

An assignment is `{:target-id <fixture/head id> :attribute <keyword>
:value <scalar/vector> :blend :replace|:max|:add}`. Attributes include `:color`
(linear RGB vector), `:dimmer`, `:pan`, `:tilt`, `:focus`, `:zoom`, `:strobe`.
The first DSL uses keyword-map nodes `{:op :const :value ...}`, `{:op :param
:key ...}`, `{:op :time}`, `{:op :position :axis :x|:y|:z}`, and arithmetic
with `:args [...]`. Rendering and authoring agents coordinate further nodes.

## Runtime and output

`afterglow.runtime.engine/advance [model frame compile-effect]` returns
`{:model <next-model> :assignments [...] :intents [...]}`. An optional worker
mapper may be supplied by an additional arity. The engine uses captured
definitions, current parameter values, logical time, launch/release state and
deterministic priority/order. It never publishes partial frames. Expired/released
instances finish according to their envelope and are removed. Logical time is
supplied by the shell. A registry revision change has an explicit reload policy.

`afterglow.render.frame/resolve-frame [fixtures assignments options]` resolves
ordered contributions and returns `{:values <per-target map> :universes
<universe to byte-array>}`. It owns all newly allocated buffers for this call;
it never sends output. Fixture descriptors contain `:id`, `:position [x y z]`,
`:universe`, `:address` (1-based), `:channels` (attribute to 1-based offset,
or coarse/fine vector), and optional fixture calibration. Colors use :red,
:green, :blue and :white channels with declared extraction policy. Invalid
patches/overlaps/out-of-range values fail before output. Preserve target order.

## Integration ownership

Root owns manifests, test runner lists, tracker, cross-module wiring and the web
studio. Agents own their named files/modules and tests. All new namespaces load
without starting hardware. Register new test namespace names with root. Work
continues beyond this slice; passing these APIs does not complete the roadmap.
