# Branch compatibility and existing fork work

Verified on 2026-11-21 using `git ls-remote --heads`, GitHub commit comparisons,
and source files fetched at the full commit IDs below. This investigation did
not change either checkout or run any remote branch's code.

The current tested baseline remains Tolglow `effects` at
`01146ece7731ec06aa1ee6409342373096c8e0dc` and Afterglow `master` at
`16b0b4cac70433e4118038dfbfd304e22d1d9018`. The user's active Afterglow branch is
pending confirmation. `tolmaster` is a strong compatibility candidate because
it contains extensions that Tolglow explicitly imports; this is source evidence,
not confirmation that it is the user's current working branch or that it starts
successfully with a clean dependency installation.

## Verified refs and relationships

“Ahead/behind” below is GitHub's comparison against the pinned `master` commit,
not a measure of feature completeness. Branch names can move after this survey.

| Ref | Verified commit | Relationship to master | Relevant source changes |
| --- | --- | --- | --- |
| `master`, `main` | `16b0b4cac70433e4118038dfbfd304e22d1d9018` | Same commit | Current default baseline; legacy protocol-based parameters and evocomputing colors |
| `tolmaster` | `4f7a82eab41862f80e43d91a8c2530b00aa51925` | 21 ahead, 0 behind | Parameter records, thi.ng color migration across effects/Push/web, typed byte access, scheduling and movement changes |
| `param-as-record` | `6960bde5c9afcd6d4ac34974fdce32e449f25cc9` | 6 ahead, 0 behind | `Param` record, instance checks, parameter constructors, oscillator parameter rewrite |
| `color-lib-swap-thing` | `bce37e4132f2cb1ba219913cbd82d489759b8d53` | 9 ahead, 0 behind | thi.ng color type and conversions; changes in effects, cue helpers, Push 2, web control, and utility code |
| `buffer-set-optimizations` | `4314219c332dae34ec5742b395f4ac0d9447aebb` | 2 ahead, 0 behind | `aset-byte` writes and `^bytes` hints for DMX clearing/sending |
| `frame-interval-calc-fixes` | `e44d9e75d376ea746af2a6ad78d0ef23f22da7b9` | 2 ahead, 0 behind | Different frame sleep/snapshot calculation and related show changes |
| `current-rotation-fixes` | `97291c2828dd67503bc6c7f483467ca84977fff6` | 2 ahead, 0 behind | Changes the math reconstructing current pan/tilt rotation |
| `redesign-oscillators` | `3dbc68cd67cce667d8825475b32b2d4c7ebf6096` | 0 ahead, 808 behind | Historical ancestor of master; the branch name does not indicate an unmerged modern oscillator implementation |
| `websocket-attempt` | `697ca66f379e082f3497bf850f91880d25be2c14` | 0 ahead, 1551 behind | Historical ancestor of master; it does not establish a working contemporary streaming web control layer |

The five current feature branches above are each **diverged from `tolmaster`**,
with master as their common ancestor. Their commits are not ancestors of
`tolmaster` according to the comparisons. Matching source changes nevertheless
appear in `tolmaster`: its oscillator file is byte-identical to the one on
`param-as-record`; its Push 2 and color-effect files are byte-identical to those
on `color-lib-swap-thing`; its movement file is byte-identical to the one on
`current-rotation-fixes`. Its show/channel sources also contain the byte-buffer
and timing changes. Do not blindly merge or cherry-pick these branches on top of
`tolmaster` as if their behavior were absent.

Comparison sources: [master to tolmaster][compare-tolmaster],
[master to parameter records][compare-param],
[master to color migration][compare-color],
[master to buffer changes][compare-buffer],
[master to timing changes][compare-timing], and
[master to rotation changes][compare-rotation].

## What Tolglow actually expects

At the pinned Tolglow commit, `tolglow/param.clj` and `tolglow/fx.clj` import
`afterglow.effects.params.Param` and construct it directly. `param.clj` uses
`params/color-type`, including in color binding and its custom color parameter
factories. `tolglow/color.clj` uses `thi.ng.color.core.HSLA` as its color type and
references `afterglow.effects.params.Param`. These are concrete dependencies,
not just comments about a possible future migration.

| Contract | Afterglow master | `param-as-record` | `color-lib-swap-thing` | `tolmaster` |
| --- | --- | --- | --- | --- |
| `Param` record class | Absent | Present | Absent | Present |
| Defined `params/color-type` | Absent | Absent, although `param?` references it | `thi.ng.color.core.HSLA` | `thi.ng.color.core.HSLA` |
| `resolve-param` evaluates parameters returned by parameters | Recursively | Recursively | Recursively | Evaluates once |
| thi.ng color requirements in `params.clj` | Absent | Absent | Present | Present |
| Oscillated parameter constructor | Reified `IParam` | `Param` record | Reified `IParam` | `Param` record |

The standalone `param-as-record` source has an unresolved-contract concern:
`param?` refers to `color-type`, but the inspected complete namespace contains no
definition or declaration for it. Its presence in `tolmaster` resolves this
particular source mismatch. This branch has not been loaded independently in
this task, so the table is a source audit rather than a runtime diagnosis.

The `tolmaster` `resolve-param` implementation calls `evaluate` once even though
its retained docstring still describes recursion. A parameter returning another
parameter therefore has different semantics from master. Before making this a
new architecture contract, capture whether the current show depends on nested
parameters and write explicit tests for the intended behavior.

Sources: [Tolglow parameter imports and bindings][tolglow-param],
[Tolglow effect imports][tolglow-fx], [master parameters][master-params],
[record branch parameters][record-params], [color branch parameters][color-params],
and [`tolmaster` parameters][tolmaster-params].

## Existing performance and device work to retain or evaluate

- **Typed DMX writes already exist in the fork.** The buffer branch replaces
  the three `aset` calls in `apply-channel-value` with `aset-byte`. It also adds
  `^bytes` bindings at `Arrays/fill` and `ByteString/copyFrom`. These changes are
  present in `tolmaster`. They target byte-array access/overload selection; no
  benchmark in this investigation establishes their speedup. Preserve byte
  parity for coarse/fine channels and inversion when evaluating them.
- **Parameter representation and dispatch have already been explored.** The
  record branch and `tolmaster` use a shared `Param` record with evaluation and
  resolution functions. `param?` uses concrete instance checks in place of the
  earlier protocol satisfaction check. Comments claim performance benefits,
  but those comments are not measurements. Record construction still accepts
  closures, so this does not itself make stateful effects pure.
- **Oscillator parameters already use the record constructor.** `tolmaster`
  retains fixed/dynamic oscillator paths and creates `Param` values in
  `build-oscillated-param`. The rewrite centralizes evaluation/presolve closures;
  it does not implement a keyword DSL, GPU compilation, or a stateless renderer.
- **Color migration affects both output and controls.** The color branch and
  `tolmaster` use thi.ng conversions in effect output, Push 2 pad colors and
  graphics, cue-variable display, web colors, and color editing. This is wider
  than swapping a type alias. Unit ranges, rounding, alpha, RGBW emission,
  arbitrary emitter hues, and palette equivalence need tests against the active
  rig. Do not overwrite these changes by rewriting only master's driver.
- **Timing is observably different.** Master uses a minimum 1 ms sleep and
  computes the next snapshot at `ended + sleep-time`. The timing branch and
  `tolmaster` permit zero sleep, skip `Thread/sleep` when zero, and compute the
  next snapshot at `ended + refresh-interval`. Changing the reference instant
  can affect beat phase and load statistics; inspect recorded traces before
  treating a branch called “fixes” as a correctness oracle.
- **Preview/aiming reconstruction also differs.** `current-rotation` on the
  rotation branch and `tolmaster` uses half-pi scaling for the normalized pan
  and tilt deltas, and changes the tilt expression. Its docstring alone does
  not establish calibration correctness. Verify known fixture orientations and
  the relationship between forward and inverse transforms before migration.

Source anchors: [typed channel writes][typed-channel],
[typed show buffers][typed-show], [oscillated parameter construction][record-lfo],
[thi.ng effect output][thing-color], [Push color conversion][push-color],
[web conversion][web-color], [frame scheduling][timing-source], and
[current rotation][rotation-source].

These are existing CPU/data-model changes. The inspected branch differences do
not provide the requested GPU backend, functional frame coordinator, shared
semantic control layer, or Push 3 support. Existing parallel rendering in master
also remains relevant: none of these branches means threading should be added
from scratch.

## Dependency and baseline limitations

The pinned `tolmaster` `project.clj` is byte-identical to the pinned master's
manifest (blob `bfd6233b9c3b8e5024a508ad693cd8e0c7fb3083`). It does not declare
thi.ng color/math dependencies even though `tolmaster` requires those namespaces.
It retains the historical Beat Link `8.0.0-SNAPSHOT` dependency. Tolglow's new
`deps.edn` establishes a test lane, not a complete application manifest. Tolglow
also references Quil and `taoensso.timbre.profiling`; their compatible versions
and the former local development profile have not been reconstructed here.
See [the pinned manifest][tolmaster-project] and [testing.md](testing.md).

The verified Tolglow legacy test run used **master** plus the documented test
dependency override, and passed 20 tests / 103 assertions. It exercises real
iterator helpers, declarative fixture construction, channels, geometry helper
inputs, calibration, and strobe definitions. It never requires Tolglow's full
parameter/effect/color application tree. Consequently it can pass while the
actual Tolglow show cannot load against master.

Those tests are a useful stable fixture/API baseline, but they are **not the
behavioral baseline of the user's modified renderer or complete show**. They do
not protect `tolmaster`'s one-step parameter resolution, thi.ng output, changed
clock scheduling, altered rotations, or Push/Web color behavior. The additional
Afterglow characterization tests are likewise evidence for their selected
checkout and dependency set, not automatic coverage of unexecuted branches.

Once the active branch is known:

1. Record both repository SHAs and reconstruct a reproducible application
   dependency manifest, including actual color/graphics/profiling dependencies.
2. Run the existing narrow tests on that branch in an isolated checkout. Keep
   any failures visible and classify genuine intended changes before changing
   assertions. Preserve the default-baseline evidence for comparison.
3. Add branch-specific characterization for nested parameters, record and
   protocol parameters, static/dynamic oscillator resolution, color units and
   RGBW buffers, calibration/rotation, and frame snapshot timing.
4. Capture representative cue/effect traces and rendered DMX buffers from the
   active fork, with output captured rather than sent to hardware. Use these
   traces as the oracle for the functional-runtime migration.
5. Benchmark the active fork with these existing CPU changes before proposing
   new SIMD/GPU speedups. Include allocations, latency distributions and output
   equivalence, not only the inner arithmetic loop.

[compare-tolmaster]: https://github.com/tolgraven/afterglow/compare/16b0b4cac70433e4118038dfbfd304e22d1d9018...4f7a82eab41862f80e43d91a8c2530b00aa51925
[compare-param]: https://github.com/tolgraven/afterglow/compare/16b0b4cac70433e4118038dfbfd304e22d1d9018...6960bde5c9afcd6d4ac34974fdce32e449f25cc9
[compare-color]: https://github.com/tolgraven/afterglow/compare/16b0b4cac70433e4118038dfbfd304e22d1d9018...bce37e4132f2cb1ba219913cbd82d489759b8d53
[compare-buffer]: https://github.com/tolgraven/afterglow/compare/16b0b4cac70433e4118038dfbfd304e22d1d9018...4314219c332dae34ec5742b395f4ac0d9447aebb
[compare-timing]: https://github.com/tolgraven/afterglow/compare/16b0b4cac70433e4118038dfbfd304e22d1d9018...e44d9e75d376ea746af2a6ad78d0ef23f22da7b9
[compare-rotation]: https://github.com/tolgraven/afterglow/compare/16b0b4cac70433e4118038dfbfd304e22d1d9018...97291c2828dd67503bc6c7f483467ca84977fff6
[tolglow-param]: https://github.com/tolgraven/tolglow/blob/01146ece7731ec06aa1ee6409342373096c8e0dc/tolglow/param.clj#L27
[tolglow-fx]: https://github.com/tolgraven/tolglow/blob/01146ece7731ec06aa1ee6409342373096c8e0dc/tolglow/fx.clj#L33
[master-params]: https://github.com/tolgraven/afterglow/blob/16b0b4cac70433e4118038dfbfd304e22d1d9018/src/afterglow/effects/params.clj#L95
[record-params]: https://github.com/tolgraven/afterglow/blob/6960bde5c9afcd6d4ac34974fdce32e449f25cc9/src/afterglow/effects/params.clj#L53
[color-params]: https://github.com/tolgraven/afterglow/blob/bce37e4132f2cb1ba219913cbd82d489759b8d53/src/afterglow/effects/params.clj#L97
[tolmaster-params]: https://github.com/tolgraven/afterglow/blob/4f7a82eab41862f80e43d91a8c2530b00aa51925/src/afterglow/effects/params.clj#L55
[typed-channel]: https://github.com/tolgraven/afterglow/blob/4f7a82eab41862f80e43d91a8c2530b00aa51925/src/afterglow/effects/channel.clj#L39
[typed-show]: https://github.com/tolgraven/afterglow/blob/4f7a82eab41862f80e43d91a8c2530b00aa51925/src/afterglow/show.clj#L170
[record-lfo]: https://github.com/tolgraven/afterglow/blob/4f7a82eab41862f80e43d91a8c2530b00aa51925/src/afterglow/effects/oscillators.clj#L357
[thing-color]: https://github.com/tolgraven/afterglow/blob/4f7a82eab41862f80e43d91a8c2530b00aa51925/src/afterglow/effects/color.clj#L91
[push-color]: https://github.com/tolgraven/afterglow/blob/4f7a82eab41862f80e43d91a8c2530b00aa51925/src/afterglow/controllers/ableton_push_2.clj#L233
[web-color]: https://github.com/tolgraven/afterglow/blob/4f7a82eab41862f80e43d91a8c2530b00aa51925/src/afterglow/web/routes/show_control.clj#L47
[timing-source]: https://github.com/tolgraven/afterglow/blob/4f7a82eab41862f80e43d91a8c2530b00aa51925/src/afterglow/show.clj#L310
[rotation-source]: https://github.com/tolgraven/afterglow/blob/4f7a82eab41862f80e43d91a8c2530b00aa51925/src/afterglow/effects/movement.clj#L58
[tolmaster-project]: https://github.com/tolgraven/afterglow/blob/4f7a82eab41862f80e43d91a8c2530b00aa51925/project.clj
