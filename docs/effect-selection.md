# Effect Solo and Mute

The show owner applies Solo and Mute through the shared Afterglow control
model. Each command gets the normal show epoch, revision and retry receipt.
[Issue #532](https://github.com/tolgraven/tolglow/issues/532)

## Selection policy

- Solo and Mute each default to exclusive. Configure either control independently.
  Changing a default affects subsequent actions.
- An exclusive action selects exactly its current targets. Repeating the action
  when exactly those targets are selected clears that control.
- An additive action adds the targets when any are missing and removes them
  when all are selected. Send `:additive? true` with the toggle to use additive
  mode for that action. The owner receives a fresh modifier value on every
  action. A held key or pad lives in the input adapter's session.
- The selection gate includes every unmuted instance when no instance is soloed.
  With Solo active, it includes only soloed instances that are unmuted. If every
  soloed instance is muted, the selection gate includes zero instances. A row
  suppressed by both controls shows Mute as the reason.
- Category actions use the shared category selectors. A compound action can use
  the native layer ID, intersected with the other selector dimensions. The
  action captures the layer's active instance IDs at that time. Empty matches
  leave the current selection in place. Stale explicit instance or layer IDs
  reject the action.
- Flags travel with running instances, including release tails. Removal or
  expiry drops the flags with the instance. A new launch starts clear. Schema 2
  restores saved selections and rekeys their instance and native layer IDs.

The frame engine applies selection before composing the ordered contribution
stream. The gate covers ordinary assignments, typed batches and native layer
transforms. Suppressed effects continue to advance state, private clocks and
intents. When a release expires at a frame boundary, that instance leaves the
selection before the frame is composed.

Selection is independent of authored parameters, opacity, gain, pause and show
blackout. `:included-instance-ids` is eligibility under this policy. The
projection follows render input order and reports Solo, Mute, suppression reason,
and native layer per instance. Layer rows include member IDs and `:none`,
`:partial`, or `:all` for each control.
[Shared controls](system-controls.md) · [Native layers](native-composition.md)

## Domain API

Pass these payloads to `tolglow.show/submit!`. A caller holding an immutable show
model can pass the same payload and a command envelope to
`afterglow.control.model/apply-command`. Inspect `:effect-selection` in
`tolglow.show/snapshot`, the Studio state endpoint, or `afterglow.control.views/snapshot`. Call
`afterglow.control.effect-selection/projection` on the model.

```clojure
{:op :effect-selection/configure :modes {:mute :additive}}
{:op :effect-selection/toggle :control :solo
 :selector {:instance-ids ["0:1"]}}
{:op :effect-selection/toggle :control :solo
 :selector {:layer-id layer-id} :additive? true}
```

The toggle acknowledgement lists targeted, changed, and selected instance IDs
in render input order. A state change emits one event with the same fields.
Retrying a command ID with identical data returns its original acknowledgement.
Invalid inputs return a structured rejection.

## Replay and remaining parity checks

The previous running-effect controls provide Pause and Zero/Restore. The
related Push proposals cover effect display and grouping, parameter gestures,
and category filtering. Solo and Mute reuse the existing category selectors.
[Studio controls](studio-control-layouts.md) · [#469](https://github.com/tolgraven/tolglow/issues/469)
· [#470](https://github.com/tolgraven/tolglow/issues/470) · [#471](https://github.com/tolgraven/tolglow/issues/471)

The JVM replay launches three simultaneous effects with levels `0.50`, `0.25`,
and `0.75` in control order `0:2`, `0:1`, `0:3`. It captures the projection
after each toggle and checks the same ordered parameter, system, and effect
state at every step. A show owner renders the same sequence in both CPU modes.
The Blend fixture receives all eligible effects in priority and launch order.

| Action | Included instance IDs | Wash, Side, Accent, Blend DMX bytes |
| --- | --- | --- |
| Initial state | `0:2`, `0:1`, `0:3` | `64, 128, 191, 191` |
| Solo `0:1` | `0:1` | `64, 0, 0, 64` |
| Add Solo `0:2` | `0:2`, `0:1` | `64, 128, 0, 64` |
| Mute `0:2` | `0:1` | `64, 0, 0, 64` |
| Add Mute `0:3` | `0:1` | `64, 0, 0, 64` |
| Solo `0:2` | None | `0, 0, 0, 0` |
| Clear Solo `0:2` | `0:1` | `64, 0, 0, 64` |
| Mute `0:3` exclusively | `0:2`, `0:1` | `64, 128, 0, 64` |
| Clear Mute `0:3` | `0:2`, `0:1`, `0:3` | `64, 128, 191, 191` |

The tests also start a real macro, exercise removal and release expiry, and
round-trip a selected layer through an in-memory Transit save and restore.
They submit selection commands through an unscheduled show owner and verify
receipts, duplicate delivery, layer rekeying, and the parameter values after
restore. Commands with `:additive? false` and with the field omitted verify
that a preceding additive action has no effect on later commands.
[Replay and lifecycle tests](../test/tolglow/effects/selection_test.clj)

Studio Perform provides Solo and Mute for individual running effects, category
filters, and compound layers. Use Shift for an additive click or Add once on a
touchscreen. The header exposes both defaults and each row displays its own
selection and suppression state. Push bindings, modifier release, and device
disconnect still need controller verification.
[REPL and Studio parity](repl-studio-parity.md) · [Issue #532](https://github.com/tolgraven/tolglow/issues/532)
