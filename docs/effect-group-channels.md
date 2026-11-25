# Active effect group channels

Tolglow's group channel model supplies stable group IDs, committed levels and
active member lists for mixer clients. Manual grouping uses a tree. A cue
belongs to one manual group, and an active instance can be assigned to a
different group for its lifetime. Each instance is subject to the levels of
its assigned group and that group's ancestors. [Issue #553](https://github.com/tolgraven/tolglow/issues/553)

One auto overlay can be active alongside the manual tree.

- The **Fixtures** overlay matches any declared target or its fixture parent on the current
  patch. The target estimate intersects the effect definition, each output
  and the launch target selection. The first match controls the whole instance,
  including its output on other fixtures.
- The **Categories** overlay uses the same captured definition categories as the shared
  effect view. A group can match any or all of its listed categories.
- The **Cue pages** overlay matches any authored page containing the cue. A cue can occupy
  several pages. The generated Recent page is outside this overlay.

Overlay groups are evaluated in authored order. Every match appears in the
projection, with the first identified as the winner. Only the winner contributes
a level, including when that level is zero or unity. Changing the active
overlay retains the previously configured levels. Releasing instances remain
members until they finish. A new instance receives its cue's authored manual
placement.

When a native transform inherits targets, the fixture projection reports
`:target-source :unresolved` unless the caller supplies its footprint. Pass
`:target-ids-by-instance` with footprints from the same patch and frame before
group attenuation. An empty collection is a known empty footprint. The live
owner uses declared targets for the current patch. The frame after a definition
reload uses the newly committed definition for membership.

## Render contract

Multiply the manual ancestor levels and winning overlay level into one member
factor. Apply it at the existing instance-opacity boundary after evaluating the
effect. In an ordinary contribution the result affects the opacity of its
assignment or batch blend. In an authored native layer the result affects
member opacity after the member's own transforms. Subsequent members read that
result. The authored layer opacity still mixes the final layer with its
backdrop at the layer's original priority position. Group levels are additional
to the existing instance alpha and release envelope. [Afterglow native layers](https://github.com/tolgraven/afterglow/blob/99f61c5bed2850daaeb87bc5b84ddf0d3c60bf64/doc/architecture/native-layers.md)

The live owner captures frame factors before evaluating effects. The factors
reach the renderer through Afterglow's `:afterglow.runtime.engine/opacity-factors`
input, leaving authored parameters and observable instance alpha independent.
Spatial cue masks retain the factor when they replace a leader's native member
opacity. Output shaping, budgeting and publication continue after composition.

At unity, the mixer has no render factor to apply. Its membership and display
order are independent of render order. The renderer test places an independent
write between two mixer members. Unity retains the original contribution
order and DMX bytes. The half-level case resolves to `0.625`. Putting the same
two members into a native layer at unity resolves to `0.5` because the layer
anchors at its last member. A second test measures `0.5` at member opacity and
`0.4375` with the same factor applied after the composite. [Renderer tests](../test/tolglow/effects/group_channels_test.clj)

## Domain API

Use `tolglow.effects.group-channels/apply-operation` with a show model and one of
these commands. The return includes `:model`, `:result` and `:events`. Level
values range from zero to one and default to unity. `:configure` replaces the
group document. `:overlay-set` takes a configured kind or nil.

```clojure
{:op :effect-groups/configure
 :config {:manual [{:id :stage :name "Stage"
                    :groups [{:id :washes :cue-ids [:welcome-wash]}]}]
          :active-overlay :fixtures
          :overlays {:fixtures [{:id :front :fixture-ids [:front-bar]}]}}}
{:op :effect-groups/level-set :channel-id [:manual :washes] :value 0.75}
{:op :effect-groups/assign-instance :instance-id "0:1" :group-id :stage}
{:op :effect-groups/overlay-set :overlay nil}
```

`projection` returns the shared instance order, manual ancestry, overlay
candidates and winner, committed levels and non-unity member factors. Pass
the result to `channel-rows` with `:manual` or `:overlay` and a set of expanded
channel IDs. Authored group rows stay visible when empty so a controller can
retain their identities across launches and releases. The caller owns
expansion state. `[:instance id]` addresses the existing instance controls.
[Domain source](../src/tolglow/effects/group_channels.clj)

For a running headless show, submit the same commands with
`tolglow.show/submit!`. The receipt follows the shared show epoch, expected
revision and command ID rules. Read the committed view with
`tolglow.show/effect-group-channels`.

Full show files save the group settings and instance placements. Restore
validates both, retains the last committed output, and rekeys instance IDs for
the new show epoch. Removing a manual group also clears its instance placements.
The standalone test saves a mixed frame to disk, changes its level, restores
the saved frame and checks the next rendered frame at the restored settings.
[Live and standalone tests](../test/tolglow/effects/group_channels_host_test.clj) · [Show files](show-files.md)

Push and X-Touch routing and hardware feedback are later integration steps.
