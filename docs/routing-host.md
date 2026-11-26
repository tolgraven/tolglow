# Signal routing in the running studio

The patch bay now runs in the actual owner frame, before parameter modulation and effect evaluation. `tolglow.routing` captures input, calls the pure `tolglow.integrations.patch` graph, then submits resulting `:param/set` commands through the studio's common command reducer. The graph itself opens no devices. The owner still renders once and decides whether to send physical output.

## Admission and source discovery

MIDI input passes through the synchronization adapter's normal byte decoder. Its raw `:sync/observation` events keep ordered note-on/note-off transitions, even when both arrive between render frames. MIDI source identities include transport, device/source identity, channel, and note/controller, for example `midi/midi-clock/ch/1/note/60`. Live playback uses the distinct `live/...` prefix and retains its playback origin for feedback suppression.

The existing audio analyzer's feature callback provides `audio/rms`, `audio/peak`, `audio/kick-rms`, and `audio/energy`. These are measured analyzer values, not generated demonstrations. Named [instrument stems](stems.md) add `stem/<id>/level` and `stem/<id>/onset` ports per stem; while the transport is stopped their level ports are not fed and their onset ports stay closed. Repeated feature timestamps are coalesced. Source inventory range hints are `[0,127]` for MIDI and `[0,1]` for audio/control; every saved graph has its own explicit calibration range. Audio values above the hint remain intact until the graph's range mapping clamps them.

At owner admission, the callback's physical `:observed-at-ms` stays unchanged and the owner supplies the current show `:epoch` and authoritative musical `:clock`. Callback closures belong to the epoch in which they were created. Import/restart invalidates old closures. Source discovery works while routing is disabled, so a user can first operate a controller, then select its learned source in the editor.

Inventory is bounded to 128 recently observed sources, pending input to 1,024 observations, and diagnostics to 64 entries. The existing sync callback mailbox is independently bounded to 256 events; it reports failed enqueue rather than blocking a device thread. Only configured, enabled sources enter the routing pending queue. The host frame processes input after queued show commands and composition/auto launches, so bindings see the current active instances.

## Configuration and command ownership

`POST /api/patch/save` takes `{:source "<one EDN graph>"}`. Saving validates the graph and resolves destination cue/parameter identities against the registry, including pinned effect versions. Keyword/string identities are resolved only when they identify an existing object; source port strings stay strings. Destinations currently target numeric or integer parameters and must fit the declared parameter bounds. Integer destinations use integral endpoints and round writes to an in-bounds integer.

Saving a new graph does not enable it. `POST /api/patch/enabled` takes `{:enabled? true|false}`. Changed graphs need an increasing revision. An identical save preserves progress. Save/toggle boundaries clear captured gates and queued input, so enabling requires fresh input. Disabling leaves the last canonical parameter value in place. A new gate-off without a captured gate-on does not attach itself to a newly launched cue.

The model field is:

```clojure
{:routing {:spec canonical-patch
           :state captured-pure-patch-state
           :enabled? false
           :generation 0
           :ports {} :pending []
           :last-audio-time nil :diagnostics []}}
```

`:generation` advances on graph replacement or toggling and participates in command IDs. Reusing a patch's local frame counter cannot collide with an earlier activation's command receipts. Commands carry `:source {:type :patch ...}` and use the same validation/history path as human controls. Patch writes do not create a manual modulation hold. Human edits retain their existing hold behavior, and overlays still apply after the canonical routed parameter value.

The latest running instance of a logical cue is available for new gates. A captured release tail belongs to its original instance; a replacement launch cannot inherit it. Rejected writes produce diagnostics and clear that destination's committed-value cache so a later frame can retry. A failed effect/render frame retains the owner's previously admitted pending queue for a later attempt.

## Save, restore, clock changes, and projection

Schema-2 show files include the complete graph, progress, queue, input receipts, inventory, and generation. Import validates the field and remaps captured activation IDs and queued epochs to the restored show. Pending observations keep their event IDs, preventing duplicate application. Millisecond input timestamps remain physical metadata.

The shared [clock rebasing helper](clock-domains.md) shifts routing envelope/source/route beat anchors, queued observation beats, and inventory clock beats on source changes or seeks. Millisecond coordinates do not move. Accepted pre-loop input can have a negative historical beat after a loop to zero; it is applied at the next available owner coordinate, retains its original historical clock for timeout accounting, and reports `:late-sample-clamped`. Ordinary advancing frames do not rebase again.

`GET /api/patch` and regular studio snapshots expose `{:enabled? ... :spec ... :source "..." :sources [...] :values [...] :diagnostics [...]}`. Each source has `:source-id`; destination values are maps with `:destination-id`, `:instance-id`, and `:value`, avoiding JSON maps with vector keys. Regular polling includes this projection. No transport is started by reading it.

## Software ingress and checks

A trusted REPL session can feed the same bounded mailbox without opening a device:

```clojure
(def patch-input (tolglow.show/sync-callbacks runtime))
((:events! patch-input)
 {:type :sync/observation
  :observation {:source-id "software/demo/level"
                :event-id "gesture-1" :kind :control
                :value 0.75 :gate? true :confidence 1.0 :origin :software
                :observed-at-ms (:time-ms (tolglow.show/snapshot runtime))}})
```

Recreate the callback after show import/restart and use a new event identity for each observation. Scheduled studio frames consume the input; tests call `show/tick!` explicitly.

`tolglow.studio.routing-host-test` covers real decoded MIDI bytes through an injected transport, actual analyzer feature callbacks, release tails, restored activation IDs, stale callbacks, graph validation, re-enable receipt identity, bounded admission/inventory, rejected-write retries, queued historical input, and repeated source/seek changes. Its eight tests contain 88 assertions. These are executable host checks with no physical MIDI/audio/lighting device. They do not claim real-device latency, audio DSP quality, or an installed Live pairing.
