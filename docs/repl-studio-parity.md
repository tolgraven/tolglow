# Using the same show from the REPL and Studio

When a performer can do something to a show, they should be able to find and do it from both the running REPL and Studio. A cue pad and a Clojure form can feel quite different while addressing the same cue, parameter or instance. This is a standing review agreement under R94. The examples below cover part of the show. [Request register](request-coverage.md) · [GitHub Issues](https://github.com/tolgraven/tolglow/issues)

## Try the round trip with Ember

Open **Perform** in a preview show with the **Night studies** demo page. In the header, **Open Clojure REPL** opens a trusted session with this show's handle bound to `runtime`. In an external JVM REPL, use the handle returned when that same application started. [Studio controls](studio-ui.md) · [REPL access and session bindings](web-repl.md)

Run the following forms individually. An accepted launch returns the ID of the running instance. The **Ember** card in Perform belongs to that instance. The unfired pad in the cue grid represents its definition.

```clojure
(require '[tolglow.show :as show])

(def launched
  @(show/submit! runtime {:op :cue/launch :cue-id :ember
                          :launch {:mode :persistent}}))

(select-keys launched [:status :instance-id])
```

On the active card, move **Level**. Read the result in the REPL, then send a value back and watch the same control update.

```clojure
(defn active-ember []
  (some #(when (= (:id %) (:instance-id launched)) %)
        (:instances (show/snapshot runtime))))

(get-in (active-ember) [:params :level])

@(show/submit! runtime {:op :param/set :instance-id (:instance-id launched)
                        :param :level :value 0.35})
```

Use **Release** on the card or submit the form below. The release envelope can keep the instance visible while it fades. To start again from Studio, select **Toggle** and tap the idle **Ember** pad. Its new running instance has a new ID. [Demo definitions](../src/tolglow/demo.clj) · [Studio command handling](../src/studio/tolglow/studio/events.cljs) · [Runtime snapshots and command submission](../src/tolglow/show.clj)

```clojure
@(show/submit! runtime {:op :instance/end :instance-id (:instance-id launched)})
```

## View recent cues

In **Perform**, the **Recent cues** tab puts the latest successful launch in
the top-left pad. The page holds up to 64 distinct cues and updates when a cue
launches. Launch a cue from the REPL with the same command shown above and
compare the tab with the committed page in the snapshot.

```clojure
(:recent-page (show/snapshot runtime))
```

## Suspend automatic section patterns

When the show starts with an enabled section pattern policy, open **Perform**
and find **Section patterns** above the live controls. On a narrow screen, first
select **Live**. **Suspend** and **Resume** send the shared show settings
operation. **Override active** means the show has committed the suspension.
**Override off** allows automatic patterns when the policy's timing and input
conditions are met.

After clicking **Suspend**, read the committed setting in the running REPL.
Clear it there and verify that the open Studio view returns to **Override off**.
Use `true` in the same command to suspend patterns from the REPL.

```clojure
(require '[tolglow.show :as show])

(get-in (show/snapshot runtime) [:settings :section-patterns :override?] false)

@(show/submit! runtime :settings
               {:scope :section-patterns :values {:override? false}})
```

## Reconnect a local track reference

Supply the local mpv socket and track catalog in the host's `:section-reference`
configuration. The catalog uses canonical absolute track paths and the
configured profile's identity and duration.

```clojure
;; Add this to the show options alongside the enabled :section-patterns policy.
{:section-reference {:profile profile
                     :provenance {:source-id :mpv :session-id (random-uuid) :clock-id :show-monotonic}
                     :mpv {:socket-path "/home/operator/.local/run/mpv.sock"
                           :catalog {track (select-keys profile [:reference :duration-ms])}}}}
```

After player reconnect, show restart or restore, open **Connections → Track
reference** and choose **Reconnect mpv**. The status distinguishes unavailable,
acquiring, aligned, ambiguous and stale. A fresh session waits for matching
observations from the configured player. **Patterns suspended** reports the current manual
override, which reconnect preserves.

```clojure
(require '[tolglow.show :as show])

(show/section-reference-status runtime)
(let [{:keys [runtime-id epoch]} (show/section-reference-status runtime)]
  (show/reconnect-section-reference! runtime {:runtime-id runtime-id :expected-epoch epoch}))
```

The REPL and Studio receive the fresh reference session ID or a refusal from
the same show operation. For manually owned mpv connections, use the existing
[REPL adapter flow](../src/tolglow/integrations/README.md#observe-a-known-local-recording-in-mpv).

## Reviewing a show feature

- **Find it from either side.** Name the view, control label and public REPL entry point. Include the shared command, input units, identifiers and scope so an operator who knows one surface can recognize the other.
- **Try both directions.** Edit through Studio and inspect committed show data from the REPL. Edit through the REPL with Studio open and check that its view updates. For a read-only feature, compare the two readings for the same show and instance.
- **Check a refusal.** Use a stale instance or an invalid input for the changed operation. Inspect the REPL acknowledgement and Studio's failure feedback. Verify that the last committed state is still observable. A local slider draft can move before a server acknowledgement arrives.
- **Name the boundary.** Explain a gesture or display affordance that belongs to one surface and identify the underlying show behavior. For example, Studio handles a cancelled finger press by releasing the instance that press launched. At the REPL the operator directly addresses that instance with `:instance/end`. Private rendering helpers are outside this operator workflow.
- **Track a missing path.** Give the gap a domain owner, GitHub issue and an observable acceptance check. Label a gap open until both paths have been exercised. Update the operator guide when the feature lands.

## A tracked gap

The running REPL can discover and restore the current show's working, named, and prior-session checkpoints through `show/recovery-status` and `show/load-latest-show!`. `show/discard-prior-recovery!` dismisses the prior-session checkpoint. Studio's checkpoint controls show the source and modification time and report the owner's restore or discard result. [Operator examples](show-files.md#working-recovery-and-manual-checkpoints) · [Studio checkpoint controls](https://github.com/tolgraven/tolglow/pull/563)

The tethered Push saved-show browser previews the latest named and working recovery checkpoints. With explicit checkpoint and Confirm buttons, the device confirms the file shown on the display through the shared import owner, shows progress or refusal, and reopens in a fresh session after a committed restore. Verification uses virtual MIDI and captured Push 2 and Push 3 displays. Browsing registered effects, cues, and waveforms and applying their actions remain tracked in the catalog issue. [Push controls](push-display-packaging.md#browse-saved-shows) · [Saved-show browser](https://github.com/tolgraven/tolglow/pull/581) · [Issue #552](https://github.com/tolgraven/tolglow/issues/552)

At revision `0681a694`, Studio's effect keyframe editor offers **Sample at a fixed interval** and **Limit automatic slowdown** for an individual effect. The runtime also accepts a show-wide scheduler setting and exposes timing diagnostics. In a preview show, this REPL command disables automatic keyframes for the show. The public status function reads the current diagnostic state when one exists.

```clojure
(require '[tolglow.effects.cadence :as cadence])

@(show/submit! runtime :settings
               {:scope :adaptive-keyframes :values {:enabled? false}})
(cadence/status runtime)
```

The global toggle and ranked timing view were missing from Studio at that revision. The **Perceptual scheduling and interpolation** owner tracks this in [issue #307](https://github.com/tolgraven/tolglow/issues/307). Close the gap when an operator can find and change the global policy in Studio, inspect the same committed setting in the REPL and compare diagnostics for the same show. [Keyframe editor](../src/studio/tolglow/studio/keyframe_editor.cljs) · [Scheduler behavior tests](../test/tolglow/predictive_keyframes_test.clj) · [GitHub Issues](https://github.com/tolgraven/tolglow/issues)
