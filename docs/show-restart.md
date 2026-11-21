# True show restart

`{:op :show/restart}` is an owner command with a new show epoch. It now clears both Afterglow instances and Tolglow's active performance ownership. It is different from importing a saved execution, which intentionally restores captured progress.

On a successful restart, the studio:

- Retains registry definitions, cue pages, rig, composition definitions, and user settings.
- Clears composition runs/tokens, disables auto mode, and drops its scheduling/ownership state while retaining its configuration.
- Clears modulation controllers and human holds. Manual controller configurations are retained under `[:settings :modulation :saved-controllers]`; modulator cue definitions remain in the registry. Neither starts itself again.
- Keeps response definitions visible with `:enabled? false` and a fresh unused envelope state. Disabled responses are excluded from middleware; explicitly submitting a response starts it again.
- Retains the routing graph, disables it, advances its generation, and clears captured gates, release tails, input queues, learned observations, and audio timestamp coalescing state. Fresh input is required after explicitly enabling it again.
- Clears captured audio/sync variables, controller leases, and active device ownership. Device closure happens after publication outside the frame lock.
- Rejects remaining queued commands from the previous epoch, including stale prepared imports and callback closures. A failed/retried restart command cannot repeat cleanup in the new epoch.

The same REPL, agent, fixture-service, and reactive managers remain alive. REPL namespaces, scratch text, and local bindings survive. Existing graph definitions remain registered. Agent tool commands retain their captured epoch and cannot use old authorization to modify the new show. Fresh human input or newly emitted registered reactive actions may deliberately launch new cues.

## Active input handoff

The owner first advances the connection generation and invalidates synchronization input/IO tokens. That publication step performs no device calls, worker joins, callback invocation, or transport waits. Old audio/sync callback wrappers immediately reject delivery.

After the owner releases the frame lock, `connections/rebind-restart!` replaces callbacks on the captured active input runs. Audio switches callbacks at the next read/block boundary and keeps its open source, sample counter, and DSP state. An in-flight block retains its old callback ownership and is discarded at the host boundary. Synchronization serializes a rebind marker on its event worker, discards old queued input, resets captured Live held/recording bookkeeping, and then admits new input. It keeps its physical transports, selected clock, settings, and wanted flags.

Run identities and connection generations both matter. A delayed rebind cannot replace a new run started after restart; a delayed stop carries its captured run identity and cannot close a newer run. Restore, a subsequent restart, and host close invalidate an outstanding rebind. A concurrent explicit stop clears the wanted flag, so restart completion cannot undo it. None of these operations replays a saved MIDI gate.

Queued synchronization IO checks its generation before execution, and old in-flight responses cannot re-enter the event queue. A remote write already in progress cannot be rolled back by changing ownership; the host does not claim transactional cancellation of an external device or LiveAPI request.

## Verification

`tolglow.studio.restart-host-test` covers an active composition, auto mode, modulation/holds, response, routing, and a live REPL local across restart; rejection of a same-frame stale cue launch; a blocked MIDI callback plus queued old input; a blocked audio read and block-boundary handoff; unchanged open run identities; preserved wanted flags; inactive settings through save/restore; and stale stop/rebind operations after ownership changes. Injected source closure checks confirm it runs outside the host lock. No physical hardware is opened in these tests.

Disabled auto mode may validly have `:state nil`. Schema-2 validation and restore now preserve that inactive representation instead of requiring or inventing a live planner state.
