# Resume RDM fixture management

This branch is **work in progress, unverified, not ready to merge**. Work paused
at the user's credit limit. No physical RDM/DMX/MIDI discovery or output occurred.

Branch: `codex/rdm-rig-fixture-management`; worktree `/workspace/tolglow-rdm`.
Base at creation: `eecef8e` (origin tracking was stale). Root corrected the
repository fetch refspec; explicitly fetch current master before integration:

```sh
git fetch origin master:refs/remotes/origin/master
```

## Implemented but not verified

- `src/tolglow/studio/rdm/codec.clj`: bounded protobuf fields, native-endian OLA
  RPC framing, UID/request/reply validation, typed PID data and generic commands.
- `src/tolglow/studio/rdm/transport.clj`: separate cancellable OLA socket,
  request deadlines, ACK_TIMER queued-message polling and NACK data.
- `src/tolglow/studio/rdm.clj`: inert manager, one bounded worker, explicit
  session, jobs/cancel, saved configuration/bindings/observations, proposed patch
  validation and explicit address adoption. No output sink changes.
- Narrow `studio.clj` lifecycle/queue/Ring service hooks, `web.clj` routes, test
  runner namespace placeholders and work ledger claim.

Only an early `require` of codec/transport and the first manager version passed.
The latest host/codec edits have **not been checked**. Last started require
command had not been polled when pause arrived. No tests, frontend or browser
validation exist yet. Runner currently references two test namespaces that must
be added before any aggregate suite can pass. Do not represent this checkpoint
as functioning or complete support.

## Verified source references

Upstream sources inspected from OpenLightingProject/ola master on 2026-11-21:

- https://github.com/OpenLightingProject/ola/blob/master/common/protocol/Ola.proto
  (`ForceDiscovery`, `GetUIDs`, `RDMCommand`, UID and RDMResponse fields).
- https://github.com/OpenLightingProject/ola/blob/master/common/rpc/Rpc.proto
  (REQUEST/RESPONSE/error types, request ID/method/body fields).
- https://github.com/OpenLightingProject/ola/blob/master/python/ola/rpc/StreamRpcChannel.py
  (protocol version 1 in high four bits, native-endian 32-bit length header).
- https://github.com/OpenLightingProject/ola/blob/master/python/ola/OlaClient.py
  (ACK_TIMER payload units are 100ms; OLA handles ACK_OVERFLOW assembly).
- https://github.com/OpenLightingProject/ola/blob/master/data/rdm/pids.proto
  (standard identity, labels, supported PIDs, personality, DMX address, sensors,
  identify and QUEUED_MESSAGE payloads).

Downloaded copies are `/tmp/tolglow-rdm-research/`; pin a source commit and retain
license/provenance when writing final docs or independent codec fixtures.

## Agreed contract and ownership

Root approved a separate RDM connection/worker, no lighting deadline socket.
GET `/api/rdm` is passive. POST actions: connect/disconnect/discover/read/command/
cancel/associate/unassociate/adopt. Mutations require runtime-id + expected-epoch;
device jobs additionally require current session-id. Observations enter the
owner queue as `:rdm-update`, persisted under `[:extensions :tolglow.rdm/state]`.
Restore/restart must cancel old jobs and remain disarmed. Fixture associations
are additive; preserve existing names, fixture IDs, definitions and groups.

3D editor agent agreed on new `[rdm/panel]` with its own subscriptions mounted
after `[fixture-library/panel db]` in rig-view. Only add the require and mount to
views.cljs; do not alter its 3D editor changes. UI and CSS are not written yet.
Direct ArtRdm is explicitly future work; initial transport is OLA with a plugin
and interface that actually support RDM.

## Next work, in order

1. Read root/nested AGENTS and merge current master carefully; root has many
   separate host/runner/ledger changes in flight.
2. Compile latest source; audit session races, cancellation, shutdown and
   post-commit reconciliation. Particularly ensure connect cannot arm a new epoch
   after an earlier request admission; avoid holding manager lock while a
   callback waits for the host lock (current connect! publishes under manager
   lock). Do not hold the lighting lock across IO or socket shutdown.
3. Audit generic SET validation: raw address/personality byte lengths before
   indexing, request/response command-class checks, queued-message subdevice
   identity (root QUEUED_MESSAGE can return a subdevice's original response),
   unexpected PID/class should not silently be retried on an ordinary ACK.
   DNS resolution currently occurs before bounded socket connect, like the
   existing OLA sink; do not claim the absolute deadline bounds DNS.
4. Add real-codec loopback peer tests, ideally independent generated protobuf
   oracle. Cover split packets/oversized messages/bad IDs/NACK/ACK_TIMER, timeout,
   cancel/epoch/stop, queue bounds, malformed response, unsupported PID, no IO on
   create/status/restore, actual host publication and complete save/load.
5. Validate address/personality against current physical/readback footprint and
   show collision rules. Writes must not silently mutate the show patch; adoption
   is explicit. Personality channel semantics cannot be inferred from footprint.
   Preserve source definition IDs and shorthand authoring structures.
6. Implement re-frame panel + CSS + frontend tests: show fixtures/configurations,
   discovery/linking, device readback, typed control forms, generic GET/SET,
   visible unsupported/error states, job cancellation, stale response guards,
   touch targets and phone/iPad layout. Passive mount only loads saved status.
7. Add final docs/rdm.md + docs index + roadmap followups; no hardware support
   claims without hardware evidence. Mark ledger done only after real tests.
8. Run focused and aggregate JVM lanes, production frontend build and actual
   Ring/browser workflow against owned loopback RDM peer; coordinate CPU windows
   with profiler. Commit, push, PR and attach artifact; root merges after CI.

Environment: Clojure CLI `/workspace/.tools/clojure-1.12.7.1681/bin`,
`JAVA_TOOL_OPTIONS=-Duser.home=/workspace/.tools/java-home`,
`CLJ_CONFIG=/workspace/.tools/clojure-config`. Paired dependency is
`/workspace/afterglow`. No frontend node_modules symlink or QA server was created.
