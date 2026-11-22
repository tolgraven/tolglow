# Transit show files, schema 2

A show file stores the complete portable domain model and can also store its
last committed output frame. It is not a preset containing only cue definitions.
The pure implementation is `tolglow.persistence`; the studio host owns
file/HTTP IO, clock rebasing, runtime resource recreation and atomic publication.
There is no schema-1 or old-runtime compatibility adapter.

## Public API

```clojure
(require '[tolglow.persistence :as files])

(files/freeze model)                    ; -> {:schema-version 2 :model ...}
(files/freeze model portable-output)    ; adds :frame-output
(files/write-string model)             ; Transit JSON text
(files/write-string model portable-output)
(files/read-string source)             ; exactly one validated document
(files/thaw current-model document)    ; complete replacement model, no IO
```

The writer uses the Studio Transit codec. The reader accepts a single Transit document and can import an existing schema-2 EDN map. It rejects
multiple documents/trailing forms, non-schema-2 input and documents larger than
16 MiB of UTF-8 text. Freeze also enforces the serialized-size bound, so an oversized
cache cannot produce a file that only fails later on load. The model is validated before return; `thaw` separately
establishes fresh identity/ownership relative to the current show. All failures
throw before a replacement is returned and leave the original model untouched.

## What survives

- Effect/cue/fixture definitions, historical versions, parameters/defaults,
  pages and manual placements, variables, rig fixtures/room/trusses, master and
  blackout state.
- Every active instance's captured definition/version, parameter values, local
  state, launch mode, ordering, phase, start/release positions and musical or
  millisecond timers. A captured instance may outlive deletion of its cue.
- Explicit transport beat, meter, tempo, play/pause and logical time, plus frame
  bookkeeping. Beat/bar/phrase durations retain captured meter and progress;
  they are not converted into fixed milliseconds during save/restore.
- Saved compositions and running captured definitions, sequence RNG/position,
  pause clocks, launch tokens and ownership; auto-mode configuration, RNG,
  interval timer, selection tick and owned instances; response envelopes and
  selections.
- Integration data, including Ableton cue/parameter/clip links, logical device
  pairings and settings, Beat Link descriptors, audio settings, agent provider
  configuration, credential references and chat history. Port names/IDs and
  endpoints are portable settings; actual open ports/sockets/threads are not.
- Portable reactive registrations with stable IDs, source/var names as strings
  and explicit data bindings. Runtime subscription caches/listeners are rebuilt,
  never serialized.
- Explicit `:extensions` data. Unknown top-level model keys are rejected rather
  than silently discarded; extensions are opaque and preserved. Opaque effect
  state and extension strings are not rewritten as if every string were an ID.

The reader permits finite numbers (including ratios), strings, keywords, characters,
booleans, nil, vectors, lists, sets and maps with recursively portable keys.
This supports page coordinates, historical version keys and ownership tokens.
The shared codec preserves seven thi.ng.color records, exact musical fractions,
timestamps (Transit date), UUIDs and byte arrays (Transit binary). World-space
aim and position use ordered numeric vectors. Canonical event data uses maps
with keyword keys. A boxed JVM byte travels as an integer. A 512-byte universe
travels as binary and restores its original unsigned channel values. Explicit
signed channel vectors retain their signed representation. Unknown records,
functions, symbols and refs remain outside the portable model. Callback source is represented as a string,
not a serialized executable function. Raw secret fields such as `:api-key`,
`:password`, `:authorization` and access/refresh tokens are rejected. The agent
manager exports redacted configuration/history with an opaque credential
reference; a separately owned local credential store resolves that reference.
No secret is recovered by evaluating a show document.

## Validation before publication

The model's registry is checked for matching IDs/versions, supported definition
schemas, historical/current consistency and valid current cue references and
parameters. Every effect definition, including captured running versions, is
compiled through the actual declarative compiler to catch unsupported DSL forms.
The rig passes the actual patch compiler, including channel collisions and
coarse/fine/universe addressing. Pages must reference registered cues.

Instances are checked for valid phase/order/parameters, captured definition
identity and finite time. Musical timers must match their source duration and
captured meter. Composition definitions/specifications, running ownership and
sequence clocks, auto RNG/timers, response middleware and Ableton mappings are
validated. The agent manager and reactive registry provide their own pure
validation for portable exported state; this boundary does not execute a saved
callback, start a model request or open a physical connection.

Pure validation can reject invalid structure and supported DSL syntax. It does
not prove arbitrary callback source is safe or that an external provider/device
still exists. Root host restoration applies explicit connection/reconnect policy
through the known session adapters and reports failures as connection state,
not as a partially published show model.

## Fresh identity without losing progress

`thaw` sets the epoch to `inc(max(current-epoch, saved-epoch))` and advances the
command revision beyond both saved and current revisions. It assigns fresh
instance IDs in saved launch order and adjusts the allocation counters. Old
commands cannot match the new epoch or accidentally target a new activation.

Composition owner IDs, sequence active tokens and token-to-instance mappings are
rekeyed together. Auto ownership and instance selectors, operational Ableton/
audio references and reactive reference data follow the new IDs. Ownership
references to instances that already ended are pruned. RNG state, effect state,
step boundaries and musical timers are preserved; loading does not consume a
random value or run an extra effect frame.

Command receipts are cleared. External browser/device source leases and gesture
IDs are not revived: preserved manual instances receive a fresh restore source;
composition/auto instances receive their new domain owner. A saved held cue can
remain active as show state, but an old disconnected pad/browser has no claim on
it; a current controller can stop it explicitly. Ableton logical links survive,
while held-note callbacks and recording echo receipts are reset for the new
connection session. Historical chat/tool results retain their original IDs as
historical text/data, rather than being rewritten into current control actions.

## Logical clock rebasing

The restored model retains the saved `:time-ms`, transport beat and timer origins.
The host must atomically rebase its monotonic elapsed-time offset to this saved
logical point before the next frame. It must not take the maximum of the old
show's wall time and saved time, nor advance paused musical timers simply because
loading took several seconds. Physical timestamps and audio sample counters are
re-established by their owned adapters; saved transport position is the musical
state, not a claim that a previously connected peer kept playing while offline.

## Optional last committed frame

A frame may be stored as portable data:

```clojure
{:schema-version 2
 :model {...}
 :frame-output {:values {:fixture-id {...}}
                :target-order [:fixture-id]
                :universes {1 [0 0 127 ...]}}}
```

The output contains exactly `:values`, `:target-order` and `:universes`. Rich
fixture values may be a selected preview subset, but every key must name a saved
rig target. Target order must match the full rig, universe IDs must match the compiled patch, and every universe must contain
512 integer bytes in signed `[-128,127]` or unsigned `[0,255]` representation.
The pure serializer never accepts live byte arrays. The host converts arrays to
vectors from the **same committed model/output snapshot** before freezing and
converts validated vectors back to owned arrays after reading. When a renderer
uses compact fields, the full rich projection is materialized only on save to
preserve movement history; routine UI projection can remain a small subset.

A complete load publishes this saved output immediately and skips an extra
engine evaluation on that restoration tick. The next normal tick resumes from
the saved local state and clock. This avoids advancing a stateful effect merely
to obtain its first restored preview. A model-only document remains supported;
the host can publish a blank initial output until the next regular frame.

## Owned resource restoration

`tolglow.connections` owns the agent manager and local credential adapter,
fixture cache, audio capture manager and reactive graph. `create` starts no
capture or provider request. A saved credential reference may perform an explicit
local lookup. The default file adapter resolves a provider credential only for
the provider and complete request endpoint at which it was configured. If an
import changes that destination, settings and conversations still load and the
operator can enter a credential for the new destination. A provider credential
saved before destination binding also requires one local re-entry. Custom
credential adapters can implement `:scope` as a function returning scoped
`:get!`, `:put!` and `:delete!` operations. An adapter without that facility
supports runtime-only provider credentials.
Agent state and fixture cache use `:integrations :agents` and `:fixtures`.
Capture uses `:integrations :audio-capture` with `{:version 1 :config ...
:capture-wanted? boolean}`; separate audio parameter-routing settings under
`:integrations :audio`, Ableton links and Beat Link settings remain intact.
Sync exports `:integrations :sync {:version 1 :config ... :wanted? boolean}`.
Preparing/restoring this data opens no transport; the host explicitly resumes a
wanted sync connection after clock rebase. Retired callbacks are generation-gated.

The host stages a restore with `prepare-restore`, which validates data, creates
replacement managers and resolves permitted reactive handlers. Invalid data or
compilation failure leaves the current bundle untouched. Source handlers require
the explicit `:reactive-options {:trusted-source? true}` option on the host.
Without that option a saved Var can still match an existing local registration,
including one installed at the REPL before a live reload. A cold start can
authorize a specific qualified symbol in `[:reactive-options :allowed-vars]`.
Studio installs its built-in clock and fault subscriptions before loading a
saved graph. A candidate is
bound to its owner and resource generation, and carries its target model epoch.

Inside the same host lock that publishes the restored model/output, call
`commit-restore!`. This only swaps resource ownership; it performs no IO, waits,
callbacks or cancellation. Call `retire!` on the returned retired bundle outside
that lock. Always call `abort-prepared!` on unused candidates. Stale, foreign or
already consumed candidates cannot replace live resources. Cleanup is idempotent
and attempts every resource even when an injected transport fails to close.

Retiring cancels old agent work but never replays saved chat tools. Old tool
callbacks retain their original epoch and must be rejected by the host. Audio
callback generation gates reject retired sessions immediately after commit.
Capture stays stopped until the host rebases logical time and explicitly calls
`resume-audio!`; the new worker establishes a new sample-counter time origin.
No saved run ID, PCM worker or analyzer sample timestamp is resumed.

After normal model commits, the host calls `publish!` on connections. Watchers
enqueue semantic actions; the next host cycle calls `drain-actions!` and stamps
fresh command identity, epoch, source and time. Export includes registered
handler data, custom ratom values, watcher comparisons and pending actions. A
watcher restored at the same observed value does not fire twice. Anonymous
opaque handlers must be registered durably before the graph can be exported.

## Evidence and remaining integration gates

Cue presets are saved as the versioned cue's optional `:saved-params` map.
Launching resolves effect defaults, authored cue parameters, saved parameters,
then explicit launch overrides in that order. Saving a preset captures current
typed instance parameters without overwriting authored defaults; clearing it
publishes a new cue version. Both operations use the common command reducer.
Registry publication and show import validate saved values against the cue's
selected effect version, including pinned historical effects. Composition slot
validation uses the same selected version and precedence. The combined
registry/persistence/composition regression gate passed **40 tests / 324
assertions**, including rejected malformed file values and launch after restore.
This is additional schema-2 cue data, so no file schema migration is needed.

The final focused suite passed **19 tests / 165 assertions**, including rich
active state roundtrip, older captured definitions, paused musical timers,
composition/auto/Ableton rekeying, exact output preservation, invalid data
rejection, provider configuration/history normalization, settings, fixture-cache
validation and reactive registration/cell/watcher/pending-action restoration.
Custom tagged readers were explicitly tested and never invoked while validating
saved source. API tests use real compiler/registry/patch and integration
validators. Final host-suite counts belong in [testing](testing.md).

The connections suite including sync passed **16 tests / 158 assertions**. This covers inert
construction, local reference resolution without secret export, complete show
roundtrip, rejected restores, concurrent/stale candidate commits, cleanup
failure isolation, preserved reactive state, interrupted chats and synthetic PCM
capture restarting with a new origin. Transport tests use injected providers and
PCM sources; no physical capture or external provider account was used.

The resource-host suite passed **12 tests / 80 assertions**, covering model/output
restoration, no extra effect step, clock rebase, stale/pending candidates, device
open races and Unicode atomic file replacement. Real connection outcomes remain
a separate physical/provider gate.
No pure serializer test establishes physical device reconnection or successful
external-provider authentication.

## File and HTTP encoding

Studio downloads `tolglow-show.transit` from `GET /api/export` with media type
`application/transit+json`. The file picker accepts Transit shows and previous
schema-2 EDN saves. The REPL's `save-show!` writes the same payload with atomic
replacement. A client can also POST the downloaded bytes directly to `/api/import` with the same media type. Application structure, registry versions, ownership and restoration
rules remain part of schema 2. Transit supplies the representation of its values.
