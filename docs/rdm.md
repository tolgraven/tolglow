# RDM fixture discovery and configuration

Rig → RDM Fixtures lists the show's fixtures beside a device inspector. Opening
the panel reads saved application state only. **Enable RDM** creates an explicit
session; **Discover**, **Read device** and each control button send separate
requests. Disabling the session cancels pending work. Loading a show or restarting
the runtime invalidates the session; saved settings never reconnect automatically.

The initial transport is OLA's RDM RPC service, normally `127.0.0.1:9010`. The
selected OLA universe must be patched to a plugin and physical interface that
support RDM. DMX output support alone does not establish RDM support. Direct
Art-Net RDM/ArtTod discovery remains a separate implementation task. Existing
OLA and direct ArtDMX lighting sinks remain unchanged.

Afterglow owns RPC framing and RDM commands in `afterglow.rdm.codec` and `afterglow.rdm.transport`. Tolglow manages jobs, show-epoch fencing and saved device associations in `tolglow.rig.rdm`.

## Workflow

1. Set the OLA host/port and enable the session. Choose the RDM universe and run
   discovery. Enter a known UID manually when discovery is unavailable. UIDs use
   the `1234:01020304` format; universe numbers are OLA's unchanged 0..65535 IDs.
2. Select a discovered UID and **Read device**. The inspector displays manufacturer,
   model, label, software, physical DMX address, footprint, personality, subdevice
   count, supported PIDs and available sensors. Unsupported fields retain their
   NACK status; absent data is never presented as a measured value.
3. Select the corresponding show fixture and **Link selected fixture**. The
   association preserves the fixture ID, definition, groups and channel map. A
   UID/subdevice can belong to one show fixture in the same universe. Linking is
   independent from patch changes.
4. Address, personality, label and identify buttons send explicit SET requests.
   Address/personality changes require previously read footprint information and
   reject proposed overlap with another show fixture when the device is linked.
   The device is read
   back after a successful typed SET, with verified/mismatch/unavailable status.
5. **Adopt observed address** changes the show's patch separately, using the last
   observed address/footprint and the normal renderer patch validator. It does
   not infer channel meanings from a personality number. Use the fixture library
   to select the correct channel definition when changing operating modes.
   Adoption requires the inspected device and readback timestamp to still match;
   a changed association or newer observation requires another review.

The advanced inspector accepts GET or SET, numeric/hexadecimal PID, subdevice
0..512 and up to 231 hexadecimal parameter bytes. Broadcast device/subdevice SET
is deliberately outside this initial inspector. Generic unknown PID replies
retain bytes. Known replies decode the documented payload structure. Sensor
type/unit/prefix codes remain visible alongside raw values rather than guessing
a unit or scale. At most 32 personalities and 32 sensors are expanded per read;
the inspector flags truncation, and individual PIDs remain accessible through
the advanced controls.

## API and ownership

`GET /api/rdm` returns passive session/job state, saved observations and show
fixture descriptions. Reitit registers literal POST routes for `connect`,
`disconnect`, `discover`, `read`, `command`, `cancel`, `associate`, `unassociate`
and `adopt` below `/api/rdm/`. JSON and Transit use the same handlers. Mutations
require `runtime-id` and `expected-epoch`; device jobs also require the current
`session-id`. A job receipt contains `job-id`; polling the passive endpoint
reports queued/running/completed/failed/cancelled results.

Example typed command body:

```clojure
{:runtime-id "current-runtime" :expected-epoch 0 :session-id "current-session"
 :universe 1 :uid "1234:01020304" :sub-device 0
 :command :set :parameter :address :value 200}
```

The owner model stores configuration, UID associations, discovered identities
and observations under `[:extensions :tolglow.rdm/state]`. Sockets, sessions,
workers and active jobs are runtime-only. Jobs publish through the existing
command queue with epoch and RDM session guards; a response queued before
disconnect cannot update a later session. Restore retains full portable data and
requires explicit session enabling before further device operations.

RDM uses one independent worker, at most eight admitted jobs, 32 recent job
records and 512 retained device observations. Every job owns its OLA socket and
closes it on success, error, cancellation or deadline. No RDM IO runs inside an
effect, frame publication, or the DMX output socket. Request timeouts default to
2 seconds and the entire job deadline to 30 seconds; these physical IO budgets
do not alter musical timing. System DNS resolution can precede socket timeout
handling; use an IP address when that distinction matters.

The client checks RPC correlation ID, framed message bounds, RDM source UID
when supplied, PID, subdevice and response class. OLA owns RDM transaction and
checksum validation and ACK_OVERFLOW assembly. ACK_TIMER does not re-send SET:
the worker waits its documented 100ms units then retrieves QUEUED_MESSAGE,
bounded by the job deadline and 32 responses. NACK reasons and transport failures
remain explicit job/field data. A timeout cannot prove whether a physical SET
reached its device; read the device before trying another change.

## Protocol sources and verification limits

Implementation references were checked against OpenLightingProject/ola commit
[`3f870a79b69e08a8553df360c82635113c192504`](https://github.com/OpenLightingProject/ola/tree/3f870a79b69e08a8553df360c82635113c192504):

- [Ola.proto](https://github.com/OpenLightingProject/ola/blob/3f870a79b69e08a8553df360c82635113c192504/common/protocol/Ola.proto): discovery, UID and RDM request/reply schemas.
- [Rpc.proto](https://github.com/OpenLightingProject/ola/blob/3f870a79b69e08a8553df360c82635113c192504/common/rpc/Rpc.proto): RPC message classes and correlation.
- [StreamRpcChannel.py](https://github.com/OpenLightingProject/ola/blob/3f870a79b69e08a8553df360c82635113c192504/python/ola/rpc/StreamRpcChannel.py): native-endian version/length framing.
- [OlaClient.py](https://github.com/OpenLightingProject/ola/blob/3f870a79b69e08a8553df360c82635113c192504/python/ola/OlaClient.py): response behavior and ACK_TIMER units.
- [pids.proto](https://github.com/OpenLightingProject/ola/blob/3f870a79b69e08a8553df360c82635113c192504/data/rdm/pids.proto): standard parameter layouts and ranges.

These upstream files retain their original LGPL licenses; this implementation
uses the published wire definitions without copying the upstream client.
Golden packet expectations and owned loopback peers test the protocol boundary.
Actual olad/plugin/USB fixture qualification and physical iPad Safari remain
separate gates. Tests and browser demos do not activate physical hardware.

## Repeating the browser gate

Build the optimized frontend with `npm run studio:build`. In a separate terminal,
start the explicit QA host. It has preview output and creates its own TCP OLA
peer; there is no real olad or fixture connection:

```sh
lein run -m tolglow.rdm-qa
```

Wait for `Owned RDM QA host ready`, then run `node test/studio/rdm_browser.mjs`.
The script reads `target/rdm-qa.json`, drives the real Ring/re-frame workflow and
writes its report and desktop/iPad/phone screenshots to `target/rdm-browser/`.
Set `STUDIO_CHROMIUM` to a different Chromium binary when needed. Stop the QA
host after the run; its shutdown hook closes the server, runtime and peer.
Each repeat needs a fresh QA host because the test deliberately adopts a patch.

On 2026-11-21, `./bin/test parity` passed 551 tests / 4,617 assertions and
`npm run studio:test` passed 161 / 860. `npm run studio:build` produced 179 files
with zero warnings. The browser gate passed 14 checks, including discovery,
linking, overlap rejection, SET/readback, separate adoption, labels, identify,
unsupported-PID reporting and responsive layout, with no component errors.
The aggregate JVM lane includes a regression proving a blocked RDM request
does not block cue commands or frame publication, plus full save/load disarming.
