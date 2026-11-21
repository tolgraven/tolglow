# OLA and direct Art-Net output

Studio retains OLA output for USB DMX interfaces and all other transports that
olad can drive. Its direct Art-Net sender remains independent of OLA. Both
implement the same owned sink contract and receive complete universe buffers
from the [output scheduler](output-presentation.md).

Requiring namespaces and starting Studio without output options never opens an
output connection. Loading a show file does not arm physical output. Choose one
sink explicitly for the current process:

```sh
# Preview only (the default)
./bin/studio

# OLA daemon on this computer, TCP port 9010
./bin/studio --output ola

# Explicit OLA destination
./bin/studio --ola-host lighting.local --ola-port 9010

# Direct Art-Net, without an OLA daemon
./bin/studio --artnet-host 10.0.0.42
```

`--host` and `--port` select the Studio HTTP listener; `--ola-host` and
`--ola-port` select the OLA daemon. `--artnet-port` defaults to 6454.
Conflicting output choices are rejected before opening either transport.
There is currently one physical sink per Studio process, not implicit fan-out.

In the REPL:

```clojure
(require '[tolglow.studio :as studio])
(def running (studio/start! {:ola {:host "127.0.0.1" :port 9010
                                  :connect-timeout-ms 1000
                                  :write-timeout-ms 25}}))
;; Stop sends the host's final blackout, then closes the owned connection.
(studio/stop! running)
```

The connect and write defaults are each 1000ms. These are transport timeouts,
not musical durations. Set the write budget to the desired output deadline;
25ms is one frame at 40fps. The connect budget covers TCP connection setup;
system hostname resolution occurs before that setup and can take longer.

## Patching USB DMX through OLA

Install and configure olad using the [OLA documentation](https://www.openlighting.org/ola/).
Connect the USB interface, enable its matching OLA plugin, and patch its output
port to the OLA universe used by the rig. OLA's web interface normally runs at
`http://localhost:9090`. Tolglow sends DMX to that universe; OLA owns USB device
discovery, plugin settings, output-port patching and any merge configuration.
Starting Tolglow does not change those daemon settings.

| Sink | Universe addressing | Channel payload | Transport |
| --- | --- | --- | --- |
| OLA | Universe ID passes through unchanged, 0..65535 | 1..512 bytes | Native OLA TCP streaming RPC |
| Direct Art-Net | Rig universe 1..32768 maps to wire Port-Address 0..32767 | Even length, 2..512 bytes | ArtDMX UDP datagrams |

Use OLA universe 1 for rig universe 1. The direct Art-Net mapping deliberately
retains the application's existing one-based convention; it is not OLA's
Art-Net plugin mapping. If OLA itself sends Art-Net, its Net/Subnet settings and
universe mapping apply instead. The most recent Art-Net specification deprecates
Port-Address zero for sACN compatibility; changing existing patch numbering is a
separate explicit rig migration.

## What the four-universe limit means

Art-Net 3 introduced a **15-bit Port-Address: 32,768 universes**, not a four-universe
protocol ceiling. Art-Net 3 gateway discovery/binding has four-port groupings.
OLA's Art-Net plugin defaults to four output ports; its current `output_ports`
setting can create more, but its README says only the first four appear in
ArtPoll messages. Its Net/Subnet configuration is also shared by the device.
These plugin/discovery restrictions are reasons to retain direct Art-Net for
large pixel installations. They are not restrictions on USB DMX output through
OLA or on the Art-Net protocol's address space.

Sources surveyed on 2026-11-21:

- [OLA Art-Net plugin, revision 3f870a7](https://github.com/OpenLightingProject/ola/blob/3f870a79b69e08a8553df360c82635113c192504/plugins/artnet/README.md)
- [OLA FAQ: universe numbering and plugin mapping](https://www.openlighting.org/ola/get-help/ola-faq/)
- [Official Art-Net background: version 3 and binding](https://art-net.org.uk/background/)
- [Official Art-Net specification](https://art-net.org.uk/downloads/art-net.pdf)

Art-Net™ Designed by and Copyright Artistic Licence.

## Connection ownership and failure behavior

`tolglow.studio.output/open-sink!` accepts `{:ola {...}}`, `{:artnet {...}}`, or
neither. `open-ola!` and `open-artnet!` return
`{:send! (fn [universes frame-number]) :close! (fn []) :description {...}}`.
The host owns their lifetime. Output happens on the dedicated serial publication
thread, outside pure effect evaluation and frame-worker state transitions.

OLA uses `StreamDmxData` with `STREAM_REQUEST`, the native protocol's one-way
output operation. Each sink owns one TCP connection and selector. It validates
and copies all buffers before writing a frame, preserves universe order, and
advances transport request IDs even when the source frame number repeats. A
gathering write batches the universe messages under one write deadline. There
is no unbounded background request queue or process-global OLA connection.
Concurrent callers cannot interleave frames, and close can interrupt a pending
write without waiting for the writer's lock.

A failed or timed-out write closes that sink: continuing a partially written RPC
on the same stream would corrupt framing. Reopening is explicit. Successful
local writes do **not** acknowledge daemon acceptance or physical fixture
delivery. OLA's streaming RPC has no response; check daemon/device diagnostics
when commissioning a rig. This differs from the previous Afterglow client's
acknowledged `UpdateDmxData` operation. There is no automatic reconnect, port
patching, RDM management or daemon-discovery UI in this slice.

The small encoder covers only OLA's output message, with no dependency on the
old client's global atoms or lifecycle. Wire definitions come from the pinned
[Ola.proto](https://github.com/OpenLightingProject/ola/blob/3f870a79b69e08a8553df360c82635113c192504/common/protocol/Ola.proto),
[Rpc.proto](https://github.com/OpenLightingProject/ola/blob/3f870a79b69e08a8553df360c82635113c192504/common/rpc/Rpc.proto)
and [native channel implementation](https://github.com/OpenLightingProject/ola/blob/3f870a79b69e08a8553df360c82635113c192504/common/rpc/RpcChannel.cpp).
OLA framing uses native byte order; communicating hosts must agree on endianness
(current Apple Silicon and common x86 machines are little-endian).

## Verification and remaining hardware gate

`tolglow.studio.output-test` exercises both real UDP and TCP loopback sockets,
universe/byte boundaries, source-buffer ownership, repeated source frames,
idempotent close, frame validation before publication, failure retirement, and
a non-reading TCP peer that forces the OLA write deadline. Injected transports
cover deterministic close/write concurrency without a daemon or fixture.

`tolglow.studio.output-options-test` exercises CLI selection, mutually exclusive
sinks, startup failure cleanup and the actual Studio scheduler/start/stop with
an injected OLA transport. `tolglow.studio.ola-protocol-parity-test`, in the
dependency-rich parity lane, compares 336 combinations against the existing
ola-clojure generated Java protobuf codec. Production output does not load that
client. Focused commands and results belong in the integration test record.

The 2026-11-21 workstream verification on Clojure 1.12.7 passed
`./bin/test parity`: **490 tests / 4,140 assertions**, including the pure suite
and both new namespaces. The focused output/CLI suite passed **16 tests / 117
assertions**. These counts overlap; they are not additive.

These tests do not start olad, activate USB DMX hardware or claim physical
delivery/throughput. A real USB interface and the intended olad version remain
the commissioning gate; large-installation network throughput is also unmeasured.
