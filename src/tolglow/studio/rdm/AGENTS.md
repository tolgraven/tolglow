# RDM device management

RDM is a separately owned control transport. Never share its socket, worker or
deadline with DMX output. Requiring these namespaces and reading status are inert.
An explicit session and action are required for discovery, GET and SET. Bound
jobs, RPC messages and device enumeration. Close sockets on timeout/cancellation.

The Art-Net specification, OLA protocol and PID references are linked in `docs/rdm.md`. Do not invent
protocol fields or hardware capabilities. Preserve NACK/ACK_TIMER information,
validate request/reply identity and retain raw parameter bytes for unknown PIDs.
Persist configuration/bindings/observations, never sockets, workers or arming.
Physical readback and the show's patch are different until deliberate adoption.

Tests use real codecs against owned loopback peers or injected external
transports. No test discovers or controls physical fixtures. Hardware and
plugin qualification remain separate from protocol and browser evidence.
