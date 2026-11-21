# Tests

Run real production namespaces/functions and dependencies. Do not copy source,
skip failed assertions or replace whole dependency namespaces to manufacture a
green lane. Capture only the IO boundary being excluded from the test.

- `utils.iters-test` preserves iterator consumption, row ordering, truncation,
  transient accumulation and empty inputs. Zero-input `multi-iter` remains a
  separately tracked legacy contract concern; do not call it without a bound.
- Fixture and compatibility tests use the actual fork and exercise fine/sparse
  offsets, calibration, colors, parameters, cue metadata and explicit context.
  `compatibility-test` intentionally requires/reloads core/live and traps
  unexpected activation. Normal arithmetic tests should require narrow modules.
- Registry/compiler/composition/automation tests use explicit time, beat and RNG
  data. Test semantic expectations and boundaries, not a copy of implementation.
  Include ownership, release, duplicate input, invalid revisions and pause cases.
- Studio and integration tests cover the shared model, host lifecycle, Ring
  behavior and bounded local HTTP/provider boundaries. Loopback server tests are
  allowed when owned and closed; external provider accounts and production
  endpoints are not part of the default suites.
- Concurrent tests compare observable results across completion orders and
  prove owned resource cleanup. Do not rely on a particular scheduler ordering
  or use noisy performance assertions as correctness gates. Bound waits and
  release workers even on failure.
- Tests never activate a physical rig, discover MIDI ports, start OLA/Art-Net
  output or create a Processing sketch. Hardware checks must be separately
  selected and reported. Browser checks distinguish viewport emulation from
  actual iPad/phone hardware.

Runners explicitly list selected namespaces. Add new tests to the appropriate
runner through its owner and report the command actually executed. Parity lanes
include pure tests; focused and aggregate counts overlap. See
[testing](../docs/testing.md) for evidence and remaining compatibility gates.

Expanded performance tests must cover type-aware bounded modulation, seeded
replay, normalized derivative behavior across frame rates, per-note/channel
expressive ownership and category overlap/override semantics. Controller bytes
need cited reference/capture provenance and separate physical-device evidence.
Test staged restore failure/cleanup and stale callbacks using actual resource
owners with injected transports. Link/AbleSet/Live dual-host claims require
explicit selected-source, disconnect and single-output-authority checks; a
loopback protocol test alone does not prove the external host.
