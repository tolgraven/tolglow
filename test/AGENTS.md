# Tests

Keep the fast baseline separate from integration and hardware checks. The
baseline must require production namespaces normally and execute their public
functions; do not copy source definitions, skip failing assertions, or install
fake dependencies to manufacture a green result.

- `tolglow.utils.iters-test` requires only Clojure and the repository root on the
  classpath. It protects iterator consumption, row ordering, truncation,
  transient-vector accumulation, and empty inputs.
- `tolglow.fixtures-test` requires the real Afterglow fork and its dependencies.
  It protects declarative channel expansion, pixel positions and offsets,
  calibration, and strobe ranges. A failed namespace load is an integration
  failure, not a passing test or a reason to stub the channel constructors.
- Do not require `tolglow.core`, `tolglow.live`, graphics sketches, or full show
  startup in unit tests. Some existing namespaces contain top-level actions.
- Test bugs with behavioral examples before fixing them. Keep regression tests
  after the fix. Document any untested boundary and the exact command used.
- Use explicit snapshots and seeded randomness for new effect tests. Avoid
  sleeps, timing assertions on shared CI workers, and real network or MIDI
  devices. Future concurrent tests must compare observable outputs across
  worker counts, not assume a particular execution order.
- Integration fixtures should use in-memory show state and captured output.
  Any hardware test must be separately selected and disabled by default.

Existing cases deliberately avoid zero-input `multi-iter`: all three variants
currently report `hasNext` forever with no source iterators. Track that as a bug
before defining a supported zero-input contract. The tests also do not yet cover
sparse/fine-channel prefix footprints in declarative strips, cross-universe
strips, effect lifecycle, or live cue reload.
