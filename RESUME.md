# Paused effect fault isolation host — unverified WIP

Paused on coordinator request to conserve credits. Branch
`codex/effect-error-isolation-host`, worktree `/workspace/tolglow-effect-isolation`.
Base `eecef8e`; merge current master using an explicit remote-tracking ref fetch.
Paired backend: `/workspace/afterglow-effect-isolation/RESUME.md`.

## Implemented here

- `studio/effect_faults.clj`: renderer-attributed rejection loop using pure engine
  records; bounded owned asynchronous logging queue with dropped/failed counters.
- Studio commits healthy evaluated output, publishes fault journal in ordinary
  snapshot, logs committed events outside model lock, closes log owner on stop.
- Active fault identities remap on full-show restore; historical journal remains
  historical. Shared retry uses existing Ring `/api/command` with `:instance/retry`.
- New actual host tests exercise real DSL division by zero, worker evaluation,
  captured DMX, reactive subscription/handler, Ring param/retry, save/restore,
  restart and bounded blocked logging. Existing blackout test now asserts an
  instance fault rather than whole-frame failure.

## Verification and known work

Studio namespace loaded successfully against paired WIP Afterglow.
The first new host-test run failed to compile at line77 (`finally` out of context)
because two test forms had extra closing parentheses. Those two parentheses have
been corrected **but no test was rerun after the pause instruction**.
No aggregate Tolglow test has run for this work. No PR exists yet.

Next steps:

1. Merge latest master (other agents own disjoint studio operation hooks).
2. Run focused new tests using this exact paired dependency override:
   `clojure -Sdeps '{:deps {tolgraven/afterglow {:local/root "../afterglow-effect-isolation"}} :paths ["src" "resources" "test" "dev"]}' -M -e '(require (quote tolglow.studio.effect-faults-test)) (clojure.test/run-tests (quote tolglow.studio.effect-faults-test)) (shutdown-agents)'`.
3. Integrate profiler's renderer attribution follow-up (parent knows ownership)
   before testing renderer-late malformed output. Add actual zero-distance aim,
   overflow/blend and unsupported target regressions, no fake renderer namespace.
4. Review logger creation validation/cleanup on invalid log option, bounded
   diagnostic fields, suppression across restore, retry failure/recovery and
   healthy frame performance. Root approved persistence diagnostic remapping.
5. Write `docs/effect-fault-isolation.md`, update AGENTS contract, ledger evidence
   and runner entries; full pure/parity against final paired Afterglow.
6. Pin final Afterglow commit in tests.yml, push and create focused PRs. Root
   merges only after green checks. Keep claims honest until these gates pass.

Environment: PATH `/workspace/.tools/clojure-1.12.7.1681/bin`,
JAVA_TOOL_OPTIONS `-Duser.home=/workspace/.tools/java-home`,
CLJ_CONFIG `/workspace/.tools/clojure-config`.
No owned process remains running. No physical output activated.
