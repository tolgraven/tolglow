# Work log

## 2026-11-21 — Architecture foundation

Checked out `tolgraven/tolglow` default `effects` and `tolgraven/afterglow` default
`master` into separate local repositories. Read `tolgraven/tolgrAVen` only as a
Neovim implementation reference. Source SHAs are in codebase-survey.md; fork
branch relationships are in branch-compatibility.md. No remote branches,
production services, lights or devices were changed.

Completed independent agent work:

| Agent/lane | Owned output | Evidence |
| --- | --- | --- |
| Tolglow characterization | Iterator/fixture tests and source/test folder guidance | 20 tests / 103 assertions passed |
| Afterglow characterization | Context, channel, timing, assigner and show lifecycle tests | Full engine lane 47 tests / 379 assertions passed |
| Runtime design | Functional migration and rendering/acceleration plans | Source-grounded contracts and acceptance gates |
| Control design | Shared control/web architecture and folder guidance | Actual Push/web/Neovim source reviewed; kernel independently reviewed |
| Product specifications | Show authoring and integrations | Request coverage reviewed against the 30-item ledger |
| Runtime kernel | New isolated context evaluator and tests | Pure lane 13 tests / 51 assertions; actual forced out-of-order worker execution |
| Integrator | Build/test tooling, CI definitions, ledger, survey and guidance | Local suite execution and consistency checks |
| Benchmark follow-up | Actual assignment reduction/channel packing harness | 100/1,000/10,000-head runs; retained raw samples and stable checksums |

The characterization tests call real code and dependencies. No whole namespaces
were stubbed. Existing color/fade/step tests are retained. A vacuous core test was
replaced with meaningful show-context tests. The only production addition is
`afterglow.runtime`, which is explicitly experimental and unconnected to the
legacy show loop. There are no Tolglow production changes.

Observed commands after final runner edits:

```text
afterglow/bin/test legacy  -> 47 tests, 379 assertions, 0 failures/errors
tolglow/bin/test legacy    -> 20 tests, 103 assertions, 0 failures/errors
afterglow/bin/test pure    -> 13 tests, 51 assertions, 0 failures/errors
tolglow/bin/test pure      -> 10 tests, 57 assertions, 0 failures/errors
```

The legacy totals include the pure lanes. The local runtime was OpenJDK 21.0.13,
Clojure 1.11.4, CLI 1.11.1.1435. CI defines Java 17 and 21 lanes but hosted CI
and local Java 17 execution remain unobserved. Local proxy/cache configuration
was used to download dependencies and is not part of either repository.

Test tooling derives application dependencies from `project.clj` without eval
or Leiningen plugin/prep execution. The historical Beat Link 8.0.0-SNAPSHOT was
unavailable; a visible test-only override uses the stable 8.0.0 release. Neither
the original application manifest nor DJ Link compatibility was changed/tested.

An initially suspected iterator defect was disproved by executing the new tests
against original source: zero-arity `conj!` is valid in Clojure 1.11.4. The proposed
production edit was reverted. One strict Boolean assertion for legacy `:ending`
was corrected to its existing truthy keyword contract, with no production edit.

The default-branch baseline is not the full user's application pair. Tolglow
imports `Param` and `color-type` missing on Afterglow `master`; `tolmaster` has
these and several optimization experiments, but also undeclared thi.ng
requirements. The active branch/local changes were requested from the user.
Until confirmed, full application startup and migration remain explicitly
blocked in the ledger. Do not silently switch branches or throw away fork work.

The measured assignment/packing stage baseline is recorded separately in
[benchmark-baseline.md](benchmark-baseline.md), with workload, environment,
checksums, percentiles and excluded costs. It does not establish end-to-end
renderer throughput, network performance, GPU benefit or visual fidelity.
The serial stage's observed p50 was 36.110 ms for 1,000 RGBW heads and 366.670 ms
for 10,000. It measures default-branch code, not the existing typed-buffer
optimizations on `tolmaster`; compare the correct active branch before conclusions.

New CI workflow files, shell entry points, local Markdown links and work-ledger
validation are checked locally. Hardware, full startup, browser interaction and
packaging are outside this foundation's verified coverage. No changes were
pushed and no pull requests were created.
Both local checkouts use `codex/architecture-foundation`; changes are left
uncommitted for review. Completed agent claims are released in the ledger.

Next: confirm the active application pair, preserve and benchmark existing
`tolmaster` improvements, capture whole-frame replay fixtures, and implement the
shared control/coordinator contracts before rewriting individual devices.
