# Code-cue validation proof for Tolglow #311

The proof attaches one Clojure source expression to the versioned effect owned by a cue. The grid cell and its running instance resolve to the same cue and open the same source. A checked candidate runs in a separate model through the actual Tolglow compiler, Afterglow effect engine and capture renderer. Admission checks the exact UTF-8 buffer and the captured show epoch, cue version, effect version, active instances, their parameters and targets, and the rig. It then publishes the already checked definition through `:registry/publish`. The normal `:preserve-state` engine policy changes the running output while retaining its instance ID.

The proof uses one explicit source format. A restricted Clojure expression returns the portable `:name`, `:params` and `:outputs` fields of its effect. The host assigns the effect ID and version, and the registry stores the source alongside that revision. Source from the version archive can run through the same check and publish flow as a newer version. This slice covers cues that exclusively own their effect and opt into state-preserving reload. [Proof](proof/code_cue.clj) · [Tests](proof/code_cue_test.clj)

## Existing boundaries

- The modern cue model refers to an effect by ID and can pin a version. Its current Studio definition route reads EDN. The older `tolglow.cue/code` calls the legacy Afterglow closure API and has no source handle to recover from a running instance. [Registry](https://github.com/tolgraven/tolglow/blob/56a785c5/src/tolglow/registry.clj) · [Studio routes](https://github.com/tolgraven/tolglow/blob/56a785c5/src/tolglow/studio/web.clj) · [Legacy wrapper](https://github.com/tolgraven/tolglow/blob/56a785c5/src/tolglow/cue.clj#L467)
- Tolglow bundles Neovim WASM and the Bruvbox subset. The pinned reference website also carries Clojure syntax and indentation, vim-sexp, rainbow parentheses, commentary and other editing helpers; its Meta and NERDTree integrations are useful for repository navigation. External-process plugins require separately supported adapters. [Tolglow bundle provenance](https://github.com/tolgraven/tolglow/blob/56a785c5/resources/studio/vendor/neovim/README.md) · [Reference profile](https://github.com/tolgraven/tolgraven/blob/9daaea09577d9c7b80519e5c309583faf789c855/doc/neovim.md#browser-profile-and-repository-workspace)
- The packaged application source archive contains committed Tolglow and Afterglow Git objects, extracted as versioned checkouts. The Develop workspace runs named checks on paired dirty worktrees and fences results with fingerprints. The native host reads the selected show folder through a separate SCI context and exposes live show commands to its REPL. These are three distinct sources of state. The proof creates a fresh SCI guest for drafts and uses no live REPL context. [Packaged archive](https://github.com/tolgraven/tolglow/blob/56a785c5/native/Packaging/archive-sources) · [Workspace checks](https://github.com/tolgraven/tolglow/blob/56a785c5/docs/dev-workspaces.md#checks-and-candidate-readiness) · [Native show source](https://github.com/tolgraven/tolglow/blob/56a785c5/native/EmbeddedHost/src/tolglow/native/show_source.clj)

## Contracts for the packaged Studio path

1. Resolve **Edit code** from the grid and running instance through the active show's cue identity. Establish ownership of the selected show's actual source before making the action available. If that source lives in a file, use the selected show root and fence dirty snapshots and writes by the exact file hash. The proof keeps the source in the cue-owned effect record.
2. Mount Tolglow's Neovim WASM worker on that cue and port the applicable pinned reference plugins and mappings with their license and integrity manifest. The current engine reads buffer `0`, which follows the current Neovim buffer. Pin the cue buffer handle before enabling plugins that can switch windows. Capture its changedtick and complete text together and use that same handle for lint annotations and manual checks. Confirm keyboard and touch behavior in the packaged app. [Neovim engine](https://github.com/tolgraven/tolglow/blob/56a785c5/resources/studio/neovim-engine.js#L30)
3. Bind asynchronous lint results to the cue ID, buffer generation and draft hash. Project the current result to Neovim, the cue and the Apply action. The proof provides Clojure reader diagnostics. Full live lint, Neovim annotations and the cue status UI are pending.
4. Evaluate submitted drafts in a bounded worker process using a minimal portable preview capture. Keep the live model, host credentials and physical output handles with the show owner. A configured isolated runner is required for a browser request. Docker was available in this environment with no local images. The harness runs only its fixed test inputs.
5. Store completed checks inside the authenticated show owner. A browser may request apply using the check's opaque ID and a fresh read of the cue buffer. Recheck its source digest and current identity on the show owner queue, then publish the checked definition. Report reload observation from the engine and offer the same staged path for the previous source. The prototype calls the pure command transition directly, without mounting an HTTP endpoint or implementing the live queue entry point.
6. Verify bad syntax, a throwing expression, a failed output capture, a working running-cue update, an out-of-order or stale result, and rollback in the packaged Studio app. The headless proof covers those domain transitions except the browser delivery order. [Issue acceptance](https://github.com/tolgraven/tolglow/issues/311#issuecomment-7151258346)

## Reproduce the headless check

The harness ran with the committed source of Tolglow `56a785c527034a5914f0f1833c10741c2f4c8282`, Afterglow `99f61c5bed2850daaeb87bc5b84ddf0d3c60bf64`, Clojure 1.12.7 and SCI 0.15.60. In a Tolglow checkout paired with those Afterglow sources, start with the manifest's embedded-host classpath and prepend this directory.

```sh
proof_dir=/workspace/artifacts/tolglow-code-cue-311
cue_classpath=$(lein -o with-profile +embedded-host classpath)
timeout 90s java -Xmx512m -Djava.awt.headless=true \
  -cp "$proof_dir:$cue_classpath" clojure.main -m proof.code-cue-test
```

The recorded run used clean `git archive` exports of both `src` trees and the cached manifest dependencies, including SCI. [Check output](check.txt)
