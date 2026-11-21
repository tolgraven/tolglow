# Paused: 3D rig and drawn aim-path editor

Paused at user request to conserve allowance. This branch is a committed implementation checkpoint, **not a verified merge candidate**.

- Worktree: `/workspace/tolglow-rig-editor-ui`
- Branch: `codex/interactive-rig-editor-ui`
- Integrated master `479e61e`; navigation commits `6c77866`/`e77a7af` were first cherry-picked, then master merged.
- Parent aim backend checkpoints `bed9a55` and `3cb07bc` cherry-picked (local `7013164`/`b999185`). Parent PR15 owns their final integration.
- Root reports newer master `89dd03a` and Afterglow `6e82fcf`; not integrated/verified here. Fetch explicitly: `git fetch origin master:refs/remotes/origin/master` (old repository refspec fetched only effects).

## Implemented

`rig_editor.cljs` provides re-frame draft pose manipulation, numeric XYZ/rotation, selected fixture/truss controls, bounded undo, discard, plane/snap controls, musical aim-path authoring, target selection, explicit Create / Create + run, and reopening saved paths with expected-version. Once paths explicitly launch one-shot. Runtime/epoch changes clear old drafts; callbacks carry identity/request guards.

`editor.js` owns bounded screen-space picking and ray/plane intersection, pointer capture/cancellation and camera suspension. No worker round trip is required for picking. `overlay.js` draws authoring markers/paths/plane/selection after fog with the shared renderer. Scene pose updates reuse fixture/truss objects; structural keys exclude pose. Worker proxy carries only the editor projection. Existing fixture library and top-down alternative remain available.

Shared file hunks: `views.cljs` rig-view + require/full props; `interop.cljs` owned canvas event binding; renderer pose/overlay lifecycle; math structural key; worker proxy editor input/projection. Navigation APIs remain intact. RDM agent will independently mount `[rdm/panel]` after fixture-library panel; that exact mount/require is coordinated.

## Observed checks

- Frontend 142 tests / 751 assertions passed before the last saved-path/rebase regression additions. Direct Node test runner also exited zero.
- Five `editor.test.js` tests passed after latest input changes: real Three transforms, pose reuse without rebuild, bounded picking, planes, overlay cleanup, gesture identity/cancel.
- Optimized Studio release: 149 files, zero warnings, before final camera-view/worklight controls and Escape-to-Explore changes.
- Actual compiled Chromium smoke: desktop editor mounted; worker rendered; no console/page errors. Screenshot `/tmp/rig-editor-desktop.png`. SwiftShader first frame was slow (~7 seconds); this is not hardware performance evidence.
- Parent backend focused gate: 12 tests / 84 assertions and aggregate parity checkpoint recorded in ledger.

## Still required

1. Re-run final frontend tests + direct Node runner; final production build. Last build session `93105` was started but completion was not collected before pause.
2. Actual browser pointer move/rotate, cancel/Escape, undo, numeric precision, rig save/load, drawing/editing path, Create without launch, explicit one-shot Run, stale publication/restart, saved-path edit/version conflict.
3. iPad/phone layout and real touch cancellation, worker/fallback editor lifecycle, full preview regression. Existing smoke only proves rendering/mount, not these workflows.
4. Update docs/aim-paths with user workflow, screenshots and measured limits; complete ledger evidence only after verification. New item remains in progress.
5. Integrate latest master/backend PR, check ledger for duplicate IDs/ownership, run roadmap validation and applicable regression gate. Commit generated engine bundles as repository currently tracks them.
6. Push/PR only as a reviewed implementation; do not describe this checkpoint as fully done.

## Runtime notes

Owned capture-only Studio was started on port8515 (tool sessions99794,24645,4994; earlier process266007 stopped). No physical output flags were used. A stop attempt before pause found no matching owned Java processes; verify port/process ownership before reusing. Other ports8500/8502/8511 belong to other agents/root and must not be stopped. Temporary smoke script `/tmp/rig-ui-smoke.mjs` is not a full regression test.

Build environment: PATH `/workspace/.tools/clojure-1.12.7.1681/bin`, JAVA_TOOL_OPTIONS `-Duser.home=/workspace/.tools/java-home`, CLJ_CONFIG `/workspace/.tools/clojure-config`. Recreate node_modules symlink to `/workspace/tolglow/node_modules` if needed; removed for clean git status. Chromium `/usr/bin/chromium`.
