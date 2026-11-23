# Paired application workspaces

`tolglow.development.workspace` owns fresh Git worktrees for changes to the application
and its Afterglow dependency. It is separate from show state, effect editing and
the REPL. Creating a manager creates no directories, branches or processes.
Creating a workspace uses each repository's committed `HEAD` unless a specific
base revision was supplied. Existing uncommitted work stays in its original
checkout; it is neither copied nor reset.

The two worktrees share an ID and branch name (`tolglow-agent/<id>`) but remain
independent Git repositories. Their common parent makes Tolglow's `../afterglow`
dependency resolve to the paired fork. Each repository root and workspace parent
is configured by the host, not chosen through arbitrary browser paths.

In Studio, open **Develop** to create or select a paired workspace. **Files** opens the selected repository’s editor with Save and Read disk beside its engine picker. On a phone, **Browse files** brings back the tree while keeping the current buffer. **Diff**, **Checks & preview** and **Terminal** use the selected workspace. **Coding agent** opens the application-task composer and progress feed. A task’s **Review workspace changes** action selects its own workspace and opens its diff.

## Service API

```clojure
(def manager
  (workspace/create-manager
    {:repositories {:tolglow "/projects/tolglow" :afterglow "/projects/afterglow"}
     :workspace-parent "/projects/agent-workspaces"
     :runner isolated-runner
     :check-profiles {:pure {:repo :tolglow :argv ["./bin/test" "pure"]
                            :timeout-ms 180000 :max-output-bytes 1048576}}}))
(workspace/create! manager {:id "cue-editor"})
(workspace/tree manager "cue-editor" :tolglow {:offset 0 :limit 500})
(workspace/read-file manager "cue-editor" :tolglow "src/tolglow/studio.clj")
(workspace/write-file! manager "cue-editor" :tolglow "src/tolglow/studio.clj"
                       {:expected-hash previous-sha256 :content edited-text})
(workspace/diff manager "cue-editor")
(workspace/check! manager "cue-editor" :pure) ; run-id, result promise, cancel!
(workspace/stage-preview! manager "cue-editor" {:required-checks [:pure]})
(workspace/close! manager)
```

`status` returns plain data, including `:running-checks`, completed results and a
prepared candidate. File writes require the previous SHA256 or `:missing` for a
new file. UTF-8 text is limited to two MiB; binary files report size/hash without
becoming an editable text buffer. New files require an existing parent directory.
Directory creation/deletion/renames are not yet exposed through this file API.

File access traverses `SecureDirectoryStream` handles with `NOFOLLOW_LINKS` at
each component. Absolute paths, `..`, `.`, `.git`, backslashes, control characters
and symlink traversal are rejected. A write checks its hash and replaces the file
through the same directory handle, preserving existing POSIX executable bits.
These APIs require a filesystem offering secure directory streams (the tested
Linux filesystem does). A concurrent external editor can still change a regular
file after the hash check; this is optimistic concurrency, not a distributed file
lock across unrelated editors.

Tree listing is paginated and excludes ignored files. Diff includes tracked
changes and untracked names. Git hooks, fsmonitor, external diff/textconv and
captured filter commands are disabled for the service's read operations. Worktree
Git/common-directory paths are captured at creation. Each later service read
uses a private temporary administration directory containing the current owned
HEAD and a trusted `commondir`, with an explicit path to the owned index. This
also supports staged split indexes. Editing a worktree's `.git` pointer or
`commondir` cannot redirect those reads: Git 2.56.1's ref backend can follow the
on-disk pointer even when `GIT_COMMON_DIR` reports the captured directory, so
setting environment variables alone is insufficient. The temporary view is
removed after success, failure or cancellation; the service does not repair or
overwrite the original metadata. Fingerprints and check-result admission use
these same reads. Arbitrary commands inside an explicitly opened terminal retain
their own Git behavior. The private `runtime-paths` API exposes the captured directories
only to the container service. Public status omits those metadata mount paths.

## Restart recovery

Successful creation and source writes atomically maintain
.tolglow-workspaces.edn in the configured workspace parent, outside every
container's writable workspace mount. It records issued workspace identities,
paired Git branches/base revisions, captured metadata directories and revision
counters. A filesystem lock and merge preserve registrations made by another
Studio instance sharing the parent. It contains no terminals, processes, provider credentials or check
approval. Catalog writes are bounded to one MiB, use mode 0600, sync their bytes,
and replace through a secure directory handle.

On application startup, the host explicitly calls workspace/recover!.
Manager construction itself remains inert. Recovery verifies source repositories,
canonical paths, Git common/admin directory ownership, current branch and
registered worktree before publishing any recovered entries. An unsafe entry
rejects the whole recovery; it does not adopt arbitrary directories. Dirty edited
files are preserved exactly. Old check results and prepared preview readiness are
cleared, so a restarted service must run its required checks again.

Worktrees created before this catalog existed remain on disk, but are not
automatically adopted by directory scanning. Their ownership must be explicitly
re-established from trusted creation records. Saving/loading a lighting show does
not substitute for this development catalog.

The focused workspace suite passes **16 tests / 100 assertions**, including dirty
worktree recovery, unchanged hashes and paths, invalidated approvals, tampered
mounts, symlink rejection, trailing-document rejection and two independent
Studio owners retaining each other's issued workspaces. Combined with the
terminal lifecycle suite, the checkpoint is **22 tests / 139 assertions**.

## Checks and candidate readiness

The Develop panel subscribes to `[:development/workspace "lighting"]` for the selected workspace, on Studio’s existing operator-authenticated Transit WebSocket. It receives the initial status followed by changes to running checks, their results, the worktree revision and preview readiness. An edit or a completed check in another client or at the REPL also updates the panel, even while the lighting clock is paused. Switching workspaces, hiding the page or leaving Develop releases the observer when its last client leaves. Reconnecting requests the current status. The one-time workspace list supplies available check profiles. REPL and HTTP callers can still request a status snapshot directly.

Only named host-configured argv profiles can run. Without a runner, `check!`
rejects the operation explicitly. The default application uses the
[Docker runner](terminal.md). The exported `run-trusted-process!` helper is for
trusted Git operations and explicit disposable test fixtures; it is not a sandbox
for project code.

The runner receives `:workspace-id`, `:workspace-root`, `:cwd`, `:argv`,
`:timeout-ms`, `:max-output-bytes` and optional `:stdin`. It returns `:exit-code`,
`:output`, `:timed-out?` and `:truncated?`. Two owned workers and an eight-job queue
bound check concurrency. Cancellation interrupts the runner; container cleanup
belongs to that runner. Closing the manager cancels active/queued check handles
but retains worktrees and branches for review.

A check fingerprints both worktrees before and after execution. Any intervening
edit makes its result stale. Fingerprints include HEAD and tracked/unignored file
contents through an 8 KiB buffer, with default limits of 64 MiB per file and
1 GiB per paired workspace. `:fingerprint-max-file-bytes` and
`:fingerprint-max-bytes` configure explicit bounds; the existing Afterglow PSD
documentation assets alone exceed 256 MiB. Limits count bytes actually read,
including file growth. Symlinks are rejected at this gate. Preparing a candidate requires at least one
configured check, every required check passed, and exact current fingerprints.
A successful file write clears prior readiness. An external edit may leave a
previous status projection visible until refreshed; any eventual activation must
revalidate its fingerprint again.

Preparing a candidate starts no preview server, changes no running show and does
not commit, merge or deploy. Watcher summaries, automatic after-turn reload,
target-specific last-good rollback and actual Codex/Claude Code execution adapters
remain separate work. Worktree directories survive service shutdown; the owned
catalog recovery described above restores their registrations on explicit startup.

## Validation

`tolglow.development.workspace-test` creates real disposable Git repositories and
paired worktrees. It covers main-checkout preservation, hash conflicts, atomic
replacement permissions, binary/tree bounds, symlink/metadata traversal,
captured-Git-directory redirection, current-fingerprint readiness, edits during a
check, output limits, blocked stdin timeouts and cancellation. No actual project
checkout is modified by those tests.

Observed on 2026-11-21: **12 tests / 81 assertions passed**, including cleanup
when metadata discovery fails after both worktrees were added. The terminal's
independent unit and explicit Docker results are recorded in its documentation.
The streaming fingerprint regression also hashes 288 MiB of sparse regular assets
and checks explicit per-file/total limits without allocating asset-sized buffers.
An earlier combined run passed **19 tests / 139 assertions**; see the subsequently
discovered Docker creation/cancellation issue in the terminal documentation.

The subsequent Git metadata isolation regression passed **18 tests / 120
assertions** on Git 2.56.1. Initial fixture commits now have distinct messages,
so identical trees created within one second cannot accidentally share a commit
ID and mask cross-repository ref lookup. The tests reproduce the original
`fatal: bad object HEAD` failure before the fix, then verify redirected pointer
isolation, current owned HEAD/staged-index behavior, recovered diffs, check
fingerprints and temporary-view cleanup.
The full pure lane on this branch passed **457 tests / 3,610 assertions**.

Run `lein run -m tolglow.qa.workspace-layout` after building Studio to exercise the paired Git workflow, compact layout and bundled Neovim in Chromium. The harness creates two temporary local repositories and uses the preview output sink.
