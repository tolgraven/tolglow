# Integrated development workspace

This extends the original authoring, agent and Neovim goals; it does not replace
them. The application must support live lighting authoring and deliberate changes
to the application itself from a cohesive workspace. Both use actual execution and
source editing, not a terminal-shaped mock UI.

## Current boundary

The studio already runs ClojureScript/re-frame/Reagent over Ring/Jetty, provides
streamed agent conversations and durable show state, and embeds an actual Neovim
WASM instance for definition editing. The reactive kernel supports durable var or
source registrations, explicit captured bindings, cells, watchers and semantic
queued actions. [Show files](../show-files.md) preserve these forms as data.

The new JVM REPL boundary is being integrated at `studio/repl.clj` and
`studio/repl_manager.clj`: isolated namespaces, serial bounded evaluation queues,
stdout/stderr/result events, completion and documentation metadata, cooperative
interruption, text history/scratch and lazily owned sessions. The module owner
reports 29 tests / 241 assertions together with existing connections tests. This
is backend evidence; a completed browser popup, complete repository editor,
application-agent worktrees and Docker terminal do not follow from that gate.

## Quake-style popup and Bruvbox interface

Bruvbox means the actual [tolgraven/bruvbox](https://github.com/tolgraven/bruvbox)
theme, including the palette used by the reference site's Neovim and highlighting.
It is not a spelling variant of Gruvbox. Reuse the pinned theme with its licence
and map REPL tokens to the same palette; do not substitute a similar-looking theme.

Provide a quick popup/drop-down REPL that can open without losing the current cue,
rig, patchbay or editor context. It needs a configurable keyboard shortcut,
resizable height, pin/close controls, a compact mobile presentation and a Bruvbox
palette consistent with surrounding editors. Preserve focus and selection when
hiding/reopening; do not steal musical keyboard or controller inputs unexpectedly.

The input supports multiline forms, history/search, completion, namespace/alias
awareness, docstrings/arglists and useful syntax/error locations. Stream actual
stdout and stderr separately from printed values, with request IDs and session
state. Include interrupt, clear output and an explicit namespace/session chooser.
Reconnect may recover known session output; it must not blindly evaluate an old
form twice. Evaluation status is visible even if the popup is hidden.

Acceptance includes keyboard-only navigation, iPad touch/virtual-keyboard use,
phone layout, Unicode output, long output bounds, cancellation, reconnect and
concurrent show performance. A cooperative JVM interrupt cannot safely terminate
arbitrary uncooperative code; do not claim hard process isolation from a thread.

## Actual Neovim across source and scratch

Extend the real WASM Neovim integration to a navigable repository file tree and
scratch buffers. Support open/search/jump, diagnostics, completion and evaluation
of a selected form, enclosing top-level form or whole buffer. Show the execution
namespace, target runtime, source location and returned value/error explicitly.
A save and a live evaluation are separate operations; both need visible results.

File operations use a named workspace root, a documented path policy and revision
checks. A changed disk file must produce a visible conflict rather than silently
replace a colleague's edit. Unsaved buffers, selections and scratch text should
survive normal workspace navigation. Actual Neovim key handling must be tested,
including escape, paste, Unicode, IME/virtual keyboard and device orientation.

One-off REPL forms should be able to register effects, cues, subscriptions or
watchers using the same APIs as authored files. Captured locals require an actual
macro/lexical capture point plus explicit portable bindings; arbitrary opaque JVM
closures cannot be reconstructed from a printed function. Persist source and
bindings deliberately, validate them before restore, and rebuild runtime caches.
A saved source form is not implicitly evaluated merely because an imported EDN
file contains text. See [reactive semantics](../reactive.md).

## Two agent work contexts

**Show authoring** operates on the currently selected lighting model: inspect the
rig, create/edit definitions, build a page or macro, audition an iteration, or
start/evolve an auto show. These actions flow through the shared validated command
queue and source/epoch ownership. Humans retain normal Push/web/REPL interaction.

**Application development** operates in an isolated Git branch/worktree for a
named project. Codex and Claude Code are execution adapters to investigate; the
workspace must report which executable/session is actually available, its working
directory, branch, task, status and output. Each agent task owns disjoint paths or
a deliberately isolated checkout. Editing implementation code must not mutate the
running show under a disguised cue-authoring operation.

Expose streamed task progress, tool/process status, changed files, diffs, tests
and a concise watcher summary. Keep uncommitted user work intact. Record the
relationship between a source revision, a candidate build and the active runtime
revision. An agent's claim that tests passed must link to actual execution output;
source generation alone does not count as an applied or verified change.

The workspace can send follow-up instructions, cancel a task, inspect a diff and
continue an existing session where its adapter genuinely supports those actions.
It must not invent control of an unrelated Codex desktop session. Reuse verified
CLI, MCP or application APIs; report unsupported features as unavailable.

## Reload every supported target deliberately

Provide explicit reload policies: **manual**, **after an agent turn**, and
**preview instance first**. These policies are scoped to the selected workspace
and target rather than being a global watcher that executes every changed file.
A watcher detects and debounces changes, classifies their target, and displays a
reviewable summary; it does not become a second show-state owner.

Targets include JVM namespaces, registered effect/cue definitions, frontend
ClojureScript and CSS, shared UI/device-display components and applicable fixture
or integration configuration. Each target advertises its actual reload method and
limits. Compiling a frontend module is not evidence that the JVM implementation or
a controller firmware protocol has also changed.

Prepare candidate source/definitions off the frame owner, run parsing/linting,
validation and relevant tests, then commit an accepted candidate at the target's
proper boundary. Keep the prior working code/assets/definition versions and make
**revert to last good** concrete. An invalid edit leaves the current application
and running cues usable with a visible diagnostic.

Running effects retain their explicit `:pinned`, `:preserve-state` or `:restart`
policy. Preview mode creates an isolated candidate instance/scene or capture sink
before adoption, with explicit cleanup. It must not collide with human-held cues,
reuse stale instance IDs or silently transmit output to a live rig. A reload or
rollback preserves/discards active state according to an explicit target contract;
reverting code does not automatically undo external actions already performed.

Acceptance requires actual invalid-edit/recovery/revert tests for each supported
target, running cue reload, concurrent preview cleanup, epoch/revision protection,
and a demonstrable after-turn path. The existing effect hot-reload engine alone
does not satisfy this whole-workspace workflow.

## Docker-backed zsh terminal

The integrated terminal should run a real zsh shell in a configured development
container, with a visible workspace mount, working directory and lifecycle.
Provide terminal resize, interactive input, process interrupt, reconnect and
bounded output/history. Verify actual Docker and shell availability; an inert
configuration or fake prompt is not an implemented terminal.

The container image and mounted source/build caches must be documented and
reproducible. Keep device/output ownership in the explicitly selected host; a
terminal session does not automatically inherit every MIDI, USB, audio or network
resource. The terminal, REPL evaluator and agent task are distinct resource types
with independent cleanup and status. Stopping one must not orphan the others or
stop the show unexpectedly.

## Persistence and completion gates

Save workspace preferences, selected project/branch, UI layout, scratch/history
and durable logical connection settings where appropriate. Live process handles,
threads, open PTYs and opaque credentials are runtime resources. Complete local
backups and deliberately filtered public sharing exports remain different formats.

Track the REPL backend, browser popup, full-codebase editor, application-agent
adapter, branch/worktree ownership, hot-reload/last-good system and terminal as
separate milestones. Each needs real execution tests, lifecycle failure tests and
user-facing status. A working narrow REPL or editor must not close the broad
workspace roadmap item.
