# Docker development terminal and check runner

`tolglow.development.terminal` runs actual zsh PTYs and fixed check commands in owned
Docker containers. Its constructor and `status` are inert. `availability!` is an
explicit bounded image inspection; it does not pull, build or start anything.
An unavailable Docker executable/image produces a visible error, with no host
shell fallback.

## Build and tools

From the Tolglow repository:

```sh
docker build -t tolglow-dev:local -f dev/container/Dockerfile dev/container
```

The Dockerfile pins the Node and Java base image digests and verifies the versioned
Clojure tools archive with SHA256. Debian packages use the base distribution's
current repositories, so repeat builds are not claimed bit-for-bit identical.
The updated Dockerfile targets Node 26, Java 25, Leiningen 2.13.1, Git,
Python, zsh, zsh-autosuggestions and zsh-syntax-highlighting, plus Codex CLI
`0.171.0`. The earlier image actually tested below contained Node 22, Java 21,
Clojure CLI 1.11.1.1435 and Codex 0.172.0-alpha.2; the manifest update alone does
not prove the new image was built. This is an executable installation, not a provider login or a
claim of control over an unrelated Codex desktop session. The installed CLI can run the
project's declared Clojure version. Java, Node, CLI metadata and zsh were actually
executed in the built image in this workspace.

For an environment with a custom public TLS trust bundle, supply it as a
temporary BuildKit secret without disabling verification:

```sh
docker build --secret id=build_ca,src=/etc/ssl/certs/ca-certificates.crt \
  -t tolglow-dev:local -f dev/container/Dockerfile dev/container
```

The bundle is not copied into the image. Runtime dependency fetching may need
separately configured trust and caches; neither the host Maven cache nor npm
credentials are mounted implicitly. A fresh session's home and `/tmp` are tmpfs,
so downloaded home-directory dependencies disappear when that container closes.
The image does not include Swift/Xcode or claim macOS-native builds.

## Ownership and mounts

The trusted `:resolve-workspace` callback accepts a workspace ID and returns
`workspace/runtime-paths`. Browser requests cannot choose arbitrary mount paths,
Docker flags, executable or image. The container mounts only:

- The paired workspace root read/write, at the same absolute path.
- Each captured Git common directory read-only, at its original path.
- The corresponding owned worktree administration directory read/write.

Git status/diff can use the worktree index. Main references and shared object
storage remain read-only; commits require a later explicit service-owned branch
commit operation. No Docker socket, host home, physical device, entire original
checkout or parent workspace directory is mounted. The process uses the host's
configured UID/GID, read-only container root, dropped capabilities,
`no-new-privileges`, process/memory/CPU limits and no published ports.

The service default network is `:none`. The application may explicitly configure
`:bridge` for dependency/model access; this remains container networking and does
not publish inbound ports. The root application's development manager chooses
that policy. Container lifetime is independent of live-show output ownership.

## API and byte protocol

```clojure
(def tm (terminal/create-manager
          {:resolve-workspace #(workspace/runtime-paths workspaces %)
           :image "tolglow-dev:local" :network :bridge}))
(terminal/availability! tm)
(def isolated-runner (terminal/runner tm))
(terminal/open! tm {:workspace-id "cue-editor" :repo :tolglow :rows 24 :cols 80})
(terminal/input! tm session-id base64-bytes)
(terminal/read-output tm session-id cursor)
(terminal/resize! tm session-id 40 120)
(terminal/close-session! tm session-id)
(terminal/close! tm)
```

`open!` returns plain session data. Output is a bounded byte ring, default one MiB,
with an absolute cursor. A read returns at most 64 KiB in `:data` base64 and the
next `:cursor`, plus `:available-cursor`, `:first-cursor`, `:dropped?` and lifecycle
`:state`. Re-reading a cursor does not consume another client's output. Send
decoded `Uint8Array` bytes to xterm; UTF-8 and ANSI sequences may cross chunks.
When history was overwritten, the client reports that gap and resumes at the
returned cursor rather than pretending every byte was retained.

Each input request accepts at most 64 KiB. An eight-entry queue bounds pending
input and keeps a blocked PTY writer off the HTTP/show owner. Backpressure rejects
new input visibly; clients retain order and must not replay an uncertain accepted
request. PTY resize changes both the Docker client terminal and the container
init process's actual PTY before returning. Closing a session removes its uniquely
named container and terminates its PTY bridge. Manager close also interrupts owned
checks and removes their containers. Historical output remains bounded to 32
session entries, retaining active sessions first.

The host Python bridge uses `pty.fork`, argv execution and a bounded JSON/base64
protocol. It does not evaluate terminal text as host code. Docker/zsh receives
that text only inside the owned container. The check runner similarly runs a
fixed argv vector in its own named container. Docker creation and execution have
separate lifetimes. The execution timeout begins only at start/attach; the
creation wait budget defaults to 120 seconds and is configurable by the trusted
host through `:create-timeout-ms`.

Cancellation during creation marks the named resource for cleanup and returns
without killing the outstanding create request. A separately owned thread waits
for creation to settle, then removes the container without starting it.
`status` exposes `:pending-containers` with creation/cleanup phases, and close
reports `:pending-cleanup` when necessary. Cleanup ownership survives ordinary
JVM shutdown; a stalled daemon is reported as pending instead of silently losing
the resource. This is not an immediate-removal promise when Docker is still
creating the container.

`streaming-runner` supports application-agent CLIs with separate `:stdout` and
`:stderr` line callbacks, an output-byte cap and a bounded line size. Callback
handlers run on owned reader threads and must return promptly. Prompts can use
stdin. Only explicit provider environment names, including `CODEX_API_KEY`, are accepted; values are passed
through the child process environment and Docker `--env NAME`, never command-line
arguments. CLI execution/credential redaction/conversation policy belongs to the
application-agent service, not to this transport. Tests use a dummy environment
value and local Python output, never a real provider request.

## Explicit validation gates

The ordinary `terminal-test` namespace checks inert construction, invalid
ownership/input and bounded byte cursor behavior without Docker. Run the real
Docker gate explicitly after building the image:

```sh
clojure -Sdeps '{:paths ["src" "test" "resources"]}' -M -e \
  "(require 'tolglow.studio.terminal-docker-test) (let [r (clojure.test/run-tests 'tolglow.studio.terminal-docker-test)] (System/exit (+ (:fail r) (:error r))))"
```

That gate creates disposable paired Git worktrees and actual containers, executes
tools, writes only the owned tree, checks the main Git reference mount is read-only,
exercises output truncation/timeouts/cancellation, sends input to a real zsh PTY,
resizes it, reconnects by cursor and closes resources. Missing Docker or image is
a failing gate rather than a skip. Browser xterm behavior and actual mobile IME,
virtual keyboard, focus and resize remain additional frontend/device gates.

Observed on 2026-11-21 with Docker 29.9.2: terminal unit tests **3 / 17 assertions**
and the real Docker gate **6 / 45 assertions** passed. The latter also verified
separate JSON stdout/stderr, Unicode line delivery, provider environment passing
using a dummy value, oversized-line rejection and cleanup after callback failure.
The built image was `sha256:025b2622257fd112d3876278d4416d434384d625cd2f449806a540588baeba2b`;
`codex --version` and `codex exec --help` ran inside it without a provider login.

### Later reliability finding: creation cancellation

Subsequent integrated browser testing found a gap in the earlier immediate
cleanup checks. With Docker's `vfs` storage driver, cancelling `docker run` during
its daemon-side filesystem copy can return before the container becomes visible.
An immediate `docker rm` can then run too early; the container appears later in
Created state. Eight labelled orphan containers were found and explicitly
removed by the task owner, recovering roughly 14 GiB. They were not running.

The corrected boundary uses explicit create followed by start/attach. A real
executable fixture delays creation until after cancellation and checks that
removal occurs after creation, with no intervening start. Ordinary lifecycle
tests now pass **6 tests / 39 assertions**, covering interruption, the creation
wait budget and closing a terminal while creation is pending.

The real Docker gate then passed **6 tests / 45 assertions**. A further focused
run passed **2 tests / 14 assertions**, strengthening shutdown to await all
pending creation/cleanup and checking an unavailable executable plus
`CODEX_API_KEY` delivery using a dummy value. Docker's complete container
inventory was empty afterward. These counts overlap and must not be added as
independent aggregate coverage. The tests still used the earlier built image.
