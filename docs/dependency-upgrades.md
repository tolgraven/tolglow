# Dependency upgrades — 2026-11-21

Both repositories now pin **Clojure 1.12.7**. The development image and CI use
**Clojure CLI 1.12.7.1681**. Clojure 1.13 is an alpha release and was not selected.
Release versions were queried from Maven Central, Clojars, npm, upstream GitHub
releases and Docker registry manifests; this is not a memory-based version list.

[The recorded registry inventory](dependency-inventory-2026-11-21.json) retains
the original versions, available releases and metadata URLs. A current old
version can mean an upstream project has not released newer software; those
artifacts are listed below rather than silently omitted.

## JVM dependencies

| Artifact | Previous | Selected stable |
| --- | --- | --- |
| `amalloy/ring-buffer` | 1.3.1 | 1.3.1 |
| `buddy` | 2.0.0 | `buddy/buddy-auth` 3.0.323 + `buddy/buddy-core` 1.12.0-430 |
| `camel-snake-kebab` | 0.4.3 | 0.4.3 |
| `clj-time` | 0.15.2 | 0.15.2 |
| `com.climate/claypoole` | 1.1.4 | 1.1.4 |
| `com.evocomputing/colors` | 1.0.6 | 1.0.6 |
| `com.fzakaria/slf4j-timbre` | 0.3.21 | 0.4.1 |
| `com.roomkey/lein-v` | 7.2.0 | 7.2.0 |
| `com.taoensso/timbre` | 5.2.1 | 6.8.0 |
| `com.taoensso/tower` | 3.0.2 | 3.0.2 |
| `com.taoensso/truss` | 1.6.0 | 2.5.1 |
| `com.taoensso/tufte` | 2.2.0 | 3.1.0 |
| `compojure` | 1.7.1 | 1.7.2 |
| `environ` | 1.2.0 | 1.2.0 |
| `http-kit` | 2.8.0 | 2.8.1 |
| `instaparse` | 1.5.0 | 1.5.0 |
| `java3d/j3d-core` | 1.3.1 | `org.jogamp.java3d/java3d-core` 1.7.2 |
| `java3d/j3d-core-utils` | 1.3.1 | `org.jogamp.java3d/java3d-utils` 1.7.2 |
| `java3d/vecmath` | 1.3.1 | `org.jogamp.java3d/vecmath` 1.7.2 |
| `lein-codox` | 0.10.8 | 0.10.8 |
| `lein-environ` | 1.2.0 | 1.2.0 |
| `lein-resource` | 17.06.1 | 17.06.1 |
| `lein-shell` | 0.5.0 | 0.5.0 |
| `markdown-clj` | 1.12.1 | 1.12.15 |
| `metosin/ring-http-response` | 0.9.4 | 0.9.5 |
| `ola-clojure` | 0.1.8 | 0.1.8 |
| `org.clojars.brunchboy/protobuf` | 0.8.3 | 0.8.3 |
| `org.clojure/clojure` | 1.11.4 | 1.12.7 |
| `org.clojure/core.async` | 1.6.681 | 1.9.865 |
| `org.clojure/core.cache` | 1.1.234 | 1.2.999 |
| `org.clojure/data.json` | 2.5.0 | 2.5.2 |
| `org.clojure/data.zip` | 1.1.0 | 1.1.1 |
| `org.clojure/math.numeric-tower` | 0.1.0 | 0.1.1 |
| `org.clojure/tools.cli` | 1.1.230 | 1.4.256 |
| `org.clojure/tools.nrepl` | 0.2.13 | `nrepl/nrepl` 1.8.0 |
| `org.clojure/tools.reader` | 1.4.2 | 1.6.0 |
| `org.deepsymmetry/beat-link` | 8.0.0 | 8.0.0 |
| `org.deepsymmetry/lib-carabiner` | 1.2.0 | 1.2.0 |
| `org.deepsymmetry/wayang` | 0.1.8 | 0.1.8 |
| `overtone/at-at` | 1.3.58 | 1.4.65 |
| `overtone/midi-clj` | 0.5.0 | 0.5.0 |
| `overtone/osc-clj` | 0.9.0 | 0.9.0 |
| `prone` | 2021-04-23 | 2021-04-23 |
| `re-frame/re-frame` | 1.4.7 | 1.4.7 |
| `reagent/reagent` | 2.0.1 | 2.0.1 |
| `ring-middleware-format` | 0.7.5 | 0.7.5 |
| `ring-mock` | 0.1.5 | `ring/ring-mock` 0.6.2 |
| `ring/ring-core` | 1.12.2 | 1.15.6 |
| `ring/ring-defaults` | 0.5.0 | 0.7.1 |
| `ring/ring-devel` | 1.12.2 | 1.15.6 |
| `ring/ring-jetty-adapter` | 1.12.2 | 1.15.6 |
| `ring/ring-session-timeout` | 0.3.0 | 0.3.0 |
| `selmer` | 1.12.61 | 1.13.5 |
| `thheller/shadow-cljs` | 3.5.14 | 3.5.14 |
| `thi.ng/color` | 1.5.1 | 1.5.1 |
| `thi.ng/math` | 0.3.2 | 0.3.2 |
| `uk.co.xfactory-librarians/coremidi4j` | 1.6 | 1.6 |

Leiningen uses nearest-first transitive resolution, whereas tools.deps selects
newer competing versions. Explicit Encore 3.172.1, SLF4J 2.0.20 and commons-codec
1.23.0 pins keep both builds aligned. Without the Encore pin, Timbre's older
transitive version lacks `threadlocal`, which Tufte 3.1 requires; the actual Lein
parity gate exposed this. The obsolete Buddy umbrella was replaced with the
maintained auth/core modules actually used by the application.

The maintained nREPL coordinate replaces `org.clojure/tools.nrepl`; Afterglow
uses `nrepl.server`. Timbre 6 moved Rotor to
`taoensso.timbre.appenders.community.rotor`. JogAmp replaces the obsolete Java3D
coordinates; geometry imports and parity tests use `org.jogamp.java3d` and
`org.jogamp.vecmath`. No duplicate class or runtime compatibility layer was added.

Shadow CLJS 3.5.14 resolves **ClojureScript 1.12.145**. Ring 1.15.6 resolves
**Jetty 12.1.14**. Existing adapter code runs unchanged against the updated
server API. Parity dependency readers now express the paired checkout as a
local dependency, avoiding deprecated external source paths in the new CLI.

## Browser and documentation packages

| Package | Previous | Selected |
| --- | --- | --- |
| `@capacitor/core` | 8.6.2 | 8.6.2 |
| `@capacitor/motion` | 8.0.1 | 8.0.1 |
| `@xterm/addon-fit` | 0.11.0 | 0.11.0 |
| `@xterm/xterm` | 6.0.0 | 6.0.0 |
| `react` | 19.3.1 | 19.3.1 |
| `react-dom` | 19.3.1 | 19.3.1 |
| `three` | 0.181.2 | 0.187.0 |
| `esbuild` | 0.25.12 | 0.28.2 |
| `playwright-core` | 1.57.0 | 1.65.1 |
| `shadow-cljs` | 3.5.14 | 3.5.14 |
| `@antora/cli` | 3.1.2 | 3.2.3 |
| `@antora/site-generator` | 3.1.2 | 3.2.3 |
| `@antora/lunr-extension` | ^1.0.0-alpha.8 | 1.0.0-alpha.13 |
| `@openai/codex` | 0.172.0-alpha.2 | 0.171.0 |

All direct browser package pins were checked. React, re-frame, Reagent,
Capacitor and xterm were already current. Both npm lockfiles were regenerated
with the selected direct versions. `npm outdated --json` returned `{}` in each
repository. Antora uses its current alpha-only Lunr extension channel, pinned
exactly instead of a floating range.

The container uses stable Codex **0.171.0**, replacing the previously selected
0.172.0-alpha.2. Its exact isolated `exec --json --ephemeral --ignore-user-config`
and `--no-daemon` flags were executed with `--help`; no provider request occurred.

## Toolchains, image and vendored editor

The image uses the active LTS runtime families **Node 26** and **Java 25**.
Both base images are pinned by digest in `dev/container/Dockerfile`. The CLI
archive is SHA256-verified against the upstream release asset. CI tests Java
17, 21 and 25 and uses Node 26 for frontend builds. Local gates exercised Java
21.0.13 and Temurin 25.0.5.1, and Node 26.13.1.

GitHub Actions now use checkout 7.0.1, setup-java 6.3.0, setup-node 7.1.0 and
setup-clojure 13.7.0. Leiningen 2.13.1 was downloaded and SHA256-verified for
the actual application-manifest dependency resolution gate.

The upgraded development image is
`sha256:0f3c5dfe1fb584a7afe8ff588e946e8900845b36693b0cfac64ffe56fba6e485`,
tagged `tolglow-dev:local` and `tolglow-dev:dependency-upgrade`. The previous
working image is retained as `tolglow-dev:before-dependency-upgrade`. The build
used the environment public CA bundle as a temporary BuildKit secret; no
private credentials or CA bundle were baked into the image.

Neovim WASM was upgraded from the 2026-10-13 nightly to the official
2026-11-21 nightly (`f2f87f5fc55fbb4c0307bd775d625515b2de8dea`). Stable
Neovim 0.12.6 does not publish a WASM artifact, so the existing explicit nightly
channel is retained. All three runtime files were extracted unchanged from the
hash-verified release archive. MessagePack transport uses unchanged msgpackr
2.1.4 no-eval distribution. [Runtime provenance and hashes](../resources/studio/vendor/neovim/runtime-version.json)
and [the integration notes](../resources/studio/vendor/neovim/README.md) describe
the exact artifacts. The bounded worker adapter and authentic Bruvbox source
profile remain intact.

The native Apple package has no third-party Swift dependencies. Its iOS 18 /
watchOS 11 deployment targets and Swift 5.9 package minimum are compatibility
baselines, not pinned library releases. Current XcodeGen 2.48.0 was verified
from its release metadata; Apple target builds still require macOS/Xcode.

Historical Afterglow web assets (jQuery/Bootstrap and related controls) are
retained as feature-parity reference sources. They are not loaded by the new
re-frame Studio. Updating those historical snapshots would change the parity
reference; the runtime dependency upgrade does not present them as current UI.

## Executed verification

* Afterglow parity: **269 tests / 2,943 assertions**, passing on Java 21 and 25.
* Tolglow parity: **442 tests / 3,471 assertions** on Java 21, then **445 / 3,487**
  on Java 25 after three terminal cleanup regressions entered the shared tree.
* Frontend: **125 tests / 669 assertions**, zero failures and zero warnings.
* Optimized frontend: **146 files**, zero compiler warnings, on Node 26 / Java 25.
* Preview: **16 tests**, including actual software WebGL2 fog, shadows, depth
  and finite lens-aperture behavior; the renderer owner separately checks images.
* Authentic Bruvbox in the new Neovim WASM: actual buffer/cursor, native palette,
  all 16 terminal colors and clean teardown passed in Chromium.
* Actual ephemeral Jetty 12 HTTP snapshot and SSE REPL: Unicode stdout and
  completed arithmetic result passed through the real HTTP client/server.
* Antora 3.2.3 local documentation build passed with an explicit workspace cache.
* Leiningen 2.13.1 `deps` completed against the updated application manifest.

* New-image Docker runtime: **7 tests / 59 assertions** passed, including owned
  worktree execution, Java 25 / Node 26 / CLI versions, PTY, bounded streaming,
  cancellation, pre-start failure and final empty container inventory.

These checks do not claim physical
lighting/controller/mobile-device qualification or a provider-backed agent run.

## Complete Tolglow project

The repository is a standard Clojure project: `src/tolglow/`, `test/`,
`resources/`, `dev/` and canonical root `project.clj`. The Lein manifest loads
`dev/tolglow/build_manifest.clj`, which merges explicit Studio versions with
complete fork dependencies from the sibling Afterglow manifest.
The fork manifest is read as data with reader evaluation disabled; its plugins
and startup hooks are not executed. Source/resource paths point to the declared
local fork, so an old installed Afterglow jar cannot silently take precedence.

`lein deps`, `lein run --port 8500`, `lein test-pure`, `lein test-parity` and
`lein uberjar` are supported entry points. Plain `lein test` excludes the explicit
Docker suite; hardware and container gates are never silently selected. Frontend
and benchmark entry points use the `:cljs` and `:benchmark` Lein profiles. The
frontend pins Closure Compiler v20261021 from shadow-cljs 3.5.14's POM to avoid
Lein's nearest-dependency resolution choosing an incompatible version.
The manifest regression checks actual paired source directories, version
precedence, classpath completeness and absence of a shadowing fork artifact.

The executable uberjar was built and launched from `/tmp` (outside the source
checkout). The actual HTTP server returned its snapshot, accepted an Ember cue
launch, exposed the active instance, and shut down cleanly. This validates the
packaged main class and resources instead of merely checking jar creation.

After the source-layout migration and packaged PTY resource fix, the complete
**Leiningen parity lane passed 477 tests / 3,717 assertions**. The rebuilt uberjar
contains the PTY helper as an actual `jar:` classpath resource. Plain Lein test
selection was also executed for the manifest regression: 1 test / 9 assertions.
The final resolver pins and maintained Buddy modules retain the Afterglow parity
result of 269 tests / 2,943 assertions.
