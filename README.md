# tolglow

Lighting-show authoring, performance controls and Studio built on the paired
[Afterglow fork](https://github.com/tolgraven/afterglow). The Studio uses
ClojureScript, re-frame and Reagent; the JVM backend uses Ring.

The complete project contains:

- `src/tolglow/`: JVM application, effect authoring, integrations and host.
- `src/studio/`: Studio and shared Push display components.
- `src/engines/`: specialized browser rendering engines.
- `resources/`, `test/`, `dev/`, `bin/`: assets, behavioral tests, development tools
  and command entry points.
- Root `project.clj` and `deps.edn`: application and build configuration.

Keep Tolglow and Afterglow as sibling checkouts. See the
[architecture and feature documentation](docs/README.md),
[test commands and verification limits](docs/testing.md), and
[implementation roadmap](docs/roadmap.md). Physical lighting and device output
are explicit operations; starting the Studio does not activate a rig by itself.

## Run the studio

With the Clojure CLI and Node versions listed in
[dependency upgrades](docs/dependency-upgrades.md):

```sh
git clone https://github.com/tolgraven/afterglow.git
git clone https://github.com/tolgraven/tolglow.git
cd tolglow
npm ci
npm run studio:build
./bin/studio --port 8500
```

Open `http://127.0.0.1:8500`. The local `../afterglow` dependency uses the fork's
source directly. The Leiningen project derives its dependencies from the same
manifests and checkout, rather than requiring an outdated published fork jar.

```sh
./bin/test pure
./bin/test parity
npm run studio:test
npm run preview:test
npm run studio:build
clojure -M:check-roadmap
```

The parity lane includes pure tests. Browser workflows, Docker jobs, physical
controllers and Apple builds have separate gates described in the testing guide.
Read [AGENTS.md](AGENTS.md) before changing source. Independent changes use focused
branches and pull requests targeting `master`.
