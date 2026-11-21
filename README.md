# tolglow

[Start a configured show](docs/application-startup.md) with `clojure -M:run` or `lein run`. Use `--headless true` to run the same show without Studio.

Lighting-show authoring, performance controls and Studio built on the paired
[Afterglow fork](https://github.com/tolgraven/afterglow). The Studio uses
ClojureScript, re-frame and Reagent; the JVM backend uses Ring.

## Repository map

| To work on | Open |
| --- | --- |
| JVM show logic, effects, fixture definitions and programmatic control | [src/tolglow](src/tolglow/) |
| Studio and Push UI components written with re-frame and Reagent | [src/studio](src/studio/) |
| Clojure and ClojureScript code shared across the wire | [src/shared](src/shared/) |
| Three.js renderer, fog, camera and graphics worker | [js/preview](js/preview/README.md) |
| Max for Live patch and companion scripts | [resources/devices/ableton-live](resources/devices/ableton-live/README.md) |
| Push display capture worker shipped in the JVM artifact | [resources/devices/push-display](resources/devices/push-display/README.md) |
| Browser assets, CSS and generated preview bundles served by Ring | [resources/studio](resources/studio/) |
| iPhone and Apple Watch companions | [native](native/) |
| JVM tests, JavaScript tests and browser workflows | [test](test/) |
| Profilers, development tools and command entry points | [dev](dev/) and [bin](bin/) |

The canonical JVM/build manifest is [project.clj](project.clj); JavaScript
dependencies and commands live in [package.json](package.json). Run `npm run preview:build` after editing
the graphics engine. It produces the two files the Studio loads at
`/studio/preview.js` and `/studio/preview-worker.js`.

Keep Tolglow and Afterglow as sibling checkouts. See the
[architecture and feature documentation](docs/README.md),
[test commands and verification limits](docs/testing.md), and
[implementation roadmap](docs/roadmap.md). Physical lighting and device output
are explicit operations; starting the Studio does not activate a rig by itself.

## Run the studio

With Leiningen 2.13.1 and the Java/Node versions listed in
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
source directly; `AFTERGLOW_ROOT` selects another paired checkout. The Leiningen
project merges explicit application versions with the fork's project data.

```sh
./bin/test pure
./bin/test parity
npm run studio:test
npm run preview:test
npm run studio:build
lein check-roadmap
```

The parity lane includes pure tests. Browser workflows, Docker jobs, physical
controllers and Apple builds have separate gates described in the testing guide.
Read [AGENTS.md](AGENTS.md) before changing source. Independent changes use focused
branches and pull requests targeting `master`.
