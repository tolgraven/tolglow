# Packaging

Package the Leiningen release, the production re-frame client, and the pinned paired Afterglow revision together. Check the GraalVM archive digest before running it. Preserve the runtime's licenses. Selectable application sources must reproduce both commits and support independent Git worktrees. Do not copy a developer's uncommitted files, local Git credentials or build environment into a release.

Record which machine executed the full server, what VM it used, and whether future Clojure namespaces can load. A native Swift shell does not establish Native Image compilation of the server.
