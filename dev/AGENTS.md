# Development tooling

Keep tests hardware-free and explicit about lane scope. The paired dependency
reader must not eval the fork manifest or invoke its plugins; preserve exclusions
and explicit application overrides. Keep `project.clj` canonical and use its
Leiningen profiles for backend, frontend and benchmarks. Test
runner errors must exit nonzero. Do not turn namespace loading failures into
skips. `check_roadmap.clj` validates local tracking without external writes.

Container tooling is explicit development infrastructure. Never use a host-shell
fallback for project checks or terminal input. Mount only the service-owned paired
worktree and its captured Git metadata, not the Docker socket or whole checkout
parent. Tests create disposable worktrees/containers and remove only those owned
resources. Report image build, Docker runtime and browser terminal gates separately.
