# Development tooling

Keep tests hardware-free and explicit about lane scope. The parity dependency
reader must not eval the application manifest; preserve exclusions and explain
any override. Keep `parity_deps.clj` synchronized with the Afterglow copy. Test
runner errors must exit nonzero. Do not turn namespace loading failures into
skips. `check_roadmap.clj` validates local tracking without external writes.

Container tooling is explicit development infrastructure. Never use a host-shell
fallback for project checks or terminal input. Mount only the service-owned paired
worktree and its captured Git metadata, not the Docker socket or whole checkout
parent. Tests create disposable worktrees/containers and remove only those owned
resources. Report image build, Docker runtime and browser terminal gates separately.
