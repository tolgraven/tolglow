# Development tooling

Keep tests hardware-free and explicit about lane scope. The legacy dependency
reader must not eval the application manifest; preserve exclusions and explain
any override. Keep `legacy_deps.clj` synchronized with the Afterglow copy. Test
runner errors must exit nonzero. Do not turn namespace loading failures into
skips. `check_roadmap.clj` validates local tracking without external writes.
