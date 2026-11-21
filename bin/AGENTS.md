# Command entry points

Use bash strict mode, quote paths and fail on invalid arguments. Resolve paths
relative to the script so callers may run from any directory. Pure tests must
remain independent of Afterglow; legacy tests must load the real fork. Keep
commands and dependency overrides documented in ../docs/testing.md.
