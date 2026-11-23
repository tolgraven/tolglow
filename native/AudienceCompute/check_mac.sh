#!/usr/bin/env bash
set -euo pipefail
base="$(cd "$(dirname "$0")" && pwd)"
test_dir="$(mktemp -d)"
trap 'rm -rf "$test_dir"' EXIT
xcrun swiftc -O -parse-as-library "$base/Shared/ComputeWire.swift" "$base/Tests/Loopback.swift" -o "$test_dir/probe"
"$test_dir/probe"
