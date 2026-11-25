#!/usr/bin/env bash
set -euo pipefail

if [[ $# -gt 5 ]]; then
  printf 'Usage: %s [small|mobile|desktop|all] [sdr|hdr|all] [samples] [both|portable] [results-dir]\n' "$0" >&2
  exit 64
fi
if [[ "$(uname -s)" != Darwin ]] || ! command -v xcrun >/dev/null; then
  printf 'This probe needs macOS 13 or later and Xcode command-line tools.\n' >&2
  exit 69
fi

probe_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
build_dir=$(mktemp -d "${TMPDIR:-/tmp}/tolglow-metalfx-542.XXXXXXXX")
trap 'rm -f -- "$build_dir/probe"; rmdir -- "$build_dir"' EXIT
results_dir=${5:-"$PWD/metalfx-542-results"}

sw_vers
sysctl -n hw.model machdep.cpu.brand_string
xcrun --sdk macosx --show-sdk-version
xcrun --sdk macosx clang --version
shasum -a 256 "$probe_dir"/{run.sh,probe.m,scene.h,bilinear.metal}

xcrun --sdk macosx clang -std=gnu11 -O3 -fobjc-arc -mmacosx-version-min=13.0 \
  -Wall -Wextra "$probe_dir/probe.m" \
  -framework Foundation -framework CoreGraphics -framework ImageIO \
  -framework Metal -framework MetalFX -o "$build_dir/probe"

mkdir -p -- "$results_dir"
"$build_dir/probe" "${1:-all}" "${2:-all}" "${3:-240}" "${4:-both}" \
  "$results_dir" "$probe_dir/bilinear.metal"
