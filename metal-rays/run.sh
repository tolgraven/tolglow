#!/bin/bash
set -euo pipefail

if [[ $(uname -s) != Darwin ]]; then
  echo 'status=portable reason=macOS-required'
  exit 2
fi

probe_dir=$(cd -- "$(dirname -- "$0")" && pwd)
build_dir=$(mktemp -d "${TMPDIR:-/tmp}/tolglow-metal-538.XXXXXXXX")
trap 'rm -rf "$build_dir"' EXIT

sw_vers
printf 'hardware_model=%s host_arch=%s\n' "$(sysctl -n hw.model)" "$(uname -m)"
system_profiler SPDisplaysDataType | awk '/Chipset Model:|Total Number of Cores:|Metal Support:/ {print}'
xcodebuild -version
xcrun --sdk macosx --show-sdk-version

cat > "$build_dir/Info.plist" <<'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict><key>MetalCaptureEnabled</key><true/></dict></plist>
PLIST

xcrun --sdk macosx metal -std=metal3.0 -gline-tables-only -frecord-sources -c \
  "$probe_dir/intersections.metal" -o "$build_dir/intersections.air"
xcrun --sdk macosx metallib "$build_dir/intersections.air" -o "$build_dir/intersections.metallib"
xcrun --sdk macosx clang -fobjc-arc -O2 -Wall -Wextra -mmacosx-version-min=14.0 \
  -framework Foundation -framework Metal \
  -Wl,-sectcreate,__TEXT,__info_plist,"$build_dir/Info.plist" \
  "$probe_dir/probe.m" -o "$build_dir/probe"

shasum -a 256 "$probe_dir/probe.m" "$probe_dir/intersections.metal" "$probe_dir/fixture.tsv" \
  "$build_dir/intersections.metallib" "$build_dir/probe"
"$build_dir/probe" "$build_dir/intersections.metallib" "$probe_dir/fixture.tsv"
