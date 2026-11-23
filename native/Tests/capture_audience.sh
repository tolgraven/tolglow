#!/usr/bin/env bash
# Capture the actual standalone Clip. Simulator radios cannot establish a stage session.
set -euo pipefail
native_root="$(cd "$(dirname "$0")/.." && pwd)"
output_dir="${1:?Pass an artifact directory}"
mkdir -p "$output_dir"
device="$(xcrun simctl list devices available -j | python3 -c 'import json,sys
devices = json.load(sys.stdin)["devices"]
print(next(d["udid"] for runtime,group in sorted(devices.items(), reverse=True) if ".iOS-" in runtime for d in group if d["name"].startswith("iPhone")))')"
xcrun simctl boot "$device" || xcrun simctl bootstatus "$device" -b
xcrun simctl bootstatus "$device" -b
xcrun simctl status_bar "$device" override --time 21:00
clip="$native_root/DerivedData/Build/Products/Debug-iphonesimulator/TolglowAudienceClip.app"
xcrun simctl install "$device" "$clip"
xcrun simctl io "$device" recordVideo "$output_dir/audience-clip.mp4" >/dev/null 2>&1 &
recorder=$!
trap 'kill -INT "$recorder" 2>/dev/null || true' EXIT
sleep 1
xcrun simctl launch "$device" org.tolglow.ios.Clip
sleep 3
xcrun simctl io "$device" screenshot "$output_dir/audience-clip.png"
kill -INT "$recorder"
wait "$recorder" || test -s "$output_dir/audience-clip.mp4"
trap - EXIT
