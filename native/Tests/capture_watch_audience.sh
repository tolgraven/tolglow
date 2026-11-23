#!/usr/bin/env bash
# Exercise the unpaired solo enrollment surface without manufacturing a peripheral or workout.
set -euo pipefail
native_root="$(cd "$(dirname "$0")/.." && pwd)"
output_dir="${1:?Pass a capture directory}"
mkdir -p "$output_dir"
device="$(xcrun simctl list devices available -j | python3 -c 'import json, sys
devices = json.load(sys.stdin)["devices"]
print(next(d["udid"] for runtime, group in sorted(devices.items(), reverse=True) if ".watchOS-" in runtime for d in group if "Apple Watch" in d["name"]))')"
xcrun simctl boot "$device" || xcrun simctl bootstatus "$device" -b
xcrun simctl bootstatus "$device" -b
result="$output_dir/ui.xcresult"
build=(xcodebuild -project "$native_root/Tolglow.xcodeproj" -scheme TolglowWatch
       -destination "platform=watchOS Simulator,id=$device" -parallel-testing-enabled NO
       -derivedDataPath "$native_root/DerivedData" CODE_SIGNING_ALLOWED=NO)
"${build[@]}" build-for-testing
xcrun simctl io "$device" recordVideo "$output_dir/watch-audience.mp4" >"$output_dir/screen-recording.log" 2>&1 &
recorder=$!
stop_recording() { kill -INT "$recorder" 2>/dev/null || true; wait "$recorder" || true; }
trap stop_recording EXIT
"${build[@]}" -resultBundlePath "$result" -test-timeouts-enabled YES -maximum-test-execution-time-allowance 90 test-without-building
stop_recording
trap - EXIT
xcrun xcresulttool export attachments --path "$result" --output-path "$output_dir/attachments"
python3 - "$output_dir" <<'PY'
from pathlib import Path
import json, shutil, sys
out = Path(sys.argv[1])
attachments = out / "attachments"
manifest = json.loads((attachments / "manifest.json").read_text())
def walk(value):
    if isinstance(value, list):
        for v in value: yield from walk(v)
    elif isinstance(value, dict):
        if "exportedFileName" in value: yield value
        for v in value.values(): yield from walk(v)
labels = ("01-paired-phone", "02-stage-picker", "03-paired-again", "04-pulse-consent",
          "04-interrupted-stage", "05-reconnection-cancelled")
for item in walk(manifest):
    name = item.get("suggestedHumanReadableName", "")
    for label in labels:
        if label in name:
            shutil.copyfile(attachments/item["exportedFileName"], out/(label+".png"))
for label in labels:
    assert (out/(label+".png")).is_file(), f"Missing screenshot {label}"
PY
test -s "$output_dir/watch-audience.mp4"
