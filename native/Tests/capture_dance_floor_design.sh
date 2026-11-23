#!/bin/bash
# Evidence-only branch. The joined session is a UI fixture with no active radio.
set -euo pipefail
out="$1"
mkdir -p "$out"
native="$(cd "$(dirname "$0")/.." && pwd)"
cd "$native"
xcodegen generate
build() {
  xcodebuild -quiet -project Tolglow.xcodeproj -scheme "$1" -destination 'generic/platform=iOS Simulator' -derivedDataPath "$RUNNER_TEMP/audience-derived" CODE_SIGNING_ALLOWED=NO build
}
device() {
  xcrun simctl list devices available -j | python3 -c 'import json,sys
wanted=sys.argv[1]
items=((d["name"],d["udid"]) for runtime,group in sorted(json.load(sys.stdin)["devices"].items(), reverse=True) if ".iOS-" in runtime for d in group if d["name"].startswith(wanted))
print(next(items)[1])' "$1"
}
phone="$(device iPhone)"
tablet="$(device iPad)"
products="$RUNNER_TEMP/audience-derived/Build/Products/Debug-iphonesimulator"
xcrun simctl list devices | grep -E "$phone|$tablet" > "$out/devices.txt"
boot() { xcrun simctl boot "$1" || true; xcrun simctl bootstatus "$1" -b; xcrun simctl ui "$1" appearance dark; }
clip="org.tolglow.ios.Clip"
capture() {
  local d="$1" platform="$2" stage="$3" bundle="$4" app="$5"
  shift 5
  xcrun simctl terminate "$d" "$bundle" || true
  xcrun simctl install "$d" "$app"
  if [[ "$platform" == iphone && "$stage" == after ]]; then
    xcrun simctl io "$d" recordVideo "$out/iphone-entry.mp4" >/dev/null 2>&1 &
    recorder=$!
    sleep 1
  fi
  xcrun simctl launch "$d" "$bundle" "$@"
  sleep 4
  xcrun simctl io "$d" screenshot "$out/$platform-$stage.png"
  if [[ "$platform" == iphone && "$stage" == after ]]; then
    kill -INT "$recorder"
    wait "$recorder" || test -s "$out/iphone-entry.mp4"
  fi
}
git show 790d028f3e25a42a02632fa0b274216d97e72e4e:native/Audience/AudienceView.swift > Audience/AudienceView.swift
build TolglowAudienceClip
ditto "$products/TolglowAudienceClip.app" "$RUNNER_TEMP/Before.app"
git restore --source=HEAD -- Audience/AudienceView.swift
build TolglowAudienceClip
ditto "$products/TolglowAudienceClip.app" "$RUNNER_TEMP/After.app"
du -sk "$RUNNER_TEMP/Before.app" "$RUNNER_TEMP/After.app" > "$out/unsigned-simulator-clip-kib.txt"
python3 - <<'PY'
from pathlib import Path
p=Path('Audience/AudienceClient.swift')
s=p.read_text()+'\nextension AudienceClient { func showDesignFixture() { joined = true; status = "Sharing with this stage" } }\n'
p.write_text(s)
p=Path('Audience/AudienceSession.swift')
s=p.read_text().replace('init() { client.onEnd = { [weak self] in self?.stopInputs() } }','''init() {
    client.onEnd = { [weak self] in self?.stopInputs() }
    if CommandLine.arguments.contains("-joined-fixture") {
        client.showDesignFixture()
        #if !APPCLIP
        input = .motion
        #endif
        totalSteps = 24
        effort = 0.68
    }
}''')
p.write_text(s)
p=Path('iOS/TolglowPhoneApp.swift')
s=p.read_text().replace('PhoneRoot(model: model)\n                .preferredColorScheme(.dark)', '''Group {
                if CommandLine.arguments.contains("-joined-fixture") { AudienceView(session: model.audience) }
                else { PhoneRoot(model: model) }
            }.preferredColorScheme(.dark)''')
p.write_text(s)
PY
build TolglowPhone
for platform in iphone ipad; do
  device_id="$phone"
  if [[ "$platform" == ipad ]]; then device_id="$tablet"; fi
  boot "$device_id"
  capture "$device_id" "$platform" before "$clip" "$RUNNER_TEMP/Before.app"
  capture "$device_id" "$platform" after "$clip" "$RUNNER_TEMP/After.app"
  capture "$device_id" "$platform" joined-fixture org.tolglow.ios "$products/TolglowPhone.app" -joined-fixture
  xcrun simctl shutdown "$device_id"
done
