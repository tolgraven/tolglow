#!/bin/bash
# Evidence-only simulator fixtures, without a physical stage, quota, scheme or product hook.
set -euo pipefail
out="$1"; mkdir -p "$out"; cd "$(dirname "$0")/.."
xcodegen generate
python3 - <<'PY'
from pathlib import Path
p=Path('Audience/AudienceClient.swift')
p.write_text(p.read_text()+'''
#if os(iOS) && !APPCLIP
extension AudienceClient {
    func setCaptureState(_ mode: String) {
        let id = String(repeating: "a", count: 64)
        switch mode {
        case "invalid": receiveInvitation(nil)
        case "invited": receiveInvitation(id)
        case "recover":
            receiveInvitation(id); reconnecting = true; status = "Reconnecting to stage"
        case "joined":
            room = id; joined = true; offersCompute = true; status = "Sharing with this stage"
        default: break
        }
    }
}
#endif
''')
p=Path('Audience/AudienceSession.swift')
s=p.read_text()
anchor='        client.onSuspend = { [weak self] in self?.pauseInputs() }'
assert s.count(anchor)==1
p.write_text(s.replace(anchor,anchor+'''
        #if !APPCLIP
        if let i = CommandLine.arguments.firstIndex(of: "-design-state"), i+1 < CommandLine.arguments.count {
            let mode=CommandLine.arguments[i+1]
            client.setCaptureState(mode)
            if mode == "joined" { input = .motion; totalSteps = 24; effort = 0.68 }
        }
        #endif'''))
p=Path('iOS/TolglowPhoneApp.swift')
s=p.read_text(); anchor='@State private var tab = PhoneTab.perform'; assert s.count(anchor)==1
p.write_text(s.replace(anchor,'@State private var tab = CommandLine.arguments.contains("-design-state") ? PhoneTab.danceFloor : .perform'))
PY
build() {
  xcodebuild -quiet -project Tolglow.xcodeproj -scheme "$1" -destination 'generic/platform=iOS Simulator' -derivedDataPath "$RUNNER_TEMP/dance-recovery-derived" CODE_SIGNING_ALLOWED=NO build
}
build TolglowAudienceClip
build TolglowPhone
products="$RUNNER_TEMP/dance-recovery-derived/Build/Products/Debug-iphonesimulator"
device() {
  xcrun simctl list devices available -j | python3 -c 'import json,sys
wanted=sys.argv[1]
print(next(d["udid"] for runtime,group in sorted(json.load(sys.stdin)["devices"].items(),reverse=True) if ".iOS-" in runtime for d in group if d["name"].startswith(wanted)))' "$1"
}
for platform in iphone ipad; do
  if [[ "$platform" == iphone ]]; then id="$(device iPhone)"; else id="$(device iPad)"; fi
  xcrun simctl boot "$id" || true
  xcrun simctl bootstatus "$id" -b
  xcrun simctl ui "$id" appearance dark
  xcrun simctl install "$id" "$products/TolglowAudienceClip.app"
  xcrun simctl launch "$id" org.tolglow.ios.Clip
  sleep 4; xcrun simctl io "$id" screenshot "$out/$platform-entry.png"
  xcrun simctl terminate "$id" org.tolglow.ios.Clip || true
  xcrun simctl uninstall "$id" org.tolglow.ios.Clip
  xcrun simctl install "$id" "$products/TolglowPhone.app"
  capture() {
    xcrun simctl terminate "$id" org.tolglow.ios || true
    xcrun simctl launch "$id" org.tolglow.ios -design-state "$1"
    sleep 4; xcrun simctl io "$id" screenshot "$out/$platform-$1.png"
  }
  capture joined
  if [[ "$platform" == iphone ]]; then capture invalid; capture recover; capture invited; fi
  xcrun simctl shutdown "$id"
done
