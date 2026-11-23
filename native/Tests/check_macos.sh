#!/usr/bin/env bash
# Requires a Mac with Xcode 16+ SDKs and XcodeGen. Never installs or signs a device.
set -euo pipefail
native_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$native_root/Core"
swift test
cd "$native_root"
xcodegen generate
xcodebuild -project Tolglow.xcodeproj -scheme TolglowPhone \
  -destination 'generic/platform=iOS Simulator' \
  -derivedDataPath "$native_root/DerivedData" CODE_SIGNING_ALLOWED=NO build
xcodebuild -project Tolglow.xcodeproj -scheme TolglowWatch \
  -destination 'generic/platform=watchOS Simulator' \
  -derivedDataPath "$native_root/DerivedData" CODE_SIGNING_ALLOWED=NO build
xcodebuild -project Tolglow.xcodeproj -scheme TolglowAudienceBridge \
  -destination 'platform=macOS' -derivedDataPath "$native_root/DerivedData" CODE_SIGNING_ALLOWED=NO build
xcodebuild -project Tolglow.xcodeproj -scheme TolglowAudienceClip \
  -destination 'generic/platform=iOS Simulator' -derivedDataPath "$native_root/DerivedData" CODE_SIGNING_ALLOWED=NO build
test -d "$native_root/DerivedData/Build/Products/Debug-iphonesimulator/TolglowPhone.app/AppClips/TolglowAudienceClip.app"
xcrun --sdk macosx swiftc Audience/AudienceRadio.swift Tests/AudienceRadio/main.swift -o DerivedData/audience-radio-check
DerivedData/audience-radio-check
core_bin="$(swift build --package-path Core --show-bin-path)"
xcrun --sdk macosx swiftc -D AUDIENCE_RECOVERY_FIXTURE -I "$core_bin/Modules" \
  "$core_bin"/TolglowSensorCore.build/*.o Audience/AudienceRadio.swift Audience/AudienceClient.swift \
  Tests/AudienceClient/main.swift -o DerivedData/audience-client-check
DerivedData/audience-client-check
