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
"$native_root/AudienceCompute/check_mac.sh"
test -d "$native_root/DerivedData/Build/Products/Debug-iphonesimulator/TolglowPhone.app/AppClips/TolglowAudienceClip.app"
xcrun --sdk macosx swiftc Audience/AudienceRadio.swift Tests/AudienceRadio/main.swift -o DerivedData/audience-radio-check
DerivedData/audience-radio-check
