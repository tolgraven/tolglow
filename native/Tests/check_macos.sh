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
phone="$native_root/DerivedData/Build/Products/Debug-iphonesimulator/TolglowPhone.app"
test -f "$phone/audience-compute.js"
test -f "$phone/TRANSIT-LICENSE"
test ! -e "$phone/AppClips/TolglowAudienceClip.app/audience-compute.js"
"$native_root/AudienceCompute/check_mac.sh"
test -d "$native_root/DerivedData/Build/Products/Debug-iphonesimulator/TolglowPhone.app/AppClips/TolglowAudienceClip.app"
xcrun --sdk macosx swiftc Core/Sources/TolglowSensorCore/AudienceGATTWrite.swift Audience/AudienceRadio.swift Tests/AudienceRadio/main.swift -o DerivedData/audience-radio-check
xcrun --sdk macosx swiftc macOS/ShowRegistry.swift macOS/FolderStore.swift Tests/ShowWindows/main.swift -o DerivedData/show-windows-check
DerivedData/show-windows-check
DerivedData/audience-radio-check
core_bin="$(swift build --package-path Core --show-bin-path)"
xcrun --sdk macosx swiftc -D AUDIENCE_RECOVERY_FIXTURE -I "$core_bin/Modules" \
  "$core_bin"/TolglowSensorCore.build/*.o Audience/AudienceRadio.swift Audience/AudienceClient.swift \
  Tests/AudienceClient/main.swift -o DerivedData/audience-client-check
DerivedData/audience-client-check
xcrun --sdk macosx swiftc -swift-version 5 macOS/RoomWindows.swift Tests/RoomWindows/main.swift -o DerivedData/room-windows-check
DerivedData/room-windows-check
