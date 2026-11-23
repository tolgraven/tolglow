"""Measure the uncompressed Release simulator products carried by the iOS app."""

import json
import plistlib
import subprocess
import sys
from pathlib import Path


def product(app, bundle_id, platform, families):
    with (app / "Info.plist").open("rb") as source:
        info = plistlib.load(source)
    if (info["CFBundleIdentifier"] != bundle_id
            or platform not in info["CFBundleSupportedPlatforms"]
            or sorted(info["UIDeviceFamily"]) != families):
        raise ValueError(f"Unexpected bundle identity, platform or device family in {app.name}")
    executable = app / info["CFBundleExecutable"]
    architectures = subprocess.check_output(["xcrun", "lipo", "-archs", executable], text=True)
    byte_count = sum(f.stat().st_size for f in app.rglob("*") if not f.is_symlink() and f.is_file())
    return {"bundle": bundle_id, "platform": platform, "deviceFamilies": families,
            "architectures": architectures.split(), "uncompressedFileBytes": byte_count}


def report(app):
    return {
        "configuration": "Release",
        "measurement": "Unpacked simulator files. Parent includes both embedded products.",
        "phoneAndTablet": product(app, "org.tolglow.ios", "iPhoneSimulator", [1, 2]),
        "embeddedClip": product(app / "AppClips/TolglowAudienceClip.app", "org.tolglow.ios.Clip",
                                "iPhoneSimulator", [1, 2]),
        "embeddedWatch": product(app / "Watch/TolglowWatch.app", "org.tolglow.ios.watchkitapp",
                                 "WatchSimulator", [4]),
    }


if __name__ == "__main__":
    print(json.dumps(report(Path(sys.argv[1])), indent=2))
