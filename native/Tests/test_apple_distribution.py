import json
import plistlib
import subprocess
import tempfile
import unittest
from pathlib import Path
from zipfile import ZipFile

PACKAGING = Path(__file__).resolve().parents[1] / "Packaging"


class DistributionTests(unittest.TestCase):
    def test_association_uses_signing_prefix_and_only_audience_paths(self):
        document = json.loads(subprocess.check_output([PACKAGING / "audience-association", "ABCDE12345"]))
        self.assertEqual(["ABCDE12345.org.tolglow.ios.Clip"], document["appclips"]["apps"])
        self.assertEqual([{"appIDs": ["ABCDE12345.org.tolglow.ios"], "components": [{"/": "/audience/*"}]}],
                         document["applinks"]["details"])

    def test_count_the_signed_clip_in_each_variant(self):
        with tempfile.TemporaryDirectory() as directory:
            for variant, prefix in [("phone", "Payload/Phone.app/AppClips/Clip.app/"),
                                    ("tablet", "Payload/Clip.app/")]:
                info = plistlib.dumps({"CFBundleIdentifier": "org.tolglow.ios.Clip",
                                       "CFBundleSupportedPlatforms": ["iPhoneOS"]})
                with ZipFile(Path(directory) / (variant + ".ipa"), "w") as archive:
                    archive.writestr(prefix + "Info.plist", info)
                    archive.writestr(prefix + "Clip", b"1234")
                    archive.writestr("SwiftSupport/ignored", b"x" * 50)
            result = json.loads(subprocess.check_output([PACKAGING / "device-clip-sizes", directory]))
            self.assertEqual([{"variant": f"{variant}.ipa", "clipUncompressedFileBytes": len(info) + 4}
                              for variant in ("phone", "tablet")], result)

    def test_missing_clip_is_reported(self):
        with tempfile.TemporaryDirectory() as directory:
            result = subprocess.run([PACKAGING / "device-clip-sizes", directory], capture_output=True)
            self.assertNotEqual(0, result.returncode)
            self.assertIn(b"no exported device IPA", result.stderr)


if __name__ == "__main__":
    unittest.main()
