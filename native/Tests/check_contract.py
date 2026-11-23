#!/usr/bin/env python3
"""Static manifest/protocol checks only: this does not compile or execute Swift."""
import json
import re
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
FIXTURES = ROOT / "Core/Tests/TolglowSensorCoreTests/Fixtures"


def read_osc_strings(packet):
    assert len(packet) % 4 == 0
    result = []
    at = 0
    while at < len(packet):
        end = packet.index(0, at)
        padded = (end + 4) // 4 * 4
        assert packet[end:padded] == bytes(padded-end)
        result.append(packet[at:end].decode("utf-8"))
        at = padded
    return result


class Contract(unittest.TestCase):
    def test_wire_fixture_matches_json(self):
        address, tags, text = read_osc_strings(bytes.fromhex((FIXTURES / "observation.osc.hex").read_text()))
        self.assertEqual(address, "/tolglow/sensor/v1")
        self.assertEqual(tags, ",s")
        self.assertEqual(json.loads(text), json.loads((FIXTURES / "observation.json").read_text()))

    def test_fixture_uses_exact_identity_units_and_test_credential(self):
        value = json.loads((FIXTURES / "observation.json").read_text())
        self.assertEqual(value["version"], 1)
        self.assertEqual(value["reference-frame"], "x-arbitrary-z-vertical")
        self.assertTrue(value["pairing-token"].startswith("TEST-ONLY-"))
        self.assertEqual(set(value["values"]), {"roll", "pitch", "yaw", "accel-x", "accel-y", "accel-z", "rotation-x", "rotation-y", "rotation-z"})
        self.assertEqual(value["gestures"][0]["type"], "primary-action")

    def test_watch_target_has_no_direct_network_transport(self):
        source = "\n".join(p.read_text() for folder in ["watchOS", "Shared"] for p in (ROOT / folder).glob("*.swift"))
        self.assertNotIn("import Network", source)
        self.assertNotIn("NWConnection", source)
        self.assertIn("sendMessageData", source)
        self.assertIn(".handGestureShortcut(.primaryAction, isEnabled: motion.armed)", source)

    def test_manifest_both_targets_and_permissions(self):
        manifest = (ROOT / "project.yml").read_text()
        self.assertIn("platform: iOS", manifest)
        self.assertIn("platform: watchOS", manifest)
        self.assertIn("NSMotionUsageDescription:", manifest)
        self.assertIn("NSLocalNetworkUsageDescription:", manifest)
        self.assertNotIn("UIBackgroundModes", manifest)
        self.assertIn("WKCompanionAppBundleIdentifier: org.tolglow.ios", manifest)
        self.assertTrue((ROOT / "Core/Package.swift").exists())

    def test_optional_watch_workout_discards_health_data(self):
        manifest = (ROOT / "project.yml").read_text()
        watch = manifest.split("  TolglowWatch:\n    type: application", 1)[1].split("schemes:")[0]
        for item in ["com.apple.developer.healthkit: true", "NSHealthShareUsageDescription:",
                     "NSHealthUpdateUsageDescription:", "WKBackgroundModes: [workout-processing]"]:
            self.assertIn(item, watch)
        source = (ROOT / "watchOS/DanceWorkout.swift").read_text()
        self.assertIn("builder?.discardWorkout()", source)
        self.assertNotIn("finishWorkout", source)
        self.assertIn("config.activityType = .socialDance", source)
        self.assertIn("store.requestAuthorization", source)
        self.assertNotIn("UserDefaults", source)
        view = (ROOT / "watchOS/DanceAudienceView.swift").read_text()
        self.assertIn("private var sharePulse = false", view)
        self.assertIn("workout.start(withPulse: sharePulse)", view)

    def test_native_control_and_webview_share_the_operator_auth_boundary(self):
        access = (ROOT / "iOS/StudioOperatorAccess.swift").read_text()
        self.assertIn("NSURLAuthenticationMethodHTTPBasic", access)
        self.assertIn("LocalPassword.read(service: service, account:", access)
        self.assertIn("SecureField", access)
        for client in ["StudioRedirectPolicy.swift", "WebStudioView.swift"]:
            self.assertIn("StudioOperatorAccess.respond(to: challenge", (ROOT / "iOS" / client).read_text())
        self.assertIn("task.originalRequest?.url", (ROOT / "iOS/StudioRedirectPolicy.swift").read_text())
        for client in ["StudioClient.swift", "RigCalibration/RigCalibrationClient.swift",
                       "Photometry/BrightnessCalibrationClient.swift", "FogCalibration/FogCalibrationClient.swift"]:
            source = (ROOT / "iOS" / client).read_text()
            self.assertIn("config.urlCredentialStorage = nil", source)
            self.assertIn("config.httpShouldSetCookies = false", source)

    def test_credentials_live_at_owned_phone_boundary(self):
        store = (ROOT / "iOS/PairingStore.swift").read_text()
        self.assertIn("kSecAttrAccessibleWhenUnlockedThisDeviceOnly", store)
        self.assertNotRegex(store, r"UserDefaults\..*set")
        self.assertEqual(2, store.count("service: destination.keychainService, account: device"))
        self.assertIn("kSecAttrService as String: service", store)
        self.assertIn("kSecAttrAccount as String: account", store)
        osc = (ROOT / "iOS/OSCConnection.swift").read_text()
        self.assertIn("credential.destination == destination, credential.device == observation.deviceID", osc)
        watch = (ROOT / "watchOS/TolglowWatchApp.swift").read_text()
        self.assertNotIn("pairingToken", watch)

    def test_xctest_is_real_package_target(self):
        package = (ROOT / "Core/Package.swift").read_text()
        self.assertIn('.testTarget(name: "TolglowSensorCoreTests"', package)
        tests = (ROOT / "Core/Tests/TolglowSensorCoreTests/SensorTests.swift").read_text()
        self.assertEqual(len(re.findall(r"func test\w+\(", tests)), 7)


if __name__ == "__main__":
    unittest.main(verbosity=2)
