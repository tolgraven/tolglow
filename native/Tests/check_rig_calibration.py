#!/usr/bin/env python3
"""Static capture/wire manifest checks only; does not compile/execute Swift."""
import unittest
from pathlib import Path
ROOT = Path(__file__).resolve().parents[1]
class RigCalibrationContract(unittest.TestCase):
    def test_capture_uses_registered_depth_camera_and_permission(self):
        source = (ROOT / 'iOS/RigCalibration/RigScanner.swift').read_text()
        for required in ['supportsFrameSemantics(.sceneDepth)', 'AVCaptureDevice.requestAccess',
                         'configuration.worldAlignment = .gravity', 'frame.sceneDepth',
                         'frame.camera.intrinsics', 'frame.camera.transform', 'frame.timestamp * 1000',
                         'depth.confidenceMap', 'displayTransform(for:', '.inverted()',
                         'CVPixelBufferGetBytesPerRow', 'case .normal', 'pending.count >= 128']:
            self.assertIn(required, source)
        self.assertIn('NSCameraUsageDescription:', (ROOT / 'project.yml').read_text())
    def test_app_owns_scan_lifecycle_and_transport(self):
        app = (ROOT / 'iOS/TolglowPhoneApp.swift').read_text()
        client = (ROOT / 'iOS/RigCalibration/RigCalibrationClient.swift').read_text()
        self.assertIn('rigCalibration.suspend()', app)
        self.assertIn('RigCalibrationView(', app)
        self.assertIn('api/calibration', client)
        self.assertIn('"op": "heartbeat"', client)
        self.assertIn('"armed?": true', client)
        self.assertIn('requestGeneration == generation', client)
        self.assertNotIn('NWConnection', client)
        self.assertNotIn('UIBackgroundModes', (ROOT / 'project.yml').read_text())
if __name__ == '__main__':
    unittest.main(verbosity=2)
