import XCTest
@testable import TolglowSensorCore

final class FogSightlineTests: XCTestCase {
    private func patch(_ pixel: (Int, Int) -> UInt8) throws -> Double {
        var meter = FogPatchMeter(transfer: .linear)
        for row in 0..<16 {
            for column in 0..<16 {
                let byte = pixel(row, column)
                meter.sample(row: row, column: column, red: byte, green: byte, blue: byte)
            }
        }
        return try meter.luminance()
    }

    func testPatchesRejectAnEdgeOrClippedPixels() throws {
        XCTAssertEqual(try patch { _, _ in 102 }, 0.4, accuracy: 1e-12)
        XCTAssertThrowsError(try patch { _, x in x < 8 ? 128 : 12 })
        XCTAssertThrowsError(try patch { _, _ in 250 }) { error in
            XCTAssertEqual(error as? FogSightlineError, .clipped)
        }
        for width in [480, 640, 1280] {
            let left = SightlinePatch.bright.pixel(column: 15, row: 15, width: width, height: 480)
            let right = SightlinePatch.dark.pixel(column: 0, row: 0, width: width, height: 480)
            XCTAssertLessThan(left.x, right.x)
            XCTAssertTrue((0..<480).contains(left.y))
        }
    }

    func testContrastReferenceAndInvalidSamples() throws {
        var baseline = FogBaseline()
        let clear = try FogReading(bright: 0.55, dark: 0.05)
        for _ in 0..<7 { try baseline.accept(clear) }
        XCTAssertFalse(baseline.ready)
        try baseline.accept(clear)
        XCTAssertTrue(baseline.ready)
        // Veiling light adds the same level to both adjacent patches.
        let fog = try FogReading(bright: 0.375, dark: 0.125)
        XCTAssertEqual(fog.contrast / baseline.contrast!, 0.5, accuracy: 1e-12)
        XCTAssertThrowsError(try baseline.accept(fog))
        XCTAssertThrowsError(try baseline.accept(FogReading(bright: 0.045, dark: 0.01)))
        var drift = FogBaseline()
        try drift.accept(FogReading(bright: 0.54, dark: 0.05))
        for _ in 0..<50 { try drift.accept(clear) }
        XCTAssertThrowsError(try drift.accept(FogReading(bright: 0.59, dark: 0.05)))
        XCTAssertThrowsError(try FogReading(bright: .nan, dark: 0))
        XCTAssertThrowsError(try FogReading(bright: 0.1, dark: 0.2))
    }

    func testHostWireObservationsAndPartialFit() throws {
        let reading = try FogReading(bright: 0.6, dark: 0.2)
        let sample = FogTraceSample(phase: .clearing, reading: reading, cameraUptime: 512003.35, sessionStart: 512001.2)
        XCTAssertEqual(sample.fields["elapsed-ms"] as? Int, 2150)
        XCTAssertEqual(sample.fields["phase"] as? String, "clearing")
        let decoder = JSONDecoder()
        let complete = try decoder.decode(FogFit.self, from: Data(#"{"clearance-half-life-s":45,"mixing-seconds":6,"extinction-per-m":0.7,"r-squared":0.89,"samples":65}"#.utf8))
        XCTAssertEqual(complete.mixing, 6)
        XCTAssertEqual(complete.rSquared, 0.89)
        let clearance = try decoder.decode(FogFit.self, from: Data(#"{"clearance-half-life-s":45}"#.utf8))
        XCTAssertEqual(clearance.clearance, 45)
        XCTAssertNil(clearance.extinction)
    }

    func testMovingOrRotatingThePhoneInvalidatesTheSightline() {
        let home = FogPose(x: 0, y: 0, z: 0, w: 1)
        XCTAssertTrue(home.holds(FogPose(x: 0, y: 0, z: 0, w: -1), accelerationG: 0.001))
        XCTAssertFalse(home.holds(home, accelerationG: 0.2))
        let halfAngle = 4.0 * Double.pi / 360
        XCTAssertFalse(home.holds(FogPose(x: sin(halfAngle), y: 0, z: 0, w: cos(halfAngle)), accelerationG: 0))
    }
}
