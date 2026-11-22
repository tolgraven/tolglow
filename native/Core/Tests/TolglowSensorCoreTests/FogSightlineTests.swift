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
        XCTAssertThrowsError(try patch { _, _ in 250 })
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
        XCTAssertThrowsError(try baseline.accept(FogReading(bright: 0.012, dark: 0.01)))
        XCTAssertThrowsError(try FogReading(bright: .nan, dark: 0))
        XCTAssertThrowsError(try FogReading(bright: 0.1, dark: 0.2))
    }

    func testMovingOrRotatingThePhoneInvalidatesTheSightline() {
        let home = FogPose(x: 0, y: 0, z: 0, w: 1)
        XCTAssertTrue(home.holds(FogPose(x: 0, y: 0, z: 0, w: -1), accelerationG: 0.001))
        XCTAssertFalse(home.holds(home, accelerationG: 0.2))
        let halfAngle = 4.0 * Double.pi / 360
        XCTAssertFalse(home.holds(FogPose(x: sin(halfAngle), y: 0, z: 0, w: cos(halfAngle)), accelerationG: 0))
    }
}
