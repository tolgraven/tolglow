import XCTest
@testable import TolglowSensorCore

final class PhotometryTests: XCTestCase {
    func frame(_ transfer: SDRTransfer, _ rgb: (UInt8, UInt8, UInt8)) throws -> Double {
        var f = PhotometryFrame(transfer: transfer)
        for _ in 0..<100 { f.sample(red: rgb.0, green: rgb.1, blue: rgb.2) }
        return try f.luminance()
    }

    func testLinearizationAndChannelClipping() throws {
        XCTAssertEqual(try frame(.sRGB, (128,128,128)), 0.21586, accuracy: 0.00001)
        XCTAssertEqual(try frame(.linear, (128,0,0)), 0.2126 * 128 / 255, accuracy: 0.00001)
        XCTAssertEqual(try frame(.rec709, (128,128,128)), 0.26148, accuracy: 0.0001)
        XCTAssertThrowsError(try frame(.sRGB, (253,1,5)))
    }

    func testDarkSubtractionRepeatabilityAndFlicker() throws {
        let primary = try BrightnessReading(dark: 0.1, white: 0.4)
        let alternate = try BrightnessReading(dark: 0.2, white: 0.8)
        XCTAssertEqual(alternate.signal / primary.signal, 2, accuracy: 1e-12)
        XCTAssertThrowsError(try BrightnessReading(dark: 0.5, white: 0.501))
        XCTAssertThrowsError(try BrightnessReading(dark: .nan, white: 0.6))
        var frames = Array(repeating: 0.3, count: 15)
        frames[2] = 0.31
        XCTAssertEqual(try PhotometryFrame.stableMedian(frames), 0.3)
        XCTAssertThrowsError(try PhotometryFrame.stableMedian(frames + Array(repeating: 0.2, count: 8)))
    }
}
