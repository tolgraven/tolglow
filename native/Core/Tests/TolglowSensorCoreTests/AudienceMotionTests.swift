import Foundation
import XCTest
@testable import TolglowSensorCore

final class AudienceMotionTests: XCTestCase {
    func testCanonicalSignedAxesAndBoundedDecode() throws {
        let axes = try XCTUnwrap(AudienceMotion(acceleration: SIMD3(-1.5, 2, 0), rotation: SIMD3(0.5, 0, -4)))
        let packet = try XCTUnwrap(AudienceSample(serial: 14, steps: 6, effort: 1, motion: axes))
        XCTAssertEqual(packet.bytes.count, 35)
        XCTAssertEqual(Array(packet.bytes[11..<15]), [0xbf, 0xc0, 0, 0])
        XCTAssertEqual(AudienceSample(bytes: packet.bytes), packet)
        for invalid in [Float.nan, .infinity, -.infinity, 257] {
            var bytes = packet.bytes
            withUnsafeBytes(of: invalid.bitPattern.bigEndian) { bytes.replaceSubrange(11..<15, with: $0) }
            XCTAssertNil(AudienceSample(bytes: bytes))
        }
        XCTAssertNil(AudienceSample(bytes: Data(packet.bytes.dropLast())))
        XCTAssertNil(AudienceMotion(values: ["accel-x": 1]))
    }

    func testDelayedWristPacketCannotSupersedeFreshPhoneAxes() throws {
        let axes = try XCTUnwrap(AudienceMotion(acceleration: .zero, rotation: .zero))
        let measured = AudienceMotion.wristTimestamp(ageAtSend: 0.15, sent: 50, received: 50.1, uptime: 100)
        XCTAssertEqual(axes.fresh(measured: measured, at: 100.2), axes)
        XCTAssertNil(axes.fresh(measured: measured, at: 100.4))
        let delayed = AudienceMotion.wristTimestamp(ageAtSend: 0.1, sent: 50, received: 51, uptime: 100)
        XCTAssertNil(axes.fresh(measured: delayed, at: 100))
        XCTAssertNil(AudienceMotion.wristTimestamp(ageAtSend: .nan, sent: 50, received: 50, uptime: 100))
    }

    func testOnlyConsentingGuestsContributeAndWithdrawingClearsEachAxis() throws {
        let a = UUID(), b = UUID(), c = UUID()
        var floor = AudienceFloor()
        func sample(_ serial: UInt32, _ x: Double?) throws -> AudienceSample {
            let axes = x.flatMap { AudienceMotion(acceleration: SIMD3($0, 0, 0), rotation: SIMD3(0, 2, 0)) }
            return try XCTUnwrap(AudienceSample(serial: serial, steps: 0, effort: 0, motion: axes))
        }
        XCTAssertTrue(floor.receive(try sample(1, -6), from: a, at: 0))
        XCTAssertTrue(floor.receive(try sample(1, 2), from: b, at: 0))
        XCTAssertTrue(floor.receive(try sample(1, nil), from: c, at: 0))
        let first = floor.drain(at: 0)
        XCTAssertEqual(first.count, 3)
        XCTAssertEqual(first.motionCount, 2)
        XCTAssertEqual(first.motion["accel-x"], -2)
        XCTAssertTrue(floor.receive(try sample(2, nil), from: a, at: 0.2))
        XCTAssertEqual(floor.drain(at: 0.2).motion["accel-x"], 2)
        floor.leave(b)
        XCTAssertTrue(floor.drain(at: 0.4).motion.isEmpty)
    }
}
