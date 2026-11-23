import Foundation
import XCTest
@testable import TolglowSensorCore

final class AudienceWatchReceiptTests: XCTestCase {
    func testRetiresSessionRejectsStaleAndAcceptsWrap() throws {
        let old = UUID(), next = UUID()
        var receipt = AudienceWatchReceipt()
        let first = try XCTUnwrap(AudienceSample(serial: UInt32.max, steps: 7, effort: 0.8, pulse: 94))
        let second = try XCTUnwrap(AudienceSample(serial: 0, steps: 9, effort: 0.4))
        XCTAssertTrue(receipt.accept(session: old, sample: first, sent: 50, now: 50))
        XCTAssertTrue(receipt.accept(session: old, sample: second, sent: 50.2, now: 50.2))
        XCTAssertFalse(receipt.accept(session: old, sample: second, sent: 51, now: 51))
        XCTAssertFalse(receipt.accept(session: next, sample: second, sent: 51, now: 54))
        XCTAssertTrue(receipt.accept(session: next, sample: second, sent: 54, now: 54.2))
        XCTAssertFalse(receipt.end(session: old))
        XCTAssertTrue(receipt.end(session: next))
        XCTAssertFalse(receipt.accept(session: next, sample: first, sent: 54, now: 54.3))
    }

    func testOnlyFreshWorkoutPulseCanLeaveWatch() {
        let start = Date(timeIntervalSinceReferenceDate: 1000)
        var pulse = AudiencePulse(started: start)
        pulse.observe(75, measured: start.addingTimeInterval(-1), now: start)
        XCTAssertNil(pulse.bpm(at: start))
        pulse.observe(90.6, measured: start, now: start)
        XCTAssertEqual(pulse.bpm(at: start.addingTimeInterval(12)), 91)
        XCTAssertNil(pulse.bpm(at: start.addingTimeInterval(12.01)))
        pulse.observe(100, measured: start.addingTimeInterval(20), now: start.addingTimeInterval(20))
        pulse.observe(78, measured: start.addingTimeInterval(19), now: start.addingTimeInterval(20))
        XCTAssertEqual(pulse.bpm(at: start.addingTimeInterval(20)), 100)
        XCTAssertTrue(pulse.interrupted(at: start.addingTimeInterval(81)))
    }

    func testEndCanArriveBeforeFirstSample() throws {
        let id = UUID()
        var receipt = AudienceWatchReceipt()
        XCTAssertFalse(receipt.end(session: id))
        let sample = try XCTUnwrap(AudienceSample(serial: 1, steps: 0, effort: 0))
        XCTAssertFalse(receipt.accept(session: id, sample: sample, sent: 1, now: 1.1))
    }
}
