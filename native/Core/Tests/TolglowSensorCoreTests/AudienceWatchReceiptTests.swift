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

    func testPulseWithdrawalKeepsOneDancerAndTheSameStepCounterOnBothRoutes() throws {
        for route in AudienceWatchRoute.allCases {
            let start = Date(timeIntervalSinceReferenceDate: 1000)
            let guest = UUID(), session = UUID()
            var pulse = AudiencePulse(started: start), relay = AudienceWatchReceipt(), floor = AudienceFloor()
            func sample(_ serial: UInt32, _ steps: UInt32) throws -> AudienceSample {
                try XCTUnwrap(AudienceSample(serial: serial, steps: steps, effort: 0.75, pulse: pulse.bpm(at: start)))
            }
            func deliver(_ packet: AudienceSample, at time: Double) throws -> Bool {
                let decoded = try XCTUnwrap(AudienceSample(bytes: packet.bytes))
                if route == .pairedPhone && !relay.accept(session: session, sample: decoded, sent: time, now: time) {
                    return false
                }
                return floor.receive(decoded, from: guest, at: time)
            }
            pulse.observe(99, measured: start, now: start)
            let old = try sample(20, 5)
            XCTAssertTrue(try deliver(old, at: 0))
            XCTAssertEqual(floor.drain(at: 0).pulse, 99)
            XCTAssertTrue(try deliver(sample(21, 6), at: 0.2))
            XCTAssertEqual(floor.takeSteps(), 1)

            pulse.withdraw()
            XCTAssertTrue(try deliver(sample(22, 6), at: 0.21))
            let cleared = floor.drain(at: 0.22)
            XCTAssertEqual(cleared.count, 1); XCTAssertEqual(cleared.steps, 0)
            XCTAssertEqual(cleared.pulseCount, 0); XCTAssertNil(cleared.pulse)
            if route == .pairedPhone { XCTAssertEqual(relay.current, session) }

            pulse.observe(145, measured: start, now: start.addingTimeInterval(0.3))
            XCTAssertFalse(try deliver(old, at: 0.3))
            XCTAssertTrue(try deliver(sample(23, 7), at: 0.5))
            let next = floor.drain(at: 0.5)
            XCTAssertEqual(next.count, 1); XCTAssertEqual(next.steps, 1); XCTAssertEqual(next.effort, 0.75, accuracy: 0.003)
            XCTAssertNil(next.pulse); XCTAssertFalse(pulse.interrupted(at: start.addingTimeInterval(120)))
        }
    }

    func testEndCanArriveBeforeFirstSample() throws {
        let id = UUID()
        var receipt = AudienceWatchReceipt()
        XCTAssertFalse(receipt.end(session: id))
        let sample = try XCTUnwrap(AudienceSample(serial: 1, steps: 0, effort: 0))
        XCTAssertFalse(receipt.accept(session: id, sample: sample, sent: 1, now: 1.1))
    }
}
