import XCTest
@testable import TolglowSensorCore

final class AudienceTests: XCTestCase {
    func sample(_ seq: UInt32, _ steps: UInt32, _ effort: Double = 0.5, _ bpm: Int? = nil) -> AudienceSample {
        AudienceSample(serial: seq, steps: steps, effort: effort, pulse: bpm)!
    }

    func testPacketAndOptionalPulse() {
        let input = sample(0x01020304, 256, 1, 88)
        XCTAssertEqual(Array(input.bytes), [1,1,2,3,4,0,0,1,0,255,88])
        XCTAssertEqual(AudienceSample(bytes: input.bytes), input)
        XCTAssertNil(AudienceSample(bytes: input.bytes + Data([0])))
        XCTAssertNil(AudienceSample(serial: 0, steps: 0, effort: .nan))
        XCTAssertNil(AudienceSample(serial: 0, steps: 0, effort: 1, pulse: 300))
    }
    func testLostAndReorderedPacketsAndCounterWrap() {
        let id = UUID()
        var floor = AudienceFloor()
        XCTAssertTrue(floor.receive(sample(.max - 1, .max - 1), from: id, at: 1))
        XCTAssertTrue(floor.receive(sample(1, 1), from: id, at: 1.8))
        XCTAssertFalse(floor.receive(sample(1, 1), from: id, at: 1.9))
        XCTAssertFalse(floor.receive(sample(.max, .max), from: id, at: 2))
        XCTAssertFalse(floor.receive(sample(2, 900), from: id, at: 2))
        XCTAssertEqual(floor.drain(at: 2).steps, 3)
        XCTAssertEqual(floor.drain(at: 2).steps, 0)
        XCTAssertEqual(floor.drain(at: 4.4).count, 0)
    }
    func testOneAndManyGuestsAndRejoin() {
        var floor = AudienceFloor(capacity: 96)
        let ids = (0...96).map { _ in UUID() }
        XCTAssertTrue(floor.receive(sample(0, 12, 1, 100), from: ids[0], at: 0))
        XCTAssertEqual(floor.drain(at: 0).effort, 1)
        for i in 1..<96 { XCTAssertTrue(floor.receive(sample(0, 0, i.isMultiple(of: 2) ? 1 : 0), from: ids[i], at: 0)) }
        XCTAssertFalse(floor.receive(sample(0, 0), from: ids[96], at: 0))
        let all = floor.drain(at: 1)
        XCTAssertEqual(all.count, 96)
        XCTAssertEqual(all.effort, 0.5)
        XCTAssertEqual(all.pulseCount, 1)
        XCTAssertEqual(all.pulse, 100)
        floor.leave(ids[0])
        XCTAssertNil(floor.drain(at: 1).pulse)
        XCTAssertTrue(floor.receive(sample(0, 1000), from: ids[0], at: 1.1))
        XCTAssertEqual(floor.drain(at: 1.2).steps, 0)
        XCTAssertEqual(floor.drain(at: 4).count, 0)
    }
    func testOverflowPreservesPendingGesturesAndBatchesAtTheOSCLimit() throws {
        var queue = AudienceSteps(limit: 10)
        try queue.append(7)
        XCTAssertThrowsError(try queue.append(4))
        XCTAssertEqual(queue.pending, 7)
        try queue.append(3)
        XCTAssertEqual(queue.takeBatch(), 8)
        XCTAssertEqual(queue.takeBatch(), 2)
        XCTAssertEqual(queue.takeBatch(), 0)
    }
    func testSustainedMotionAndDistinctPeaks() {
        var meter = DanceMeter()
        for i in 0..<40 { meter.observe(strength: 1, at: Double(i)/20) }
        XCTAssertEqual(meter.steps, 1)
        XCTAssertGreaterThan(meter.effort, 0.99)
        meter.observe(strength: 0, at: 2); meter.observe(strength: 1, at: 2.1)
        meter.observe(strength: 0, at: 1); meter.observe(strength: .nan, at: 3)
        XCTAssertEqual(meter.steps, 2)
    }
}
