import Foundation
import XCTest
@testable import TolglowSensorCore

final class AudienceStageHeartbeatTests: XCTestCase {
    func testHealthyEmptyStageKeepsPublishingUntilRadioOrTransportLoss() throws {
        var output = AudienceStageOutput(), floor = AudienceFloor()
        var received = [Observation]()
        let dancer = UUID()
        let motion = try XCTUnwrap(AudienceMotion(acceleration: SIMD3(1, 2, 3), rotation: .zero))
        func tick(_ time: Double, available: Bool = true) throws {
            XCTAssertEqual(output.tick(floor.drain(at: time), available: available), 0)
            if let frame = output.take(from: &floor, at: time) {
                let observation = try frame.levels.observation(steps: frame.steps, source: "audience", session: "show",
                    sampleID: Int64(received.count), at: time, token: "stage-qualification")
                received.append(try SensorOSC.decode(SensorOSC.encode(observation)))
            }
            XCTAssertNil(output.take(from: &floor, at: time))
        }
        try tick(0, available: false)
        XCTAssertTrue(received.isEmpty)
        for i in 1...20 { try tick(Double(i) / 5) }  // Healthy, before anyone arrived.
        let sample = try XCTUnwrap(AudienceSample(serial: 0, steps: 19, effort: 0.5, pulse: 110, motion: motion))
        XCTAssertTrue(floor.receive(sample, from: dancer, at: 4.19))
        try tick(4.2)
        XCTAssertEqual(received.last?.values["pulse-bpm"], 110)
        XCTAssertEqual(received.last?.values["motion-count"], 1)
        for i in 22...55 { try tick(Double(i) / 5) }  // Includes a silent guest expiring at 6.69.
        XCTAssertEqual(received.count, 55)
        let empty = received.filter { $0.timeMS <= 4000 || $0.timeMS >= 6800 }
        XCTAssertEqual(empty.count, 42)
        for packet in empty {
            XCTAssertEqual(packet.values, ["audience-count": 0, "dance-effort": 0, "dance-steps": 0,
                                          "motion-count": 0, "pulse-count": 0, "pulse-present": 0])
            XCTAssertEqual(packet.gestures, [])
        }
        let next = try XCTUnwrap(AudienceSample(serial: 1, steps: 37, effort: 1))
        XCTAssertTrue(floor.receive(next, from: UUID(), at: 11.1))
        try tick(11.2)
        XCTAssertEqual(received.last?.values["audience-count"], 1)
        XCTAssertEqual(received.last?.values["dance-steps"], 0)
        XCTAssertNil(received.last?.values["pulse-bpm"])
        for i in 57...90 { try tick(Double(i) / 5, available: false) }
        XCTAssertEqual(received.count, 56)
        try tick(18.2)
        XCTAssertEqual(received.count, 57)
        XCTAssertEqual(received.last?.values["audience-count"], 0)
        guard let path = ProcessInfo.processInfo.environment["TOLGLOW_STAGE_TRACE"] else { return }
        let trace = received.map { ["time_ms": $0.timeMS, "participants": $0.values["audience-count"]!,
                                    "pulse_count": $0.values["pulse-count"]!, "steps": Double($0.gestures.count)] }
        try JSONEncoder().encode(trace).write(to: URL(fileURLWithPath: path), options: .atomic)
    }

    func testEmptyHeartbeatsDoNotReplayOnsetsAndPeriodicUpdatesCoalesceUnderBackpressure() throws {
        var output = AudienceStageOutput(), floor = AudienceFloor()
        let guest = UUID()
        let sample = try XCTUnwrap(AudienceSample(serial: 0, steps: 0, effort: 0.6))
        XCTAssertEqual(output.tick(floor.drain(at: 0), available: true), 0)
        XCTAssertTrue(floor.receive(sample, from: guest, at: 0.01))
        try output.onsets.append(1)
        output.onsets.openFrame(at: 0.025)
        let frame = try XCTUnwrap(output.take(from: &floor, at: 0.025))
        XCTAssertEqual(frame.levels.count, 1)
        XCTAssertEqual(frame.steps, 1)
        XCTAssertNil(output.take(from: &floor, at: 0.03))
        try output.onsets.append(3)
        XCTAssertEqual(output.tick(floor.drain(at: 0.2), available: true), 0)
        XCTAssertEqual(output.tick(floor.drain(at: 3), available: true), 3)
        XCTAssertEqual(output.tick(floor.drain(at: 3.2), available: true), 0)
        let idle = try XCTUnwrap(output.take(from: &floor, at: 3.2))
        XCTAssertEqual(idle.levels.count, 0)
        XCTAssertEqual(idle.steps, 0)
        XCTAssertNil(output.take(from: &floor, at: 3.2))
        XCTAssertEqual(output.tick(floor.drain(at: 3.4), available: true), 0)
        XCTAssertEqual(output.reset(), 0)
        XCTAssertNil(output.take(from: &floor, at: 3.4))
        try output.onsets.append(2)
        output.onsets.openFrame(at: 3.425)
        let orphaned = try XCTUnwrap(output.take(from: &floor, at: 3.425))
        XCTAssertEqual(orphaned.expired, 2)
        XCTAssertEqual(orphaned.steps, 0)
        XCTAssertNil(output.take(from: &floor, at: 3.425))
    }
}
