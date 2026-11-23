import Foundation
import XCTest
@testable import TolglowSensorCore

final class AudienceCrowdTests: XCTestCase {
    func testNinetySixGuestsAcrossASongWithLossReplayRejoinAndPulseOptOut() throws {
        let people = (0..<96).map { UUID(uuidString: String(format: "00000000-0000-0000-0000-%012d", $0))! }
        var floor = AudienceFloor()
        var steps = Array(repeating: UInt32(0), count: people.count)
        var delivered = steps
        var latestEffort = Array(repeating: 0.0, count: people.count)
        var latestPulse = Array<Int?>(repeating: nil, count: people.count)
        var total = 0
        let frames = 900  // Three minutes at five source updates per second.
        for tick in 0..<frames {
            let time = Double(tick) / 5
            var expectedSteps = 0
            for (i, id) in people.enumerated() {
                if tick > 0 && (tick + i) % 5 == 0 { steps[i] += 1 }
                if i == 0 && tick == 301 { floor.leave(id) }
                if i == 0 && (301..<307).contains(tick) { continue }
                let rejoined = i == 0 && tick == 307
                if rejoined { steps[i] = 0; delivered[i] = 0 }
                if tick > 0 && tick < frames - 1 && !rejoined && (tick + i) % 17 == 0 { continue }
                let pulse = (i % 3 == 0 && tick < 450) ? 80 + i % 40 : nil
                let sample = try XCTUnwrap(AudienceSample(serial: UInt32(tick), steps: steps[i],
                                             effort: Double((tick + i) % 21) / 20, pulse: pulse))
                XCTAssertTrue(floor.receive(sample, from: id, at: time))
                if (tick + i) % 19 == 0 { XCTAssertFalse(floor.receive(sample, from: id, at: time + 0.001)) }
                if tick > 0 && !rejoined { expectedSteps += Int(steps[i] - delivered[i]) }
                latestEffort[i] = sample.effort
                latestPulse[i] = sample.pulse
                delivered[i] = steps[i]
            }
            let included = (301..<307).contains(tick) ? Array(1..<96) : Array(0..<96)
            let result = floor.drain(at: time)
            XCTAssertEqual(result.steps, expectedSteps, "tick \(tick)")
            XCTAssertEqual(result.count, included.count)
            XCTAssertEqual(result.effort, included.map { latestEffort[$0] }.reduce(0, +) / Double(included.count), accuracy: 1e-10)
            let pulses = included.compactMap { latestPulse[$0] }
            if pulses.isEmpty { XCTAssertNil(result.pulse) }
            else { XCTAssertEqual(try XCTUnwrap(result.pulse), Double(pulses.reduce(0, +)) / Double(pulses.count), accuracy: 1e-10) }
            total += result.steps
        }
        XCTAssertGreaterThan(total, 17_000)
        let cleared = floor.drain(at: Double(frames) / 5 + floor.timeout)
        XCTAssertEqual(cleared.count, 0)
        XCTAssertEqual(cleared.steps, 0)
        XCTAssertEqual(cleared.effort, 0)
        XCTAssertNil(cleared.pulse)
    }

    func testWatchChoosesExactlyOneAvailableRoute() {
        for phone in [false, true] {
            for stage in [false, true] {
                XCTAssertEqual(AudienceWatchRoute.pairedPhone.ready(phoneReachable: phone, stageJoined: stage), phone)
                XCTAssertEqual(AudienceWatchRoute.nearbyStage.ready(phoneReachable: phone, stageJoined: stage), stage)
            }
        }
    }
}
