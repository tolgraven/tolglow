import Foundation
import XCTest
@testable import TolglowSensorCore

final class AudienceLatencyTests: XCTestCase {
    private func sample(_ serial: UInt32, _ steps: UInt32) -> AudienceSample {
        AudienceSample(serial: serial, steps: steps, effort: 0.5, pulse: 100)!
    }

    private func record(_ rows: [[String: Double]], name: String) throws {
        guard let directory = ProcessInfo.processInfo.environment["TOLGLOW_AUDIENCE_TIMING_DIR"] else { return }
        let url = URL(fileURLWithPath: directory).appendingPathComponent(name + ".json")
        try JSONEncoder().encode(rows).write(to: url, options: .atomic)
    }

    // Round-trip each frame through the production OSC codec. The virtual clock
    // advances at known phase offsets, independently of host or BLE scheduling.
    private func deliver(_ frames: inout AudienceOnsetFrames, at time: Double) throws -> [Observation] {
        frames.openFrame(at: time)
        var packets = [Observation]()
        while frames.ready {
            let count = frames.takeBatch()
            XCTAssertTrue((1...8).contains(count))
            let packet = try Observation(deviceID: "audience", sessionID: "show",
                sampleID: Int64(packets.count), timeMS: time * 1000, values: ["dance-steps": Double(count)],
                gestures: (0..<count).map { _ in Gesture(type: "dance-step") })
            packets.append(try SensorOSC.decode(SensorOSC.encode(packet)))
        }
        return packets
    }

    func testOneDancerAtEveryPhaseFromDetectedPeakToEncodedOSC() throws {
        var meter = DanceMeter(), floor = AudienceFloor(), frames = AudienceOnsetFrames()
        let id = UUID()
        XCTAssertTrue(floor.receive(sample(0, 0), from: id, at: 0.9))
        var delays = [Double]()
        var rows = [[String: Double]]()
        for phase in 0..<25 {
            let onset = 1 + Double(phase) * 0.5 + (Double(phase) + 0.1) / 1000
            meter.observe(strength: 0, at: onset - 0.01)
            meter.observe(strength: 1, at: onset)
            XCTAssertEqual(meter.steps, UInt32(phase + 1))
            let update = sample(UInt32(phase + 1), meter.steps)
            XCTAssertTrue(floor.receive(update, from: id, at: onset))
            try frames.append(floor.takeSteps())
            XCTAssertFalse(floor.receive(update, from: id, at: onset + 0.0001))
            let time = AudienceOnsetFrames.nextBoundary(after: onset)
            let packets = try deliver(&frames, at: time)
            let receipt = try XCTUnwrap(packets.first)
            XCTAssertEqual(packets.count, 1)
            XCTAssertEqual(receipt.gestures.map(\.type), ["dance-step"])
            delays.append(receipt.timeMS - onset * 1000)
            rows.append(["detected_ms": onset * 1000, "osc_ms": receipt.timeMS, "phase_ms": Double(phase) + 0.1])
            XCTAssertEqual(floor.drain(at: time).steps, 0)
            XCTAssertTrue(try deliver(&frames, at: time).isEmpty)
        }
        try record(rows, name: "solo")
        delays.sort()
        XCTAssertEqual(delays[12], 12.9, accuracy: 0.00001)
        XCTAssertEqual(delays[23], 23.9, accuracy: 0.00001)
        XCTAssertEqual(delays[24], 24.9, accuracy: 0.00001)
        print(String(format: "Audience detection to encoded OSC, 25 phases: p50=%.1f ms, p95=%.1f ms, max=%.1f ms", delays[12], delays[23], delays[24]))
    }

    func testSimultaneousOnsetsAcrossNinetySixGuestsAndFourPacketsPerFrame() throws {
        var floor = AudienceFloor(), frames = AudienceOnsetFrames()
        let ids = (0..<96).map { UUID(uuidString: String(format: "00000000-0000-0000-0000-%012d", $0))! }
        let onset = 1.0001
        for id in ids {
            XCTAssertTrue(floor.receive(sample(0, 0), from: id, at: 0.9))
            XCTAssertTrue(floor.receive(sample(1, 1), from: id, at: onset))
            try frames.append(floor.takeSteps())
        }
        var unique = Set<String>()
        var rows = [[String: Double]]()
        for tick in 1...3 {
            let time = Double(40 + tick) / 40
            let packets = try deliver(&frames, at: time)
            XCTAssertEqual(packets.count, 4)
            XCTAssertEqual(packets.map { $0.gestures.count }, [8, 8, 8, 8])
            unique.formUnion(packets.flatMap { $0.gestures.map(\.id) })
            rows.append(["detected_ms": onset * 1000, "osc_ms": time * 1000, "delivered": Double(unique.count)])
            XCTAssertTrue(try deliver(&frames, at: time).isEmpty)
        }
        try record(rows, name: "crowd")
        XCTAssertEqual(unique.count, ids.count)
        XCTAssertEqual(floor.drain(at: 1.2).steps, 0)
        XCTAssertEqual(frames.pending, 0)
        print("Audience burst of 96 onsets: 32 per frame; first OSC at 24.9 ms, final OSC at 74.9 ms")
    }

    func testRejoinExpiryReplayAndBoundedBacklog() throws {
        var floor = AudienceFloor(), frames = AudienceOnsetFrames()
        let id = UUID()
        XCTAssertTrue(floor.receive(sample(9, 23), from: id, at: 0))
        XCTAssertEqual(floor.takeSteps(), 0)
        XCTAssertFalse(floor.receive(sample(8, 22), from: id, at: 0.01))
        floor.leave(id)
        XCTAssertTrue(floor.receive(sample(0, 30), from: id, at: 1))
        XCTAssertEqual(floor.takeSteps(), 0)
        XCTAssertTrue(floor.receive(sample(1, 31), from: id, at: 1.30))
        try frames.append(floor.takeSteps())
        XCTAssertEqual(try deliver(&frames, at: 1.325).flatMap(\.gestures).count, 1)
        XCTAssertEqual(floor.drain(at: 4).count, 0)
        XCTAssertTrue(floor.receive(sample(3, 41), from: id, at: 5))
        XCTAssertEqual(floor.takeSteps(), 0)
        try frames.append(2_048)
        XCTAssertThrowsError(try frames.append(1))
        XCTAssertEqual(frames.pending, 2_048)
    }
}
