import Foundation
import XCTest
@testable import TolglowSensorCore

final class AudienceGATTTests: XCTestCase {
    typealias Fragment = AudienceGATTWrite.Fragment
    let id = UUID()

    func fragment(_ offset: Int, _ value: Data?, field: AudienceGATTWrite.Field? = .hello, central: UUID? = nil) -> Fragment {
        Fragment(central: central ?? id, field: field, offset: offset, value: value)
    }

    func testWholeAndSegmentedWritesAcrossAllLegalPayloadSizes() throws {
        var cases = [[String: Any]]()
        for (field, count) in [(AudienceGATTWrite.Field.hello, 65), (.samples, 39), (.samples, 63)] {
            let bytes = Data((0..<count).map(UInt8.init))
            for mtu in [1, 7, 17, 20, 27, count] {
                let parts = stride(from: 0, to: count, by: mtu).map { offset in
                    fragment(offset, bytes.subdata(in: offset..<min(offset + mtu, count)), field: field)
                }
                var inspected = 0
                let joined = try AudienceGATTWrite.assemble(parts) { inspected += 1; return $0 }
                XCTAssertEqual(inspected, parts.count)
                XCTAssertEqual(joined.central, id)
                XCTAssertEqual(joined.field, field)
                XCTAssertEqual(joined.value, bytes)
                cases.append(["length": joined.value.count, "width": mtu, "fragments": parts.compactMap { $0.value?.count }])
            }
            print("GATT byte identity verified for \(count)-byte payload, 1/7/17/20/27-byte fragments and a single write")
        }
        if let out = ProcessInfo.processInfo.environment["TOLGLOW_AUDIENCE_GATT_DIR"] {
            let url = URL(fileURLWithPath: out).appendingPathComponent("gatt-traces.json")
            try JSONSerialization.data(withJSONObject: cases, options: [.sortedKeys]).write(to: url, options: .atomic)
        }
    }

    func testInvalidTransactionsNeverReachTheCommitBoundary() {
        let first = fragment(0, Data(repeating: 1, count: 20))
        let tail = Data(repeating: 2, count: 45)
        let cases: [([Fragment], AudienceGATTWrite.Failure)] = [
            ([], .length), ([first], .length), ([fragment(1, tail)], .offset),
            ([first, fragment(21, tail)], .offset), ([first, fragment(19, tail)], .offset),
            ([first, first], .offset), ([first, fragment(20, nil)], .length),
            ([first, fragment(20, Data())], .length), ([first, fragment(20, tail, central: UUID())], .mixed),
            ([first, fragment(20, tail, field: .samples)], .mixed),
            ([fragment(0, tail, field: nil)], .unsupported), ([fragment(0, Data(count: 66))], .length),
            ([first, fragment(20, Data(count: 46))], .length),
            ([fragment(0, Data(count: 40), field: .samples)], .length),
            ([fragment(0, Data(count: 64), field: .samples)], .length),
            (Array(repeating: first, count: 66), .length)
        ]
        var commits = 0
        for (parts, expected) in cases {
            do {
                _ = try AudienceGATTWrite.assemble(parts, inspect: { $0 })
                commits += 1
                XCTFail("Unexpectedly accepted batch")
            } catch { XCTAssertEqual(error as? AudienceGATTWrite.Failure, expected) }
        }
        XCTAssertEqual(commits, 0)
    }

    func testRefusedStepCapacityDoesNotAdvanceTheParticipantOrSerial() throws {
        var floor = AudienceFloor()
        let baseline = try XCTUnwrap(AudienceSample(serial: 1, steps: 0, effort: 0.2))
        let onset = try XCTUnwrap(AudienceSample(serial: 2, steps: 1, effort: 0.8))
        XCTAssertTrue(floor.receive(baseline, from: id, at: 0))
        XCTAssertFalse(floor.receive(onset, from: id, at: 0.4, reserveSteps: { _ in false }))
        let old = floor.drain(at: 0.4)
        XCTAssertEqual(old.effort, 0.2)
        XCTAssertEqual(old.steps, 0)
        XCTAssertTrue(floor.receive(onset, from: id, at: 0.4))
        XCTAssertEqual(floor.drain(at: 0.4).steps, 1)
    }
}
