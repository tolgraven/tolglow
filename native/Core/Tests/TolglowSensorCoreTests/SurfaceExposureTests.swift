import XCTest
@testable import TolglowSensorCore

final class SurfaceExposureTests: XCTestCase {
    @MainActor func testSyntheticCameraCarriesPresentationReceiptsAcrossBothExposures() async throws {
        var events = [String](), position = 0
        try await SurfaceExposure.pair(present: { phase in
            events.append(phase); return phase + "-receipt"
        }, frames: {
            var meter = PhotometryFrame(transfer: .linear)
            let level = UInt8(position == 0 ? 10 : 80)
            for _ in 0..<256 { meter.sample(red: level, green: level, blue: level) }
            return Array(repeating: try meter.luminance(), count: 15)
        }, verifyStance: { events.append("steady") }, record: { index, receipt, levels in
            XCTAssertEqual(index, position)
            XCTAssertEqual(levels[0], Double(index == 0 ? 10 : 80) / 255, accuracy: 1e-6)
            position += 1; events.append(receipt)
        })
        XCTAssertEqual(events, ["dark", "steady", "steady", "dark-receipt", "white", "steady", "steady", "white-receipt"])
    }
    @MainActor func testUnsteadyExposureIsNeverSubmitted() async {
        var presented = [String](), submitted = [Int]()
        do {
            try await SurfaceExposure.pair(present: { phase in presented.append(phase); return phase },
                frames: { (0..<15).map { $0.isMultiple(of: 2) ? 0.2 : 0.6 } },
                verifyStance: {}, record: { i, _, _ in submitted.append(i) })
            XCTFail("Expected a flicker failure")
        } catch { XCTAssertTrue(submitted.isEmpty); XCTAssertEqual(presented, ["dark"]) }
    }
}
