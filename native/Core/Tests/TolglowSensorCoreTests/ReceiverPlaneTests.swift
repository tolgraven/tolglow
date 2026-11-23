import XCTest
@testable import TolglowSensorCore

final class ReceiverPlaneTests: XCTestCase {
    func testTranslatedRotatedBoundaryUsesMetricOrientedBounds() throws {
        let center = SIMD2<Float>(3, -4)
        let points: [SIMD2<Float>] = [SIMD2(2, -6), SIMD2(5.2, -3.6), SIMD2(4, -2), SIMD2(0.8, -4.4)]
        let plane = try XCTUnwrap(ReceiverPlane(boundary: points))
        XCTAssertEqual(plane.center.x, center.x, accuracy: 0.0001)
        XCTAssertEqual(plane.center.y, center.y, accuracy: 0.0001)
        XCTAssertEqual(min(plane.size.x, plane.size.y), 2, accuracy: 0.0001)
        XCTAssertEqual(max(plane.size.x, plane.size.y), 4, accuracy: 0.0001)
        for corner in points {
            XCTAssertTrue(plane.corners.contains { abs($0.x - corner.x) + abs($0.y - corner.y) < 0.0001 })
        }
    }

    func testUnusableBoundariesDoNotProduceReceivers() {
        XCTAssertNil(ReceiverPlane(boundary: []))
        XCTAssertNil(ReceiverPlane(boundary: [.zero, .zero, .zero]))
        XCTAssertNil(ReceiverPlane(boundary: [.zero, SIMD2(1, 0), SIMD2(2, 0)]))
        XCTAssertNil(ReceiverPlane(boundary: [.zero, SIMD2(1, 0), SIMD2(.nan, 2)]))
    }
}
