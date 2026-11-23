import XCTest
@testable import TolglowSensorCore

final class ReceiverPlaneTests: XCTestCase {
    func testTranslatedRotatedBoundaryUsesMetricOrientedBounds() throws {
        let center = SIMD2<Float>(3, -4), u = SIMD2<Float>(0.8, 0.6), v = SIMD2<Float>(-0.6, 0.8)
        let points = [center - 2*u - v, center + 2*u - v, center + 2*u + v, center - 2*u + v]
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
