import XCTest
@testable import TolglowSensorCore

final class PairingDestinationTests: XCTestCase {
    func testPairingsAreDistinctPerHostAndPort() throws {
        let stage = try XCTUnwrap(PairingDestination(host: "stage.local", port: 9000))
        let other = try XCTUnwrap(PairingDestination(host: "other.local", port: 9000))
        let alternative = try XCTUnwrap(PairingDestination(host: "stage.local", port: 9001))
        XCTAssertNotEqual(stage.keychainService, other.keychainService)
        XCTAssertNotEqual(stage.keychainService, alternative.keychainService)
        XCTAssertEqual(stage, PairingDestination(host: " stage.local ", port: 9000))
        XCTAssertNil(PairingDestination(host: "  ", port: 9000))
        XCTAssertNil(PairingDestination(host: "stage.local", port: 0))
    }
}
