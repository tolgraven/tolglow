import Foundation
import XCTest
@testable import TolglowSensorCore

final class StudioOriginTests: XCTestCase {
    func testCredentialChallengeMustMatchRealmAndVenue() throws {
        let venue = try XCTUnwrap(StudioOrigin(scheme: "https", host: "stage.local", port: 443))
        func accepts(_ host: String = "stage.local", _ scheme: String = "https", _ port: Int = 0,
                     _ realm: String = "Tolglow Studio", _ isProxy: Bool = false, _ failures: Int = 0) -> Bool {
            venue.acceptsOperatorChallenge(scheme: scheme, host: host, port: port, realm: realm,
                                           isProxy: isProxy, previousFailures: failures)
        }
        XCTAssertTrue(accepts())
        XCTAssertTrue(accepts("STAGE.LOCAL", "https", 443))
        XCTAssertFalse(accepts("other.local"))
        XCTAssertFalse(accepts("stage.local", "http", 443))
        XCTAssertFalse(accepts("stage.local", "https", 8500))
        XCTAssertFalse(accepts("stage.local", "https", 443, "Other"))
        XCTAssertFalse(accepts("stage.local", "https", 443, "Tolglow Studio", true))
        XCTAssertFalse(accepts("stage.local", "https", 443, "Tolglow Studio", false, 1))
    }

    func testCredentialScopeMatchesOnlySecureOriginAndEffectivePort() throws {
        let stage = try XCTUnwrap(StudioOrigin(url: XCTUnwrap(URL(string: "https://STAGE.local:443/api/state"))))
        XCTAssertEqual(stage, StudioOrigin(scheme: "https", host: "stage.local", port: 443))
        XCTAssertEqual(stage, StudioOrigin(scheme: "https", host: "stage.local", port: nil))
        XCTAssertNotEqual(stage, StudioOrigin(scheme: "https", host: "another.local", port: 443))
        XCTAssertNotEqual(stage.keychainAccount, StudioOrigin(scheme: "https", host: "stage.local", port: 8500)?.keychainAccount)
        XCTAssertNil(StudioOrigin(scheme: "http", host: "stage.local", port: 80))
        XCTAssertNil(StudioOrigin(url: try XCTUnwrap(URL(string: "https://user@stage.local"))))
        XCTAssertNotNil(StudioOrigin(scheme: "http", host: "127.0.0.1", port: 8500))
        XCTAssertEqual(StudioOrigin(scheme: "http", host: "[::1]", port: 8500), StudioOrigin(scheme: "http", host: "::1", port: 8500))
    }
}
