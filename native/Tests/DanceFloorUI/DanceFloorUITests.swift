import XCTest

final class DanceFloorUITests: XCTestCase {
    func testBrowseAfterValidInvitationShowsBluetoothFailure() {
        checkBrowseFailure(invitation: "invited")
    }

    func testBrowseAfterInvalidInvitationShowsBluetoothFailure() {
        checkBrowseFailure(invitation: "invalid")
    }

    private func checkBrowseFailure(invitation: String) {
        let app = XCUIApplication()
        app.launchArguments = ["-design-state", invitation, "-simulate-bluetooth-off"]
        app.launch()
        let browse = app.buttons["Browse without this invitation"]
        XCTAssertTrue(browse.waitForExistence(timeout: 12))
        if !browse.isHittable { app.swipeUp() }
        browse.tap()

        let error = app.staticTexts["Allow Bluetooth and turn it on to join"]
        XCTAssertTrue(error.waitForExistence(timeout: 8))
        XCTAssertTrue(app.buttons["Find a stage"].exists)
        XCTAssertFalse(browse.exists)
        let capture = XCTAttachment(screenshot: app.screenshot())
        capture.name = "browse-failed-" + invitation
        capture.lifetime = .keepAlways
        add(capture)
    }
}
