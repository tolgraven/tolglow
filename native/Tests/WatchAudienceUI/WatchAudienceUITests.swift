import XCTest

final class WatchAudienceUITests: XCTestCase {
    func testHeartSharingConsentExplainsSavedShowAverages() {
        let app = XCUIApplication()
        app.launch()
        let floor = app.buttons["dance-floor"]
        XCTAssertTrue(floor.waitForExistence(timeout: 12))
        floor.tap()
        let pulse = app.buttons["choose-pulse"]
        for _ in 0..<4 where !pulse.isHittable { app.swipeUp() }
        XCTAssertTrue(pulse.isHittable)
        pulse.tap()
        let consent = app.staticTexts["pulse-consent"]
        XCTAssertTrue(consent.waitForExistence(timeout: 5))
        XCTAssertTrue(consent.label.contains("saved show"))
        XCTAssertTrue(consent.label.contains("only you"))
        capture("04-pulse-consent", app)
        let confirm = app.buttons["confirm-pulse"]
        for _ in 0..<4 where !confirm.isHittable { app.swipeUp() }
        XCTAssertTrue(confirm.isHittable)
        confirm.tap()
        let workout = app.buttons["Start dance workout"]
        for _ in 0..<4 where !workout.isHittable { app.swipeUp() }
        XCTAssertTrue(workout.isHittable)
    }

    func testChooseSoloStageAndReturnToPairedMode() {
        let app = XCUIApplication()
        app.launch()
        let floor = app.buttons["dance-floor"]
        XCTAssertTrue(floor.waitForExistence(timeout: 12))
        floor.tap()
        capture("01-paired-phone", app)
        let stage = app.buttons["route-direct"]
        XCTAssertTrue(stage.waitForExistence(timeout: 5))
        stage.tap()
        let search = app.buttons["Find stages"]
        // Scroll the real Watch view into the enrollment controls.
        if !search.isHittable { app.swipeUp() }
        XCTAssertTrue(search.waitForExistence(timeout: 5))
        capture("02-stage-picker", app)
        let join = app.buttons["Join with motion"]
        for _ in 0..<3 where !join.isHittable { app.swipeUp() }
        XCTAssertTrue(join.exists)
        XCTAssertFalse(join.isEnabled)
        app.swipeDown()
        let paired = app.buttons["route-paired"]
        if !paired.isHittable { app.swipeDown() }
        paired.tap()
        XCTAssertTrue(app.staticTexts["Your Watch and iPhone share one place on the floor."].exists)
        capture("03-paired-again", app)
    }

    func testCancelInterruptedStage() {
        let app = XCUIApplication()
        app.launchArguments = ["--preview-audience-reconnection"]
        app.launch()
        let floor = app.buttons["dance-floor"]
        guard floor.waitForExistence(timeout: 12) else {
            XCTFail("Dance floor is missing from the Watch home screen")
            return
        }
        let nearbyStage = app.buttons["route-direct"]
        floor.tap()
        // A cold simulator can drop the first tap. Retry only while the
        // home-screen link is still visible and can receive the tap.
        if !nearbyStage.waitForExistence(timeout: 4), floor.isHittable {
            floor.tap()
        }
        guard nearbyStage.waitForExistence(timeout: 5) else {
            XCTFail("The Watch did not navigate to Dance floor")
            return
        }
        let message = app.staticTexts["Reconnecting to West floor · 1 of 3"]
        let cancel = app.buttons["Cancel reconnection"]
        XCTAssertTrue(message.waitForExistence(timeout: 5))
        for _ in 0..<3 where !cancel.isHittable { app.swipeUp() }
        guard cancel.isHittable else {
            XCTFail("Cancel reconnection is not available in Dance floor")
            return
        }
        capture("04-interrupted-stage", app)
        cancel.tap()
        let find = app.buttons["Find stages"]
        for _ in 0..<3 where !find.isHittable { app.swipeDown() }
        XCTAssertTrue(find.waitForExistence(timeout: 5))
        XCTAssertFalse(cancel.exists)
        capture("05-reconnection-cancelled", app)
    }

    private func capture(_ name: String, _ app: XCUIApplication) {
        let image = XCTAttachment(screenshot: app.screenshot())
        image.name = name
        image.lifetime = .keepAlways
        add(image)
    }
}
