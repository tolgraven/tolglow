import XCTest

final class WatchAudienceUITests: XCTestCase {
    func testHeartSharingConsentExplainsSavedShowAverages() {
        let app = XCUIApplication()
        app.launch()
        let floor = app.buttons["dance-floor"]
        XCTAssertTrue(floor.waitForExistence(timeout: 12))
        floor.tap()
        let pulse = app.switches["Share heart rate"]
        for _ in 0..<4 where !pulse.isHittable { app.swipeUp() }
        XCTAssertTrue(pulse.isHittable)
        pulse.tap()
        let consent = app.staticTexts["pulse-consent"]
        XCTAssertTrue(consent.waitForExistence(timeout: 5))
        XCTAssertTrue(consent.label.contains("saved show")); XCTAssertTrue(consent.label.contains("only you"))
        for _ in 0..<3 where !consent.isHittable { app.swipeUp() }
        capture("04-pulse-consent", app)
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

    private func capture(_ name: String, _ app: XCUIApplication) {
        let image = XCTAttachment(screenshot: app.screenshot())
        image.name = name
        image.lifetime = .keepAlways
        add(image)
    }
}
