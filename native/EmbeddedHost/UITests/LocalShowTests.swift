import XCTest

final class LocalShowTests: XCTestCase {
    private func record(_ name: String) {
        let proof = XCTAttachment(screenshot: XCUIScreen.main.screenshot())
        proof.name = name
        proof.lifetime = .keepAlways
        add(proof)
    }

    func testRunStopAndRestartTheLocalShow() throws {
        let app = XCUIApplication()
        app.launch()
        let studio = app.descendants(matching: .any).matching(NSPredicate(format: "label == %@", "Studio")).firstMatch
        XCTAssertTrue(studio.waitForExistence(timeout: 20))
        studio.tap()
        app.buttons["On this device"].tap()
        for cycle in 1...2 {
            let open = app.buttons["Open included show"]
            XCTAssertTrue(open.waitForExistence(timeout: 20))
            record("start-\(cycle)")
            open.tap()
            let page = app.webViews.firstMatch
            XCTAssertTrue(page.waitForExistence(timeout: 80))
            XCTAssertTrue(page.descendants(matching: .any).matching(NSPredicate(format: "label CONTAINS[c] %@", "Ember"))
                .firstMatch.waitForExistence(timeout: 60))
            record("running-\(cycle)")
            app.buttons["Stop show"].tap()
            XCTAssertTrue(app.staticTexts["Local show stopped."].waitForExistence(timeout: 20))
        }
        XCTAssertTrue(app.buttons["Open included show"].waitForExistence(timeout: 20))
        record("closed")
    }
}
