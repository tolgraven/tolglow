import XCTest

final class LocalShowTests: XCTestCase {
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
            open.tap()
            let page = app.webViews.firstMatch
            XCTAssertTrue(page.waitForExistence(timeout: 80))
            XCTAssertTrue(page.descendants(matching: .any).matching(NSPredicate(format: "label CONTAINS[c] %@", "Cues"))
                .firstMatch.waitForExistence(timeout: 60))
            let proof = XCTAttachment(screenshot: XCUIScreen.main.screenshot())
            proof.name = "Local show on iPad \(cycle)"
            proof.lifetime = .keepAlways
            add(proof)
            app.buttons["Stop show"].tap()
        }
        XCTAssertTrue(app.buttons["Open included show"].waitForExistence(timeout: 20))
    }
}
