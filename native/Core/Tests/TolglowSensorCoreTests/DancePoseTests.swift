import XCTest
@testable import TolglowSensorCore

final class DancePoseTests: XCTestCase {
    typealias Point = DancePose.Point

    private func dancer(x: Double = 0.5, width: Double = 0.10, kick: Double = 0) -> DancePose {
        let hip = Point(x, 0.55)
        return DancePose(leftHip: hip + Point(-width / 2, 0), rightHip: hip + Point(width / 2, 0),
                         legs: [.leftKnee: hip + Point(-width / 2, -width),
                                .leftAnkle: hip + Point((-0.5 + kick) * width, -2 * width),
                                .rightKnee: hip + Point(width / 2, -width),
                                .rightAnkle: hip + Point(width / 2, -2 * width)])!
    }

    func testPasserbyAndReorderedDetectionsCannotTriggerSteps() {
        var tracker = DancePoseTracker()
        let watcher = dancer()
        XCTAssertEqual(tracker.strength([watcher], at: 0), 0)
        XCTAssertEqual(tracker.strength([dancer(x: 0.85, kick: 1), watcher], at: 0.1), 0, accuracy: 1e-8)
        XCTAssertEqual(tracker.strength([watcher, dancer(x: 0.84)], at: 0.2), 0, accuracy: 1e-8)
        XCTAssertEqual(tracker.strength([dancer(kick: 0.4)], at: 0.3), 1, accuracy: 1e-8)
    }

    func testImageTranslationAndDistance() {
        for width in [0.05, 0.10, 0.20] {
            var tracker = DancePoseTracker()
            _ = tracker.strength([dancer(width: width)], at: 0)
            XCTAssertEqual(tracker.strength([dancer(x: 0.52, width: width)], at: 0.1), 0, accuracy: 1e-8)
            XCTAssertEqual(tracker.strength([dancer(x: 0.54, width: width, kick: 0.2)], at: 0.2),
                           0.5, accuracy: 1e-8)
        }
    }

    func testCameraApproachAndRetreatDoNotCountAsSteps() {
        var tracker = DancePoseTracker()
        var meter = DanceMeter()
        for (frame, width) in [0.05, 0.06, 0.10, 0.13, 0.08, 0.06].enumerated() {
            let time = Double(frame) / 10
            let quiet = tracker.strength([dancer(width: width)], at: time)
            XCTAssertEqual(quiet, 0, accuracy: 1e-8)
            meter.observe(strength: quiet, at: time)
        }
        XCTAssertEqual(meter.steps, 0)
        let kick = tracker.strength([dancer(width: 0.08, kick: 0.4)], at: 0.6)
        meter.observe(strength: kick, at: 0.6)
        XCTAssertEqual(meter.steps, 1)
    }

    func testAmbiguityMissingLandmarksAndReacquisition() {
        var tracker = DancePoseTracker()
        _ = tracker.strength([dancer()], at: 1)
        XCTAssertEqual(tracker.strength([dancer(kick: 0.9), dancer(x: 0.55)], at: 1.1), 0)
        XCTAssertEqual(tracker.strength([dancer(kick: 0.9)], at: 1.2), 0)
        XCTAssertEqual(tracker.strength([], at: 1.3), 0)
        XCTAssertEqual(tracker.strength([dancer(x: 0.85)], at: 1.4), 0)
        XCTAssertEqual(tracker.strength([dancer(x: 0.85, kick: 0.9)], at: 2), 0)
        XCTAssertEqual(tracker.strength([dancer(x: 0.85)], at: 2.1), 1, accuracy: 1e-8)
        XCTAssertNil(DancePose(leftHip: .zero, rightHip: Point(0.1, 0), legs: [.leftKnee: .zero]))
        XCTAssertNil(DancePose(leftHip: .zero, rightHip: .zero, legs: [.leftKnee: .zero, .rightKnee: .zero]))
    }

    func testOnlyTheKickBecomesADanceStep() throws {
        var tracker = DancePoseTracker()
        var meter = DanceMeter()
        var rows = ["time,center,kick,other,strength,steps"]
        for frame in 0..<25 {
            let time = Double(frame) / 10
            let x = 0.45 + Double(min(frame, 6)) * 0.01
            let kick = (10...14).contains(frame) ? 0.65 : 0.0
            let crossing = (13...16).contains(frame)
            let otherX = crossing ? x + 0.04 : 0.87
            let subject = dancer(x: x, kick: kick)
            let other = dancer(x: otherX)
            let strength = tracker.strength(frame.isMultiple(of: 2) ? [subject, other] : [other, subject], at: time)
            meter.observe(strength: strength, at: time)
            rows.append("\(time),\(x),\(kick),\(otherX),\(strength),\(meter.steps)")
        }
        XCTAssertEqual(meter.steps, 1)
        if let path = ProcessInfo.processInfo.environment["TOLGLOW_POSE_TRACE"] {
            try rows.joined(separator: "\n").write(toFile: path, atomically: true, encoding: .utf8)
        }
    }
}
