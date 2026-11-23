import XCTest
@testable import TolglowSensorCore

final class AudienceRecoveryTests: XCTestCase {
    func testOnlyPreviouslyJoinedStageCanResumeEvenAfterBrowsingWithoutQR() throws {
        var radio = AudienceRecovery()
        let id = UUID()
        XCTAssertNil(radio.retry(at: 10))
        XCTAssertFalse(radio.joined(id, fingerprint: "stage-b", name: "Floor", invitation: "stage-a"))
        XCTAssertTrue(radio.joined(id, fingerprint: "stage-a", name: "Floor", invitation: nil))
        let first = try XCTUnwrap(radio.retry(at: 10))
        XCTAssertEqual(first.peripheral, id)
        XCTAssertEqual(first.status, "Reconnecting to Floor · 1 of 3")
        XCTAssertFalse(radio.canStart(first, at: 10.49))
        XCTAssertTrue(radio.canStart(first, at: 10.5))
        XCTAssertFalse(radio.canStart(first, at: first.end))
        XCTAssertFalse(radio.accepts(peripheral: UUID(), fingerprint: "stage-a", invitation: nil))
        XCTAssertFalse(radio.joined(id, fingerprint: "stage-b", name: "Floor", invitation: nil))
        XCTAssertFalse(radio.joined(id, fingerprint: "stage-a", name: "Floor", invitation: "other-room"))
        XCTAssertTrue(radio.joined(id, fingerprint: "stage-a", name: "Floor", invitation: "stage-a"))
        XCTAssertFalse(radio.canStart(first, at: 11))
        XCTAssertEqual(radio.retry(at: 12)?.number, 1)
    }

    func testRetriesRespectDeadlineAndCancellationInvalidatesScheduledCallbacks() throws {
        var radio = AudienceRecovery()
        let id = UUID()
        radio.joined(id, fingerprint: "stage", name: "Floor", invitation: nil)
        let one = try XCTUnwrap(radio.retry(at: 0))
        let two = try XCTUnwrap(radio.retry(at: one.end))
        let three = try XCTUnwrap(radio.retry(at: two.end))
        XCTAssertEqual([one.begin, two.begin, three.begin], [0.5, 7.5, 15.5])
        XCTAssertEqual(three.end, 21.5)
        XCTAssertFalse(radio.canStart(one, at: two.begin))
        XCTAssertNil(radio.retry(at: three.end))
        XCTAssertNil(radio.retry(at: 100))

        for interrupt in ["leave", "background", "radio-off"] {
            radio.joined(id, fingerprint: "stage", name: "Floor", invitation: nil)
            let pending = try XCTUnwrap(radio.retry(at: 50))
            radio.cancel()
            XCTAssertFalse(radio.canStart(pending, at: pending.begin), interrupt)
            XCTAssertNil(radio.retry(at: pending.end), interrupt)
        }
        radio.joined(id, fingerprint: "stage", name: "Floor", invitation: nil)
        _ = radio.retry(at: 5)
        XCTAssertNil(radio.retry(at: 30), "a delayed foreground callback cannot extend the window")
    }

    func testInvitationContinuityForAnEstablishedRoomAndAnInitialPinnedScan() {
        let id = UUID()
        var radio = AudienceRecovery()
        XCTAssertTrue(radio.keepsSession(for: "stage", currentInvitation: "stage"))
        XCTAssertFalse(radio.keepsSession(for: "elsewhere", currentInvitation: "stage"))
        XCTAssertFalse(radio.keepsSession(for: nil, currentInvitation: "stage"))
        radio.joined(id, fingerprint: "browsed", name: "Floor", invitation: nil)
        XCTAssertTrue(radio.keepsSession(for: "browsed", currentInvitation: nil))
        XCTAssertFalse(radio.keepsSession(for: "renamed-stage", currentInvitation: nil))
        XCTAssertFalse(radio.keepsSession(for: nil, currentInvitation: nil))
    }

    func testDropAndResumedHandshakeDoNotReplayStepsFromTheGap() throws {
        let id = UUID()
        var radio = AudienceRecovery()
        var floor = AudienceFloor()
        func sample(_ serial: UInt32, _ steps: UInt32) throws -> AudienceSample {
            try XCTUnwrap(AudienceSample(serial: serial, steps: steps, effort: 0.5))
        }
        radio.joined(id, fingerprint: "pin", name: "Dance floor", invitation: nil)
        XCTAssertTrue(floor.receive(try sample(1, 400), from: id, at: 0))
        XCTAssertTrue(floor.receive(try sample(2, 401), from: id, at: 0.5))
        let before = floor.drain(at: 0.5)
        XCTAssertEqual(before.steps, 1)
        print("RECOVERY 0.5 | Sharing | Active guests \(before.count) | Live steps \(before.steps)")
        floor.leave(id)
        let attempt = try XCTUnwrap(radio.retry(at: 1))
        print("RECOVERY 1.0 | \(attempt.status) | Active guests \(floor.drain(at: 1).count) | Samples discarded")
        XCTAssertTrue(radio.canStart(attempt, at: 1.6))
        XCTAssertTrue(radio.joined(id, fingerprint: "pin", name: "Dance floor", invitation: nil))
        let baseline = try sample(12, 408)
        XCTAssertTrue(floor.receive(baseline, from: id, at: 2))
        let resumed = floor.drain(at: 2)
        XCTAssertEqual(resumed.steps, 0)
        print("RECOVERY 2.0 | Stage key verified | Active guests \(resumed.count) | Resumed at step \(baseline.steps)")
        XCTAssertFalse(floor.receive(baseline, from: id, at: 2.1))
        print("RECOVERY 2.1 | Duplicate dropped | Active guests \(floor.drain(at: 2.1).count) | No step emitted")
        XCTAssertTrue(floor.receive(try sample(13, 409), from: id, at: 2.5))
        let levels = floor.drain(at: 2.5)
        XCTAssertEqual(levels.steps, 1)
        XCTAssertEqual(levels.count, 1)
        print("RECOVERY 2.5 | Sharing | Active guests \(levels.count) | New live steps \(levels.steps)")
    }
}
