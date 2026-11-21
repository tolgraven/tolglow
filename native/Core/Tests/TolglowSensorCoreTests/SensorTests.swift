import XCTest
@testable import TolglowSensorCore

final class SensorTests: XCTestCase {
    func sample(_ value: Double = 0.5) throws -> Observation {
        try Observation(deviceID: "watch-a", sessionID: "session-a", sampleID: 7,
                        timeMS: 1250, values: ["roll": value], gestures: [Gesture(id: "tap-1")],
                        referenceFrame: "x-arbitrary-z-vertical", calibrationID: "neutral-a")
    }
    func testAtomicOSCRoundTripAndPadding() throws {
        let original = try sample()
        let data = try SensorOSC.encode(original)
        XCTAssertEqual(data.count % 4, 0)
        XCTAssertEqual(try SensorOSC.decode(data), original)
        XCTAssertEqual(String(data: data.prefix(19), encoding: .utf8), "/tolglow/sensor/v1\0")
        XCTAssertThrowsError(try SensorOSC.decode(data + Data([0, 0, 0, 0])))
        XCTAssertThrowsError(try SensorOSC.decode(Data(data.dropLast())))
    }
    func testMalformedDecodedDataCannotBypassValidation() throws {
        XCTAssertThrowsError(try Observation.decode(Data(#"{"version":2,"device-id":"w","session-id":"s","sample-id":1,"time-ms":2,"values":{},"gestures":[]}"#.utf8)))
        XCTAssertThrowsError(try sample(.infinity))
        XCTAssertThrowsError(try Observation(deviceID: "w", sessionID: "s", sampleID: -1, timeMS: 0, values: [:]))
        XCTAssertThrowsError(try Observation(deviceID: "w", sessionID: "s", sampleID: 1, timeMS: .nan, values: [:]))
    }
    func testPairingKeepsObservationIdentityAndIsNotImplicit() throws {
        let original = try sample()
        XCTAssertNil(original.pairingToken)
        let admitted = try original.authenticated(with: "test-only-token-123")
        XCTAssertEqual(admitted.sampleID, original.sampleID)
        XCTAssertEqual(admitted.sessionID, original.sessionID)
        XCTAssertEqual(admitted.gestures, original.gestures)
        XCTAssertEqual(try SensorOSC.decode(SensorOSC.encode(admitted)), admitted)
        XCTAssertThrowsError(try original.authenticated(with: ""))
    }
    func testQuaternionCalibrationAndUnits() throws {
        let angle = Double.pi / 3
        let pose = try Quaternion(x: 0, y: 0, z: sin(angle/2), w: cos(angle/2))
        XCTAssertEqual(try pose.relative(to: pose).eulerRadians["yaw"]!, 0, accuracy: 1e-12)
        XCTAssertEqual(pose.eulerRadians["yaw"]!, angle, accuracy: 1e-12)
        XCTAssertThrowsError(try Quaternion(x: 0, y: 0, z: 0, w: 0))
    }
    func testMappingIsBoundedAndExplicitlyInverted() throws {
        let mapping = try AxisMapping(axis: "roll", inputMinimum: -1, inputMaximum: 1,
                                      outputMinimum: 0, outputMaximum: 100, inverted: true)
        XCTAssertEqual(try mapping.value(for: -2), 100)
        XCTAssertEqual(try mapping.value(for: 0), 50)
        XCTAssertEqual(try mapping.value(for: 2), 0)
        XCTAssertThrowsError(try mapping.value(for: .infinity))
    }
    func testMailboxCoalescesMotionWithoutReorderingOrLosingGestures() throws {
        func observation(_ id: Int64, gesture: Bool = false) throws -> Observation {
            try Observation(deviceID: "w", sessionID: "s", sampleID: id, timeMS: Double(id),
                            values: ["roll": 0], gestures: gesture ? [Gesture(id: "g-\(id)")] : [])
        }
        var queue = ObservationMailbox(gestureCapacity: 2)
        XCTAssertEqual(try queue.offer(observation(1)), 0)
        XCTAssertEqual(try queue.offer(observation(2)), 1)
        XCTAssertEqual(try queue.offer(observation(3, gesture: true)), 1)
        try queue.offer(observation(4))
        XCTAssertEqual(try queue.offer(observation(5, gesture: true)), 1)
        XCTAssertThrowsError(try queue.offer(observation(6, gesture: true)))
        try queue.offer(observation(7))
        XCTAssertEqual(queue.count, 3)
        XCTAssertEqual(queue.take()?.sampleID, 3)
        XCTAssertEqual(queue.take()?.sampleID, 5)
        XCTAssertEqual(queue.take()?.sampleID, 7)
        XCTAssertNil(queue.take())
    }

    func testSharedHostWireFixture() throws {
        let directory = try XCTUnwrap(Bundle.module.url(forResource: "Fixtures", withExtension: nil))
        let json = try Data(contentsOf: directory.appendingPathComponent("observation.json"))
        let hex = try String(contentsOf: directory.appendingPathComponent("observation.osc.hex"), encoding: .utf8).trimmingCharacters(in: .whitespacesAndNewlines)
        let chars = Array(hex)
        let bytes = stride(from: 0, to: chars.count, by: 2).map { UInt8(String(chars[$0...$0+1]), radix: 16)! }
        let observation = try Observation.decode(json)
        XCTAssertEqual(try SensorOSC.decode(Data(bytes)), observation)
        XCTAssertEqual(try SensorOSC.decode(SensorOSC.encode(observation)), observation)
    }

}
