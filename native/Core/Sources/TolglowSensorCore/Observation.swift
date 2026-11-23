import Foundation

public enum SensorError: Error, Equatable {
    case invalid(String)
    case packetTooLarge
}

/// An observation, not a lighting command. Host time and musical position are
/// assigned by the show owner after admission. Device time is monotonic metadata.
public struct Gesture: Codable, Equatable, Sendable {
    public let id: String
    public let type: String
    public let phase: String
    public init(id: String = UUID().uuidString, type: String = "primary-action") {
        self.id = id; self.type = type; phase = "pulse"
    }
}

public struct Observation: Codable, Equatable, Sendable {
    public let version: Int
    public let deviceID: String
    public let sessionID: String
    public let sampleID: Int64
    public let timeMS: Double
    public let values: [String: Double]
    public let gestures: [Gesture]
    public let referenceFrame: String?
    public let calibrationID: String?
    /// Set only by the paired iPhone boundary. Never persist in show data or logs.
    public let pairingToken: String?

    enum CodingKeys: String, CodingKey {
        case version, values, gestures
        case deviceID = "device-id", sessionID = "session-id", sampleID = "sample-id"
        case timeMS = "time-ms", referenceFrame = "reference-frame"
        case calibrationID = "calibration-id", pairingToken = "pairing-token"
    }

    public init(deviceID: String, sessionID: String, sampleID: Int64, timeMS: Double,
                values: [String: Double], gestures: [Gesture] = [],
                referenceFrame: String? = nil, calibrationID: String? = nil,
                pairingToken: String? = nil) throws {
        version = 1; self.deviceID = deviceID; self.sessionID = sessionID
        self.sampleID = sampleID; self.timeMS = timeMS; self.values = values
        self.gestures = gestures; self.referenceFrame = referenceFrame
        self.calibrationID = calibrationID; self.pairingToken = pairingToken
        try validate()
    }

    public func validate() throws {
        func bounded(_ s: String, _ limit: Int) -> Bool {
            !s.isEmpty && s.utf8.count <= limit && !s.unicodeScalars.contains(where: { $0.value < 32 })
        }
        guard version == 1, bounded(deviceID, 128), bounded(sessionID, 128),
              sampleID >= 0, sampleID <= 9_007_199_254_740_991,
              timeMS.isFinite, timeMS >= 0,
              values.count <= 32, values.allSatisfy({ $0.key.range(of: "^[a-z][a-z0-9-]{0,63}$", options: .regularExpression) != nil && $0.value.isFinite }),
              gestures.count <= 8,
              gestures.allSatisfy({ bounded($0.id, 128) && ["primary-action", "dance-step"].contains($0.type) && $0.phase == "pulse" }),
              Set(gestures.map(\.id)).count == gestures.count,
              referenceFrame.map({ bounded($0, 128) }) ?? true,
              calibrationID.map({ bounded($0, 128) }) ?? true,
              pairingToken.map({ bounded($0, 512) && $0.count >= 16 }) ?? true
        else { throw SensorError.invalid("Malformed sensor observation") }
    }

    public func authenticated(with token: String) throws -> Observation {
        try Observation(deviceID: deviceID, sessionID: sessionID, sampleID: sampleID,
                        timeMS: timeMS, values: values, gestures: gestures,
                        referenceFrame: referenceFrame, calibrationID: calibrationID,
                        pairingToken: token)
    }

    public func json() throws -> Data {
        try validate()
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.sortedKeys, .withoutEscapingSlashes]
        return try encoder.encode(self)
    }

    public static func decode(_ data: Data) throws -> Observation {
        guard data.count <= 8192 else { throw SensorError.packetTooLarge }
        let result = try JSONDecoder().decode(Observation.self, from: data)
        try result.validate()
        return result
    }
}
